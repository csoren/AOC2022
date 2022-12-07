open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum
  
let ls_output_line_to_entry line: (string * Directory.entry) =
  match line with
  | ("dir", name) -> (name, Directory [])
  | (size, name) -> (name, File (int_of_string size))

let rec ls_output_to_directory = function
  | s :: tail -> (String.split s ~by:" " |> ls_output_line_to_entry) :: (ls_output_to_directory tail)
  | [] -> [] 

let rec run_command current_dir (procs: Console.process list) =
  match procs with
  | (Cd "..", _) :: tail ->
      (current_dir, tail)
  | (Cd name, _) :: tail -> 
      let (sub_dir, procs') = run_command (Directory.find_directory current_dir name) tail in
      let files = Directory.replace current_dir name (Directory sub_dir) in
      run_command files procs'
  | (Ls, output) :: tail ->
      let files = ls_output_to_directory output |> Directory.append current_dir in
      run_command files tail
  | [] -> 
      (current_dir, procs)

let run_commands (procs: Console.process list) =
  run_command Directory.root procs |> fst

module Part1 = struct
  let rec size_of_entry (entry: Directory.entry) =
    match entry with
      | File size -> size
      | Directory dir -> size_of_dir dir
  and size_of_dir l =
    List.map (snd %> size_of_entry) l |> List.sum

  let rec dir_sizes (dirs: Directory.directory) =
    let dir_sizes' entry =
      match entry with
        | (name, Directory.Directory files) ->
            (name, size_of_dir files) :: dir_sizes files
        | _ -> []
    in
    List.map dir_sizes' dirs |> List.flatten

  let solve lines =
    let procs = Console.processes lines in
    let dirs = run_commands procs in
    let sizes = dir_sizes dirs in
    (*
    print_endline (Directory.to_string dirs);
    print_guess stdout sizes;
    *)
    List.map snd sizes |> List.filter (fun v -> v < 100000) |> List.sum

  let print () =
    Printf.printf "Part 1, sum of sizes: %d\n" (solve input)
end

let () =
  print_newline ();
  Part1.print ()
