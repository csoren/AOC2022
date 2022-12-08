open Batteries
open Extensions

let input =
  File.lines_of "puzzle-input" |> List.of_enum
  
let ls_output_line_to_entry = function
  | ("dir", name) -> (name, Directory.Directory [])
  | (size, name) -> (name, Directory.File (int_of_string size))

let ls_output_to_directory =
  List.map (fun s -> String.split s ~by:" " |> ls_output_line_to_entry)

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

let run_commands =
  run_command Directory.root %> fst

let rec size_of_dir l =
  let size_of_entry = function
    | Directory.File size -> size
    | Directory.Directory dir -> size_of_dir dir
  in
  List.map (snd %> size_of_entry) l |> List.sum

let rec dir_sizes (dirs: Directory.directory) =
  let dir_sizes' = function
    | (_, Directory.Directory files) -> size_of_dir files :: dir_sizes files
    | _ -> []
  in
  List.map dir_sizes' dirs |> List.flatten

let first sizes =
  sizes |> List.filter ((>=) 100000) |> List.sum

let second sizes =
  let free_at_least = (List.hd sizes) - 40000000 in
  List.filter ((<) free_at_least) sizes |> List.sort (Fun.flip (-)) |> List.last

let solve lines =
  let dirs = Console.processes lines |> run_commands in
  let sizes = dir_sizes dirs in
  (first sizes, second sizes)

let () =
  print_newline ();
  let (first, second) = solve input in
  Printf.printf "Part 1, sum of sizes: %d\n" (first);
  Printf.printf "Part 2, largest dir: %d\n" (second);
