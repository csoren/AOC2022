open Batteries
open Extensions

let ls_output_line_to_entry line: (string * Directory.entry) =
  match line with
  | ("dir", name) -> (name, Directory [])
  | (size, name) -> (name, File (int_of_string size))

let rec ls_output_to_directory = function
  | s :: tail -> (String.split s ~by:" " |> ls_output_line_to_entry) :: (ls_output_to_directory tail)
  | [] -> [] 

let rec run_command current_dir (procs: Console.process list) =
  match procs with
  | (Cd name, _) :: tail -> 
      let (sub_dir, procs') = run_command (Directory.find_directory current_dir name) tail in
      (Directory.replace current_dir name (Directory sub_dir), procs')
  | (Ls, output) :: tail ->
      (ls_output_to_directory output, tail)
  | [] -> 
      (current_dir, procs)

module Part1 = struct
  let solve lines =
    let procs = Console.processes lines in
    ()    
end

let () =
  print_newline ()
