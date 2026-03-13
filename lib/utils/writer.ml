let perm : int = 0o777
let default_dir : string = "./_dumps/"

let get_loc () : string =
  match Loc.get_current_command_loc () with
  | Some { fname = InFile { dirpath; file } } -> file
  | _ -> ""
;;

(** https://discuss.ocaml.org/t/how-to-create-a-new-file-while-automatically-creating-any-intermediate-directories/14837/5
*)
let rec create_parent_dir (fn : string) =
  let parent_dir = Filename.dirname fn in
  if not (Sys.file_exists parent_dir)
  then (
    create_parent_dir parent_dir;
    Sys.mkdir parent_dir perm)
;;

let file
      ?(dir : string = default_dir)
      (name : string)
      (f : 'a -> Yojson.t)
      (x : 'a)
  : unit
  =
  create_parent_dir dir;
  let filepath : string = Filename.concat dir name in
  f x |> Yojson.to_file filepath
;;
