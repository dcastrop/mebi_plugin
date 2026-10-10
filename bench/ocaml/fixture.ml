(** Inputs read from fixture files: FSMs extracted by the plugin from the
    Rocq examples, written by [dump2fixture.py] from [MeBi Run Bisim]'s
    dumps. A fixture is one JSON object:
    {v
    { "name": "...", "source": "examples/...v", "command": "MeBi Run Bisim ...",
      "bisimilar": true, "silent": [0], "labels": { "0": "None", ... },
      "a": { "init": 2, "states": [2, ...], "edges": [[2, 3, 7], ...] },
      "b": { ... } }
    v}
    with states and labels as the plugin encoded them in that command, so
    the two sides share one encoding. [labels] is for reading only. *)

open Yojson.Safe.Util

let ints (j : Yojson.Safe.t) : int list = List.map to_int (to_list j)

let side (j : Yojson.Safe.t) : Input.side =
  { init = to_int (member "init" j)
  ; states = ints (member "states" j)
  ; edges =
      List.map
        (fun e ->
          match ints e with
          | [ f; l; g ] -> f, l, g
          | _ -> failwith "fixture: an edge is not [from, label, goto]")
        (to_list (member "edges" j))
  }
;;

(** [load path] is the fixture at [path].
    @raise Failure if it is not one. *)
let load (path : string) : Input.t =
  let j = Yojson.Safe.from_file path in
  try
    { name = to_string (member "name" j)
    ; silent = ints (member "silent" j)
    ; a = side (member "a" j)
    ; b = Some (side (member "b" j))
    ; bisimilar = Some (to_bool (member "bisimilar" j))
    }
  with
  | Type_error (msg, _) -> failwith (Printf.sprintf "%s: %s" path msg)
;;

(** [load_dir dir] is every [*.json] fixture in [dir], by file name. *)
let load_dir (dir : string) : Input.t list =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".json")
  |> List.sort String.compare
  |> List.map (fun f -> load (Filename.concat dir f))
;;
