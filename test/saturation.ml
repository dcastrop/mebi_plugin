open Mebi_plugin

let dump (prefix : string) (m : Fsm.t) : unit =
  let output_path1 : string =
    Dump_to_file.run
      (Default ())
      (Some (Printf.sprintf "%s test_pre_sat" prefix), Auto ())
      (JSON ())
      (FSM m)
  in
  Utils.Logging.Log.normal
    ~params:(Utils.Params.Default.log ~mode:(OCaml ()) ())
    (Printf.sprintf "dumped file to: '%s'" output_path1);
  let output_path2 : string =
    Dump_to_file.run
      (Default ())
      (Some (Printf.sprintf "%s test_post_sat" prefix), Auto ())
      (JSON ())
      (FSM (Fsm.saturate m))
  in
  Utils.Logging.Log.normal
    ~params:(Utils.Params.Default.log ~mode:(OCaml ()) ())
    (Printf.sprintf "dumped file to: '%s'" output_path2)
;;

let m1 : Fsm.t =
  { init = Some (Mebi_wrapper.E.of_int 1, None)
  ; terminals =
      Model.States.of_list
        [ Mebi_wrapper.E.of_int 5, None; Mebi_wrapper.E.of_int 6, None ]
  ; alphabet =
      Model.Alphabet.of_list
        [ Mebi_wrapper.E.of_int 7, (Some "a", Some false)
        ; Mebi_wrapper.E.of_int 8, (Some "b", Some false)
        ; Mebi_wrapper.E.of_int 9, (Some "t", Some true)
        ; Mebi_wrapper.E.of_int 10, (Some "c", Some false)
        ; Mebi_wrapper.E.of_int 11, (Some "d", Some false)
        ]
  ; states =
      Model.States.of_list
        [ Mebi_wrapper.E.of_int 1, None
        ; Mebi_wrapper.E.of_int 2, None
        ; Mebi_wrapper.E.of_int 3, None
        ; Mebi_wrapper.E.of_int 4, None
        ; Mebi_wrapper.E.of_int 5, None
        ; Mebi_wrapper.E.of_int 6, None
        ]
  ; edges =
      Model.transition_list_to_edges
        [ ( (Mebi_wrapper.E.of_int 1, None)
          , (Mebi_wrapper.E.of_int 9, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 2, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 1, None)
          , (Mebi_wrapper.E.of_int 7, (Some "a", Some false))
          , (Mebi_wrapper.E.of_int 3, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 3, None)
          , (Mebi_wrapper.E.of_int 10, (Some "c", Some false))
          , (Mebi_wrapper.E.of_int 5, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 2, None)
          , (Mebi_wrapper.E.of_int 9, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 1, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 2, None)
          , (Mebi_wrapper.E.of_int 8, (Some "b", Some false))
          , (Mebi_wrapper.E.of_int 4, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 4, None)
          , (Mebi_wrapper.E.of_int 11, (Some "d", Some false))
          , (Mebi_wrapper.E.of_int 6, None)
          , None )
        ]
  ; info = None
  }
;;

let m2 : Fsm.t =
  { init = Some (Mebi_wrapper.E.of_int 1, None)
  ; terminals =
      Model.States.of_list
        [ Mebi_wrapper.E.of_int 5, None; Mebi_wrapper.E.of_int 6, None ]
  ; alphabet =
      Model.Alphabet.of_list
        [ Mebi_wrapper.E.of_int 7, (Some "a", Some false)
        ; Mebi_wrapper.E.of_int 8, (Some "b", Some false)
        ; Mebi_wrapper.E.of_int 9, (Some "c", Some false)
        ; Mebi_wrapper.E.of_int 10, (Some "d", Some false)
        ; Mebi_wrapper.E.of_int 11, (Some "e", Some false)
        ; Mebi_wrapper.E.of_int 12, (Some "f", Some false)
        ; Mebi_wrapper.E.of_int 13, (Some "g", Some false)
        ; Mebi_wrapper.E.of_int 14, (Some "t", Some true)
        ]
  ; states =
      Model.States.of_list
        [ Mebi_wrapper.E.of_int 1, None
        ; Mebi_wrapper.E.of_int 2, None
        ; Mebi_wrapper.E.of_int 3, None
        ; Mebi_wrapper.E.of_int 4, None
        ; Mebi_wrapper.E.of_int 5, None
        ; Mebi_wrapper.E.of_int 6, None
        ; Mebi_wrapper.E.of_int 7, None
        ]
  ; edges =
      Model.transition_list_to_edges
        [ ( (Mebi_wrapper.E.of_int 1, None)
          , (Mebi_wrapper.E.of_int 7, (Some "a", Some false))
          , (Mebi_wrapper.E.of_int 2, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 1, None)
          , (Mebi_wrapper.E.of_int 8, (Some "b", Some false))
          , (Mebi_wrapper.E.of_int 3, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 1, None)
          , (Mebi_wrapper.E.of_int 14, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 4, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 2, None)
          , (Mebi_wrapper.E.of_int 11, (Some "e", Some false))
          , (Mebi_wrapper.E.of_int 6, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 3, None)
          , (Mebi_wrapper.E.of_int 10, (Some "d", Some false))
          , (Mebi_wrapper.E.of_int 5, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 3, None)
          , (Mebi_wrapper.E.of_int 14, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 2, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 3, None)
          , (Mebi_wrapper.E.of_int 14, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 4, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 4, None)
          , (Mebi_wrapper.E.of_int 9, (Some "c", Some false))
          , (Mebi_wrapper.E.of_int 5, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 4, None)
          , (Mebi_wrapper.E.of_int 14, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 2, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 5, None)
          , (Mebi_wrapper.E.of_int 13, (Some "g", Some false))
          , (Mebi_wrapper.E.of_int 7, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 6, None)
          , (Mebi_wrapper.E.of_int 12, (Some "f", Some false))
          , (Mebi_wrapper.E.of_int 7, None)
          , None )
        ; ( (Mebi_wrapper.E.of_int 6, None)
          , (Mebi_wrapper.E.of_int 14, (Some "t", Some true))
          , (Mebi_wrapper.E.of_int 2, None)
          , None )
        ]
  ; info = None
  }
;;
