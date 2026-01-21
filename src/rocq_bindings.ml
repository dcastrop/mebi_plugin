module Instructions = struct
  type t =
    | Undefined
    | Done
    | Arg of
        { root : Constr.t
        ; index : int
        ; cont : t
        }

  exception Rocq_bindings_CannotAppendDone of unit

  let rec append (x : t) : t -> t = function
    | Arg { root; index; cont } -> Arg { root; index; cont = append x cont }
    | Undefined -> x
    | Done -> raise (Rocq_bindings_CannotAppendDone ())
  ;;

  let rec length : t -> int = function
    | Undefined -> 0
    | Done -> -1
    | Arg { cont; _ } -> 1 + length cont
  ;;

  let rec to_string (env : Environ.env) (sigma : Evd.evar_map) : t -> string
    = function
    | Undefined -> "Undefined"
    | Done -> "Done"
    | Arg { root; index; cont } ->
      Printf.sprintf
        "Arg { root: %s; index: %i; cont: %s}"
        (Rocq_utils.Strfy.constr env sigma root)
        index
        (to_string env sigma cont)
  ;;
end

(***********************************************************************)

(* NOTE: used locally -- DO NOT EXPORT *)
module C = Rocq_utils.C

(***********************************************************************)

type t =
  | No_Bindings
  | Use_Bindings of arg_maps

and arg_maps =
  { from : map option
  ; action : map option
  ; goto : map option
  }

and map = extractor_binding Rocq_utils.C.t
and extractor_binding = Names.Name.t * Instructions.t

let update_map (cmap : map) (k : Constr.t) ((name, inst) : extractor_binding)
  : unit
  =
  match C.find_opt cmap k with
  | None -> C.add cmap k (name, inst)
  | Some (name', inst') ->
    let f = Instructions.length in
    (match Int.compare (f inst) (f inst') with
     | -1 -> C.replace cmap k (name, inst)
     | _ -> ())
;;

let to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      ?(envsigma : (Environ.env * Evd.evar_map) option = None)
  : t -> string
  = function
  | No_Bindings -> "NoBindings"
  | Use_Bindings { from; action; goto } ->
    let f ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ()) (m : map)
      : string
      =
      Utils.Strfy.list
        ~args:{ args with style = Some (Utils.Strfy.record_style ()) }
        (Args
           (fun ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
             ((k, (name, inst)) : Constr.t * extractor_binding) ->
             Option.cata
               (fun (env, sigma) ->
                 Utils.Strfy.record
                   [ "constr", Rocq_utils.Strfy.constr env sigma k
                   ; ( "binding"
                     , Utils.Strfy.record
                         ~args
                         [ "name", Rocq_utils.Strfy.pp (Names.Name.print name)
                         ; "instructions", Instructions.to_string env sigma inst
                         ] )
                   ])
               "(Cannot show if ~envsigma:None)"
               envsigma))
        (C.to_seq m |> List.of_seq)
    in
    let g = Utils.Strfy.option (Args f) in
    (* Utils.Strfy. *)
    Utils.Strfy.record
      ~args
      [ "from", g from; "action", g action; "goto", g goto ]
;;
