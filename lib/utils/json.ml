let option ?(as_elt : bool = false) (f : ?as_elt:bool -> 'a -> Yojson.t)
  : 'a option -> Yojson.t
  = function
  | None -> `Null
  | Some x -> f ~as_elt x
;;

module type S = sig
  type k

  val name : string
  val json : ?as_elt:bool -> k -> Yojson.t
  val to_string : ?pretty:bool -> k -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
  val write : ?dir:string -> string -> k -> unit
end

module Make
    (Log : Logger.S)
    (X : sig
       type k

       val name : string
       val json : ?as_elt:bool -> k -> Yojson.t
     end) : S with type k = X.k = struct
  include X

  let to_string ?(pretty : bool = true) (x : k) : string =
    if pretty
    then json x |> Yojson.pretty_to_string
    else json x |> Yojson.to_string
  ;;

  let log
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Debug)
        ?(s : string = X.name)
        (x : k)
    : unit
    =
    Log.thing ~__FUNCTION__ m s x (Of to_string)
  ;;

  let write
        ?(dir : string = Utils.FileWriter.default_dir)
        (name : string)
        (x : k)
    : unit
    =
    (* TODO: *)
    Utils.FileWriter.create_parent_dir dir;
    let filepath : string =
      Printf.sprintf
        "%s | %s | %s%s"
        Utils.FileWriter.get_local_timestamp
        (Utils.FileWriter.get_loc ())
        name
        ".json"
      |> Filename.concat dir
    in
    Printf.sprintf "Writing to: %s" filepath |> Log.info;
    let oc = open_out filepath in
    try
      json ~as_elt:false x |> Yojson.pretty_to_channel oc;
      close_out oc;
      Log.info "Finish Writing."
    with
    | e ->
      close_out_noerr oc;
      raise e
  ;;
end

module Thing = struct
  module Make
      (Log : Logger.S)
      (X : sig
         type k

         val name : string
         val json : ?as_elt:bool -> k -> Yojson.t
       end) : S with type k = X.k =
    Make
      (Log)
      (struct
        type k = X.k

        let name = X.name

        let json ?(as_elt : bool = false) (x : k) : Yojson.t =
          let y : Yojson.t = X.json x in
          if as_elt then y else `Assoc [ X.name, y ]
        ;;
      end)
end

module Map = struct
  module type S' = sig
    include S

    val compare : k -> k -> int
  end

  module Make
      (Log : Logger.S)
      (X : sig
         module Map : Hashtbl.S

         type value

         val name : string
       end)
      (K : S' with type k = X.Map.key)
      (V : S' with type k = X.value) : S with type k = X.value X.Map.t =
    Make
      (Log)
      (struct
        type k = X.value X.Map.t

        let name = X.name

        let json ?(as_elt : bool = false) (x : k) : Yojson.t =
          let y : Yojson.t =
            `List
              (X.Map.to_seq x
               |> List.of_seq
               |> List.sort (fun (ka, va) (kb, vb) ->
                 Utils.compare_chain [ K.compare ka kb; V.compare va vb ])
               |> List.map (fun (k, v) ->
                 `Assoc
                   [ K.name, K.json ~as_elt:true k
                   ; V.name, V.json ~as_elt:true v
                   ]))
          in
          if as_elt then y else `Assoc [ X.name, y ]
        ;;
      end)
end

module Set = struct
  module Make
      (Log : Logger.S)
      (X : sig
         module Set : Set.S

         val name : string
         val json : ?as_elt:bool -> Set.elt -> Yojson.t
       end) : S with type k = X.Set.t =
    Make
      (Log)
      (struct
        type k = X.Set.t

        let name = X.name

        let json ?(as_elt : bool = false) (x : X.Set.t) : Yojson.t =
          let y : Yojson.t =
            `List
              (X.Set.fold
                 (fun (x : X.Set.elt) (acc : Yojson.t list) ->
                   X.json ~as_elt:true x :: acc)
                 x
                 []
               |> List.rev)
          in
          if as_elt then y else `Assoc [ X.name, y ]
        ;;
      end)
end

module List = struct
  module Make
      (Log : Logger.S)
      (X : sig
         include sig
           type k

           val json : ?as_elt:bool -> k -> Yojson.t
         end

         val name : string
       end) : S with type k = X.k list =
    Make
      (Log)
      (struct
        type k = X.k list

        let name = X.name

        (** ... *)
        let json ?(as_elt : bool = false) (xs : k) : Yojson.t =
          let y : Yojson.t =
            `List
              (List.rev xs
               |> List.fold_left
                    (fun (acc : Yojson.t list) (x : X.k) ->
                      X.json ~as_elt:true x :: acc)
                    [])
          in
          if as_elt then y else `Assoc [ X.name, y ]
        ;;
      end)
end
