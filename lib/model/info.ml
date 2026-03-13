module type S = sig
  type base
  type constructorbindings
  type labels

  module Meta : sig
    module Bounds : sig
      type t =
        | States of int
        | Transitions of int
        | Merged of t * t

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t
        -> unit
    end

    module RocqLTS : sig
      type t =
        { base : base
        ; constructors : constructorbindings list
        }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t
        -> unit
    end

    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : Bounds.t
      ; lts : RocqLTS.t list
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
    val merge : t -> t -> t
    val merge_opt : t option -> t option -> t option
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : labels
    ; num_states : int
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val merge : ?num_states:int -> t -> t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Labels : Labels.S)
    (Bindings : sig
       module Instructions : sig
         type t
       end

       module ConstrMap : sig
         include Hashtbl.S with type key = Constr.t

         type v = Names.Name.t * Instructions.t
         type t' = v t
       end

       type t =
         | No_Bindings
         | Use_Bindings of
             { from : ConstrMap.t' option
             ; action : ConstrMap.t' option
             ; goto : ConstrMap.t' option
             }
     end)
    (ConstructorBindings : sig
       type t =
         { index : int
         ; name : string
         ; bindings : Bindings.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
     end) :
  S
  with type base = Base.t
   and type constructorbindings = ConstructorBindings.t
   and type labels = Labels.t = struct
  type base = Base.t
  type constructorbindings = ConstructorBindings.t
  type labels = Labels.t

  module Meta = struct
    module Bounds = struct
      type t =
        | States of int
        | Transitions of int
        | Merged of t * t

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "Bounds"

            let json ?(as_elt : bool = false) (x : t) : Yojson.t =
              let rec f : t -> Yojson.t = function
                | States i -> `Assoc [ "by", `String "states"; "num", `Int i ]
                | Transitions i ->
                  `Assoc [ "by", `String "transitions"; "num", `Int i ]
                | Merged (a, b) ->
                  `Assoc [ "Merged", `Assoc [ "a", f a; "b", f b ] ]
              in
              f x
            ;;
          end)
    end

    module RocqLTS = struct
      type t =
        { base : base
        ; constructors : constructorbindings list
        }

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "RocqLTS"

            let json ?(as_elt : bool = false) (x : t) : Yojson.t =
              `Assoc
                [ "base", Base.json x.base
                ; ( "constructors"
                  , `List
                      (List.map
                         (ConstructorBindings.json ~as_elt:true)
                         x.constructors) )
                ]
            ;;
          end)
    end

    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : Bounds.t
      ; lts : RocqLTS.t list
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Meta"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "complete", `Bool x.is_complete
              ; "merged", `Bool x.is_merged
              ; "bounds", Bounds.json ~as_elt:true x.bounds
              ; "rocq lts", `List (List.map (RocqLTS.json ~as_elt:true) x.lts)
              ]
          ;;
        end)

    let merge (a : t) (b : t) : t =
      let x : t =
        { is_complete = a.is_complete && b.is_complete
        ; is_merged = true
        ; bounds = Merged (a.bounds, b.bounds)
        ; lts =
            List.merge
              (fun (a : RocqLTS.t) (b : RocqLTS.t) ->
                Base.compare a.base b.base)
              a.lts
              b.lts
        }
      in
      x
    ;;

    let merge_opt (a : t option) (b : t option) : t option =
      match a, b with
      | None, None -> None
      | Some a, Some b -> Some (merge a b)
      | Some a, None -> Some { a with is_merged = true }
      | None, Some b -> Some { b with is_merged = true }
    ;;
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : labels
    ; num_states : int
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Info"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "num states", `Int x.num_states
            ; "meta", Json.option ~as_elt:true Meta.json x.meta
            ; "weak labels", Labels.json ~as_elt:true x.weak_labels
            ]
        ;;
      end)

  (** [merge a b] returns a new [t] with a union of [weak_labels] and [meta=(Meta.merge_opt ...)].
  *)
  let merge ?(num_states : int = -1) (a : t) (b : t) : t =
    { meta = Meta.merge_opt a.meta b.meta
    ; weak_labels = Labels.union a.weak_labels b.weak_labels
    ; num_states
    }
  ;;
end
