module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : Labels.S with type elt = Label.t)
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
     end) : sig
  module Meta : sig
    module Bounds : sig
      type t =
        | States of int
        | Transitions of int
        | Merged of t * t

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module RocqLTS : sig
      type t =
        { enc : Base.t
        ; constructors : ConstructorBindings.t list
        }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : Bounds.t
      ; lts : RocqLTS.t list
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : Labels.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val merge : t -> t -> t
end = struct
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
        { enc : Base.t
        ; constructors : ConstructorBindings.t list
        }

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "RocqLTS"

            let json ?(as_elt : bool = false) (x : t) : Yojson.t =
              `Assoc
                [ "enc", Base.json x.enc
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
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : Labels.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Info"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "meta", Json.option ~as_elt:true Meta.json x.meta
            ; "weak labels", Labels.json ~as_elt:true x.weak_labels
            ]
        ;;
      end)

  (** [merge a b] returns a new [t] with a union of [weak_labels] and [meta=None].
  *)
  let merge (a : t) (b : t) : t =
    { meta = None; weak_labels = Labels.union a.weak_labels b.weak_labels }
  ;;
end
