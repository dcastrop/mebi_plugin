module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val non_silent : t -> t *)
     end)
    (Bindings : sig
       module Instructions : sig
         type t =
           | Undefined
           | Done
           | Arg of
               { root : Constr.t
               ; index : int
               ; cont : t
               }

         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

         exception Rocq_bindings_CannotAppendDone of unit

         (* val append : t -> t -> t *)
         (* val length : t -> int *)
       end

       module ConstrMap : sig
         include Hashtbl.S with type key = Constr.t

         type v = Names.Name.t * Instructions.t
         type t' = v t

         (* val json : ?as_elt:bool -> t' -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t' -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit *)
         (* val update : t' -> Constr.t -> v -> unit *)

         exception Rocq_bindings_CannotFindBindingName of Evd.econstr

         (* val find_name
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr
            -> Names.Name.t

            val extract_binding_map
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr
            -> Constr.t
            -> t' mm

            val make_opt
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr * Constr.t
            -> t' option mm *)
       end

       type t =
         | No_Bindings
         | Use_Bindings of
             { from : ConstrMap.t' option
             ; action : ConstrMap.t' option
             ; goto : ConstrMap.t' option
             }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val use_no_bindings : ConstrMap.t' option list -> bool

    val extract
      :  (Evd.econstr * Names.Name.t) list
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
      -> t mm *)
     end)
    (ConstructorBindings : sig
       type t =
         { index : int
         ; name : string
         ; bindings : Bindings.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val extract_info : 'a Rocq_ind.t -> t list mm *)
       (* val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

    exception Rocq_bindings_BindingInstruction_NotApp of Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_Undefined of Evd.econstr * Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of Evd.econstr * int

    exception Rocq_bindings_BindingInstruction_NEQ of Evd.econstr * Constr.t

    val get_bound_term
      :  Evd.econstr
      -> Bindings.Instructions.t
      -> Evd.econstr mm

    val get_explicit_bindings
      :  Evd.econstr * Bindings.ConstrMap.t' option
      -> Evd.econstr Tactypes.explicit_bindings mm

    val get
      :  Enc.t
      -> Enc.t option
      -> Enc.t option
      -> Bindings.t
      -> Evd.econstr Tactypes.bindings mm *)
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
        { enc : Enc.t
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
        { enc : Enc.t
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
                [ "enc", Enc.json x.enc
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
