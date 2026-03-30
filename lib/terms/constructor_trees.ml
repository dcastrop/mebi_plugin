module type S = sig
  type constructor_tree
  type t = constructor_tree list

  include Json.S with type k = t
end

module Make (Log : Logger.S) (Constructor_tree : Constructor_tree.S) :
  S with type constructor_tree = Constructor_tree.t = struct
  type constructor_tree = Constructor_tree.t
  type t = Constructor_tree.t list

  include
    Json.List.Make
      (Log)
      (struct
        type k = Constructor_tree.t

        let name = "Constructors"
        let json = Constructor_tree.json
      end)
end
