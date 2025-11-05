module Pair = struct
  (** [snd] is a term (e.g., destination) that we want to check unifies with [fst] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
  type t = EConstr.t * EConstr.t

  let to_string env sigma ((x, y) : t) : string =
    let f = Strfy.econstr env sigma in
    let g : string * EConstr.t -> string =
      Strfy.tuple ~is_keyval:true Strfy.str f
    in
    Strfy.tuple ~force_newline:true g g (("lhs", x), ("rhs", y))
  ;;
end

module Problem = struct
  (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
  *)
  type t = Pair.t * Mebi_constr.Tree.t

  let to_string env sigma ((p, t) : t) : string =
    let f = Pair.to_string env sigma in
    let fs = Strfy.tuple ~is_keyval:true Strfy.str f ("pair", p) in
    let g = Mebi_constr.Tree.to_string in
    let gs = Strfy.tuple ~is_keyval:true Strfy.str g ("tree", t) in
    Strfy.list ~force_newline:true ~use:("{", "}") Strfy.str [ fs; gs ]
  ;;
end

module Problems = struct
  type t = Problem.t list

  let to_string env sigma : t -> string =
    Strfy.list ~force_newline:true (Problem.to_string env sigma)
  ;;
end
