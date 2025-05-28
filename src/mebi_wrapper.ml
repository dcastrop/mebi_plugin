(* module M = Mebi_monad

   module MkF = Hashtbl.Make (struct
   type t = M.term

   let equal t1 t2 = EConstr.eq_constr !M.init.coq_ctx t1 t2

   let hash t =
   Constr.hash
   (EConstr.to_constr
   ?abort_on_undefined_evars:(Some false)
   !M.the_coq_ctx
   t)
   ;;
   end) *)
