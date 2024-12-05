(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
  module Examples = struct
    (** [exa_1] is `Example 3.2.5` on page 106. *)
    let exa_1 : fsm * fsm =
      let (s : fsm) =
        { init = { id = 0; pp = "s0" }
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 1; pp = "s1" } } )
              ; ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 2; pp = "s2" } } )
                (* s1 *)
              ; ( { id = 1; pp = "s1" }
                , { label = 2; to_state = { id = 2; pp = "s2" } } )
                (* s2 *)
              ; ( { id = 2; pp = "s2" }
                , { label = 2; to_state = { id = 2; pp = "s2" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; pp = "t0" }
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 1; pp = "t1" } } )
                (* t1 *)
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 1; pp = "t1" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      s, t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : fsm * fsm =
      let (s : fsm) =
        { init = { id = 0; pp = "s0" }
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 1; pp = "s1" } } )
              ; ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 2; pp = "s2" } } )
                (* s1 *)
              ; ( { id = 1; pp = "s1" }
                , { label = 1; to_state = { id = 3; pp = "s3" } } )
              ; ( { id = 1; pp = "s1" }
                , { label = 2; to_state = { id = 4; pp = "s4" } } )
                (* s2 *)
              ; ( { id = 2; pp = "s2" }
                , { label = 1; to_state = { id = 4; pp = "s4" } } )
                (* s3 *)
              ; ( { id = 3; pp = "s3" }
                , { label = 1; to_state = { id = 0; pp = "s0" } } )
                (* s4 *)
              ; ( { id = 4; pp = "s4" }
                , { label = 1; to_state = { id = 0; pp = "s0" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; pp = "t0" }
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 1; pp = "t1" } } )
              ; ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 3; pp = "t3" } } )
                (* t1 *)
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 2; pp = "t2" } } )
              ; ( { id = 1; pp = "t1" }
                , { label = 1; to_state = { id = 5; pp = "t5" } } )
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 5; pp = "t5" } } )
                (* t2 *)
              ; ( { id = 2; pp = "t2" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
                (* t3 *)
              ; ( { id = 3; pp = "t3" }
                , { label = 1; to_state = { id = 4; pp = "t4" } } )
                (* t4 *)
              ; ( { id = 4; pp = "t4" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
                (* t5 *)
              ; ( { id = 5; pp = "t5" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
              ; ( { id = 5; pp = "t5" }
                , { label = 1; to_state = { id = 4; pp = "t4" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      s, t
    ;;
  end

  (** [KS90] follows algorithm by Kanellakis and Smolka. *)
  module KS90 = struct
    let s = 1
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    let s = 1
  end
end

let bisim_foo : label = 0
