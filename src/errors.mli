
  module type S = sig
    type t =
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family

    exception MEBI_exn of t

    val invalid_sort_lts : Sorts.family -> exn
    val invalid_sort_type : Sorts.family -> exn
  end

  module Make : (_ : Rocq_context.SRocq_context) -> S