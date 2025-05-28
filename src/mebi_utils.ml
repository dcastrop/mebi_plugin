(** used by [g_mebi.mlg]*)
let ref_list_to_glob_list (l : Libnames.qualid list) : Names.GlobRef.t list =
  List.fold_left
    (fun (acc : Names.GlobRef.t list) (s : Libnames.qualid) ->
      List.append acc [ Nametab.global s ])
    []
    l
;;
