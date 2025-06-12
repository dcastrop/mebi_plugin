let ref_to_glob (r:Libnames.qualid) : Names.GlobRef.t = Nametab.global r ;;

let ref_list_to_glob_list (l : Libnames.qualid list) : Names.GlobRef.t list =
  List.fold_left
    (fun (acc : Names.GlobRef.t list) (s : Libnames.qualid) ->
      ref_to_glob s :: acc)
    []
    (List.rev l)
;;
