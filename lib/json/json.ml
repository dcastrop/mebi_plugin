include Yojson

module Basic = struct
  include Basic

  let rec compare (a : t) (b : t) : int =
    match a, b with
    | `Null, `Null -> 0
    | `Bool a, `Bool b -> Bool.compare a b
    | `Int a, `Int b -> Int.compare a b
    | `Float a, `Float b -> Float.compare a b
    | `String a, `String b -> String.compare a b
    | `List xs, `List ys -> List.compare compare xs ys
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      List.compare
        (fun (k1, v1) (k2, v2) ->
          match String.compare k1 k2 with 0 -> compare v1 v2 | n -> n)
        xs
        ys
    | `Null, _ -> -1
    | _, `Null -> 1
    | `Bool _, _ -> -1
    | _, `Bool _ -> 1
    | `Int _, _ -> -1
    | _, `Int _ -> 1
    | `Float _, _ -> -1
    | _, `Float _ -> 1
    | `String _, _ -> -1
    | _, `String _ -> 1
    | `List _, _ -> -1
    | _, `List _ -> 1
  ;;
end

let rec compare (a : t) (b : t) : int =
  match a, b with
  | `Null, `Null -> 0
  | `Bool a, `Bool b -> Bool.compare a b
  | `Int a, `Int b -> Int.compare a b
  | `Float a, `Float b -> Float.compare a b
  | `String a, `String b -> String.compare a b
  | `List xs, `List ys -> List.compare compare xs ys
  | `Assoc xs, `Assoc ys ->
    let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
    let xs = List.stable_sort compare_keys xs in
    let ys = List.stable_sort compare_keys ys in
    List.compare
      (fun (k1, v1) (k2, v2) ->
        match String.compare k1 k2 with 0 -> compare v1 v2 | n -> n)
      xs
      ys
  | `Intlit a, `Intlit b | `Floatlit a, `Floatlit b | `Stringlit a, `Stringlit b
    ->
    Basic.compare (Basic.from_string a) (Basic.from_string b)
  | `Null, _ -> -1
  | _, `Null -> 1
  | `Bool _, _ -> -1
  | _, `Bool _ -> 1
  | `Int _, _ -> -1
  | _, `Int _ -> 1
  | `Float _, _ -> -1
  | _, `Float _ -> 1
  | `String _, _ -> -1
  | _, `String _ -> 1
  | `List _, _ -> -1
  | _, `List _ -> 1
  | `Intlit _, _ -> -1
  | _, `Intlit _ -> 1
  | `Floatlit _, _ -> -1
  | _, `Floatlit _ -> 1
  | `Stringlit _, _ -> -1
  | _, `Stringlit _ -> 1
;;
