type 'a tree =
  | Leaf
  | TwoNode of 'a * 'a tree * 'a tree
  | ThreeNode of 'a * 'a * 'a tree * 'a tree * 'a tree

type 'a t = 'a tree

let empty = Leaf

let is_empty t =
  match t with
  | Leaf -> true
  | _ -> false

let rec mem x t =
  match t with
  | Leaf -> false
  | TwoNode (v, left, right) ->
      if x = v then true else if x < v then mem x left else mem x right
  | ThreeNode (v1, v2, left, middle, right) ->
      if x = v1 || x = v2 then true
      else if x < v1 then mem x left
      else if x < v2 then mem x middle
      else mem x right

(* Helper function to handle insertion, returning either a Node or an Up signal
   with a new node. *)
let rec insert x t =
  let rec insert_internal x t =
    match t with
    | Leaf -> `Up (TwoNode (x, Leaf, Leaf))
    | TwoNode (v, left, right) -> (
        if x = v then `Node (TwoNode (v, left, right))
        else if x < v then
          match insert_internal x left with
          | `Up new_child -> `Up (ThreeNode (x, v, new_child, right, Leaf))
          | `Node new_left -> `Node (TwoNode (v, new_left, right))
        else
          match insert_internal x right with
          | `Up new_child -> `Up (ThreeNode (v, x, left, new_child, Leaf))
          | `Node new_right -> `Node (TwoNode (v, left, new_right)))
    | ThreeNode (v1, v2, left, middle, right) -> (
        if x = v1 || x = v2 then `Node (ThreeNode (v1, v2, left, middle, right))
        else if x < v1 then
          match insert_internal x left with
          | `Up new_left ->
              `Up (TwoNode (v1, new_left, TwoNode (v2, middle, right)))
          | `Node new_left ->
              `Node (ThreeNode (v1, v2, new_left, middle, right))
        else if x < v2 then
          match insert_internal x middle with
          | `Up new_middle ->
              `Up (TwoNode (v2, TwoNode (v1, left, new_middle), right))
          | `Node new_middle ->
              `Node (ThreeNode (v1, v2, left, new_middle, right))
        else
          match insert_internal x right with
          | `Up new_right ->
              `Up (TwoNode (v2, TwoNode (v1, left, middle), new_right))
          | `Node new_right ->
              `Node (ThreeNode (v1, v2, left, middle, new_right)))
  in
  match insert_internal x t with
  | `Up new_root -> new_root
  | `Node tree -> tree

let to_string string_of_element t =
  let rec tree_to_string = function
    | Leaf -> "Leaf"
    | TwoNode (v, left, right) ->
        Printf.sprintf "TwoNode(%s, %s, %s)" (string_of_element v)
          (tree_to_string left) (tree_to_string right)
    | ThreeNode (v1, v2, left, middle, right) ->
        Printf.sprintf "ThreeNode(%s, %s, %s, %s, %s)" (string_of_element v1)
          (string_of_element v2) (tree_to_string left) (tree_to_string middle)
          (tree_to_string right)
  in
  tree_to_string t
