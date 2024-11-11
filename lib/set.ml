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
    | Leaf -> (`Up (TwoNode (x, Leaf, Leaf)), true)
    | TwoNode (v, left, right) ->
        if x = v then (`Node (TwoNode (v, left, right)), false)
        else if x < v then
          let result, grow = insert_internal x left in
          if grow then
            match result with
            | `Up new_child ->
                (`Up (ThreeNode (x, v, new_child, right, Leaf)), true)
            | `Node new_left -> (`Node (TwoNode (v, new_left, right)), false)
          else (`Node (TwoNode (v, left, right)), false)
        else
          let result, grow = insert_internal x right in
          if grow then
            match result with
            | `Up new_child ->
                (`Up (ThreeNode (v, x, left, new_child, Leaf)), true)
            | `Node new_right -> (`Node (TwoNode (v, left, new_right)), false)
          else (`Node (TwoNode (v, left, right)), false)
    | ThreeNode (v1, v2, left, middle, right) ->
        if x = v1 || x = v2 then
          (`Node (ThreeNode (v1, v2, left, middle, right)), false)
        else if x < v1 then
          let result, grow = insert_internal x left in
          if grow then
            match result with
            | `Up new_left ->
                (`Up (TwoNode (v1, new_left, TwoNode (v2, middle, right))), true)
            | `Node new_left ->
                (`Node (ThreeNode (v1, v2, new_left, middle, right)), false)
          else (`Node (ThreeNode (v1, v2, left, middle, right)), false)
        else if x < v2 then
          let result, grow = insert_internal x middle in
          if grow then
            match result with
            | `Up new_middle ->
                ( `Up
                    (TwoNode
                       ( x,
                         TwoNode (v1, left, new_middle),
                         TwoNode (v2, middle, right) )),
                  true )
            | `Node new_middle ->
                (`Node (ThreeNode (v1, v2, left, new_middle, right)), false)
          else (`Node (ThreeNode (v1, v2, left, middle, right)), false)
        else
          let result, grow = insert_internal x right in
          if grow then
            match result with
            | `Up new_right ->
                (`Up (TwoNode (v2, TwoNode (v1, left, middle), new_right)), true)
            | `Node new_right ->
                (`Node (ThreeNode (v1, v2, left, middle, new_right)), false)
          else (`Node (ThreeNode (v1, v2, left, middle, right)), false)
  in
  match insert_internal x t with
  | `Up new_root, _ -> new_root
  | `Node tree, _ -> tree

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

let create_three_node v1 v2 left middle right =
  ThreeNode (v1, v2, left, middle, right)

let create_two_node v1 left right = TwoNode (v1, left, right)
