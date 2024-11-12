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

let rec insert x t =
  (* Check if x already exists in the tree *)
  if mem x t then (* If x is already present, return the tree as-is *)
    t
  else
    (* Proceed with insertion only if x is not present *)
    let rec insert_internal x t =
      match t with
      | Leaf ->
          (* Case 1: Inserting into an empty tree (Leaf) *)
          (TwoNode (x, empty, empty), true)
      | TwoNode (v, left, right) ->
          if x < v then
            let new_left, grow = insert_internal x left in
            if grow then
              match new_left with
              | TwoNode (v', Leaf, Leaf) ->
                  (ThreeNode (v', v, empty, right, empty), false)
              | TwoNode (v', sleft, sright) ->
                  (ThreeNode (v', v, sleft, sright, right), false)
              | _ -> failwith "Can't merge a ThreeNode"
            else (TwoNode (v, new_left, right), false)
          else
            let new_right, grow = insert_internal x right in
            if grow then
              match new_right with
              | TwoNode (v', Leaf, Leaf) ->
                  (ThreeNode (v, v', left, empty, empty), false)
              | TwoNode (v', sleft, sright) ->
                  (ThreeNode (v, v', left, sleft, sright), false)
              | _ -> failwith "Can't merge a ThreeNode"
            else (TwoNode (v, left, new_right), false)
      | ThreeNode (v1, v2, left, middle, right) ->
          if x < v1 then
            let new_left, grow = insert_internal x left in
            if grow then
              match middle with
              | Leaf ->
                  ( TwoNode
                      (v1, TwoNode (x, empty, empty), TwoNode (v2, empty, empty)),
                    true )
              | _ -> (TwoNode (v1, new_left, TwoNode (v2, middle, right)), true)
            else (ThreeNode (v1, v2, new_left, middle, right), false)
          else if x < v2 then
            let new_middle, grow = insert_internal x middle in
            if grow then
              match middle with
              | TwoNode (_, sleft, sright) ->
                  ( TwoNode
                      (x, TwoNode (v1, left, sleft), TwoNode (v2, sright, right)),
                    true )
              | _ ->
                  ( TwoNode
                      (x, TwoNode (v1, empty, empty), TwoNode (v2, empty, empty)),
                    true )
            else (ThreeNode (v1, v2, left, new_middle, right), false)
          else
            let new_right, grow = insert_internal x right in
            if grow then
              match middle with
              | Leaf ->
                  ( TwoNode
                      (v2, TwoNode (v1, empty, empty), TwoNode (x, empty, empty)),
                    true )
              | _ -> (TwoNode (v2, TwoNode (v1, left, middle), new_right), true)
            else (ThreeNode (v1, v2, left, middle, new_right), false)
    in
    let new_root, _ = insert_internal x t in
    new_root

let create_three_node v1 v2 left middle right =
  ThreeNode (v1, v2, left, middle, right)

let create_two_node v1 left right = TwoNode (v1, left, right)
