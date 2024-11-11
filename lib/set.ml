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
  let rec insert_internal x t =
    match t with
    | Leaf ->
        (* Case 1: Inserting into an empty tree (Leaf) - Create a new TwoNode
           with x as the only element. - Set grow to true, as the tree's height
           has increased by one. *)
        (TwoNode (x, empty, empty), true)
    | TwoNode (v, left, right) ->
        if (* Case 2: Inserting into a TwoNode *)
           x = v then
          (* If x already exists in this TwoNode, no need to insert - Return the
             current TwoNode with grow = false (height unchanged). *)
          (TwoNode (v, left, right), false)
        else if x < v then
          (* If x is less than the current node value v, insert into the left
             child *)
          let new_left, grow = insert_internal x left in
          if grow then
            (* If inserting into the left child caused it to grow, we need to
               convert the TwoNode into a ThreeNode to keep the 2-3 tree
               structure balanced. *)
            match new_left with
            | TwoNode (v, Leaf, Leaf) ->
                (ThreeNode (x, v, empty, right, empty), false)
            | TwoNode (v, sleft, sright) ->
                (ThreeNode (x, v, sleft, sright, right), false)
            | _ -> failwith "DOESNT WORK"
          else
            (* If no growth happened in the left subtree, simply update the left
               child without increasing height *)
            (TwoNode (v, new_left, right), false)
        else
          (* If x is greater than the current node value v, insert into the
             right child *)
          let new_right, grow = insert_internal x right in
          if grow then
            (* If inserting into the right child caused it to grow, convert the
               TwoNode into a ThreeNode. *)
            match new_right with
            | TwoNode (x, Leaf, Leaf) ->
                (ThreeNode (v, x, left, empty, empty), false)
            | TwoNode (x, sleft, sright) ->
                (ThreeNode (v, x, left, sleft, sright), false)
            | _ -> failwith "NOPE"
          else
            (* No growth occurred in the right subtree; update the right child
               without increasing height *)
            (TwoNode (v, left, new_right), false)
    | ThreeNode (v1, v2, left, middle, right) ->
        if (* Case 3: Inserting into a ThreeNode *)
           x = v1 || x = v2 then
          (* If x already exists in this ThreeNode, no need to insert - Return
             the current ThreeNode with grow = false (height unchanged). *)
          (ThreeNode (v1, v2, left, middle, right), false)
        else if x < v1 then
          (* If x is less than the first value v1, insert into the left child *)
          let new_left, grow = insert_internal x left in
          if grow then
            match middle with
            | Leaf ->
                ( TwoNode
                    (v1, TwoNode (x, empty, empty), TwoNode (v2, empty, empty)),
                  true )
            | _ -> (TwoNode (v1, new_left, TwoNode (v2, middle, right)), true)
          else
            (* No growth in the left subtree; update the left child without
               increasing height *)
            (ThreeNode (v1, v2, new_left, middle, right), false)
        else if x < v2 then
          (* If x is between v1 and v2, insert into the middle child *)
          let new_middle, grow = insert_internal x middle in
          if grow then
            (* If inserting into the middle child caused it to grow, split the
               ThreeNode and promote x to create a balanced structure. *)
            match middle with
            | Leaf ->
                ( TwoNode
                    (x, TwoNode (v1, empty, empty), TwoNode (v2, empty, empty)),
                  true )
            | _ ->
                ( TwoNode
                    ( x,
                      TwoNode (v1, left, new_middle),
                      TwoNode (v2, new_middle, right) ),
                  true )
          else
            (* No growth in the middle subtree; update the middle child without
               increasing height *)
            (ThreeNode (v1, v2, left, new_middle, right), false)
        else
          (* If x is greater than v2, insert into the right child *)
          let new_right, grow = insert_internal x right in
          if grow then
            (* If inserting into the right child caused it to grow, split the
               ThreeNode and promote v2 to create a balanced structure. *)
            match middle with
            | Leaf ->
                ( TwoNode
                    (v2, TwoNode (v1, empty, empty), TwoNode (x, empty, empty)),
                  true )
            | _ -> (TwoNode (v2, TwoNode (v1, left, middle), new_right), true)
          else
            (* No growth in the right subtree; update the right child without
               increasing height *)
            (ThreeNode (v1, v2, left, middle, new_right), false)
  in
  (* Start the recursive insertion from the root of the tree. After insertion,
     if the root grows in height, we return the new root; otherwise, we return
     the modified tree. *)
  let new_root, _ = insert_internal x t in
  new_root

let create_three_node v1 v2 left middle right =
  ThreeNode (v1, v2, left, middle, right)

let create_two_node v1 left right = TwoNode (v1, left, right)
