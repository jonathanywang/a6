type 'a tree =
  | Leaf
  | TwoNode of 'a * 'a tree * 'a tree
  | ThreeNode of 'a * 'a * 'a tree * 'a tree * 'a tree

type 'a t = 'a tree
(* AF: A value of type ['a t] represents a set of elements stored in a 2–3 tree
   structure. Each node in the tree may be either: a Leaf, representing an empty
   set; a TwoNode, which stores a single element and has exactly two children; a
   ThreeNode, which stores two elements and has exactly three children. *)

(* RI: For any tree of type ['a t]: In a TwoNode, the left and right subtrees
   must both satisfy the representation invariant, and all elements in the left
   subtree are less than the element in the node, while all elements in the
   right subtree are greater. In a ThreeNode, the left, middle, and right
   subtrees must all satisfy the representation invariant, with all elements in
   the left subtree less than the first element in the node, all elements in the
   middle subtree greater than the first element but less than the second
   element, and all elements in the right subtree greater than the second
   element. The tree must not contain duplicate elements. Only the Leaf node may
   represent an empty set. *)

(** [empty] is the empty set, represented by a Leaf node.
    @return an empty 2–3 tree of type ['a t] *)
let empty = Leaf

(** [is_empty t] checks if the set [t] is empty.
    @param t the tree to check for emptiness
    @return [true] if [t] is empty, [false] otherwise *)
let is_empty t =
  match t with
  | Leaf -> true
  | _ -> false

(** [mem x t] checks if element [x] is a member of set [t].
    @param x the element to search for
    @param t the tree in which to search
    @return [true] if [x] is found, [false] otherwise *)
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

(** [to_string string_of_element t] converts the tree [t] to a string.
    @param string_of_element a function to convert elements to strings
    @param t the tree to convert to a string
    @return the string representation of [t] *)
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

(** [insert x t] inserts element [x] into set [t]. If [x] is already present,
    returns [t] unchanged. This function preserves the 2–3 tree structure.
    @param x the element to insert
    @param t the tree into which [x] will be inserted
    @return a new tree with [x] inserted, if not already present *)
let rec insert x t =
  if mem x t then t
  else
    let rec insertion x t =
      match t with
      | Leaf -> (TwoNode (x, empty, empty), true)
      | TwoNode (v, left, right) ->
          if x < v then
            let new_left, grow = insertion x left in
            if grow then
              match new_left with
              | TwoNode (v', Leaf, Leaf) ->
                  (ThreeNode (v', v, empty, right, empty), false)
              | TwoNode (v', sleft, sright) ->
                  (ThreeNode (v', v, sleft, sright, right), false)
              | _ -> failwith "Can't merge a ThreeNode"
            else (TwoNode (v, new_left, right), false)
          else
            let new_right, grow = insertion x right in
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
            let new_left, grow = insertion x left in
            if grow then
              match middle with
              | Leaf ->
                  ( TwoNode
                      (v1, TwoNode (x, empty, empty), TwoNode (v2, empty, empty)),
                    true )
              | _ -> (TwoNode (v1, new_left, TwoNode (v2, middle, right)), true)
            else (ThreeNode (v1, v2, new_left, middle, right), false)
          else if x < v2 then
            let new_middle, grow = insertion x middle in
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
            let new_right, grow = insertion x right in
            if grow then
              match middle with
              | Leaf ->
                  ( TwoNode
                      (v2, TwoNode (v1, empty, empty), TwoNode (x, empty, empty)),
                    true )
              | _ -> (TwoNode (v2, TwoNode (v1, left, middle), new_right), true)
            else (ThreeNode (v1, v2, left, middle, new_right), false)
    in
    let new_root, _ = insertion x t in
    new_root

(** [create_three_node v1 v2 left middle right] constructs a ThreeNode with
    specified values and children.
    @param v1 the first value of the ThreeNode
    @param v2 the second value of the ThreeNode
    @param left the left child subtree
    @param middle the middle child subtree
    @param right the right child subtree
    @return a new ThreeNode with given values and children *)
let create_three_node v1 v2 left middle right =
  ThreeNode (v1, v2, left, middle, right)

(** [create_two_node v1 left right] constructs a TwoNode with a specified value
    and children.
    @param v1 the value of the TwoNode
    @param left the left child subtree
    @param right the right child subtree
    @return a new TwoNode with given value and children *)
let create_two_node v1 left right = TwoNode (v1, left, right)
