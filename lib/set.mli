type 'a t
(** Type 'a t represents a polymorphic set of elements of type 'a *)

val empty : 'a t
(** [empty] is the empty set. *)

val is_empty : 'a t -> bool
(** [is_empty s] returns true if [s] is empty, otherwise false. *)

val mem : 'a -> 'a t -> bool
(** [mem x s] returns true if [x] is in set [s], otherwise false. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string s] returns a string representation of set [s], useful for
    debugging. *)

val insert : 'a -> 'a t -> 'a t
(** [insert x s] returns a new set that includes [x] in [s]. If [x] is already
    in [s], returns [s] unchanged. *)

val create_three_node : 'a -> 'a -> 'a t -> 'a t -> 'a t -> 'a t
(** [create_three_node v1 v2 left middle right] creates a ThreeNode for testing. *)

val create_two_node : 'a -> 'a t -> 'a t -> 'a t
(** [create_three_node v1 v2 left middle right] creates a ThreeNode for testing. *)
