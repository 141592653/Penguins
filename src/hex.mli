
type pos = int * int
type 'a grid = 'a array array

(** Directions *)

type dir = E | NE | SE | W | SW | NW

(** Liste de toutes les directions possibles *)
val all_directions : dir list

(** Mouvements *)

type move = dir * int
val move : pos -> dir -> pos
val move_n : pos -> move -> pos

(** [path_of_moves p moves] est la liste des positions
  * obtenues par applications successives des mouvements
  * de [moves] à partir de [p]. *)
val path_of_moves : pos -> move list -> pos list
		    
(**Tests functions *)		
val tests : OUnit2.test list
