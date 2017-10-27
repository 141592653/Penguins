(** This module contains the abstract class Player
 * and the classes for human and AI Player *)


(**This class represents eather a human player or an AI player*)
class virtual player : string -> Hex.pos ->
       object
			 
	 (**Position of the player*)
	 val pos : Hex.pos
		     
	 (**Name of the player *)
	 val name : string

	 (**This is the move computed by play*)
	 val next_move : Hex.move

	 (**set to false by play, it's value is true
	  *  when next_move is the correct move*)
	 val is_ready : bool
			   
	 (**Asks the player to play*)
	 method virtual play : unit -> unit

	 (** Name getter*)
	 method get_name : string

	 (** Position getter*)
	 method get_pos : Hex.pos

	 (** Is_ready getter*)
	 method is_ready : bool

	 (** Is_ready setter*)
	 method set_ready : bool -> unit

	 (** Moves the player*)
	 method move : Hex.move -> unit
       end

(** This class represents human players *)
class humanPlayer : string -> Hex.pos ->
      object
	inherit player
	method  play : unit -> unit
      end
