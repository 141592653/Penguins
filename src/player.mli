(** This module contains the abstract class Player
 * and the classes for human and AI Player *)


(**This class represents eather a human player or an AI player*)
class virtual player : string -> Hex.pos ->
       object
			 
	 (**Position of the player*)
	 val pos : Hex.pos
		     
	 (**Name of the player *)
	 val name : string
		      
	 (**Asks the player to play*)
	 method virtual play : unit -> Hex.move
					  
	 method get_name : string
	 method get_pos : Hex.pos
	 method move : Hex.move -> unit
       end

(** This class represents human players *)
class humanPlayer : string -> Hex.pos ->
      object
	inherit player
	method  play : unit -> Hex.move
      end
