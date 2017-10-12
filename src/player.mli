(**This class represents eather a human player or an IA player*)
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
