open OUnit2
       
class virtual player (a_name:string) (a_pos:Hex.pos) =
	object (self)
	  val name = a_name
	  val  mutable pos = a_pos	 
	  method virtual play : unit -> Hex.move
	  method get_name = name
	  method get_pos = pos
	  method move m = pos <- Hex.move_n pos m
	end

