open OUnit2
       
class virtual player (a_name:string) (a_pos:Hex.pos) =
	object (self)
	  val name = a_name
	  val mutable pos = a_pos
	  val mutable next_move = (Hex.W,0)
	  val mutable is_ready = false
	  method virtual play : unit
	  method get_name = name
	  method get_pos = pos
	  method is_ready = is_ready
	  method set_ready a_is_ready = is_ready <- a_is_ready
						      
	  method set_pos a_pos =
	    pos <- a_pos;
	    is_ready <- false
		     
	  method move m =
	    pos <- Hex.move_n pos m;
	    is_ready <- false
	  method virtual is_human : bool
	end

class humanPlayer (a_name:string) (a_pos:Hex.pos) =
object (self)
  inherit player a_name a_pos
  method is_human = true

  (*TODO*)
  method play = ()
end	 
