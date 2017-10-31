open OUnit2
       
class virtual player (a_name:string) (a_pos:Hex.pos) =
	object (self)
	  val name = a_name
	  val mutable pos = a_pos
	  val mutable next_move = (Hex.W,0)
	  val mutable is_ready = false
	  method virtual play : unit -> unit
	  method get_name = name
	  method get_pos = pos
	  method is_ready = is_ready
	  method set_ready a_is_ready = is_ready <- a_is_ready
						      
	  method move m =
	    pos <- Hex.move_n pos m;
	    is_ready <- false
	end

class humanPlayer (a_name:string) (a_pos:Hex.pos) =
object (self)
  inherit player a_name a_pos
  (*TODO*)
  method play () = ()
end	 
