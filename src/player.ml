open OUnit2
       
class virtual player (name:string) =
	object (self)
	  method virtual play : unit
	  method get_name = name
	end



  
