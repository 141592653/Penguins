open OUnit2

(**Module type pour les maps*)
module type map = sig
    
    (**Éléments de la map*)
    type elt = ICE | WATER | PLAYER of int
					 
    (**Tableau représentant la map*)
    val map : elt array array
		  
    (**Tableau contenant l'ensemble des joueurs*)
    val players : Player.player array
				
    (**Numéro du joueur dont c'est le tour*)
    val turn : int 
		 
    (**Déplace le joueur dont le nom est passé en argument *)
    val move : string -> Hex.move -> unit

  end
       
(**Informations nécessaires à la création d'une map*)
module type mapInfo = sig
    val json_file : string
  end

module CreateMap (MI:mapInfo): map = struct
  
  type elt = ICE | WATER | PLAYER of int
				       
  let json = Yojson.Basic.from_channel (open_in MI.json_file)
    
  let map = [|[||]|]
  let players = [||]
  let turn = 0
  let move pl_name move = ()
  
end
				       
 (* Fonctions de test*)
module TestFile : mapInfo = struct
  let json_file = "test/parse_test.json"
end
			      
module TestMap : map = CreateMap(TestFile)

let test_parser ctxt =
  assert_equal TestMap.turn 0

let tests = ["parser">::test_parser]
