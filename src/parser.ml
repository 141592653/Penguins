open OUnit2
open Yojson

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



  let parse_player p =
    let (name,turn) = p in
    match turn with
      |`Int i -> (name,i)
      |_ -> failwith "The turn of a player must be an integer."
    
  (*parse players*)
  let parse_players p =
    match p with
      | `Assoc l -> List.map parse_player l
      | _ -> failwith ("The player structure of "
		       ^MI.json_file
		       ^" should be an Associative list")
      
  (*parse name players or map*)
  let parse_npm  name players map npm =
    match fst npm with
    |"name" -> begin
      match snd npm with
	|`String s -> name := s
        |_ -> failwith "The name of a map should be a string"
    end
    |"map" -> begin
      match snd npm with
	|`String s -> map := s
        |_ -> failwith "The name of the file containing the map \
			should be a string"
    end
    |"players" -> players := parse_players (snd npm)
    |_ -> failwith ("The entry "^(fst npm)^ " is not understood")

  (*This is the parser of a json file*)
  let parse_main_json json_a = 
    (*by default, the name is the name of the json file without the extension*)
    let default_name = String.sub MI.json_file 0
				  (String.length MI.json_file - 5) in 
    let name_ref = ref default_name
    and players_ref = ref []
    and map_ref = ref (default_name ^".txt") in
    begin
      match json_a with
      | `Assoc l -> List.iter (parse_npm name_ref players_ref map_ref) l
      | _ -> failwith ("The main structure of "
		       ^MI.json_file
		       ^" should be an Associative list")
    end;
    (!name_ref,!players_ref,!map_ref)




  				       
  let json =
    try
      Yojson.Basic.from_channel (open_in MI.json_file)
    with
    |Json_error log ->
      failwith ("The file "
		      ^ MI.json_file
		      ^" is not a json file.\
			Here is the log of the json parser : "
		      ^log)
    
  let (name,players,map) = parse_main_json json 
      
  
  let map = [|[||]|]
  let players = [||]
  let turn = 0
  let move pl_name move = ()
  
end


				       
				       
(* Test functions*)
module TestWrongJson : mapInfo = struct
    let json_file = "test/wrong_json.json"
end

module TestWrongJson2 : mapInfo = struct
    let json_file = "test/wrong_json2.json"
end
								     
module TestFile : mapInfo = struct
  let json_file = "test/parse_test.json"
end
			      
module TestMap : map = CreateMap(TestFile)

let test_wrong_json ctxt = 
  try
    let module WrongJson : map = CreateMap(TestWrongJson) in
    assert_equal 0 1
  with
  |Failure _ -> ()

let test_wrong_json2 ctxt = 
  try
    let module WrongJson2 : map = CreateMap(TestWrongJson2) in
    assert_equal 0 1
  with
  |Failure _ -> ()
		  
let test_parser ctxt =
  assert_equal TestMap.turn 0
	       
let tests = ["wrong json">::test_wrong_json;
	     "wrong json2">::test_wrong_json2;
	     "parser">::test_parser]
