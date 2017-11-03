open OUnit2
open Yojson

(**Éléments de la map*)
type elt = ICE of int| WATER | PENGUIN

let name = ref ""
let map = ref [|[||]|]
let players = ref [||]
let turn = ref 0
let get_name () = !name
let get_cell l c =
  if l >=0 && c >= 0&& l < Array.length !map && c <  Array.length !map.(0) then
    !map.(l).(c)
  else
    WATER
      
let dimensions () = (Array.length !map , Array.length !map.(0))

let get_map () = Array.map Array.copy !map
let get_players () = !players
let get_turn () = !turn



		   

(* ********************** Début parsing **************************** *)
let parse_player p =
  let (name,pos) = p in
  match pos with
    |`Int i -> (name,i)
   |_ -> failwith "The position of a player must be an integer."
		  
(*parse players*)
let parse_players p =
  match p with
  | `Assoc l -> List.map parse_player l
  | _ -> failwith ("The player structure  should be an Associative list")
		  
(*parse name players map or turn*)
let parse_npmt  players_tmp map_file npmt =
  match fst npmt with
  |"name" -> begin
	     match snd npmt with
	       |`String s -> name := s
              |_ -> failwith "The name of a map should be a string"
	   end
  |"map" -> begin
	    match snd npmt with
	      |`String s -> map_file := s
             |_ -> failwith "The name of the file containing the map \
			     should be a string"
	  end
  |"turn" -> begin
	     match snd npmt with
	       |`Int i -> turn := i
              |_ -> failwith "The name of the file containing the map \
			      should be a string"
	   end
  |"players" -> players_tmp := parse_players (snd npmt)
  |_ -> failwith ("The entry "^(fst npmt)^ " is not understood")

(*This is the parser of a json file*)
let parse_main_json json_a = 
  let players_tmp = ref [] and map_file = ref (!name ^".txt") in
  begin
    match json_a with
    | `Assoc l -> List.iter (parse_npmt players_tmp map_file) l
    | _ -> failwith ("The main structure ofµ should be an Associative list")
  end;
  (!players_tmp,!map_file)



(* Renvoie la position du premier caractère différent de ' '
 * s'il existe et -1 sinon *) 
let find_not_space s =
  let i = ref 0 and n = String.length s in 
  while !i<n && s.[!i] == ' ' do 
    i := !i + 1
    done;
  if !i < n then
    !i
  else
    -1

(* Pour que le décalage soit toujours le même, on ajoute ou non une ligne 
 * d'eau afin de rétablir le décalage souhaité.*)
let [@warning "-8"] parse_map map_file =
  let in_chan = open_in map_file in 
  let lines = ref [] in
  let max_length = ref 0 in
  let max_length_num = ref 0 in 
  let nb_lines = ref 0 in

  (* on veut savoir si la première ligne est décalée ou bien si c'est la seconde
   * Si first_line_shift vaut true, c'est que la première ligne 
   * est décalée vers la droite par rapport à la seconde.
   *)
  let first_line_shift = ref false in
  let first_line_shift_found = ref false in

  (*ici, on commence par déterminer la longueur de la plus grande ligne*)
  begin 

    try 
      while true do
	
	let line = input_line in_chan in
	
	if not !first_line_shift_found then 
	  begin
	    let fst_not_space = find_not_space line in
	    if fst_not_space >= 0 then
	      (first_line_shift := ((!nb_lines + fst_not_space) mod 2) = 1;
	       first_line_shift_found := true);
	  end;
	nb_lines := !nb_lines +1 ;
	
	(* ici, on ne met pas else car on doit rentrer dans la condition 
	 * même si on a mis first_line_shift_found à true juste avant*)
	if !first_line_shift_found then
	  begin
	    if String.length line > !max_length then 
	      (max_length_num := !nb_lines;
	       max_length := String.length line);
	    
	    lines := line::!lines;
	    
	  end
      done
    with
      End_of_file -> () 
  end;

  (*devrait être +1 ou 0 ... ????*)
  nb_lines := !nb_lines + if !first_line_shift then 0 else -1;
  max_length_num := !max_length_num +  if !first_line_shift then 1 else 0;
  
  
  (* line_length : si la ligne considérée est décalée (ie son modulo 2 est 0) ,
   * !max_length est pair et on tombe sur ce qu'on veut 
   * Sinon, on doit ajouter 1. D'où  !max_length_num mod 2. µ *)
  let line_length = ((!max_length_num mod 2) + !max_length)/2  in
  map  := Array.make_matrix !nb_lines line_length WATER ; 
  let penguins_pos = ref [] in 
  
  (* ici nb_lines sera le numéro de la ligne en cours de traitement dans la 
   * grille on va ici écrire la grille à partir de la fin. *)
  nb_lines := !nb_lines - 1;
  
  while !lines <> [] do 
    let  line::q = !lines in
    let pos_line = ref (1 - !nb_lines mod 2)  in 
    let c = ref 0 in (* c est le numéro de la colonne courante*)
    let line_png_pos = ref [] in (*png = penguin*)
    
    while !pos_line < String.length line do
      begin
	match line.[!pos_line] with
	|' ' -> ()
	|'#' -> !map.(!nb_lines).(!c) <- PENGUIN;
		line_png_pos := (!nb_lines,!c)::!line_png_pos
	|ice -> let n = int_of_char ice in
	      if n >= 48 && n <= 57 then
		!map.(!nb_lines).(!c) <- ICE (n-48)
	      else
		!map.(!nb_lines).(!c) <- ICE 1
      end;      
      c := !c + 1;
      pos_line := !pos_line + 2
				
    done;
    penguins_pos := (List.rev !line_png_pos) @ !penguins_pos;
    
    lines := q;
    nb_lines := !nb_lines - 1
  done;
  !penguins_pos
    
    


    
let json_from_file s =
  try
    Yojson.Basic.from_channel (open_in s)
  with
  |Json_error log ->
    failwith ("The file is not a json file.\
		Here is the log of the json parser : "
	      ^log)

	     
let default_player = new Player.humanPlayer "" (0,0)

let open_map s =
  (*by default, the name is the name of the json file without the extension*)
  name := String.sub s 0 (String.length s - 5);
  let json = json_from_file s in
  let (players_tmp,map_file) = parse_main_json json in
  let players_pos = parse_map map_file in
  let nb_players = List.length players_pos in   
  players := if nb_players = List.length players_tmp then
		Array.make nb_players default_player
	      else
		failwith "The number of players specified in the \
			  json file is not the same as which of the map file";

  let num_pl = ref 0 in
		 
  let create_player pl_tmp =
    let (pl_name,n) = pl_tmp in
    !players.(!num_pl) <- new Player.humanPlayer pl_name (List.nth players_pos n);
    num_pl := !num_pl + 1 in 
			
  List.iter create_player players_tmp

	    
(* ********************** Fin parsing **************************** *)

	    


(* ******************** Pretty printing ************************* *)
	    
(*Permet de convertir une grille de booléens en grille de caractères*)
let char_grid_of_map () = 
  let gc = Array.make_matrix (Array.length !map) (Array.length !map.(0)) ' ' in
  for l = 0 to Array.length !map - 1 do
    for c = 0 to Array.length !map.(0) - 1 do
      match !map.(l).(c) with
      |ICE n ->	gc.(l).(c) <- char_of_int (48 + n)
      |_ -> ()
	      
    done
  done;
  for i = 0 to Array.length !players - 1 do
    let (l,c) = !players.(i)#get_pos in
    gc.(l).(c) <- '#'
  done;
  gc

let pp_map f =
  Hex.pp_grid f (char_grid_of_map ())

let pp_grid f g =
  let gc = Array.make_matrix (Array.length !map) (Array.length !map.(0)) ' ' in
  for l = 0 to Array.length g - 1 do
    for c = 0 to Array.length g.(0) - 1 do
      match g.(l).(c) with
      |ICE n ->	gc.(l).(c) <- char_of_int (48 + n)
      |_ -> ()
	      
    done
  done;
  Hex.pp_grid f gc

(*Donne le charactère en fonction de l'indice de la position dans le chemin*)
let path_char i =
  if i < 26 then
    char_of_int (97 + i)
  else if i < 52 then
    char_of_int (65 + i-26)
  else
    '?'
      
     
let pp_path f path =    
  let gc = char_grid_of_map () in
  let rec pp_path_tmp l i = match l with
    |[] -> ()
    |pos::q -> (*Printf.printf "(%d,%d)" (fst pos) (snd pos);*)
      gc.(fst pos).(snd pos) <- path_char i;
      pp_path_tmp q (i+1)
  in
  pp_path_tmp path 0;
  Hex.pp_grid f gc

(* *********************** Fin pretty_printing ******************** *)


let legal_move_n p m =
  let (d,n) = m in
  let p' = ref p in
  let legal = ref true in 
  for i = 1 to n do
    p' := Hex.move !p' d;
    (*ici si on dépasse du tableau , l'accès lèvera une exception.*)
    try
      match !map.(fst !p').(snd !p') with
      |ICE _ -> ()
      |_ -> legal := false
    with
    |_ -> legal := false
  done;
  (!legal,!p')


let player_num_from_name pl_name =
  let found = ref false and i = ref 0  in
  while not !found && !i < Array.length !players do
    if !players.(!i)#get_name = pl_name then
      found := true
    else
      i := !i + 1
  done;
  if !i =  Array.length !players then
    raise (Invalid_argument (pl_name ^ " n'est pas un nom de joueur"))
  else
    !i
	   
let move pl_name m =
  let n = player_num_from_name pl_name  in
  let old_p = !players.(n)#get_pos in 
  let (legal,new_p) = legal_move_n old_p m in
  if not legal then
    raise (Invalid_argument "Le mouvement n'est pas valide.")
  else
    (!players.(n)#set_pos new_p;
     !map.(fst old_p).(snd old_p) <- WATER;
     !map.(fst new_p).(snd new_p) <- PENGUIN
    )
	      
	




(* ********************** Test functions ************************* *)

let test_wrong_json _ = 
  try
    open_map "test/wrong_json.json";
    assert_equal 0 1
  with
  |Failure _ -> ()

let test_wrong_json2 _ = 
  try
    open_map "test/wrong_json2.json";
    assert_equal 0 1
  with
  |Failure _ -> ()
		  
let test_parser _ =
  open_map "test/parse_test.json";
  assert_equal !turn 1;
  assert_equal !name "Niveau 1";
  assert_equal (Array.length !players) 4;
  assert_equal !players.(1)#get_name "42";
  assert_equal !players.(0)#get_pos (3,0);
  assert_equal !map.(1) [|PENGUIN;ICE 1;ICE 1;ICE 1;ICE 1;ICE 1;WATER;
				 WATER; ICE 1;ICE 1;ICE 1;ICE 1;ICE 1;PENGUIN|]
			       
let tests = ["wrong json">::test_wrong_json;
	     "wrong json2">::test_wrong_json2;
	     "parser">::test_parser]
