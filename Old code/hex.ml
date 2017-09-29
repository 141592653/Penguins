type pos = int * int
type 'a grid = 'a array array
type dir = E | NE | SE | W | SW | NW
type move = dir * int

(*Q : Pb dans le mli que j'ai modifié*)
let all_directions = [E;NE;SE;W;SW;NW]


(* Ici, on va considérer que le première ligne d'une grille est 
 * toujours décalée vers la droite par rapport à la seconde
 * comme c'est présenté dans le début de la partie 3.
 * Ceci est important pour calculer les mouvements correctement.*)


(* Pour calculer move, il faut noter que selon qu'on se trouve 
 * sur une ligne paire ou une ligne impaire, 
 * la formule pour calculer les mouvements de type N_ S_ est différente.*)
let move p d = 
  let (pl,pc) = p in (*l et c veulent dire respectivement ligne et colonne*)
  let line_type = pl mod 2 in
  match d with
  |E  -> (pl,pc+1)
  |NE -> (pl-1,pc+1-line_type)
  |SE -> (pl+1,pc+1-line_type)
  |W  -> (pl,pc-1)
  |NW -> (pl-1,pc-line_type)
  |SW -> (pl+1,pc-line_type)


let move_n p m =
  let (d,n) = m in 
  let p' = ref p in 
  for i = 0 to n - 1 do 
    p' := move !p' d
  done;
  !p'

let rec path_of_moves p ml = match ml with
  | [] -> []
  | m :: q ->  let p' = move_n p m in 
	       p'::path_of_moves p' q

(*Q : Doit-on mettre la première position?*)
let path_of_moves_test = path_of_moves (3,2) [(NE,2);(E,1);(SW,2);(NW,1);(E,3);(NW,2);(W,1)]
(*Résultat attendu : [(1,3);(1,4);(3,3);(2,2);(2,5);(0,4);(0,3)] *)


(*f est un formatter*)
let print_bool_ppg f b = 
  if b then
    Format.fprintf f "x"
  else
    Format.fprintf f " "



let pp_grid f g = 
  if not (Array.length g = 0) then 
    begin
      Format.fprintf f "@[<v 2>  ";

      (*ligne du haut*)
      for i = 1 to 2*Array.length g.(0) + 2 do
	Format.fprintf f "_";
      done;
      Format.fprintf f "@,";

      (*corps*)
      for i = 0 to Array.length g - 1 do 
	Format.fprintf f "|";
	if i mod 2 = 0 then 
	  Format.fprintf f " ";
	for j = 0 to Array.length g.(i)-2 do 
	  Format.fprintf f "%c " g.(i).(j)
	done;

	(* dernier caractère de la ligne 
	 * (pour ne pas afficher d'espace supplémentaire) *)
	Format.fprintf f "%c" g.(i).(Array.length g.(i)-1);
	if i mod 2 = 1 then 
	  Format.fprintf f " ";
	Format.fprintf f "|@," 
      done;

      (*ligne du bas*)
      for i = 1 to 2*Array.length g.(0) + 2 do(*int_of_float ((2.*.float_of_int (Array.length g.(0)) +. 2.)*. (16.9/.13.)) do*)
	Format.fprintf f "‾";
      done; 

      Format.fprintf f "@,@]"
    end



(*Permet de convertir une grille de booléens en grille de caractères*)
let grid_char_of_grid_bool pos g = 
  let gc = Array.make_matrix (Array.length g) (Array.length g.(0)) ' ' in
  for l = 0 to Array.length g - 1 do
    for c = 0 to Array.length g.(0) - 1 do
      if (l,c) = pos then
	gc.(l).(c) <- '#'
      else if g.(l).(c) then 
	gc.(l).(c) <- 'x'
      else
	gc.(l).(c) <- ' '
    done
  done;
  gc

let pp_grid_bool pos_grid = 
  let  (start_pos,g) = pos_grid in
  pp_grid Format.std_formatter (grid_char_of_grid_bool start_pos g)
  



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



(* On veut que la grille de sortie respecte les conventions
 * posées au début de ce fichier.
 * Ainsi, on ne va pas prendre en compte les lignes d'eau du début
 * du fichier et on va simplement ajouter une ou deux lignes 
 * d'eau afin de rétablir le décalage souhaité.*)
let from_channel in_chan = 
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
    ignore (input_line in_chan); (*on ignore <problem>*)

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

	(* ici, on ne met pas else car on doit rentrer dans la condition 
	 * même si on a mis first_line_shift_found à true juste avant*)
	if !first_line_shift_found then
	  begin
	    if String.length line > !max_length && line <> "</problem>"then 
	      (max_length_num := !nb_lines;
	       max_length := String.length line);
	
	    lines := line::!lines;
	    nb_lines := !nb_lines + 1	
	  end
      done
    with
      End_of_file -> () 
  end;

  (* Suppression de </problem> et ajout des lignes vides en haut et en bas
   * nb_lines : on remplace la ligne du bas par une ligne d'eau
   * et on ajoute une ligne d'eau en haut ou deux 
   * pour rétablir le décalage vers la droite.*)
  nb_lines := !nb_lines + if !first_line_shift then 2 else 1;
  max_length_num := !max_length_num +  if !first_line_shift then 2 else 1;
  lines := "" :: (List.tl !lines);
  
  
  (* line_length : si la ligne considérée est décalée (ie son modulo 2 est 0) ,
   * !max_length est pair et on tombe sur ce qu'on veut 
   * Sinon, on doit ajouter 1. D'où  !max_length_num mod 2. 
   * On ajoute 2 pour que la première et la dernière colonne soient de l'eau *)
  let line_length = !max_length/2 + !max_length_num mod 2 + 2 in
  let grid_ret = Array.make_matrix !nb_lines line_length false in 
  let start_pos = ref (0,0) in 
  
  (* ici nb_lines sera le numéro de la ligne en cours de traitement dans la grille
   * on va ici écrire la grille à partir de la fin. *)
  nb_lines := !nb_lines - 1; 
  
  while !lines <> [] do 
    let line::q = !lines in
    let pos_line = ref (1 - !nb_lines mod 2)  in 
    let c = ref 1 in (* c est le numéro de la colonne courante
		      * commence à 1 car eau sur la première colonne*)
   
    while !pos_line < String.length line do
      begin
	match line.[!pos_line] with
	|' ' -> ()
	|'#' -> start_pos := (!nb_lines,!c);
		grid_ret.(!nb_lines).(!c) <- true
	|_ -> grid_ret.(!nb_lines).(!c) <- true
      end;      
      c := !c + 1;
      pos_line := !pos_line + 2
				
    done;
    
    lines := q;
    nb_lines := !nb_lines - 1
  done;
  (!start_pos,grid_ret)

    
