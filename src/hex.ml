open OUnit2
       
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
  for _ = 0 to n - 1 do 
    p' := move !p' d
  done;
  !p'

let rec path_of_moves p ml = match ml with
  | [] -> []
  | m :: q ->  let p' = move_n p m in 
	       p'::path_of_moves p' q
				 

let pp_grid f g = 
  if not (Array.length g = 0) then 
    begin
      Format.fprintf f "@[<v 0>";

      (*ligne du haut*)
      for _ = 1 to 2*Array.length g.(0) + 2 do
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
      for _ = 1 to 2*Array.length g.(0) + 2 do(*int_of_float ((2.*.float_of_int (Array.length g.(0)) +. 2.)*. (16.9/.13.)) do*)
	Format.fprintf f "‾";
      done; 
      
      Format.fprintf f "@,@]"
    end
      
(*Q : Doit-on mettre la première position?*)
let path_of_moves_test _  =
  assert_equal (path_of_moves (3,2)
			      [(NE,2);(E,1);(SW,2);(NW,1);(E,3);(NW,2);(W,1)])
	       [(1,3);(1,4);(3,3);(2,2);(2,5);(0,4);(0,3)] 

let tests = ["path_of_moves">::path_of_moves_test]


