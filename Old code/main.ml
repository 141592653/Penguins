open Hex
let testing = ref false
let priority_size_tmp = ref 1000

		  
let test_func () = 
  Format.fprintf Format.std_formatter "Test de la fonction accessible"
  
  (*Hex.pp_grid_bool (Path.accessible (Path.grid_set ()) )*)

let string_of_bool b = 
  if b then 
    "true"
  else
    "false"

let string_of_dir d = 
  match d with 
  |NE -> "NE"
  |NW -> "NW"
  |SE -> "SE"
  |SW -> "SW"
  |W -> "W"
  |E -> "E"

let string_of_move m = 
  "("^string_of_dir (fst m)^ "," ^ string_of_int (snd m) ^ ")"
		
let main_func file =
  let bg = Hex.from_channel (open_in file) in  (*bool_grid*)
  Format.fprintf Format.std_formatter "@[<v 0>Tux est perdu sur la banquise :( !@,Heureusement on va l'aider.@,Voici les données que nous envoie sa balise Argos : @,@]";
  let module M : Paths.S  =  struct
	     let grid = snd bg end  in
  
  let module P = Paths.Make(M) in
  P.priority_size := !priority_size_tmp;
  Hex.pp_grid_bool bg;
   if !testing then
    begin 
      Format.fprintf Format.std_formatter "@[<v 0>============================== Section de tests =========================== @,@,Test de la fonction accessible : @,@]";
      Hex.pp_grid_bool ((fst bg),(P.grid_of_set (P.accessible P.grid_set (fst bg))));
      Format.fprintf Format.std_formatter "@[<v 0>Test de disconnected : @, La fonction renvoie %s@,@]" (string_of_bool (P.disconnected P.grid_set (fst bg)));
      Format.fprintf Format.std_formatter "@[<v 0>@,@,Test de split : @,@]";
      let split_list =  P.split P.grid_set (fst bg) in 
      List.iter (fun set -> Hex.pp_grid_bool ((-1,-1),P.grid_of_set set) ) split_list;
      Format.fprintf Format.std_formatter "@[<v 0>@,@,Test de all_moves : @]";
      List.iter (fun m -> Format.fprintf Format.std_formatter "%s " (string_of_move m))  (P.all_moves P.grid_set (fst bg));
      Format.fprintf Format.std_formatter "@[<v 0>@,@, @]"
      
    end;
  Format.fprintf Format.std_formatter "@[<v 0>Calcul de la solution ... @,@]";
  let time = Sys.time () in 
  let mp = P.maxpath (fst bg) in
  Format.fprintf Format.std_formatter "@[<v 0>Taille du chemin trouvé : %d@,Voici la solution :@,@]" (fst mp);
  
  P.pp_path Format.std_formatter (snd mp);
  Format.fprintf Format.std_formatter "@[<v 0>Temps de calcul : %f secondes@,Nombre d'éléments extraits de la file : %d@,@]" ( Sys.time() -. time) !P.prior_calls
  
  
 

   


let main() = 
  Arg.parse [("-p",Arg.Int (fun n -> priority_size_tmp := n), "Permet de régler la taille maximum de la file de priorité. Si une exception comportant le message \"le tas est plain\" est levée, augmenter cette valeur.Par défaut : 300.");("-t",Arg.Unit (fun () -> testing := true),"Appel des fonctions de test")]  main_func "Ce programme aide Tux à se repérer sur la banquise pour qu'il puisse manger plein de poissons."

let () = main()
