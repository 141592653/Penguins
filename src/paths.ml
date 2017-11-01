open MapIO

module type S = sig
    val grid : MapIO.elt Hex.grid 
end

module Make (M : S) = struct
  let priority_size = ref 0
  let prior_calls =ref 0
		       
  module FinPos : Bitset.FIN with type t = Hex.pos = struct
    type t = Hex.pos
    let line_length = Array.length M.grid.(0)
    let max = (Array.length M.grid)*line_length - 1 
    let to_int pos = fst pos* line_length + snd pos
    let of_int n = (n / line_length, n mod line_length)
  end

  module HSet : Bitset.SET with type elt = Hex.pos = Bitset.Make(FinPos)
  
  let grid_set  = HSet.init (fun pos ->
			     match M.grid.(fst pos).(snd pos) with
			     |ICE _ -> true
			     |_-> false)

  let grid_of_set set = 
    let new_grid = Array.make_matrix (Array.length M.grid)
				     (Array.length M.grid.(0))
				     WATER in
    for i = 0 to Array.length M.grid - 1 do
      for j = 0 to Array.length M.grid.(0) - 1 do 
	if HSet.member set (i,j) then 
	  new_grid.(i).(j) <- ICE 1
      done
    done;
    for i = 0 to Array.length (get_players ()) - 1 do
      new_grid.(fst (MapIO.get_players ()).(i)#get_pos)
      .(snd (MapIO.get_players ()).(i)#get_pos) <- PENGUIN
    done;
    new_grid

 
  
  let all_moves set elt =
    let moves  = ref [] in 
    let rec explore_dir dir pos i =
      if HSet.member set pos then ( 
	moves := (dir,i)::!moves;
	explore_dir dir (Hex.move pos dir) (i+1)
      )
    in 
    List.iter (fun dir -> explore_dir dir (Hex.move elt dir) 1) Hex.all_directions;
    !moves


  let all_dirs set elt = 
    let rec elim_ice l = match l with 
      |[] -> []
      |dir::q -> 
	begin
	  match HSet.member set (Hex.move elt dir) with 
	  |true -> dir :: elim_ice q
	  |false -> elim_ice q
	end
    in
    elim_ice Hex.all_directions

  (** XXX amélioration possible: tail rec *)	     
  let rec accessible set elt = 
    let cc = ref HSet.empty in (*cc pour composante connexe*)
    cc := HSet.add !cc elt;
    let rec explore el dirs = match dirs with 
      |[] -> ()
      |dir::q -> 
	begin
	  let next_el = Hex.move el dir in
	  match HSet.member set next_el with 
	  |true ->
	    begin
	      
	      if not (HSet.member !cc next_el) then
		(cc :=  HSet.add !cc next_el;
		 explore next_el Hex.all_directions)
	    end;
	    explore el q
			     
	  |false -> explore el q
	end
    in
    explore elt Hex.all_directions;
    !cc

  let neighbours set elt =
    let nb = Array.make 6 (-1,-1) in
     let rec elim_ice l i = match l with 
      |[] -> ()
      |dir::q -> 
	  let next_el = Hex.move elt dir in 
	  if HSet.member set next_el then
	    nb.(i) <- next_el;
	  elim_ice q (i+1)
    in
    elim_ice Hex.all_directions 0;
    nb
	   

  let disconnected set elt =
    let nbours = neighbours set elt in
    let set_neighbours = ref HSet.empty in 
    let a_neighbour = ref (-1,-1) in (*un des voisins*)
    for i = 0 to Array.length nbours - 1 do 
      if fst nbours.(i) >= 0 then (
	set_neighbours := HSet.add !set_neighbours nbours.(i);
	a_neighbour := nbours.(i)	  
      )
    done;
    (*ici, l'idée est de regarder la connexité de l'ensemble formé uniquement des voisins de [el]*)
    if fst !a_neighbour >= 0 then
      (* XXX utiliser accessible est un peu lourd *)
      HSet.cardinal (accessible !set_neighbours !a_neighbour) <> HSet.cardinal !set_neighbours
    else
      false


 
    
  let split set elt =
    let ccs = ref [] in (*ccs pour composantes connexes*)
    let nbours = neighbours set elt in
    let del_elt = HSet.remove set elt in 
    
    for i = 0 to Array.length nbours - 1 do
      if fst nbours.(i) >= 0 then
	begin
	  let new_cc = accessible del_elt nbours.(i) in
	  for j = i + 1 to Array.length nbours - 1 do
	    if fst nbours.(i) >= 0 && HSet.member new_cc nbours.(j) then
	      nbours.(j) <- (-1,-1)
	  done;
	  ccs := new_cc :: !ccs
	end
    done;
    !ccs

  module HMap = struct 
    let table = Hashtbl.create 100
    (*à chaque configuration, on associe un nombre de marquage, un pré-chemin et un post-chemin et leur longueur respective (tout ça dans status)*)
                  (* XXX pour "formaliser" ce commentaire tu aurais pu
                   * déclarer un type *)

    let rm_conf conf =
      Hashtbl.remove table conf
						 
    let add_conf conf status =
      rm_conf conf; (*ceci permet de garder une taille raisonnable de la table*)
      Hashtbl.add table conf status 
                      (* XXX Hashtbl.replace aurait été avantageux *)
    let find_conf conf = 
      try 
	Some (Hashtbl.find table conf)
      with 
	Not_found -> None
		       
   
  end

		  (*le premier entier  est la longueur du pré chemin, le second le nombre de cases restantes *)
  (* XXX et le booléen ? je vais devoir le deviner plus loin... *)
  module KeysDis : Priority.ORDERED with type t = bool*int*int = struct 
    type t = bool*int*int
    let compare (b1,x1,x2) (b2,y1,y2) =
      match (b1,b2) with
      |(false,true) -> -1
      |(true,false) -> 1
      |_ ->
      let sum1 = 2*x1 + x2 and sum2 = 2*y1 +y2 in
      if sum1 > sum2 then 
	-1
      else if sum1 = sum2 then 
	0
      else
	1
 end
  module KeysSum : Priority.ORDERED with type t = bool*int*int = struct 
    type t = bool*int*int
    let compare (b1,x1,x2) (b2,y1,y2) =
      let sum1 = 2*x1 + x2 and sum2 = 2*y1 +y2 in
      if sum1 > sum2 then 
	-1
      else if sum1 = sum2 then 
	0
      else
	1
  end

 module NoKeys : Priority.ORDERED with type t = bool*int*int= struct 
    type t =  bool*int*int
    let compare x y =
       0
  end
						       

  module Prior = Priority.Make(KeysDis)

			      

  (* permet de trouver la composante connexe à laquelle appartient elt dans un tableau de composantes connexes ccs
   * Attention ! ccs doit être non vide et au moins une composante doit contenir elt
*)
  let rec find_cc ccs elt  =
    match ccs with
    |[] -> failwith "Aucune composante connexe n'a été trouvée"
    |cc::q -> if HSet.member cc elt then
		cc
	      else
		find_cc q elt

		

  let maxpath init_elt =
    (*initialisation*)
    (* XXX c'est rude avec aussi peu de doc.. *)
    let prior = Prior.create !priority_size (false,0,0) (HSet.empty,init_elt) in
    ignore (Prior.insert prior (false,0,HSet.cardinal grid_set) (grid_set,init_elt));
    HMap.add_conf (grid_set,init_elt) (-1,(0,(grid_set,init_elt)),(-1,[]));
    let max_path = ref (-1) in
    let root_tagged = ref false in


    (*si on est sur une feuille*)
    let rec lift_post_path conf (post_path_size,post_path) =
      match HMap.find_conf conf with
      |Some(tag_nb,(pre_path_size,pre_conf),(old_path_size,old_path)) ->
	if post_path_size > old_path_size then (
	  HMap.add_conf conf (tag_nb,(pre_path_size,pre_conf),(post_path_size,post_path));
	  (*il ne faut pas continuer si on est à le racine*)
	  if pre_path_size > 0 then 
	    lift_post_path pre_conf (post_path_size + 1, (snd conf)::post_path)
	  else (*sinon, mise à jour de max_path*)
	    max_path := post_path_size
	    
	)
      |None -> failwith "lift_post_path : conf non trouvée"

    in

    let rec lift_tag conf =
      match HMap.find_conf conf with
      |Some(tag_nb,(pre_path_size,pre_conf),post_path) ->
	if fst post_path = 1 && pre_path_size > 0 then
	  (*alors c'est une feuille et ce n'est pas la racine*)
	  lift_tag pre_conf
	else
	  begin
	    HMap.add_conf conf (tag_nb - 1,(pre_path_size,pre_conf),post_path);
	    
	    if tag_nb = 1 then(
	      if pre_path_size = 0 then (*on a gagné !*)
		root_tagged := true
	      else
		lift_tag pre_conf
	    )
	  end
      |None -> failwith "Impossible de diminuer le tag d'une configuration"
      
    in 


    (*parcours des fils d'un noeud, new_path_size vaut la taille de pre_path + 1*)
    let rec browse_moves mov_l pre_conf ccs pre_path new_path_size =
      
      let add_pre_tag = ref 0 in
      
      let (_,elt) = pre_conf in 
      match mov_l with
      |[] -> 0
      |m::q ->
	let next_elt = Hex.move_n elt m in 
	let next_set = find_cc ccs next_elt in
	if (new_path_size + HSet.cardinal next_set <= !max_path) then
	  lift_tag pre_conf
	else
	  begin
	    let next_conf = (next_set,next_elt) in
	    
	    match HMap.find_conf next_conf  with
	    |None ->
	      add_pre_tag := 1;
	      HMap.add_conf next_conf (-1,(new_path_size,pre_conf),(0,[]));
	      ignore (Prior.insert prior (disconnected next_set next_elt,new_path_size,HSet.cardinal next_set) next_conf)
	    |Some(nb_tag,(old_path_size,old_pre_conf),(post_path_size,post_path)) ->
	      if new_path_size > old_path_size && (nb_tag > 0 || nb_tag = -1)  then (
		add_pre_tag := 1;
		lift_tag old_pre_conf;
		HMap.add_conf next_conf (nb_tag,(new_path_size,pre_conf),(post_path_size,post_path));
		(*TODO decrease_key*)
		ignore (Prior.insert prior (disconnected next_set next_elt, new_path_size,HSet.cardinal (fst pre_conf)) next_conf)
	      );
	      lift_post_path pre_conf (post_path_size + 1,next_elt::post_path)
	  end;
		  
	!add_pre_tag + browse_moves q pre_conf ccs  pre_path new_path_size
    in
    

    (*traitement d'une configuration*)
    let treat_conf conf =
      let (set,elt) = conf in
      match HMap.find_conf conf with
      (*si la configuration est dans la file normalement elle est dans la table
       de plus, elle ne doit pas être marquée*)
      |Some(tag_nb,(pre_path_size,pre_conf),(post_path_size,post_path)) when tag_nb <> 0 ->
        
	let moves = all_moves set elt in
	if moves = [] then
	  (
	    lift_post_path conf (1,[]);
	    lift_tag conf;
	    HMap.add_conf conf (1,(pre_path_size,pre_conf),(0,[]))	    
	  )
	else
	   begin
	   let ccs = if disconnected set elt then
		       split set elt
		     else
		       [HSet.remove set elt] in 
	   let new_tag_nb = browse_moves moves conf ccs pre_conf (pre_path_size + 1) in
	   if tag_nb = -1 then (
	     (match HMap.find_conf conf with
	      |Some(_,(pre_size,pre_conf),post) ->
		HMap.add_conf conf (new_tag_nb,(pre_size,pre_conf),post);
		if new_tag_nb = 0 then
		  lift_tag pre_conf		 
	     |None -> failwith "conf non trouvée dans treat_conf");
	     
	   )
	   
	  end
	  
      |_-> failwith "Une configuration dans la file de priorité n'a pas été insérée dans la table"

    in

    (*boucle principale : gestion des priorités*)
    let rec max_path_tmp () =
      prior_calls := !prior_calls +1;
      if Prior.size prior > 0  && not !root_tagged then (
	treat_conf (Prior.value (Prior.extract_min prior));
	max_path_tmp ()
      )
    in
    
    
    
    max_path_tmp ();
    match HMap.find_conf (grid_set,init_elt) with
    |Some(_,_,(post_path_size,post_path)) ->(post_path_size - 1,post_path)
    |None -> failwith "L'algo a lamentablement échoué."
      
		 
		 
     
    
end
