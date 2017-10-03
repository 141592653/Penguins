module type ORDERED = sig
    type t
    val compare : t -> t -> int
  end

module Make (M:ORDERED) = struct
  (*Je code ici un tas min (la plus petite priorité est à la racine)

   ici, on va utiliser l'implémentation classique d'une file de priorité à l'aide d'un tas implémenté par un tableau avec la numérotation Sosa :
 - La racine est à la case 0, son fils gauche1, son fils droit 1, le fils gauche du fils gauche 2, ...
 - Fils gauche du noeud n : 2*n + 1 
 - Fils droit du noeud n : 2*n + 2
 - Père du noeud n : (n-1)/2 (partie entière)
 *)
  
  type 'a node = {key:M.t; value : 'a}
  type 'a queue = {mutable s:int; a:('a node) array} (*s pour size, a pour array*)

  (*petite fonction pour échanger deux éléments d'un tableau*)
  let swap a i j =
    let sauv = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- sauv
 
  let create max_size dummy_key dummy_value =
    {s=0;a=Array.make max_size {key = dummy_key;value = dummy_value}}

  let size q = q.s

  (*permet de faire remonter un élément dont le priorité est plus forte que celle de ses parents, i est l'indice de l'élément*)
  let go_up q i =
    let {s=s;a=a} = q in
    if i >= s then 
      failwith "go_up a reçu un indice inexistant"
    else
      let fils = ref i and pere = ref ((i-1)/2) in
      
      (*cette boucle s'arrête nécessairement car (0-1)/2 = 0*)
      while M.compare a.(!fils).key a.(!pere).key < 0 do
	swap a !fils !pere;
	fils := !pere;
	pere := (!fils-1)/2       
      done
	       

  let insert q key value =
    let {s=s;a=a} = q  and nd = {key=key;value=value} in
    match s = Array.length a with
    |true -> failwith "Le tas est plein, il est possible d'augmenter sa taille avec l'option -p."
    |false ->
      a.(s) <- nd;
      q.s <- q.s + 1;
      go_up q s;
      
      nd




    (*fonction intermédiaire permettant de supprimer un élément en connaissant son indice dans le tableau *)
  let remove_index q i =
    let {a=a;s=s} =q in
    if i >= s then
      failwith "L'indice donne a remove_index n'existe pas. Cela veut probablement dire que le tas est vide (extract_min a echoue)."
    else
      begin 
	let pere = ref i and min_key = ref a.(i).key and min_fils = ref 0  and fini =ref false in
	
	swap a i (s-1); (*met l'élément qu'on veut supprimer à la fin*)
	(*puis on fait descendre le second élément*)
	while 2* !pere + 1 < s-1 && not !fini do

	  (*on commence par comparer les deux fils*)
	  if 2* !pere + 1 = s-2 then
	    (min_key := a.(s-2).key;
	     min_fils := s-2)
	  else
	    begin
	      if M.compare a.(2* !pere+1).key a.(2* !pere+2).key < 0 then
		(min_key := a.(2* !pere+1).key;
		 min_fils := 2* !pere+1)
	      else
		(min_key := a.(2* !pere+2).key;
		 min_fils := 2* !pere+2)
	    end;

	  if M.compare a.(!pere).key !min_key <= 0 then
	    fini := true
	  else
	    (swap a !pere !min_fils;
	     pere := !min_fils)
	done;
	q.s <- q.s - 1
	  
      end


  (*on déduit extract_min de remove_index de manière immédiate : *)
  let extract_min q =
    let sauv_min = q.a.(0) in
    remove_index q 0;
    sauv_min

  let key nd = nd.key
  let value nd = nd.value

      
(*pour remove, on a besoin de trouver l'élément puis de le supprimer. On écrit donc une fonction qui nous permet de trouver son indice*)
  exception Member of int

  (* fonction un peu plus spécifique que member qui renvoie l'indice du noeud  
   * dans une exception si on l'a trouvé et unit sinon.
   * L'utilisation d'une exception permet de rendre la fonction plus efficace.*)
  let member_index q nd =
    let rec member_index_tmp q nd i=
      (*ici, i est le numéro du noeud qu'on est en train d'explorer*)
      let {s=s;a=a} = q in
      if i >= s then
	()
      else
	let cmp = M.compare nd.key a.(i).key in
	if cmp < 0 then
	  () (*tous les fils de l'éléments resteront strictment supérieurs à nd*)
	else if cmp = 0 &&  nd = a.(i) then
	  raise (Member i)
	else
	  (member_index_tmp q nd (2*i+1);
	   member_index_tmp q nd (2*i+2))
    in
    member_index_tmp q nd 0

		     
  (*remove se déduit aisément de member_index et remove_index*)
  let remove q nd =
    try
      member_index q nd;
      failwith "remove : l'element n'a pas ete trouve"
    with
      Member i -> remove_index q i
		     
  let member q nd =
    let is_mb = ref false in
    begin
      try
	member_index q nd
      with
	Member _ -> is_mb := true
    end;
    !is_mb

  let decrease_key q nd key =
     try
      member_index q nd;
      ignore (insert q key nd.value); (*si le noeud n'est pas dans la queue, on l'insère*)
    with
      Member i -> 
      q.a.(i) <- {key=key;value=q.a.(i).value} ;
      go_up q i
      
			
end
			    
			    (*		    
module T = Make(Int32) 
let tas = T.create 20 0l "hey" ;;
ignore (T.insert tas 10l "blub2");;
ignore (T.insert tas 5l "blub");;
	ignore (T.insert tas 8l "BLAB");;
	  ignore (T.insert tas 7l "blub");;
	  let truc = T.insert tas 6l "blub";;
	      ignore (T.insert tas 3l "blub");;
		ignore (T.insert tas 5l "blab");;
		  T.member tas truc;;
		    tas;;
		      T.decrease_key tas truc 4l;;
			T.remove tas truc;;
			  T.decrease_key tas truc 6l;;
			    T.remove tas truc;;
		    tas;;
		    T.extract_min tas;;
		      T.extract_min tas;;
			T.extract_min tas;;
			  T.extract_min tas;;
			    T.extract_min tas;;
		      T.extract_min tas;;
			T.extract_min tas;;
			 T.extract_min tas;;*)
		     

    
