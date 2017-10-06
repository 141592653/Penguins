(** Signature représentant les types finis.
 *
 * Le type [t] est équipé de fonctions [to_int] et [of_int],
 * qu'on supposera inverses l'une de l'autre.
 *
 * De plus le type est supposé fini, c'est à dire qu'on ne
 * rencontrera que des valeurs [x] telles que
 * [0 <= to_int x < max].
 *
 * La fonction [of_int] ne devra être appelée que sur des
 * valeurs [i] tel que [0 <= i < max]. *)
module type FIN = sig
    type t
    val max : int
    val to_int : t -> int
    val of_int : int -> t
  end

(** Signature décrivant un type d'ensembles [t] dont les éléments
 * sont dans [elt].
 *
 * Les opérations ne modifient jamais en place un ensemble,
 * mais ceux-ci sont manipulés dans un style persistant.
 *
 * L'égalité structurelle d'OCaml sur [t] doit correspondre à
 * l'égalité ensembliste. *)
module type SET = sig
    type t
    type elt

    (** Cardinal d'un ensemble, en temps constant *)
    val cardinal : t -> int

    (** Ensemble vide *)
    val empty : t

    (** Création d'un ensemble contenant tous les éléments
     * pour lesquels une fonction est vraie. *)
    val init : (elt -> bool) -> t

    (** Ajout d'un élément *)
    val add : t -> elt -> t

    (** Suppression d'un élément *)
    val remove : t -> elt -> t

    (** Test d'appartenance *)
    val member : t -> elt -> bool

    (** Détermine si le premier ensemble est sous-ensemble du second *)
    val subset : t -> t -> bool

    (** Itération d'une fonction sur les éléments d'un ensemble *)
    val iter : t -> (elt -> unit) -> unit
  end

		    
module Make (F : FIN) : SET with type elt = F.t = struct
  
  type t = Int64.t array
  type elt = F.t

  let t_size = (F.max / 64) + 2
  (*Permet de connaître la position de l'élément dans l'ensemble*)
  let get_pos int_el = int_el / 64 + 1
  (*Permet de connaître le numéro du bit une fois l'entier 64 bits identifié*)
  let get_bit int_el = int_el mod 64
  (*Permet d'obtenir un masque ne comportant que des 0 hormis un 1 sur l'élément qui nous intéresse *)
  let mask int_el = Int64.shift_left Int64.one (get_bit int_el)

				     
  (* Le premier entier du tableau est le cardinal*)
  let cardinal e =  Int64.to_int e.(0)
				     
  let empty = Array.make t_size Int64.zero

  (*Ici, on n'utilise pas la fonction add car ce serait plus lent*)
  let init f =
    let new_e = Array.make t_size Int64.zero in
    for i = 0 to F.max do
      if f (F.of_int i) then
	(
	  new_e.(0) <- Int64.succ new_e.(0);
	  let pos_i = get_pos i in 
      	  new_e.(pos_i) <- Int64.logor new_e.(pos_i) (mask i) 
	)
    done;
    new_e

      
      
  let add e el =
    let int_el = F.to_int el in 
    let new_e = Array.copy e in 
    let pos_el = get_pos int_el in 
    new_e.(pos_el) <- Int64.logor (mask int_el) e.(pos_el);
    (*Si les entiers sont différents, c'est que l'élément ne se trouvait pas dans l'ensemble *)
    if Int64.compare new_e.(pos_el) e.(pos_el) <> 0 then
      new_e.(0) <- Int64.succ new_e.(0);
    new_e

      

  (*La même mais en inversant tout*)
  let remove e el =
    let int_el = F.to_int el in 
    let new_e = Array.copy e in 
    let pos_el = get_pos int_el in 
    new_e.(pos_el) <- Int64.logand (Int64.lognot (mask int_el)) e.(pos_el);
    (*Si les entiers sont différents, c'est que l'élément ne se trouvait pas dans l'ensemble *)
    if Int64.compare new_e.(pos_el) e.(pos_el) <> 0 then
      new_e.(0) <- Int64.pred new_e.(0);
    new_e


  let member e el =
    let int_el = F.to_int el in 
    Int64.compare (Int64.logand (mask int_el) e.(get_pos int_el)) Int64.zero <> 0

  (*pour que e1 soit un sous-ensemble de e2, si un élément appartient à e1, alors il appartient à e2. On peut donc utiliser l'implication logique et on ne doit alors obtenir que des 1. On veut donc que la négation de e1 => e2 soit 0. Par conséquent, on peut utiliser la formule e1/\~e2 et vérifier que le résultat est nul*)
  let subset e1 e2 =
    let sub = ref true and i = ref 1 in
    while !sub && !i < t_size do
      sub := Int64.compare (Int64.logand e1.(!i) (Int64.lognot e2.(!i))) Int64.zero = 0;
      i := !i + 1
    done;
    !sub


     
  let iter e f =
    for i = 0 to F.max do
      let el = F.of_int i in
      if member e el then
	f el
    done
      


end


(*tests*)
						      
module I:FIN = struct
  type t = int
  let max = 1000
  let to_int n = n mod 1000
  let of_int n = n
end

module S = Make(I)

let s = ref (S.empty)
let pairs = S.init (fun i -> let n = I.to_int i in n mod 2 = 0)
let pairs2 = S.init (fun i -> let n = I.to_int i in n mod 2 = 0)
let p = (S.init (fun i -> let n = I.to_int i in n = 10 || n = 0 || n = 33))
	  
