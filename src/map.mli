(**Module type pour les maps*)

(**Éléments de la map*)
type elt = ICE | WATER | PLAYER of int

(**Tableau représentant la map*)
val map : elt array array

(**Tableau contenant l'ensemble des joueurs
 * Liste*)
val players : Player.player array

(**Numéro du joueur dont c'est le tour*)
val turn : int 

(**Déplace le joueur dont le nom est passé en argument *)
val move : string -> Hex.move -> unit				   
		       

		    
