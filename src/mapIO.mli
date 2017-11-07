(**Parser des fichiers de sauvegarde. Format des maps : 
 * 
 * [{
 *     "name" : "Niveau 1",
 *     "players" :
 *     {
 * 	"beauGosseDu84" : 1,
 * 	"42" : 2,
 * 	"cotcotcodète" : 3
 *      },
 *     "map" : "map_test.txt"
 *     "turn" : 2
 * }]

 * Tous les paramètres sont optionnels mais players sera par défaut la liste 
 * vide. Il faut que le nombre de joueur de la liste corresponde au nombre de
 * joueurs sur la carte.
 * L'ordre des joueurs est le même que l'ordre dans lequel les joueurs jouent.
 * Le nombre associé à chaque joueur correspond à la position du nème joueur
 * que l'on rencontre lorsqu'on lit la carte de gauche à droite et de haut en
 * bas
 * 
 * Par défaut, on n'est pas obligé de préciser "map" : le parser ira chercher
 * nom_carte.txt où nom_carte est le nom du fichier json sans l'extension du 
 * fichier.
 * 
 * Le nom de la carte sera par défaut nom_carte
 *
 * turn est le numéro du joueur dont c'est le tour de jouer. S'il n'est pas 
 * renseigné, il sera mis à 0.

 *)

    
(**Éléments de la map*)
type elt = ICE of int | WATER | PENGUIN



(**Nom de la map*)
val get_name : unit -> string
					 
(**Tableau représentant la map. Renvoie une copie du tableau.
L'utilisation de cette fonction en dehors des tests n'est pas recommandée*)
val get_map : unit -> elt array array

(**Renvoie la case demandée*)
val get_cell : int -> int -> elt

(** Renvoie (h,w) le nombre de ligne et de colonnes de la carte*)
val dimensions : unit -> int*int
	      
(**Tableau contenant l'ensemble des joueurs*)
val get_players : unit -> Player.player array
			    
(**Numéro du joueur dont c'est le tour*)
val get_turn : unit -> int 
	     
(**Utile à la création d'un nouveau jeu*)
val set_turn : int -> unit

(**Passer au joueur suivant*)
val next_turn : unit -> unit


(**Parse a map txt file. Return the grid and the list of player's positions*)
val parse_map : string -> elt array array * Hex.pos list

(**Set the map and initialize the players array. For each player the tuple
 * is (position, name, type) where type = 'Humain', 'IA Standard', etc *)
val set_map_and_players : elt array array
                          -> (Hex.pos * string * string) list
                          -> unit

(**Open a new game, in json format*)
val open_map: string -> unit

(**Text-based pretty printer of a map*)
val pp_map :Format.formatter -> unit

(**Text-based pretty printer of a map*)
val pp_grid :Format.formatter -> elt array array -> unit
				  
(** Affichage d'un chemin sous la forme d'une liste
 * de positions. Le chemin devra apparaitre, sur la grille
 * [M.grid] où les cases de glace sont représentées par '*',
 * selon une numérotation des cases du chemin par
 * a ... z A ... Z  puis ? pour les éventuelles
 * cases suivantes. *)
val pp_path : Format.formatter -> Hex.pos list -> unit

(**Version de Hex.mov_n qui indique si un mouvement est légal ou non*)
val legal_move_n : Hex.pos -> Hex.move -> bool*Hex.pos
				  
(**Déplace le joueur dont le nom est passé en argument *)
val move : string -> Hex.move -> unit
			  
		    
(**Tests functions *)		
val tests : OUnit2.test list
