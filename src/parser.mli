(**Parser des fichiers de sauvegarde. Format des maps : 
 * 
 * {
 *     "name" : "Niveau 1",
 *     "players" :
 *     {
 * 	"beauGosseDu84" : 1,
 * 	"42" : 2,
 * 	"cotcotcodète" : 3
 *      },
 *     "map" : "map_test.txt"
 *     "turn" : 2
 * }

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


(**Module type pour les maps*)
module type map = sig
    
    (**Éléments de la map*)
    type elt = ICE | WATER | PENGUIN

    (**Nom de la map*)
    val name : string
					 
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

(** Crée un module Map à partir d'un nom de fichier *)
module CreateMap (MI:mapInfo):map
			
val tests : OUnit2.test list
