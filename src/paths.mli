

module type S = sig
  val grid : MapIO.elt Hex.grid
  end

module Make (M : S) : sig
  val priority_size : int ref
  val prior_calls : int ref
			
  (** Ensembles de cases servant pour les configurations *)
  module HSet : Bitset.SET with type elt = Hex.pos

  val grid_set : HSet.t
    
  val grid_of_set : HSet.t -> (MapIO.elt Hex.grid)


  (** Liste de tous les mouvements faisables sur la configuration
      * donnée par un ensemble et une position (non présente dans
      * l'ensemble). *)
  val all_moves : HSet.t -> Hex.pos -> Hex.move list

  (** [accessible set elt] renvoie le sous-ensemble
      * correspondant à la composante connexe de [elt]. *)
  val accessible : HSet.t -> HSet.elt -> HSet.t

  (** [disconnected set elt] est une heuristique qui permet 
      * d'éliminer les cas où l'ensemble n'a pas été déconnecté 
      * en se contentant de faire une vérification locale sur les
      * voisins de [elt]*)
  val disconnected : HSet.t -> HSet.elt -> bool

  (** Si [set] a été déconnecté par la suppression de [elt],
   * renvoie la liste des nouvelles composantes connexes. *)
  val split : HSet.t -> Hex.pos -> HSet.t list

  (**Identique à split mais ne renvoie que les ccs qui 
   * appartiennent au pingouin considéré*)
  val split_exclusive : HSet.t -> Hex.pos -> HSet.t list

  (** Calcul de la solution optimale à partir d'une position
      * donnée. L'entier est simplement la longueur de la liste
      * de mouvements à effectuer. *)
  val maxpath : Hex.pos -> int * Hex.pos list

end

(**Tests functions *)		
val tests : OUnit2.test list
