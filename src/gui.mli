(** The Graphical User Interface module, with all GTK handling *)

(** This is the main function of the graphical user interface *)
val main : unit -> unit


(** Afficher un message dans la barre de statut de la fenêtre principale. *)
val st_push : string -> unit

(** Afficher un message dans la barre de statut pendant un temps déterminé.
 * valeur par défaut : 5000 ms *)
val st_flash : ?delay:int -> string -> unit
