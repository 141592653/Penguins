open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

(* charge un plateau de jeu donné dans Parser.CreateMap *)
let new_board () =
  (* ... *)
  ()

(** This is the main function*)
let main () =
  let window = GWindow.window ~width:320 ~height:240
                              ~title:"Jeu des pingouins" () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore (window#connect#destroy ~callback:Main.quit);

  (* Status bar *)
  (* TODO en bas de la fenêtre, taille fixe *)
  let status = GMisc.statusbar ~height:1 (* ~width:320 *)
                               ~packing:vbox#add
                               () in
  let st = status#new_context "contexte" in
  ignore (st#push "bienvenue dans notre jeu");

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "Fichier" in


  let load_board () =
    st#pop(); ignore (st#push "Chargement d'un nouveau jeu...");
    (* TODO boîte de dialogue, choisir un fichier de map *)
    new_board();
    st#pop(); ignore (st#flash ~delay:3000 "Chargement terminé.")
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Nouveau jeu" ~key:_N ~callback: load_board);
  ignore (factory#add_item "Quitter" ~key:_Q ~callback: Main.quit);

  (* Drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  let board_size = 35 in
  let page_size = 3 in
  (* Create the scrollbar. *)
  let adjustment =
    GData.adjustment
      ~lower:0. ~upper:(float_of_int (board_size-1))
      ~step_incr:1. ~page_incr:(float_of_int page_size) () in
  let scrollbar =
    GRange.scrollbar `HORIZONTAL ~adjustment ~packing:vbox#pack () in

  (* (\* Button *\) *)
  (* let button = GButton.button ~label:"1,1" *)
  (*                             ~packing:vbox#add () in *)
  (* button#connect#clicked ~callback: (fun () -> prerr_endline "Ouch!"); *)

  (* TODO cast pour que ça compile *)
  (* vbox#pack status#as_widget; *)

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
