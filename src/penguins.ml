open GMain
open GdkKeysyms

(* warning 32 here *)
let locale = GtkMain.Main.init ()

(** handle quit signal, ask to save game if necessary *)
let quit () =
  prerr_endline "Goodbye!";
  (* TODO *)
  Main.quit()

(** This is the main function*)
let [@warning "-48"] main () =
  (* warning -48 because we like implicit elimination of optional arguments *)
  let window = GWindow.window ~width:320 ~height:240
                              ~title:"Jeu des pingouins" () in
  ignore (window#connect#destroy ~callback:quit);

  let vbox = GPack.vbox ~packing:window#add () in

  (* Status bar *)
  let status = GMisc.statusbar ~packing:(vbox#pack ~from:`END) () in
  let st = status#new_context ~name:"st" in
  ignore (st#push "Bienvenue dans notre jeu !");

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "Fichier" in

  let load_board () =
    st#pop(); ignore (st#push "Chargement d'un nouveau jeu...");
    (* TODO boîte de dialogue, choisir un fichier de map *)
    st#pop(); ignore (st#flash ~delay:3000 "Chargement terminé.")
  in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Nouveau jeu" ~key:_N ~callback:load_board);
  ignore (factory#add_item "Quitter" ~key:_Q ~callback:quit);

  (* automatic scrolling bars *)
  let scroll = GBin.scrolled_window
                 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                 ~packing:vbox#add () in

  (* Drawing area *)
  let da = GMisc.drawing_area ~packing:scroll#add_with_viewport () in
  (* TODO taille selon taille de la map chargée *)
  da#set_size ~height:400 ~width:400;

  (* TODO gestion d'erreur *)
  let pixbuf = GdkPixbuf.from_file "img/3uSFN.png" in

  let expose _ =
    da#misc#realize();  (* avoid exception Gpointer.Null *)
    let draw = new GDraw.drawable da#misc#window in
    draw#put_pixbuf ~x:0 ~y:0 pixbuf;
    draw#put_pixbuf ~x:44 ~y:27 pixbuf;
    draw#put_pixbuf ~x:88 ~y:0 pixbuf;
    false
  in
  ignore (da#event#connect#expose expose);

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = Printexc.print main ()
(* let () = main () *)
