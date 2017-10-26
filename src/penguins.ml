open GMain
open GdkKeysyms

(* warning 32 here *)
let locale = GtkMain.Main.init ()

(* Status bar to print game informations*)
let status = GMisc.statusbar ()
let st = status#new_context ~name:"st"

(* use these functions to display messages in the status bar *)
let st_push s = ignore (st#pop(); st#push s)
let st_flash t s = ignore (st#pop(); st#flash ~delay:t s)

(* TODO même chose pour un printf dans la console ? *)

(* Drawing area to display the game board *)
let da = GMisc.drawing_area ()


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

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "Fichier" in

  (* TODO gestion d'erreur, assert sur les tailles d'image *)
  let pixbuf_ice = GdkPixbuf.from_file "img/ice.png" in
  let pixbuf_water = GdkPixbuf.from_file "img/water.png" in
  let pixbuf_penguin = GdkPixbuf.from_file "img/penguin.png" in

  (* draw the board (hexagons etc.) in the drawing area *)
  let draw_board () =
    (* suppose that MapIO.map was successfully loaded *)
    let map = MapIO.get_map() in
    (* TODO assert sur les tailles *)
    let m = Array.length map in
    let n = Array.length map.(0) in

    (* da#misc#realize();  (\* avoid exception Gpointer.Null *\) *)
    da#set_size ~height:(50*m) ~width:(56*n);
    (* make it handling mouse clicks *)
    let button_pressed ev =
      if GdkEvent.Button.button ev = 1 then (
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in

        let mouse_to_coord_ (x,y) =
          if (x < 6 || y < 3)
          then (-1,-1)
          else
            let di = (y-3) / (46*2) in
            let dj = (x-6) / 53 in
            let i = (y-3) mod (46*2) in
            let j = (x-6) mod 53 in
            (* on coupe la figure en deux verticalement *)
            let b = j < 26 in
            let jj = if b then j else 53-j in (* symétrie *)
            if i <= 13 - (jj *13)/26
            then (if b then     (* Nord-ouest *)
                    (if di > 0 && dj > 0 then (di*2-1, dj-1) else (-1,-1))
                  else          (* Nord-est *)
                    (if di > 0 && dj < n-1 then (di*2-1, dj) else (-1,-1))
                 )
            else if i >= 44 + (jj * 13)/26
            then (if b then     (* Sud-ouest *)
                    (if di < m-1 && dj > 0 then (di*2+1, dj-1) else (-1,-1))
                  else          (* Sud-est *)
                    (if di < m-1 && dj < n-1 then (di*2+1, dj) else (-1,-1))
                 )
            else (di*2,dj)
        in
        let mouse_to_coord (x,y) =
          let (i,j) = mouse_to_coord_ (x,y) in
          if i >= m || j >= n then (-1,-1) else (i,j)
        in
        let (i,j) = mouse_to_coord (x,y) in
        Printf.printf "(%d,%d)\n%!" i j;
      );
      true
    in
    da#event#add [`BUTTON_PRESS];
    da#event#connect#button_press ~callback:button_pressed;

    let expose _ =
      let draw = new GDraw.drawable da#misc#window in
      (* draw#put_pixbuf ~x:0 ~y:0 pixbuf_ice; *)
      (* draw#put_pixbuf ~x:53 ~y:0 pixbuf_water; *)
      (* draw#put_pixbuf ~x:26 ~y:46 pixbuf_ice; *)
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          (* TODO décalage à unifier avec mapIO *)
          let x = j * 53 + (if i mod 2 = 0 then 0 else 26) in
          let y = i * 46 in
          match map.(i).(j) with
          | ICE ->  draw#put_pixbuf ~x ~y pixbuf_ice;
          | WATER -> draw#put_pixbuf ~x ~y pixbuf_water;
          | PENGUIN -> draw#put_pixbuf ~x ~y pixbuf_penguin;
        done;
      done;
      false
    in
    ignore (da#event#connect#expose expose);
  in

  let load_board () =
    st_push "Chargement d'un nouveau jeu...";

    let filew = GWindow.file_chooser_dialog ~action:`OPEN
                  ~title:"Ouvrir un fichier" ~border_width:0
                  ~width:320 ~height:240
                  () in
    filew#add_filter (GFile.filter ~name:"json" ~patterns:["*.json"] ());
    filew#add_button_stock `CANCEL `CANCEL;
    filew#add_select_button_stock `OPEN `OPEN;
    begin match filew#run(), filew#filename with
      `OPEN, Some filename ->
      filew#destroy ();
      prerr_endline ("ouverture du ficher "^filename);
      (try
        MapIO.open_map filename;
        window#set_title (MapIO.get_name());
        draw_board();
        st_flash 3000 "Chargement terminé.";
       with _ -> st_flash 3000 "Erreur de chargement :"
      (* TODO plus précis *)
      )
    | _ -> filew#destroy ();
           st#pop();
    end
 in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Nouveau jeu" ~key:_N ~callback:load_board);
  ignore (factory#add_item "Quitter" ~key:_Q ~callback:quit);

  (* automatic scrolling bars *)
  let scroll = GBin.scrolled_window
                 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                 ~packing:vbox#add () in

  (* pack the drawing area *)
  scroll#add_with_viewport da#coerce;

  (* add the status bar to the bottom of the main window *)
  vbox#pack status#coerce;
  st_push "Bienvenue dans notre jeu !";

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = Printexc.print main ()
(* let () = main () *)
