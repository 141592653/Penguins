open GMain
open GdkKeysyms
open MapIO

let _locale = GtkMain.Main.init ()

let font_name = "fixed"
(* let font_name = "-*-helvetica-medium-r-normal-*-120-*" *)

(* TODO: erreur différente *)
let font = try
    Gdk.Font.load font_name
  with
    Gpointer.Null -> failwith ("gui: font " ^ font_name ^ ": not found")

(* Fenêtre principale *)
let window = GWindow.window ~width:320 ~height:240
                            ~title:"Jeu des pingouins" ()

(* Status bar to print game informations*)
let status = GMisc.statusbar ()
let st = status#new_context ~name:"st"

(* use these functions to display messages in the status bar *)
let st_push s = ignore (st#pop(); st#push s)
let st_flash ?delay:(delay=5000) s = ignore (st#pop(); st#flash ~delay s)

(* Drawing area to display the game board *)
let da = GMisc.drawing_area ()


(** handle quit signal, ask to save game if necessary *)
let quit () =
  prerr_endline "Goodbye!";
  (* TODO *)
  Main.quit()


(* TODO gestion d'erreur, assert sur les tailles d'image *)
let pixbuf_ice = GdkPixbuf.from_file "img/ice.png"
let pixbuf_water = GdkPixbuf.from_file "img/water.png"
let pixbuf_penguin = GdkPixbuf.from_file "img/penguin.png"
let pixbuf_penguin_s = GdkPixbuf.from_file "img/penguin_s.png"


(* handle mouse clicks on the game board *)
let button_pressed ev =
  if GdkEvent.Button.button ev = 1 then (
    let (m,n) = MapIO.dimensions () in
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
        if i <= (jj *13)/26
        then                (* Nord *)
          (if di > 0 then (di*2-1, dj) else (-1,-1))
        else if i >= 54 - (jj * 13)/26
        then                (* Sud *)
          (if di < m-1 then (di*2+1, dj) else (-1,-1))
        else (if b then     (* Ouest *)
                (if dj > 0 then (di*2, dj-1) else (-1,-1))
              else          (* Ici *)
                (di*2,dj))
    in
    let mouse_to_coord (x,y) =
      let (i,j) = mouse_to_coord_ (x,y) in
      if i >= m || j >= n then (-1,-1) else (i,j)
    in
    let (i,j) = mouse_to_coord (x,y) in
    Printf.printf "(%d,%d)\n%!" i j);
    true


(* draw the board (hexagons etc.) in the drawing area *)
let draw_board () =

  let (m,n) = MapIO.dimensions () in
  da#set_size ~height:(50*m) ~width:(56*n);

  let expose _ =
    (* position of the current player *)
    let (i_c,j_c) = (MapIO.get_players()).(MapIO.get_turn())#get_pos in
    let draw = new GDraw.drawable da#misc#window in
    (* draw#put_pixbuf ~x:0 ~y:0 pixbuf_ice; *)
    (* draw#put_pixbuf ~x:53 ~y:0 pixbuf_water; *)
    (* draw#put_pixbuf ~x:26 ~y:46 pixbuf_ice; *)
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        let x = j * 53 + (if i mod 2 = 1 then 0 else 26) in
        let y = i * 46 in
        match MapIO.get_cell i j with
        | ICE(v) ->  draw#put_pixbuf ~x ~y pixbuf_ice;
                     draw#string (string_of_int v) ~font ~x:(x+29) ~y:(y+37)
        | WATER -> draw#put_pixbuf ~x ~y pixbuf_water;
        | PENGUIN -> if (i,j) = (i_c,j_c)
                     then draw#put_pixbuf ~x ~y pixbuf_penguin_s
                     else draw#put_pixbuf ~x ~y pixbuf_penguin
      done;
    done;
    false
  in
  ignore (da#event#connect#expose ~callback:expose)

(* play until the turn of a human player *)
let play () =
  let players = MapIO.get_players() in
  let turn = MapIO.get_turn() in
  draw_board();
  st_push ("Au tour du joueur " ^ (string_of_int turn))
(* TODO *)

let load_game filename =
  try
    prerr_endline ("ouverture du ficher "^filename);
    MapIO.open_map filename;
    window#set_title (MapIO.get_name());
    st_flash "Chargement terminé.";
    play()
  with Failure s -> st_flash ~delay:7000 ("Erreur de chargement : "^s)

let chose_file () =
  st_push "Chargement d'un nouveau jeu...";
  let filew = GWindow.file_chooser_dialog
                ~action:`OPEN
                ~title:"Ouvrir un fichier" ~border_width:0
                ~width:320 ~height:240
                () in
  filew#add_filter (GFile.filter ~name:"json" ~patterns:["*.json"] ());
  filew#add_button_stock `CANCEL `CANCEL;
  filew#add_select_button_stock `OPEN `OPEN;
  begin match filew#run(), filew#filename with
  | `OPEN, Some filename ->
     load_game filename;
     filew#destroy ();
  | _ -> filew#destroy ();
         st#pop();
  end

let [@warning "-48"] main () =
  (* warning -48 because we like implicit elimination of optional arguments *)

  ignore (window#connect#destroy ~callback:quit);

  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "Fichier" in

  da#event#add [`BUTTON_PRESS];
  ignore (da#event#connect#button_press ~callback:button_pressed);

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "Nouveau jeu" ~key:_N ~callback:chose_file);
  ignore (factory#add_item "Ouvrir un fichier" ~key:_O ~callback:chose_file);
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

  GMain.main ()
