open GMain
open GdkKeysyms
open MapIO

let _locale = GtkMain.Main.init ()

let font_name = "fixed"
(* let font_name = "-*-helvetica-medium-r-normal-*-120-*" *)

let filename = ref ""

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
let st_flash ?delay:(delay=5000) s = ignore ((* st#pop(); *) st#flash ~delay s)

(* Drawing area to display the game board *)
let da = GMisc.drawing_area ()

(* Vrai lorsque c'est le tour d'un joueur humain *)
let click_request = ref false

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
let rec play () =
  let players = MapIO.get_players() in
  let turn = MapIO.get_turn() in
  draw_board();
  let player = players.(turn) in
  st_push ("Au tour du joueur " ^ player#get_name);
  (* TODO end game ? *)
  if player#is_human
  then
    click_request := true
  else begin
      player#play;
      (* TODO actualiser ? *)
      MapIO.next_turn();
      play();
    end


(* return the Hex.move from initial position to destination *)
(* invalid moves have second component equal to -1 *)
let move_of_pos (i_s,j_s) (i_d,j_d) =
  (* Printf.printf "(%d,%d)(%d,%d)\n%!" i_s j_s i_d j_d; *)
  if i_s = -1 || i_d = -1 then
    (Hex.E,-1)
  else if i_s = i_d
  then ((if j_s < j_d then Hex.E else Hex.W), (abs (j_s - j_d)))
  else let dy = i_s - i_d and dx = j_s - j_d in
       let x = abs dx and y = abs dy in
       (* Printf.printf "%d %d %d %d\n%!" dy y dx x; *)
       if i_s mod 2 = 0
       then
         if x = y/2 && dx >= 0
         then ((if dy > 0 then Hex.NW else Hex.SW),y)
         else if x = (y+1)/2 && dx < 0
         then ((if dy > 0 then Hex.NE else Hex.SE),y)
         else (Hex.E,-1)
       else
         if x = (y+1)/2 && dx > 0
         then ((if dy > 0 then Hex.NW else Hex.SW),y)
         else if x = y/2 && dx <= 0
         then ((if dy > 0 then Hex.NE else Hex.SE),y)
         else (Hex.E,-1)

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
    Printf.printf "(%d,%d)\n%!" i j;

    if !click_request
    then begin
        try
          let player = (MapIO.get_players()).(MapIO.get_turn()) in
          MapIO.move player#get_name (move_of_pos player#get_pos (i,j));
          click_request := false;
          MapIO.next_turn();
          play()
        with Invalid_argument s -> st_flash ~delay:3000 s
      end
  );
  true

let load_game file =
  try
    prerr_endline ("ouverture du ficher "^file);
    MapIO.open_map file;
    window#set_title (MapIO.get_name());
    st_flash "Chargement terminé.";
    filename := file;
    play()
  with Failure s -> st_flash ~delay:7000 ("Erreur de chargement : "^s)

let reload_game () =
  if !filename = ""
  then st_flash "Impossible de recharger la partie"
  else load_game !filename

let chose_game () =
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
     filew#destroy ();
     load_game filename;
  | _ -> filew#destroy ();
         st#pop();
  end

let [@warning "-48"] new_game () =
  let ask_options mapname =
    let dialog = GWindow.dialog ~title:"Créer une nouvelle partie"
                                ~modal:true (* freeze the rest of the program *)
                                ~allow_grow:true (* TODO only vertical *)
                                ~allow_shrink:false
                                ~height:250
                                () in
    let scroll = GBin.scrolled_window
                   ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
                   ~packing:(dialog#vbox#pack ~expand:true) () in
    let vbox = GPack.vbox ~packing:scroll#add_with_viewport () in

    let nb_players = 5 in   (* TODO récupérer npayers à partir de la map *)
    (* tableau des champs utiles à l'initialisation des données pour chaque
     joueur *)
    let tab = Array.make nb_players (GEdit.entry(),GEdit.combo_box_text()) in

    for i = 0 to nb_players-1 do
      let frame = GBin.frame ~label:("Joueur "^(string_of_int i))
                             ~border_width:3
                             ~packing:vbox#pack () in

      let hbox = GPack.hbox ~spacing:7
                            ~border_width:6
                            ~packing:frame#add () in

      ignore (GMisc.label ~text:"Nom"
                          ~packing:(hbox#pack ~expand:false) ());

      let entry = GEdit.entry ~text:("Joueur "^(string_of_int i))
                              ~max_length:40
                              ~has_frame:true
                              ~packing:hbox#add () in
      let combo = GEdit.combo_box_text
                    ~active:0
                    ~strings:["Humain";"IA Standard"]
                    ~packing:(hbox#pack ~expand:false) () in

      tab.(i) <- (entry,combo)
    done;

    dialog#action_area#set_layout `END;
    dialog#action_area#set_homogeneous false; (* TODO *)
    ignore (GMisc.label
                  ~text:"Premier tour"
                  ~packing:dialog#action_area#add ());
    (* liste des n premiers entiers *)
    let nlist n =
      let rec aux a =
        if a >= n then [] else a :: aux (a+1) in
      aux 0
    in
    let combo_turn = GEdit.combo_box_text
                       ~active:0
                       ~strings:(List.map string_of_int (nlist nb_players))
                       ~packing:(dialog#action_area#pack ~expand:false)() in

    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_button_stock `OK `OK;

    (* Récupérer les valeur entrées par l'utilisateur *)
    match dialog#run() with
    | `OK ->
       let turn = match (GEdit.text_combo_get_active combo_turn) with
         | None -> 0
         | Some(txt) -> int_of_string txt
       in
       for i = 0 to nb_players-1 do
         let (entry,combo) = tab.(i) in
         let txt = match (GEdit.text_combo_get_active combo) with
           | None -> "Humain"
           | Some(txt) -> txt
         in
         let player_name = match entry#text with
           | "" -> "Joueur "^(string_of_int i);
           | s -> s
         in
         ()
       done;
       (* TODO initialiser partie avec mapname et infos des joueurs *)
       dialog#destroy();
       st#pop();
    | _ -> dialog#destroy();
           st#pop();
  in

  st_push "Création d'un nouveau jeu...";
  let filew = GWindow.file_chooser_dialog
                ~action:`OPEN
                ~title:"Ouvrir une carte" ~border_width:0
                ~width:320 ~height:240
                () in
  filew#add_filter (GFile.filter ~name:"txt" ~patterns:["*.txt"] ());
  filew#add_button_stock `CANCEL `CANCEL;
  filew#add_select_button_stock `OPEN `OPEN;
  begin match filew#run(), filew#filename with
  | `OPEN, Some filename ->
     filew#destroy ();
     ask_options filename;
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
  ignore (factory#add_item "Nouveau jeu" ~key:_N ~callback:new_game);
  ignore (factory#add_item "Ouvrir un fichier" ~key:_O ~callback:chose_game);
  ignore (factory#add_item "Quitter" ~key:_Q ~callback:quit);
  ignore (factory#add_item "Recharger" ~key:_R ~callback:reload_game);

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
