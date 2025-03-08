(*
   Select both lines and do C-c C-r to paste them into REPL
    #cd "/home/jacek/Programming/OCaml/gtk_minimal/";;
    #use ".ocamlinit";;

   Select the whole buffer pressing C-x h and then press C-c C-b to evaluate the whole buffer.

  You can do C-c C-b to evaluate the whole buffer or select the following line
  and doing C-c C-r
    # use "./bin/main.ml";;
*)

open Ctypes
open Foreign

(* libraries ================================================================ *)
let libgtk = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgtk-4.so"

let libgdk = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgdk-3.so"

let libgio = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgio-2.0.so"

let libgobject = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgobject-2.0.so"

let libglib = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libglib-2.0.so"

let libcairo = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libcairo.so.2"

(* types ==================================================================== *)
type window = unit ptr

let window : window typ = ptr void

type application = unit ptr

let application : application typ = ptr void

type widget = unit ptr

let widget : widget typ = ptr void

type gpointer = unit ptr

let gpointer : gpointer typ = ptr void

(* bindings ================================================================= *)
let gtk_application_new =
  foreign "gtk_application_new"
    (string @-> int @-> returning application)
    ~from:libgtk

let gtk_application_window_new =
  foreign "gtk_application_window_new"
    (application @-> returning window)
    ~from:libgtk

let window_set_title =
  foreign "gtk_window_set_title"
    (window @-> string @-> returning void)
    ~from:libgtk

let window_set_default_size =
  foreign "gtk_window_set_default_size"
    (window @-> int @-> int @-> returning void)
    ~from:libgtk

let widget_queue_draw =
  foreign "gtk_widget_queue_draw" (gpointer @-> returning void) ~from:libgtk

let widget_get_first_child =
  foreign "gtk_widget_get_first_child"
    (gpointer @-> returning widget)
    ~from:libgtk

(* let widget_show = *)
(*   foreign ~from:libgtk "gtk_widget_show" (widget @-> returning void) *)

let application_run =
  foreign "g_application_run"
    (application @-> int @-> gpointer @-> returning int)
    ~from:libgio

let object_unref =
  foreign "g_object_unref" (gpointer @-> returning void) ~from:libgobject

(* signal connect ----------------------------------------------------------- *)

(* https://docs.gtk.org/gtk4/signal.DrawingArea.resize.html *)
let signal_connect_resize widget s cb p =
  foreign "g_signal_connect_data"
    ( gpointer @-> string
    @-> funptr
          ( gpointer
          (* self *)
          @-> int
          (* width *)
          @-> int (* height *)
          @-> gpointer
          (* user_data *)
          @-> returning void )
    @-> gpointer @-> gpointer @-> int @-> returning void )
    widget s cb p null 0 ~from:libgobject

(* https://docs.gtk.org/gobject/func.signal_connect_data.html  *)
(* https://docs.gtk.org/gio/signal.Application.activate.html *)
let signal_connect_activate app s cb p =
  foreign "g_signal_connect_data"
    ( gpointer @-> string
    @-> funptr
          ( application
          (* self *)
          @-> gpointer
          (* user_data *)
          @-> returning void )
    @-> gpointer @-> gpointer @-> int @-> returning void )
    app s cb p null 0 ~from:libgobject

(* https://docs.gtk.org/gtk4/class.EventControllerKey.html *)
(* https://docs.gtk.org/gtk4/signal.EventControllerKey.key-pressed.html
   https://docs.gtk.org/gtk4/signal.EventControllerKey.key-released.html
 *)
let signal_connect_key_x w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr
          ( gpointer (* self *) @-> int
          (* keyval *)
          @-> int
          (* keycode *)
          @-> int
          (* state *)
          @-> gpointer
          (* user_data *)
          @-> returning void )
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

(* https://docs.gtk.org/gtk4/signal.EventControllerMotion.enter.html
doubles are not ints therefor I had zeroes
*)
let signal_connect_motion_enter w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr (gpointer @-> double @-> double @-> gpointer @-> returning void)
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

let signal_connect_motion_leave w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr (gpointer @-> gpointer @-> returning void)
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

(* https://docs.gtk.org/gtk4/signal.EventControllerScroll.scroll.html *)

let signal_connect_scroll w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr (gpointer @-> double @-> double @-> gpointer @-> returning void)
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

let signal_connect_click_pressed w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr
          ( gpointer @-> int @-> double @-> double @-> gpointer
          @-> returning void )
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

let signal_connect_click_released w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr
          ( gpointer @-> int @-> double @-> double @-> gpointer
          @-> returning void )
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject

let signal_connect_close_request w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
    @-> funptr (gpointer @-> gpointer @-> returning bool)
    @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgtk

(*
   let signal_connect_next-to-do w s cb p =
  foreign "g_signal_connect_data"
    ( widget @-> string
      @-> funptr
            ( )
      @-> gpointer @-> gpointer @-> int @-> returning void )
    w s cb p null 0 ~from:libgobject
*)
let timeout_add =
  foreign "g_timeout_add"
    ( int
    @-> funptr (gpointer @-> returning bool)
    @-> gpointer @-> returning void )
    ~from:libglib

let window_present =
  foreign "gtk_window_present" (widget @-> returning void) ~from:libgtk

let application_add_window =
  foreign "gtk_application_add_window"
    (application @-> gpointer @-> returning void)
    ~from:libgtk

let box_new =
  foreign "gtk_box_new" (int @-> int @-> returning widget) ~from:libgtk

let box_append =
  foreign "gtk_box_append"
    (gpointer @-> gpointer @-> returning void)
    ~from:libgtk

let window_set_child =
  foreign "gtk_window_set_child"
    (gpointer @-> gpointer @-> returning void)
    ~from:libgtk

let drawing_area_new =
  foreign "gtk_drawing_area_new" (void @-> returning widget) ~from:libgtk

let widget_set_vexpand =
  foreign "gtk_widget_set_vexpand"
    (widget @-> bool @-> returning void)
    ~from:libgtk

(* cairo ==================================================================== *)
let set_source_rgb =
  foreign "cairo_set_source_rgb"
    (gpointer @-> double @-> double @-> double @-> returning void)
    ~from:libcairo

let set_source_rgba =
  foreign "cairo_set_source_rgba"
    (gpointer @-> double @-> double @-> double @-> double @-> returning void)
    ~from:libcairo

let paint = foreign "cairo_paint" (gpointer @-> returning void) ~from:libcairo

let select_font_face =
  foreign "cairo_select_font_face"
    (gpointer @-> string @-> int @-> int @-> returning void)
    ~from:libcairo

let set_font_size =
  foreign "cairo_set_font_size"
    (gpointer @-> double @-> returning void)
    ~from:libcairo

(* cairo ==================================================================== *)
type cairo_text_extents_t

let cairo_text_extents_t : cairo_text_extents_t structure typ =
  structure "cairo_text_extents_t"

(* how do I define the structure elements  *)
let _x_bearing = field cairo_text_extents_t "x_bearing" double

let _y_bearing = field cairo_text_extents_t "y_bearing" double

let width = field cairo_text_extents_t "width" double

let height = field cairo_text_extents_t "height" double

let _x_advvance = field cairo_text_extents_t "x_advvance" double

let _y_advvance = field cairo_text_extents_t "y_advvance" double

(* tell ctypes that there are no more fields to come *)
let () = seal cairo_text_extents_t

let cairo_text_extents =
  foreign "cairo_text_extents"
    (gpointer @-> string @-> ptr cairo_text_extents_t @-> returning void)
    ~from:libcairo

let cairo_show_text =
  foreign "cairo_show_text"
    (gpointer @-> string @-> returning void)
    ~from:libcairo

let cairo_move_to =
  foreign "cairo_move_to"
    (gpointer @-> double @-> double @-> returning void)
    ~from:libcairo

(* https://www.cairographics.org/manual/cairo-Paths.html#cairo-rectangle *)

let cairo_rectangle =
  foreign "cairo_rectangle"
    (gpointer @-> double @-> double @-> double @-> double @-> returning void)
    ~from:libcairo

(* https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-fill  *)

let cairo_fill =
  foreign "cairo_fill" (gpointer @-> returning void) ~from:libcairo

(* ========================================================================== *)
(* ====================== gdk ============================== *)

type gdk_rgba

let gdk_rgba : gdk_rgba structure typ = structure "GdkRGBA"

let red = field gdk_rgba "red" float

let green = field gdk_rgba "green" float

let blue = field gdk_rgba "blue" float

let alpha = field gdk_rgba "alpha" float

(* no more fields to come *)
let () = seal gdk_rgba

let gdk_rgba_parse =
  foreign "gdk_rgba_parse"
    (ptr gdk_rgba @-> string @-> returning bool)
    ~from:libgdk

(* let _gdk_rgba_to_string = *)
(*   foreign "gdk_rgba_to_string" (ptr gdk_rgba @-> returning string) ~from:libgdk *)

let color_to_rgba color =
  let colpointer = make gdk_rgba in
  let valid_color = gdk_rgba_parse (addr colpointer) color in
  if valid_color then Some colpointer else None

let _color_to_rgba_values color =
  let colpointer = make gdk_rgba in
  let valid_color = gdk_rgba_parse (addr colpointer) color in
  if valid_color then
    let redc = getf colpointer red in
    let greenc = getf colpointer green in
    let bluec = getf colpointer blue in
    let alphac = getf colpointer alpha in
    [redc; greenc; bluec; alphac]
  else (
    Printf.printf "invalid color detected so using grey fallback\n" ;
    [0.50; 0.50; 0.50; 0.5] )

let gdk_keyval_name =
  foreign "gdk_keyval_name" (int @-> returning string) ~from:libgdk

(* ========================================================= *)
(* events *)

let event_controller_key_new =
  foreign "gtk_event_controller_key_new"
    (void @-> returning gpointer)
    ~from:libgtk

let event_controller_motion_new =
  foreign "gtk_event_controller_motion_new"
    (void @-> returning gpointer)
    ~from:libgtk

let event_controller_scroll_new =
  foreign "gtk_event_controller_scroll_new"
    (int @-> returning gpointer)
    ~from:libgtk

let gesture_click_new =
  foreign "gtk_gesture_click_new" (void @-> returning gpointer) ~from:libgtk

(* https://docs.gtk.org/gtk4/method.GestureSingle.set_button.html *)

(* void *)
(* gtk_gesture_single_set_button ( *)
(*   GtkGestureSingle* gesture, *)
(*   guint button *)
(* ) *)
let gesture_single_set_button =
  foreign "gtk_gesture_single_set_button"
    (gpointer @-> int @-> returning void)
    ~from:libgtk

(* https://docs.gtk.org/gtk4/method.GestureSingle.get_current_button.html *)
(* guint *)
(* gtk_gesture_single_get_current_button ( *)
(*   GtkGestureSingle* gesture *)
(* ) *)

let gesture_single_get_current_button =
  foreign "gtk_gesture_single_get_current_button"
    (gpointer @-> returning int)
    ~from:libgtk

let widget_add_controller =
  foreign "gtk_widget_add_controller"
    (widget @-> gpointer @-> returning void)
    ~from:libgtk

(* drawing *)
let drawing_area_set_draw_func =
  foreign "gtk_drawing_area_set_draw_func"
    ( widget
    @-> funptr
          (widget @-> gpointer @-> int @-> int @-> gpointer @-> returning void)
    @-> gpointer @-> gpointer @-> returning void )
    ~from:libgtk
;;

(* code ===================================================================== *)
Random.self_init ()

let set_color cr color =
  match color_to_rgba color with
  | None ->
      set_source_rgb cr 0.5 0.5 0.5
  | Some qrgb ->
      set_source_rgba cr (getf qrgb red) (getf qrgb green) (getf qrgb blue) 1.0

(* game model =============================================================== *)

type float_coordinate = {x: float; y: float}

type int_coordinate = {x: int; y: int}

type mouse_coordinate = None | Some of float_coordinate

type tile_coordinate = None | Some of int_coordinate

type model =
  { mc: mouse_coordinate
  ; width: int
  ; height: int
  ; tc: tile_coordinate
  ; key_pressed: bool }

type mine_state = Empty | Mined

type field_type = Covered | Flagged | Uncovered

type field = {mine_state: mine_state; field_type: field_type}

let initial_model = {mc= None; width= 0; height= 0; tc= None; key_pressed= false}

let my_model = ref initial_model

let init_model () =
  my_model := initial_model ;
  ()

let respond_to_press kc_name kc_value modifiers =
  Printf.printf "=== RESPONDING TO PRESS %s %s %d ===" kc_name kc_value
    modifiers ;
  ( match !my_model.mc with
  | None ->
      Printf.printf "mc none "
  | Some mmc ->
      Printf.printf " with mmc at %f %f " mmc.x mmc.y ) ;
  ( match !my_model.tc with
  | None ->
      Printf.printf "tc none\n%!"
  | Some mtc ->
      Printf.printf " with mmc at %d %d\n%!" mtc.x mtc.y ) ;
  ()

let imp_resize width height =
  my_model := {!my_model with width; height} ;
  ()

let imp_mouse_move x y =
  my_model := {!my_model with mc= Some {x; y}} ;
  ()

let imp_mouse_clear () =
  Printf.printf "======== clearing mc ==================\n%!" ;
  my_model := {!my_model with mc= None} ;
  ()

let imp_tile_coordinate_set x y =
  (* Printf.printf " setting tc %d %d \n%!" x y ; *)
  my_model := {!my_model with tc= Some {x; y}}

let imp_tile_coordinate_clear () =
  (* Printf.printf " clearinng tc \n%!" ; *)
  my_model := {!my_model with tc= None}

let grid_size = 8

let grid_indexes = List.init grid_size (fun a -> 1 + a - 1)

let grid_coordinates =
  List.init (grid_size * grid_size) (fun a -> 0 + a)
  |> List.map (fun n -> (n / grid_size, n mod grid_size))

(* https://discuss.ocaml.org/t/more-natural-preferred-way-to-shuffle-an-array/217/5 *)
let rec shuffle = function
  | [] ->
      []
  | [single] ->
      [single]
  | list ->
      let before, after = List.partition (fun _elt -> Random.bool ()) list in
      List.rev_append (shuffle before) (shuffle after)

(* with the coordinates working next i need to build a grid of fields some will contain mines  *)

let grid_mines = grid_coordinates |> shuffle |> List.take 10

let new_matrix =
  let ht = Hashtbl.create (grid_size * grid_size) in
  let _iter_mines =
    List.map
      (fun cc -> Hashtbl.add ht cc {mine_state= Mined; field_type= Covered})
      grid_mines
  in
  let _iter_empty =
    List.map
      (fun ri ->
        List.map
          (fun ci ->
            let mine_is_found = Hashtbl.find_opt ht (ri, ci) in
            match mine_is_found with
            | None ->
                Hashtbl.add ht (ri, ci) {mine_state= Empty; field_type= Covered}
            | Some mine ->
                Hashtbl.add ht (ri, ci) mine )
          grid_indexes )
      grid_indexes
  in
  ht

let my_fields = ref new_matrix

let imp_uncover_field () =
  Printf.printf "going to uncover field\n%!" ;
  let tc = !my_model.tc in
  match tc with
  | None ->
      ()
  | Some mtc ->
      let current = Hashtbl.find !my_fields (mtc.y, mtc.x) in
      let new_field_type =
        match current.field_type with
        | Covered ->
            Uncovered
        | _ ->
            current.field_type
      in
      let newf = {mine_state= current.mine_state; field_type= new_field_type} in
      Hashtbl.add !my_fields (mtc.y, mtc.x) newf ;
      ()

let imp_toggle_field_flag () =
  Printf.printf "going to toggle field flag\n%!" ;
  let tc = !my_model.tc in
  match tc with
  | None ->
      ()
  | Some mtc ->
      let current = Hashtbl.find !my_fields (mtc.y, mtc.x) in
      let new_field_type =
        match current.field_type with
        | Covered ->
            Flagged
        | Flagged ->
            Covered
        | Uncovered ->
            Uncovered
      in
      let newf = {mine_state= current.mine_state; field_type= new_field_type} in
      Hashtbl.add !my_fields (mtc.y, mtc.x) newf ;
      ()

let draw_game_top_text_line1 cr =
  set_source_rgb cr 0.9 0.7 0.0 ;
  paint cr ;
  set_source_rgb cr 0.0 0.0 0.0 ;
  select_font_face cr "DejaVu Sans" 0 0 ;
  set_font_size cr 21.2 ;
  let text_string =
    match !my_model.mc with
    | None ->
        Printf.sprintf "OCaml model  %d %d" !my_model.width !my_model.height
    | Some mmc ->
        Printf.sprintf "OCaml model4 %.1f %.1f  %d %d  at %s" mmc.x mmc.y
          !my_model.width !my_model.height
          ( match !my_model.tc with
          | None ->
              "none"
          | Some mtc ->
              Printf.sprintf "%d-%d  - %s" mtc.x mtc.y
                (let field = Hashtbl.find !my_fields (mtc.y, mtc.x) in
                 match field with
                 | {mine_state= Empty; field_type= Covered} ->
                     "EC"
                 | {mine_state= Empty; field_type= Flagged} ->
                     "EF"
                 | {mine_state= Empty; field_type= Uncovered} ->
                     "EU"
                 | {mine_state= Mined; field_type= Covered} ->
                     "MC"
                 | {mine_state= Mined; field_type= Flagged} ->
                     "MF"
                 | {mine_state= Mined; field_type= Uncovered} ->
                     "MU" ) )
  in
  (* zzz *)
  let tc = addr (make cairo_text_extents_t) in
  cairo_text_extents cr text_string tc ;
  let twidth = !@(tc |-> width) in
  let _theight = !@(tc |-> height) in
  (* zzz *)
  cairo_move_to cr ((600. /. 2.) -. (twidth /. 2.)) 30. ;
  cairo_show_text cr text_string ;
  ()

let draw_game_top_text_line2 cr =
  set_color cr "blue" ;
  select_font_face cr "DejaVu Sans" 0 0 ;
  set_font_size cr 15.0 ;
  let text_string = "tra la la" in
  cairo_move_to cr 10.0 50.0 ;
  cairo_show_text cr text_string ;
  ()

let draw_game_top_text cr =
  draw_game_top_text_line1 cr ;
  draw_game_top_text_line2 cr ;
  ()

let diagnosing_mover mover ri ci =
  (* Printf.printf "diagnosing mover\n%!" ; *)
  (* Printf.printf "%s\n%!" (if mover then "mover" else "00000000") ; *)
  ( if mover then imp_tile_coordinate_set ci ri
    else
      match !my_model.tc with
      | None ->
          ()
      | Some mtc ->
          if mtc.x == ci && mtc.y == ri then imp_tile_coordinate_clear ()
          else () ) ;
  ()

let is_mouse_over tx ty bx by =
  match !my_model.mc with
  | None ->
      false
  | Some mmc ->
      let mx = mmc.x in
      let my = mmc.y in
      tx <= mx && mx <= bx && ty <= my && my <= by

let neighbours3 = List.init 3 (fun a -> -1 + a)

let withneighbours ci ri w h =
  List.map
    (fun cix -> List.map (fun rix -> (ci + cix, ri + rix)) neighbours3)
    neighbours3
  |> List.flatten
  |> List.filter (fun (a, b) -> not (a == ci && b == ri))
  |> List.filter (fun (a, b) -> a >= 0 && b >= 0)
  |> List.filter (fun (a, b) -> a < w && b < h)

let tile_text ci ri =
  Printf.sprintf "%d-%d %d" ci ri
    ( withneighbours ci ri grid_size grid_size
    |> List.filter (fun (a, b) ->
           let f = Hashtbl.find !my_fields (a, b) in
           f.mine_state == Mined )
    |> List.length )

let draw_game_matrix cr =
  let ht = new_matrix in
  let _zzz =
    List.map
      (fun ci ->
        List.map
          (fun ri ->
            let field = Hashtbl.find ht (ri, ci) in
            let offset_x = 100. in
            let offset_y = 70. in
            let wh = 48.0 in
            let tx = offset_x +. (float_of_int ci *. 50.0) in
            let ty = offset_y +. (float_of_int ri *. 50.0) in
            let bx = tx +. wh in
            let by = ty +. wh in
            let mover = is_mouse_over tx ty bx by in
            diagnosing_mover mover ri ci ;
            ( match field with
            | {mine_state= Empty; field_type= Covered} ->
                set_color cr "lime"
            | {mine_state= Empty; field_type= Flagged} ->
                set_color cr "yellow"
            | {mine_state= Empty; field_type= Uncovered} ->
                set_color cr "pink"
            | {mine_state= Mined; field_type= Covered} ->
                set_color cr "coral"
            | {mine_state= Mined; field_type= Flagged} ->
                set_color cr "purple"
            | {mine_state= Mined; field_type= Uncovered} ->
                set_color cr "red" )
            (* go to location for ri ci *) ;
            cairo_rectangle cr tx ty wh wh ;
            (* draw rectangle *)
            cairo_fill cr ;
            (* next task, drawn the number of neighbouring mines *)
            set_color cr "blue" ;
            select_font_face cr "DejaVu Sans" 0 0 ;
            set_font_size cr 15.0 ;
            let text_string = tile_text ci ri in
            cairo_move_to cr (tx +. 5.0) (ty +. 15.0) ;
            cairo_show_text cr text_string ;
            () )
          grid_indexes )
      grid_indexes
  in
  ()

let draw_game cr = draw_game_top_text cr ; draw_game_matrix cr ; ()

(* gtk functions ============================================================ *)
let cairo_draw_func _area cr _width _height _data = draw_game cr ; ()

(* key names *)
(* https://gitlab.gnome.org/GNOME/gtk/-/blob/main/gdk/gdkkeysyms.h *)
(* /usr/include/gtk-4.0/gdk/gdkkeysyms.h *)

let kc_value kc = if kc <= 255 then String.make 1 (Char.chr kc) else ""

let key_pressed_func _w kc kv s _z =
  my_model := {!my_model with key_pressed= true} ;
  Printf.printf "key kc 0x%x %d kv %d s %d  %s '%s'\n" kc kc kv s
    (gdk_keyval_name kc) (kc_value kc) ;
  Printf.printf "zzzzzzzz %s\n" (gdk_keyval_name kc) ;
  Printf.printf "%!" ;
  respond_to_press (gdk_keyval_name kc) (kc_value kc) s ;
  ()

let key_released_func _w kc kv s _z =
  let kc_value kc = if kc <= 255 then String.make 1 (Char.chr kc) else "" in
  my_model := {!my_model with key_pressed= false} ;
  Printf.printf "key presed %s\n" (if !my_model.key_pressed then "P" else "N") ;
  Printf.printf "released key kc 0x%x %d kv %d s %d  %s '%s'\n" kc kc kv s
    (gdk_keyval_name kc) (kc_value kc) ;
  Printf.printf "%!" ;
  ()

let motion_func _a x y _b =
  (* Printf.printf "motion %f %f\n" x y ; *)
  (* Printf.printf "%!" ; *)
  imp_mouse_move x y ; ()

let enter_func _a x y _b =
  Printf.printf "enter %f %f\n" x y ;
  Printf.printf "%!" ;
  imp_mouse_move x y ;
  ()

let leave_func _a _b =
  Printf.printf "leave\n" ; Printf.printf "%!" ; imp_mouse_clear () ; ()

(* returning true prevents closing the window *)
let close_request_func _self _ud =
  Printf.printf "attempting to close window\n%!" ;
  false

(* https://docs.gtk.org/gtk4/signal.DrawingArea.resize.html *)
let resize_func _w width height _ud =
  Printf.printf "resizing %d %d\n" width height ;
  Printf.printf "%!" ;
  imp_resize width height ;
  ()

let scroll_func _w dx dy _ud =
  Printf.printf "scrolling %f %f\n" dx dy ;
  Printf.printf "%!" ;
  ()

let current_button_name current_button =
  match current_button with 1 -> "LMB" | 2 -> "MMB" | 3 -> "RMB" | _ -> "HUH?"

let pressed_func a bn x y _b =
  let current_button = gesture_single_get_current_button a in
  Printf.printf "pressed %s %d %f %f\n"
    (current_button_name current_button)
    bn x y ;
  Printf.printf "%!" ;
  ( match current_button with
  | 1 ->
      imp_uncover_field ()
  | 2 ->
      ()
  | 3 ->
      imp_toggle_field_flag ()
  | _ ->
      () ) ;
  ()

let released_func a bn x y _b =
  let current_button = gesture_single_get_current_button a in
  Printf.printf "released %s %d %f %f\n"
    (current_button_name current_button)
    bn x y ;
  Printf.printf "%!" ;
  ()

(* events =================================================================== *)

let redraw_canvas window =
  widget_queue_draw (widget_get_first_child (widget_get_first_child window)) ;
  ()

let window_events _app window =
  let key_controller = event_controller_key_new () in
  widget_add_controller window key_controller ;
  signal_connect_key_x key_controller "key-pressed" key_pressed_func null ;
  signal_connect_key_x key_controller "key-released" key_released_func null ;
  signal_connect_close_request window "close-request" close_request_func null ;
  (* signal_connect_activate app "activate" activate null ; *)
  timeout_add 25
    (fun _ptr ->
      redraw_canvas window ;
      (* Printf.printf "OCaml model %f %f %d %d" !my_model.x !my_model.y *)
      (*   !my_model.width !my_model.height ; *)
      (* Printf.printf "timeout \n" ; *)
      (* Printf.printf "%!" ; *)
      true )
    null ;
  ()

let canvas_events canvas =
  let motion_controller = event_controller_motion_new () in
  let scroll_controller = event_controller_scroll_new 1 in
  let gesture_click_controller = gesture_click_new () in
  widget_add_controller canvas motion_controller ;
  widget_add_controller canvas scroll_controller ;
  gesture_single_set_button gesture_click_controller 0 ;
  widget_add_controller canvas gesture_click_controller ;
  signal_connect_motion_enter motion_controller "motion" motion_func null ;
  signal_connect_motion_enter motion_controller "enter" enter_func null ;
  signal_connect_motion_leave motion_controller "leave" leave_func null ;
  signal_connect_scroll scroll_controller "scroll" scroll_func null ;
  signal_connect_click_pressed gesture_click_controller "pressed" pressed_func
    null ;
  signal_connect_click_released gesture_click_controller "released"
    released_func null ;
  signal_connect_resize canvas "resize" resize_func null ;
  ()

(* finsh me *)
(*

    notify

 *)

(* activation =============================================================== *)
let activate : application -> gpointer -> unit =
 fun app _data ->
  let win = gtk_application_window_new app in
  application_add_window app win ;
  window_set_title win "Gtk Minimal" ;
  window_set_default_size win 600 490 ;
  (* create box with orientation vertical and spacing 0 *)
  let box = box_new 1 0 in
  let canvas = drawing_area_new () in
  widget_set_vexpand canvas true ;
  drawing_area_set_draw_func canvas cairo_draw_func null null ;
  canvas_events canvas ;
  box_append box canvas ;
  window_set_child win box ;
  window_events app win ;
  window_present win ;
  init_model () ;
  ()

let main () =
  Random.init 64 ;
  let app = gtk_application_new "org.gtk.example" 0 in
  signal_connect_activate app "activate" activate null ;
  let status = application_run app 0 null in
  object_unref app ;
  print_endline @@ string_of_int status ;
  ()
;;

(* ================ *)
main ()
