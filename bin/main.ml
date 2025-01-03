(*
   Select both lines and do C-c C-r to paste them into REPL
    #cd "/home/jacek/Programming/OCaml/ocaml_experiments/gtk_minimal";;
    #use ".ocamlinit";;
  You can do C-c C-b to evaluate the whole buffer or select the following line
  and doing C-c C-r
    # use "./bin/main.ml";;
*)

open Ctypes
open Foreign

let libgtk = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgtk-4.so"

let libgio = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgio-2.0.so"

let libgobject = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgobject-2.0.so"

let libcairo = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libcairo.so.2"

type window = unit ptr

let window : window typ = ptr void

type application = unit ptr

let application : application typ = ptr void

type widget = unit ptr

let widget : widget typ = ptr void

type gpointer = unit ptr

let gpointer : gpointer typ = ptr void

let callback_t = application @-> gpointer @-> returning void

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

(* let widget_show = *)
(*   foreign ~from:libgtk "gtk_widget_show" (widget @-> returning void) *)

let application_run =
  foreign "g_application_run"
    (application @-> int @-> gpointer @-> returning int)
    ~from:libgio

let object_unref =
  foreign "g_object_unref" (gpointer @-> returning void) ~from:libgobject

let signal_connect app s cb p =
  foreign "g_signal_connect_data"
    ( gpointer @-> string @-> funptr callback_t @-> gpointer @-> gpointer
    @-> int @-> returning void )
    app s cb p null 0 ~from:libgobject

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

let set_source_rgb =
  foreign "cairo_set_source_rgb"
    (gpointer @-> double @-> double @-> double @-> returning void)
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

let text_extents =
  foreign "cairo_text_extents"
    (gpointer @-> string @-> ptr cairo_text_extents_t @-> returning void)
    ~from:libcairo

let show_text =
  foreign "cairo_show_text"
    (gpointer @-> string @-> returning void)
    ~from:libcairo

let move_to =
  foreign "cairo_move_to"
    (gpointer @-> double @-> double @-> returning void)
    ~from:libcairo

let cairo_draw_func _area cr _width _height _data =
  set_source_rgb cr 0.9 0.0 0.0 ;
  paint cr ;
  set_source_rgb cr 0.0 0.0 0.0 ;
  select_font_face cr "DejaVu Sans" 0 0 ;
  set_font_size cr 21.2 ;
  let text_string = "OCaml is centered" in
  (* zzz *)
  let tc = addr (make cairo_text_extents_t) in
  text_extents cr text_string tc ;
  let twidth = !@(tc |-> width) in
  let theight = !@(tc |-> height) in
  (* zzz *)
  move_to cr ((600. /. 2.) -. (twidth /. 2.)) ((400.0 /. 2.) -. (theight /. 2.)) ;
  show_text cr text_string ;
  ()

let drawing_area_set_draw_func =
  foreign "gtk_drawing_area_set_draw_func"
    ( widget
    @-> funptr
          (widget @-> gpointer @-> int @-> int @-> gpointer @-> returning void)
    @-> gpointer @-> gpointer @-> returning void )
    ~from:libgtk

(* events *)

let event_controller_key_new =
  foreign "gtk_event_controller_key_new"
    (void @-> returning gpointer)
    ~from:libgtk

let widget_add_controller =
  foreign "gtk_widget_add_controller"
    (widget @-> gpointer @-> returning void)
    ~from:libgtk

let get_keyval =
  foreign "gdk_key_event_get_keyval" (gpointer @-> returning int) ~from:libgtk

(* Events
  https://docs.gtk.org/gdk4/class.Event.html
  https://docs.gtk.org/gdk4/keys.html
  https://docs.gtk.org/gdk4/class.KeyEvent.html
  https://docs.gtk.org/gdk4/method.KeyEvent.get_keyval.html
 *)

(* finish the following *)
let key_pressed_func : gpointer -> gpointer -> unit =
 (* while it compiles and runs, the ev is not the event *)
 fun ev _args ->
  print_endline "Key" ;
  Printf.printf "%x" (get_keyval ev) ;
  ()

let window_events _app window =
  let key_controller = event_controller_key_new () in
  widget_add_controller window key_controller ;
  (* finish me *)
  signal_connect key_controller "key_pressed" key_pressed_func null ;
  ()

let canvas_events _canvas =
  (* finsh me *)
  ()

(* activation *)
let activate : application -> gpointer -> unit =
 fun app _data ->
  let win = gtk_application_window_new app in
  application_add_window app win ;
  window_set_title win "Gtk Minimal" ;
  window_set_default_size win 600 400 ;
  (* create box with orientation vertical and spacing 0 *)
  let box = box_new 1 0 in
  let canvas = drawing_area_new () in
  widget_set_vexpand canvas true ;
  drawing_area_set_draw_func canvas cairo_draw_func null null ;
  canvas_events canvas ;
  box_append box canvas ;
  window_set_child win box ;
  window_events app win ;
  window_present win

let main () =
  let app = gtk_application_new "org.gtk.example" 0 in
  signal_connect app "activate" activate null ;
  let status = application_run app 0 null in
  object_unref app ;
  print_endline @@ string_of_int status
;;

(* ================ *)
main ()
