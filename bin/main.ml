(*
  Select both lines and do C-c C-r to paste them into REPL
    #cd "/home/jacek/Programming/OCaml/ocaml_experiments/gtk_minimal";;
    #use ".ocamlinit";;
  You can do C-c C-b to evaluate the whole buffer or select the following line
  and doing C-c C-r
    # use "./bin/main.ml";;
 *)

open Cairo
open Ctypes
open Foreign

let libgtk = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgtk-4.so"

let libgio = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgio-2.0.so"

let libgobject = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libgobject-2.0.so"

type window = unit ptr

let window : window typ = ptr void

type application = unit ptr

let application : application typ = ptr void

type widget = unit ptr

let widget : widget typ = ptr void

type gpointer = unit ptr

let gpointer : gpointer typ = ptr void

type cairo_t

let cairo_t : cairo_t structure typ = structure "_cairo"

let callback_t = application @-> gpointer @-> returning void

let gtk_application_new =
  foreign ~from:libgtk "gtk_application_new"
    (string @-> int @-> returning application)

let gtk_application_window_new =
  foreign ~from:libgtk "gtk_application_window_new"
    (application @-> returning window)

let window_set_title =
  foreign ~from:libgtk "gtk_window_set_title"
    (window @-> string @-> returning void)

let window_set_default_size =
  foreign ~from:libgtk "gtk_window_set_default_size"
    (window @-> int @-> int @-> returning void)

(* let widget_show = *)
(*   foreign ~from:libgtk "gtk_widget_show" (widget @-> returning void) *)

let application_run =
  foreign ~from:libgio "g_application_run"
    (application @-> int @-> gpointer @-> returning int)

let object_unref =
  foreign ~from:libgobject "g_object_unref" (gpointer @-> returning void)

let signal_connect app s cb p =
  foreign ~from:libgobject "g_signal_connect_data"
    ( application @-> string @-> funptr callback_t @-> gpointer @-> gpointer
    @-> int @-> returning void )
    app s cb p null 0

let window_present =
  foreign ~from:libgtk "gtk_window_present" (widget @-> returning void)

let application_add_window =
  foreign ~from:libgtk "gtk_application_add_window"
    (application @-> gpointer @-> returning void)

let box_new =
  foreign ~from:libgtk "gtk_box_new" (int @-> int @-> returning widget)

let drawing_area_new =
  foreign ~from:libgtk "gtk_drawing_area_new" (void @-> returning widget)

let widget_set_vexpand =
  foreign ~from:libgtk "gtk_widget_set_vexpand"
    (widget @-> bool @-> returning void)

let cairo_draw_func _area cr _width _height _data =
  set_source_rgb cr 0.9 0.0 0.0 ;
  select_font_face cr "DejaVu Sans" ~weight:Bold ;
  set_font_size cr 1.2 ;
  let te = text_extents cr "a" in
  move_to cr
    (0.5 -. (te.width /. 2.) -. te.x_bearing)
    (0.5 -. (te.height /. 2.) -. te.y_bearing) ;
  show_text cr "a"

let drawing_area_set_draw_func =
  foreign ~from:libgtk "gtk_drawing_area_set_draw_func"
    ( widget
    @-> funptr
          ( widget @-> ptr cairo_t @-> int @-> int @-> gpointer
          @-> returning void )
    @-> gpointer @-> gpointer @-> returning void )

let activate : application -> gpointer -> unit =
 fun app _data ->
  let win = gtk_application_window_new app in
  application_add_window app win ;
  window_set_title win "Gtk Minimal" ;
  window_set_default_size win 600 400 ;
  (* create box with orientation vertical and spacing 0 *)
  let _box = box_new 1 0 in
  let canvas = drawing_area_new () in
  widget_set_vexpand canvas true ;
  drawing_area_set_draw_func canvas cairo_draw_func null null ;
  (* set canvas events *)
  (* append canvas to box *)
  (* set box as child of win *)
  (* set win events *)
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
