(*
   Select both lines and do C-c C-r to paste them into REPL
    #cd "/home/jacek/Programming/OCaml/gtk_minimal/";;
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

let libglib = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libglib-2.0.so"

let libcairo = Dl.dlopen ~flags:Dl.[RTLD_NOW] ~filename:"libcairo.so.2"

type window = unit ptr

let window : window typ = ptr void

type application = unit ptr

let application : application typ = ptr void

type widget = unit ptr

let widget : widget typ = ptr void

type gpointer = unit ptr

let gpointer : gpointer typ = ptr void

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

let widget_add_controller =
  foreign "gtk_widget_add_controller"
    (widget @-> gpointer @-> returning void)
    ~from:libgtk

(* key names *)
(* https://gitlab.gnome.org/GNOME/gtk/-/blob/main/gdk/gdkkeysyms.h *)
(* /usr/include/gtk-4.0/gdk/gdkkeysyms.h *)

(* because no parameter is passed this is global and executed once *)
let codes =
  let gdkkeysyms_file = "/usr/include/gtk-4.0/gdk/gdkkeysyms.h" in
  let ht = Hashtbl.create 2300 in
  let _gathered_codes =
    In_channel.with_open_text gdkkeysyms_file In_channel.input_lines
    |> List.filter (fun s -> String.starts_with ~prefix:"#def" s)
    |> List.map (fun s -> String.split_on_char (Char.chr 32) s)
    |> List.map (fun a ->
           Hashtbl.add ht (int_of_string (List.nth a 2)) (List.nth a 1) )
  in
  Printf.printf "creating key codes hash should be done once\n" ;
  ht

let find_code kc = Hashtbl.find codes kc

let key_pressed_func _w kc kv s _z =
  let kc_value kc = if kc <= 255 then String.make 1 (Char.chr kc) else "" in
  let kc_name = find_code kc in
  Printf.printf "key kc 0x%x %d kv %d s %d  %s '%s'\n" kc kc kv s kc_name
    (kc_value kc) ;
  Printf.printf "%!" ;
  ()

let key_released_func _w kc kv s _z =
  let kc_value kc = if kc <= 255 then String.make 1 (Char.chr kc) else "" in
  let kc_name = find_code kc in
  Printf.printf "released key kc 0x%x %d kv %d s %d  %s '%s'\n" kc kc kv s
    kc_name (kc_value kc) ;
  Printf.printf "%!" ;
  ()

(* why enter and motion gie 0 coordinates??? *)
let motion_func _a x y _b =
  Printf.printf "motion %f %f\n" x y ;
  Printf.printf "%!" ;
  ()

let enter_func _a x y _b =
  Printf.printf "enter %f %f\n" x y ;
  Printf.printf "%!" ;
  ()

let leave_func _a _b = Printf.printf "leave\n" ; Printf.printf "%!" ; ()

(* returning true prevents closing the window *)
let close_request_func _self _ud =
  Printf.printf "attempting to close window\n%!" ;
  false

(* file:~/Programming/Lisp/clops-gui/src/gui-window-gtk.lisp::71 *)
let window_events _app window =
  let key_controller = event_controller_key_new () in
  widget_add_controller window key_controller ;
  signal_connect_key_x key_controller "key-pressed" key_pressed_func null ;
  signal_connect_key_x key_controller "key-released" key_released_func null ;
  signal_connect_close_request window "close-request" close_request_func null ;
  (* signal_connect_activate app "activate" activate null ; *)
  timeout_add 1000
    (fun _ptr -> Printf.printf "timeout \n" ; Printf.printf "%!" ; true)
    null ;
  ()

(* add
     handling of close request
     https://docs.gtk.org/gtk4/signal.Window.close-request.html
   *)

(* https://docs.gtk.org/gtk4/signal.DrawingArea.resize.html *)
let resize_func _w width height _ud =
  Printf.printf "resizing %d %d\n" width height ;
  Printf.printf "%!" ;
  ()

let scroll_func _w dx dy _ud =
  Printf.printf "scrolling %f %f\n" dx dy ;
  Printf.printf "%!" ;
  ()

let pressed_func _a bn x y _b =
  Printf.printf "pressed %d %f %f\n" bn x y ;
  Printf.printf "%!" ;
  ()

let released_func _a bn x y _b =
  Printf.printf "released %d %f %f\n" bn x y ;
  Printf.printf "%!" ;
  ()

let canvas_events canvas =
  let motion_controller = event_controller_motion_new () in
  let scroll_controller = event_controller_scroll_new 1 in
  let gesture_click_controller = gesture_click_new () in
  widget_add_controller canvas motion_controller ;
  widget_add_controller canvas scroll_controller ;
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
  signal_connect_activate app "activate" activate null ;
  let status = application_run app 0 null in
  object_unref app ;
  print_endline @@ string_of_int status
;;

(* ================ *)
main ()
