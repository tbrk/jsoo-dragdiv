(***********************************************************************)
(*                                                                     *)
(*              dragdiv: Basic windows for js_of_ocaml                 *)
(*                                                                     *)
(*                   Timothy Bourke (Inria/ENS)                        *)
(*                                                                     *)
(*  Copyright 2018 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under a BSD2 License, refer to the file LICENSE.                   *)
(*                                                                     *)
(***********************************************************************)

open Js_of_ocaml

type resize_type =
  | Fixed
  | SizeOnly
  | SizeAndPos

let get_id =
  let counter = ref 0 in
  function () -> incr counter; !counter

type t = {
  id  : int;
  div : Dom_html.element Js.t;
  min_width  : int;
  min_height : int;
  mutable on_resize  : unit -> unit;
  mutable preX : int;
  mutable preY : int;
  mutable resize_width  : resize_type;
  mutable resize_height : resize_type;
}

let js = Js.string
let js_pix x = js (string_of_int x ^ "px")

let drag_done ev =
  let doc = Dom_html.window##.document in
  doc##.onmouseup := Dom.no_handler;
  doc##.onmousemove := Dom.no_handler;
  Js._false

(* Window tracking *)

let locinc = 20

let locmin = 50

let locmax_x = 600
let locmax_y = 300

let lastloc = ref (locmin, locmin)

let nwindows = ref 0
let windows = ref ([] : t list)

let update_zindex () =
  let max = !nwindows + 50 in
  List.iteri
    (fun i w -> w.div##.style##.zIndex := js (string_of_int (max - i)))
    !windows

let new_window pos w =
  let x, y =
    match pos with
    | None ->
        let x, y = !lastloc in
        let x, y = if y <= locmax_y then (x, y)
                   else if x <= locmax_x then (x - locmax_y + 3 * locinc, locmin)
                   else (locmin, locmin)
        in
        lastloc := (x + locinc, y + locinc);
        x, y
    | Some p -> p
  in
  incr nwindows;
  windows := w :: !windows;
  w.div##.style##.left := js_pix x;
  w.div##.style##.top  := js_pix y

let drop_window { id = wid } =
  decr nwindows;
  windows := List.filter (fun { id } -> wid <> id) !windows

let raise_window { id = wid } =
  let w, w' = List.partition (fun { id } -> wid = id) !windows in
  windows := w @ w';
  update_zindex ()

let win_raise w ev = raise_window w; Js._false

(* Window close methods *)

let close { div } =
  div##.outerHTML := js ""

let do_close this ev =
  drop_window this;
  close this;
  Js._false

(* Window move methods *)
let move_drag ({div} as this) ev =
  let xdiff = this.preX - ev##.clientX in
  let ydiff = this.preY - ev##.clientY in
  this.preX <- ev##.clientX;
  this.preY <- ev##.clientY;
  div##.style##.top := js_pix (div##.offsetTop - ydiff);
  div##.style##.left := js_pix (div##.offsetLeft - xdiff);
  Js._false

let move_mousedown ({ div } as this) ev =
  let doc = Dom_html.window##.document in
  raise_window this;
  this.preX <- ev##.clientX;
  this.preY <- ev##.clientY;
  doc##.onmouseup := Dom.handler drag_done;
  doc##.onmousemove := Dom.handler (move_drag this);
  Js._false

(* Window resize methods *)

let resize_drag ({div} as this) ev =
  (match this.resize_width with
   | Fixed -> ()
   | SizeOnly ->
      let w = max this.min_width (ev##.clientX + this.preX) in
      div##.style##.width := js_pix w
   | SizeAndPos ->
      let w = div##.clientWidth in
      let w' = max this.min_width (w + this.preX - ev##.clientX) in
      if w <> w' then begin
        div##.style##.left  := js_pix (div##.offsetLeft + w - w');
        div##.style##.width := js_pix w';
        this.preX <- ev##.clientX
      end
  );
  (match this.resize_height with
   | Fixed -> ()
   | SizeOnly ->
      let h = ev##.clientY + this.preY in
      div##.style##.height := js_pix (max this.min_height h)
   | SizeAndPos ->
      let h = div##.clientHeight in
      let h' = max this.min_height (h + this.preY - ev##.clientY) in
      if h <> h' then begin
        div##.style##.top  := js_pix (div##.offsetTop + h - h');
        div##.style##.height := js_pix h';
        this.preY <- ev##.clientY
      end
  );
  this.on_resize ();
  Js._false

let resize_mousedown resize_width resize_height ({ div } as this) ev =
  let doc = Dom_html.window##.document in
  raise_window this;
  (match resize_width with
   | Fixed      -> ()
   | SizeOnly   -> this.preX <- div##.clientWidth - ev##.clientX;
   | SizeAndPos -> this.preX <- ev##.clientX;
  );
  (match resize_height with
   | Fixed      -> ()
   | SizeOnly   -> this.preY <- div##.clientHeight - ev##.clientY;
   | SizeAndPos -> this.preY <- ev##.clientY;
  );
  this.resize_width <- resize_width;
  this.resize_height <- resize_height;
  doc##.onmouseup := Dom.handler drag_done;
  doc##.onmousemove := Dom.handler (resize_drag this);
  Js._false

(* Main function *)

let create ?(min_width=50)
           ?(min_height=50)
           ?(width=js "200px")
           ?(height=js "100px")
           ?pos
           ?title
           ?(on_resize=fun () -> ())
           content =
  let open Dom_html in
  let doc = window##.document in
  let win = createDiv doc in
  let name = createDiv doc in
  let close = createDiv doc in

  let this = {
    id  = get_id ();
    div = win;
    on_resize  = on_resize;
    min_width  = min_width;
    min_height = min_height;
    preX = 0;
    preY = 0;
    resize_width  = Fixed;
    resize_height = Fixed;
  } in

  win##.classList##add     (js "dragdiv-window");
  name##.classList##add    (js "dragdiv-title");
  name##.classList##add    (js "noselect");
  content##.classList##add (js "dragdiv-content");
  close##.classList##add   (js "dragdiv-close");

  (* Window style *)
  win##.style##.position := js "absolute";
  win##.style##.width    := width;
  win##.style##.height   := height;
  win##.onmousedown      := Dom.handler (win_raise this);
  Dom.appendChild win name;
  Dom.appendChild win content;
  new_window pos this;

  (* Title style *)
  name##.style##.cursor := js "move";
  name##.style##.zIndex := js "auto";
  name##.textContent    := Js.Opt.option title;
  name##.onmousedown    := Dom.handler (move_mousedown this);

  (* Resize style *)
  let resize_mode l r =
    if not (l || r) then Fixed
    else if r then SizeOnly
    else SizeAndPos
  in
  let resize_cursor l r t b =
    if (b && r) || (t && l) then "nwse-resize"
    else if (t && r) || (b && l) then "nesw-resize"
    else if l || r then "ew-resize"
    else "ns-resize"
  in
  let add_resize l r t b =
    if l || r || t || b then begin
      let control = createDiv doc in
      control##.classList##add (js "dragdiv-resize");
      control##.style##.opacity := Js.def (js "0");
      control##.style##.position := js "absolute";
      control##.style##.zIndex := js (if (l || r) && (t || b) then "11" else "10");
      control##.style##.cursor := js (resize_cursor l r t b);
      if (l || r) && (t || b) then begin
        control##.style##.width  := js "8px";
        control##.style##.height := js "8px"
      end else begin
        control##.style##.width  := if not (l || r) then js "100%" else js "4px";
        control##.style##.height := if not (t || b) then js "100%" else js "4px"
      end;
      if l then control##.style##.left   := js "-2px";
      if r then control##.style##.right  := js "-2px";
      if t then control##.style##.top    := js "-2px";
      if b then control##.style##.bottom := js "-2px";
      control##.onmousedown := Dom.handler
          (resize_mousedown (resize_mode l r) (resize_mode t b) this);
      Dom.appendChild win control
    end
  in
  let h g f = g (f false false); g (f true false); g (f false true) in
  h (h (fun x -> x)) add_resize;

  (* Close style *)
  close##.style##.cursor   := js "pointer";
  close##.style##.position := js "absolute";
  close##.style##.right    := js "0";
  close##.style##.top      := js "0";
  close##.onclick          := Dom.handler (do_close this);
  Dom.appendChild name close;

  Dom.appendChild doc##.body win;
  this

let on_resize d f =
  d.on_resize <- f

