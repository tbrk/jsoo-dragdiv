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

type t

val create :    ?min_width:int
             -> ?min_height:int
             -> ?width:Js.js_string Js.t
             -> ?height:Js.js_string Js.t
             -> ?pos:int * int
             -> ?title:Js.js_string Js.t
             -> ?on_resize:(unit -> unit)
             -> Dom_html.element Js.t
             -> t

val close : t -> unit

val on_resize : t -> (unit -> unit) -> unit

