open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_of_ocaml_toplevel

let compiler_name = "OCaml"

let by_id s = Dom_html.getElementById s

let by_id_coerce s f =
  Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s

let setup_toplevel () =
  Clflags.debug := true;
  JsooTop.initialize ();
  Sys.interactive := false;
  exec'
    "module Lwt_main = struct\n\
    \  let run t = match Lwt.state t with\n\
    \    | Lwt.Return x -> x\n\
    \    | Lwt.Fail e -> raise e\n\
    \    | Lwt.Sleep -> failwith \"Lwt_main.run: thread didn't return\"\n\
    \ end";
  let header1 = Printf.sprintf "%s version %%s" compiler_name in
  (* let header2 =
    Printf.sprintf "Compiled with Js_of_ocaml version %s" Sys_js.js_of_ocaml_version
  in *)
  exec' (Printf.sprintf "Format.printf \"%s@.\" Sys.ocaml_version;;" header1);
  (* exec' (Printf.sprintf "Format.printf \"%s@.\";;" header2); *)
  exec' (Printf.sprintf "Format.printf \"Press Crtl+C to clear\";;");
  exec' (Printf.sprintf "Format.printf \"Press Crtl+R to reload\";;");
  exec' (Printf.sprintf "Format.printf \"\n\";;");
  Ppx_support.init ();
  Toploop.add_directive
    "load_js"
    (Toploop.Directive_string (fun name -> Js.Unsafe.global##load_script_ name))
    { section = "js_of_ocaml-toplevel-example"; doc = "Load the given javascript file" };
  Sys.interactive := true;
  ()

let setup_printers () =
  exec'
    "let _print_error fmt e = Format.pp_print_string fmt (Js_of_ocaml.Js_error.to_string \
     e)";
  Topdirs.dir_install_printer Format.std_formatter Longident.(Lident "_print_error");
  exec' "let _print_unit fmt (_ : 'a) : 'a = Format.pp_print_string fmt \"()\"";
  Topdirs.dir_install_printer Format.std_formatter Longident.(Lident "_print_unit")

(* error highlight *)
let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
      if Js.to_bool (e##.classList##contains (Js.string "sharp")) then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let current_position = ref 0

let highlight_location loc =
  let x = ref 0 in
  let output = by_id "output" in
  let first =
    Js.Opt.get (output##.childNodes##item !current_position) (fun _ -> assert false)
  in
  iter_on_sharp first ~f:(fun e ->
      incr x;
      let _file1, line1, col1 = Location.get_pos_info loc.Location.loc_start in
      let _file2, line2, col2 = Location.get_pos_info loc.Location.loc_end in
      if !x >= line1 && !x <= line2
      then
        let from_ = if !x = line1 then `Pos col1 else `Pos 0 in
        let to_ = if !x = line2 then `Pos col2 else `Last in
        Colorize.highlight from_ to_ e)

(* create new output line *)
let append colorize output cl s =
  if s <> "" && s <> "\n" && s <> " " && s <> "\t" then
    Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl s))
  else ()

let run _ =
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Dom_html.CoerceTo.textarea in
  let sharp_chan = open_out "/dev/null0" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in
  let caml_chan = open_out "/dev/null1" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in
  let execute () =
    let content = Js.to_string textbox##.value##trim in
    let content' =
      let len = String.length content in
      if
        try content <> "" && content.[len - 1] <> ';' && content.[len - 2] <> ';'
        with _ -> true
      then content ^ ";;"
      else content
    in
    current_position := output##.childNodes##.length;
    textbox##.value := Js.string "";
    JsooTop.execute true ~pp_code:sharp_ppf ~highlight_location caml_ppf content';
    Lwt.return_unit
  in
  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey
  in
  let shift e = Js.to_bool e##.shiftKey in
  textbox##.onkeyup :=
    Dom_html.handler (fun _ ->
        Js._true);
  textbox##.onchange :=
    Dom_html.handler (fun _ ->
        Js._true);
  let handke_key_down (e: Dom_html.keyboardEvent Js.t) =
    Js.Optdef.case
      (e##.key)
      (fun () -> Js._true)
      (fun v -> 
        let k = Js.to_string v in
        match k with
        | "Enter" when not (meta e || shift e) ->
            Lwt.async execute;
            Js._false
        | "c" when meta e ->
            output##.innerHTML := Js.string "";
            Js._true
        | "r" when meta e ->
            output##.innerHTML := Js.string "";
            setup_toplevel ();
            Js._false
        | _ -> Js._true
      )
  in
  textbox##.onkeydown :=
    Dom_html.handler handke_key_down;
  (Lwt.async_exception_hook :=
     fun exc ->
       Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc);
       match exc with
       | Js_error.Exn e ->
           let e = Js_error.to_error e in
           Firebug.console##log e##.stack
       | _ -> ());
  Lwt.async (fun () ->
      Lwt.return_unit);
  Sys_js.set_channel_flusher caml_chan (append Colorize.ocaml output "caml");
  Sys_js.set_channel_flusher sharp_chan (append Colorize.ocaml output "sharp");
  Sys_js.set_channel_flusher stdout (append Colorize.text output "stdout");
  Sys_js.set_channel_flusher stderr (append Colorize.text output "stderr");
  let readline () =
    Js.Opt.case
      (Dom_html.window##prompt (Js.string "The toplevel expects inputs:") (Js.string ""))
      (fun () -> "")
      (fun s -> Js.to_string s ^ "\n")
  in
  Sys_js.set_channel_filler stdin readline;
  setup_toplevel ();
  setup_printers ();
  textbox##.value := Js.string "";
  textbox##.disabled := Js._false;
  ()

let _ =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        run ();
        Js._false)
