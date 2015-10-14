let env = ref Predef.env

let process s =
  let p = Parser.top_phrase Lexer.token (Lexing.from_string s) in
  let (new_env, ty) = Typing.top_phrase !env p in
  env := new_env;
  Format.fprintf Format.str_formatter "%a" Pprint_ast.ty ty;
  Format.flush_str_formatter ()

let append_echo c s =
  let li = Dom_html.document##createElement(Js.string "li") in
  Dom.appendChild c li;
  Dom.appendChild li (Dom_html.document##createTextNode(Js.string s))

let rec append_line c =
  let li = Dom_html.document##createElement(Js.string "li") in
  Dom.appendChild c li;
  Dom.appendChild li (Dom_html.document##createTextNode(Js.string "@> "));
  let i = Dom_html.createInput Dom_html.document in
  Dom.appendChild li i;
  i##onkeydown <- make_event c i;
  i##focus()

and make_event c i =
  Dom_html.handler (fun e ->
    if e##keyCode = 13 then begin
      match Js.Opt.to_option c##lastChild with
      | None -> ()
      | Some li ->
        Dom.removeChild li i;
        Dom.appendChild li (Dom_html.document##createTextNode(i##value));
        append_echo c (process (Js.to_string i##value));
        append_line c
    end;
  Js._true)

let () = ignore (Lwt.bind (Lwt_js_events.domContentLoaded ()) (fun () ->
  let c = Dom_html.getElementById "canvas" in
  let ul = Dom_html.document##createElement(Js.string "ul") in
  Dom.appendChild c ul;
  append_line ul;
  Lwt.return_unit))
