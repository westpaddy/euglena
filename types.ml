open Ast

let counter = ref 0

let incr_level () = incr counter

let decr_level () = decr counter

let fresh_var () = incr counter; {t_desc = Ty_var ("a" ^ string_of_int !counter); t_level = !counter}

let new_ty desc = {t_desc = desc; t_level = !counter}

let rec repr t =
  match t.t_desc with
  | Ty_link t ->
    repr t
  | _ ->
    t

let link t1 t2 =
  t1.t_desc <- Ty_link t2

let iter f t =
  match t.t_desc with
  | Ty_var _ | Ty_const _ ->
    ()
  | Ty_fun (t1, t2) ->
    f t1; f t2
  | Ty_link t ->
    f t
