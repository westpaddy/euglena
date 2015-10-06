open Ast

let level = ref 0

let incr_level () = incr level

let decr_level () = decr level

let counter = ref 0

let fresh_var () = incr counter; {t_desc = Ty_var ("a" ^ string_of_int !counter); t_level = !level}

let new_ty desc = {t_desc = desc; t_level = !level}

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

let pp_repr fmt t =
  let subst = ref [] and counter = ref 0 in
  let rec iter weak t =
    match t.t_desc with
    | Ty_var x ->
      let x = begin try List.assoc x !subst with Not_found ->
        let v = Char.escaped (char_of_int ((Char.code 'a') + !counter mod 26)) in
        subst := (x, v) :: !subst;
        incr counter;
        v
      end in
      Format.fprintf fmt "'%s" x
    | Ty_const x ->
      Format.fprintf fmt "%s" x
    | Ty_fun (t1, t2) ->
      if weak then Format.fprintf fmt "(";
      iter true t1;
      Format.fprintf fmt " -> ";
      iter false t2;
      if weak then Format.fprintf fmt ")"
    | Ty_link t ->
      iter weak t
  in
  iter false t
