open Ast

let current_level = ref 0

let generalize_level = 1000000

let incr_level () = incr current_level; assert (!current_level < generalize_level)

let decr_level () = decr current_level; assert (0 <= !current_level)

let id = ref 0

let create_ty t_desc t_level =
  incr id; { t_desc; t_level; t_id = !id }

let new_ty desc = create_ty desc !current_level

let new_genty desc = create_ty desc generalize_level

let counter = ref 0

let fresh_var () = incr counter; new_ty (Ty_var ("a" ^ string_of_int !counter))

let rec repr t =
  match t.t_desc with
  | Ty_link t ->
    repr t
  | _ ->
    t

let link t1 t2 =
  t1.t_desc <- Ty_link t2

let saved_desc = ref []

let save_desc t d =
  saved_desc := (t, d) :: !saved_desc

let cleanup_types () =
  List.iter (fun (t, d) -> t.t_desc <- d) !saved_desc;
  saved_desc := []

let rec copy t =
  let rec copy_desc = function
    | Ty_var _ | Ty_const _ as d -> d
    | Ty_fun (t1, t2) -> Ty_fun (copy t1, copy t2)
    | Ty_link t -> copy_desc t.t_desc
    | Ty_subst _ -> assert false
  in
  let t = repr t in
  match t.t_desc with
  | Ty_subst t -> t
  | _ ->
    if t.t_level <> generalize_level then t else
      let d = t.t_desc in
      save_desc t d;
      let dst = fresh_var () in
      t.t_desc <- Ty_subst dst;
      dst.t_desc <- copy_desc d;
      dst

let iter f t =
  match t.t_desc with
  | Ty_var _ | Ty_const _ ->
    ()
  | Ty_fun (t1, t2) ->
    f t1; f t2
  | Ty_link t ->
    f t
  | Ty_subst t ->
    f t

let rec update_level t l =
  if t.t_level > l then t.t_level <- l; iter (fun t -> update_level t l) t

let rec generalize t =
  if t.t_level > !current_level then t.t_level <- generalize_level;
  iter generalize t

let instantiate t =
  let t' = copy t in
  cleanup_types ();
  t'
