let empty = []

let extend env x t = (x, t) :: env

let lookup env x = List.assoc x env
