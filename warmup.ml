let sqrt tol x =
  let close_enough a b = abs_float (b -. a) < tol in
  let average a b = (a +. b) /. 2. in
  let rec better y =
    if close_enough (y *. y) x then y else better (average y (x /. y))
  in
  better 1.

let sqrt2 = sqrt 0.0001

let rec factorial1 n = if n = 0 then 1 else n * factorial1 (n - 1)

let rec factorial2 n = match n with 0 -> 1 | n -> n * factorial1 (n - 1)

let factorial n =
  let rec acc n ans =
    if n = 0 then ans else (acc [@tailcall]) (n - 1) (ans * n)
  in
  acc n1

(* Integers are Sys.int_size bits wide and use two's complement representation. *)
(* All operations are taken modulo 2 Sys.int_size. *)
(* They do not fail on overflow. *)
let fib n =
  let rec acc a b n = match n with 0 -> a | _ -> acc b (a + b) (n - 1) in
  acc 0 1 n

(* 'a list -> 'a list *)
let rev l =
  let rec acc l res =
    match l with [] -> res | hd :: tl -> acc tl (hd :: res)
  in
  acc l []

(* f:('a -> 'b) -> 'a list -> 'b list *)
let rec map ~f l = match l with [] -> [] | hd :: tl -> f hd :: map ~f tl

let map2 ~f l =
  let rec acc f l res =
    match l with [] -> res | hd :: tl -> acc f tl (f hd :: res)
  in
  rev (acc f l [])

let rec range a b = if a = b then [b] else a :: range (a + 1) b

let roots = range 1 20 |> map2 ~f:float_of_int |> map2 ~f:sqrt2
