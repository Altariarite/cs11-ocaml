let rec insert x compare = function
  | hd :: tl when compare x hd > 0 ->
      hd :: insert x compare tl
  | l ->
      x :: l

let rec insertion_sort compare = function
  | [] ->
      []
  | hd :: tl ->
      insert hd compare (insertion_sort compare tl)

type complex = {real: float; imag: float}

let complex_of_float n = {real= n; imag= 0.}

let complex_of_int n = {real= float_of_int n; imag= 0.}

let compare_complex a b =
  let real = compare a.real b.real in
  if real = 0 then compare a.imag b.imag else real

type number = Int of int | Float of float | Complex of complex

let compare_number a b =
  let to_complex = function
    | Int i ->
        complex_of_int i
    | Float f ->
        complex_of_float f
    | Complex c ->
        c
  in
  compare_complex (to_complex a) (to_complex b)

let test =
  insertion_sort compare_number
    [Int 1; Float 3.; Int 2; Complex {real= 0.; imag= 0.}]

let rec quick_sort compare =
  let rec split e smaller bigger = function
    | [] ->
        (smaller, bigger)
    | hd :: tl ->
        if compare hd e < 0 then split e (hd :: smaller) bigger tl
        else split e smaller (hd :: bigger) tl
  in
  function
  | [] ->
      []
  | hd :: tl ->
      let smaller, bigger = split hd [] [] tl in
      quick_sort compare smaller @ [hd] @ quick_sort compare bigger

let rec merge_sort compare l =
  let rec split n xs =
    match (n, xs) with
    | 0, xs ->
        ([], xs)
    | n, x :: xs when n > 0 ->
        let xs', xs'' = split (pred n) xs in
        (x :: xs', xs'')
    | _ ->
        invalid_arg "impossible split"
  in
  let rec merge l1 l2 =
    match (l1, l2) with
    | l, [] | [], l ->
        l
    | hd1 :: tl1, hd2 :: tl2 ->
        if compare hd1 hd2 < 0 then hd1 :: merge tl1 l2 else hd2 :: merge l1 tl2
  in
  let p1, p2 = split (List.length l / 2) l in
  match l with
  | [] ->
      []
  | [x] ->
      [x]
  | _ ->
      merge (merge_sort compare p1) (merge_sort compare p2)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let example = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let rec count_leaves = function
  | Leaf ->
      1
  | Node (n, l, r) ->
      count_leaves l + count_leaves r

let count = count_leaves example

let rec map_tree f = function
  | Leaf ->
      Leaf
  | Node (n, l, r) ->
      Node (f n, map_tree f l, map_tree f r)

let new_tree = map_tree succ example

let rec insert x = function
  | Leaf ->
      Node (x, Leaf, Leaf)
  | Node (y, left, right) ->
      if x < y then Node (y, insert x left, right)
      else Node (y, left, insert x right)

let rec tree_of_list = function
  | [] ->
      Leaf
  | x :: xs ->
      insert x (tree_of_list xs)

let rec print_tree ?(indent = 0) = function
  | Leaf ->
      Printf.printf "%s- leaf\n" (String.make indent ' ')
  | Node (n, l, r) ->
      Printf.printf "%s+ %d\n" (String.make indent ' ') n ;
      print_tree ~indent:(indent + 2) l ;
      print_tree ~indent:(indent + 2) r
