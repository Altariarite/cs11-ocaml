(*
 * sudoku.ml
 *
 *     Imperative sudoku solver in ocaml.
 * 
 *)

open Printf

(* The size of each side of the board. *)
let side_size = 9

(* The size of each side of a small box. *)
let box_size = 3

(* The type of boards. *)
type board = int array array

(* Exception type for board solver. *)
exception Found of board

(*
 * Board representation:
 * -- a board is a 9x9 two-dimensional array of ints
 * -- each square contains an int between 0 and 9
 * -- 0 means that the board's real value is unknown.
 * 
 *)

(** Read in a sudoku board from a file.  Return the board. *)
let read_board filename =
  let board = Array.make_matrix 9 9 0 in
  let chan = open_in filename in
  for i = 0 to 8 do
    let line = input_line chan in
    for j = 0 to 8 do
      board.(i).(j) <- int_of_char line.[j] - int_of_char '0'
    done
  done ;
  board

(** Print a sudoku board.  Return nothing. *)
let print_board chan board =
  for i = 0 to 8 do
    let buf = Buffer.create 9 in
    for j = 0 to 8 do
      Buffer.add_char buf (char_of_int (board.(i).(j) + int_of_char '0'))
    done ;
    Buffer.add_char buf '\n' ;
    Buffer.output_buffer chan buf
  done

(** Check if placing a number is invalid *)
let is_invalid (board : board) row col n =
  let in_row = Array.exists (fun x -> x = n) board.(row) in
  let in_col =
    Array.exists (fun i -> board.(i).(col) = n) (Array.init 9 (fun i -> i))
  in
  let in_box =
    let found = ref false in
    let box_row = row / 3 * 3 in
    let box_col = col / 3 * 3 in
    for i = 0 to 2 do
      for j = 0 to 2 do
        if board.(box_row + i).(box_col + j) = n then found := true
      done
    done ;
    !found
  in
  in_box || in_row || in_col

let next_empty board =
  let exception R of int * int in
  try
    for i = 0 to 8 do
      for j = 0 to 8 do
        if board.(i).(j) = 0 then raise (R (i, j)) else ()
      done
    done ;
    None
  with R (i, j) -> Some (i, j)

(** Solve a sudoku board.
    Return an option type giving the solution (a board)
    or None if no solution exists. *)
let solve_board board =
  let rec solve (board : board) : bool =
    match next_empty board with
    | None ->
        true
    | Some (x, y) ->
        let rec try_num n =
          if n > 9 then (
            board.(x).(y) <- 0 ;
            false )
          else if is_invalid board x y n then try_num (n + 1)
          else (
            board.(x).(y) <- n ;
            if solve board then true else try_num (n + 1) )
        in
        try_num 1
  in
  if solve board then Some board else None

(** Solve a sudoku board taken from a file, and print the result. *)
let solve_board_from_file filename =
  let b = read_board filename in
  match solve_board b with
  | None ->
      printf "Board has no solution.\n"
  | Some b ->
      print_board stdout b

(** Entry point. *)
let _ =
  if Array.length Sys.argv <> 2 then (
    fprintf stderr "usage: %s board\n" Sys.argv.(0) ;
    exit 1 )
  else (
    solve_board_from_file Sys.argv.(1) ;
    exit 0 )
