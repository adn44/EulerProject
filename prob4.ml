open List;;

(* make a list from an integer, example 123 -> [1;2;3] *)
let list_of_int = fun n ->
  let rec local = fun k acc ->
    match k with
      |0 -> acc
      |_ -> local (k/10) ((k mod 10)::acc)
  in local n [];;


(* check if two lists are equals or not, example:
*  [1;2;3] -> [1;2;3]   -> true
*  [1;2;3] -> [1;3;2]   -> false
*  [1;2;3] -> [1;2;3;4] -> false
*)
let liste_egale = fun l ll ->
  if length l != length ll then false
  else 
    let rec local = fun i bool ->
      if i == length l then bool
      else if nth l i != nth ll i then local (length l) false
      else  local (i+1) bool
    in local 0 true;;


(* return true if the number n is a palindrom, false otherwise *)
let est_palindrome = fun n -> if liste_egale (list_of_int n) (rev (list_of_int n)) then true else false;;


(* return the largest palindrom number resulting on the product of 2 numbers of
 * 3 digits *)
let palindrome_max = 
  let rec local = fun n k maxpal ->
    if est_palindrome (n*k) then local n (k-1) (max maxpal (n*k))
    else if n == 100 && k == 100 then maxpal
    else if k == 100 then local (n-1) 999 maxpal
    else local n (k-1) maxpal
        in local 999 999 0;;
