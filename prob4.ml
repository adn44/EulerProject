(* reverse a integer, example 123 -> 321 *)
let reverse = fun x ->
  let rec local = fun n acc ->
    if n <= 0 then acc
    else local (n/10) (10*acc + n mod 10)
  in local x 0;;


(* return true if the number n is a palindrom, false otherwise *)
let is_palindrom = fun n -> if n = (reverse n) then true else false;;


(* return the largest palindrom number resulting on the product of 2 numbers of
 * 3 digits *)
let max_palindrom = 
  let rec local = fun n k max_pal ->
    if is_palindrom (n*k) then local n (k-1) (max max_pal (n*k))
    else if n = 100 && k = 100 then max_pal
    else if k = 100 then local (n-1) 999 max_pal
    else local n (k-1) max_pal
        in local 999 999 0;;
