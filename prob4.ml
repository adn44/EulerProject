open List;;

let list_of_int = fun n ->
  let rec local = fun k acc ->
    match k with
      |0 -> acc
      |_ -> local (k/10) ((k mod 10)::acc)
  in local n [];;

let liste_egale = fun l ll ->
  if length l != length ll then false
  else 
    let rec local = fun i bool ->
      if i == length l then bool
      else if nth l i != nth ll i then local (length l) false
      else  local (i+1) bool
    in local 0 true;;

let est_palindrome = 
  let rec local = fun n k maxpal ->
    if liste_egale (list_of_int (n*k)) (rev (list_of_int (n*k))) then local n (k-1) (max maxpal (n*k))
    else if n == 100 && k == 100 then maxpal
    else if k == 100 then local (n-1) 999 maxpal
    else local n (k-1) maxpal
        in local 999 999 0;;
