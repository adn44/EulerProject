(* test si un nombre est premier ou non *)
let est_premier = fun x ->
  let rec local = fun n ->
    if n > ( int_of_float ( sqrt ( float_of_int x ) ) ) then true
    else 
      if (x mod n) == 0 then false else local (n +1)
  in local 2;;

(* cherche le plus grand facteur premier de x *)
let max_fact_prem = fun x ->
  let rec local = fun n k acc ->
    if est_premier n then if acc == 0 then n else acc
    else
      if k mod 2 == 0 && k != 2 then local n (k - 1) acc
      else
        if est_premier k then
          if n mod k == 0 then local (n / k) k (max acc k)
          else local n (k - 1) acc
        else local n (k - 1) acc
  in local x (int_of_float ( sqrt ( float_of_int x ) ) ) 0;;

