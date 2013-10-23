(* retourne la somme de tous les termes de la suite de fibonacci inférieur à 4 000 000 *)
let sum_fibo =
  let rec local = fun n k acc ->
    print_int n;
    print_string " ";
    if n >= 4000000 then acc
    else if n mod 2 == 0 then local (n + k) n (acc + n)
    else local (n + k) n acc
  in local 1 1 0;;

