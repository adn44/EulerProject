(* retourne la somme de tous les multiples de 3 ou de 5 inférieur ou égale à mille *)
let multiple3_5 =
  let rec local = fun k acc ->
    if k == 1000 then acc
    else
      if (k mod 3 == 0) || (k mod 5 == 0) then local (k + 1) (acc + k)
      else local (k + 1) acc
  in local 0 0;;
