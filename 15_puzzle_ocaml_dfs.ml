exception Fail;;
exception NotFound;;

(* definizione tipo stato -> una lista di interi*)
type stato = St of int list;;

(* definizione del tipo di grafo*)
type 'a graph = Graph of ('a -> 'a list);;

(* funzione che permette di trovare l'indice della casella vuota *)
let findBlank (St(state)) =
  let rec aux index = function
    [] -> raise Fail
    | x::rest -> if x = 0 then index
    else aux (index+1) rest
  in aux 0 state;;

(* funzione che permette di scambiare due caselle con indice x e y*)
let scambia state x y =
  let rec aux index x y = function
    [] -> []
    | hd::rest ->
      if index = x then (List.nth state y)::aux (index+1) x y rest
      else if index = y then (List.nth state x)::aux (index+1) x y rest
      else hd::aux (index+1) x y rest
    in aux 0 x y state;;

(* funzioni di movimento con opportuni controlli*)
let muoviSU (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if blankIndex < 4 then raise Fail
  else St(scambia state blankIndex (blankIndex-4));;

let muoviGIU (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if blankIndex > 11 then raise Fail
  else St(scambia state blankIndex (blankIndex+4));;

let muoviDX (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if (blankIndex mod 4) = 3 then raise Fail
  else St(scambia state blankIndex (blankIndex+1));;

let muoviSX (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if (blankIndex mod 4) = 0 then raise Fail
  else St(scambia state blankIndex (blankIndex-1));;


(* funzione test obiettivo *)
let isGoal state = 
  let statoGoal = St[1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;0]
in state = statoGoal;;


(* funzioni di comodo per stampa layout *)
let stampaStato (St(state)) = 
    let stampaNum x = print_int(x); if x>9 then print_string(" ") else print_string("  ")
    in let aCapo x = if (x mod 4) = 3 then print_newline()
  in let rec aux index = function
    [] -> print_newline()
    | x::rest -> stampaNum x; aCapo index; aux (index+1) rest
in aux 0 state;;

let rec stampaCammino = function
  [] -> print_newline()
  | x::rest -> stampaStato x; stampaCammino rest;;


(* funzione successori che rappresenta il grafo *)
let giocoDel15 state =
  let rec aux = function
    [] -> []
    | f::rest ->
      try f state :: aux rest
      with Fail -> aux rest
  in aux [muoviSU; muoviGIU; muoviDX; muoviSX];;
