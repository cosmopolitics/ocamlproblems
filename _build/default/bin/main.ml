
let rec lst list = 
  match list with
  | [] -> None
  | [x] -> Some x 
  | _ :: tl -> lst tl

let rec lsttwo list = 
  match list with
  | [] -> None
  | [_] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: tl -> lsttwo tl

let rec nth n list =
  match (n, list) with
  | (_ ,[]) -> raise (Failure "nth")
  | (0 ,x :: _) -> x
  | (i, _ :: tl) -> nth (i-1) tl

let rec llen list count = 
  match list with
  | [] -> count
  | _ :: tl -> llen tl (count + 1)
let listlen list = llen list 0

let rec rlist list out = 
  match list with 
  | [] -> out 
  | hd :: tl -> rlist tl (hd:: out)
let reverse list = rlist list []

let pali list =
  list = reverse list

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec aux list outlist =
    match list with
    | [] -> outlist
    | One x :: tl -> aux tl (x :: outlist)
    | Many y :: tl -> 
        aux y outlist |> aux tl 
  in
  aux list [] |> reverse

let rec compress list =
  match list with
  | [] -> list
  | a :: [] -> [a]
  | a :: (b :: _ as tl) -> 
      if a = b then compress tl else a :: compress tl

let packdups list =
  let rec aux list outlist = 
    match outlist, list with
    | _, [] -> outlist 
    | [], hd :: tl -> aux tl (hd:: [] :: outlist)
    | outhead :: _, hd :: tl -> 
        if outhead = hd then aux tl outlist else aux tl (hd :: outlist)
  in 
  aux list [] |> reverse
;;

let () = 
  let _ = lst ['1' ; '2' ; '3' ; '4'] in
  let _ = lsttwo ['1' ; '2' ; '3' ; '4'] in
  let _ = nth 2 ['1' ; '2' ; '3' ; '4'] in
  let _ = listlen ['1' ; '2' ; '3' ; '4'] in
  let _ = reverse ['1' ; '2' ; '3' ; '4'] in
  let _ = pali ['1' ; '2' ; '3' ; '4'] in
  let _ = 
    flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] 
(*    |> List.iter print_endline  *)
  in
  let _ = 
    compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] 
    |> List.iter print_endline
  in
  let _ = 
    packdups ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
    |> List.iter print_endline
  in
  ()
