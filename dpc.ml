type    stream     = char list
and  'a parser  = stream -> 'a result list
and  'a result  = Delayed of 'a delayed | Result of 'a * stream
and  'a delayed = unit -> 'a result list

let token p = function
  h :: tl when h = p -> [Result (h, tl)]
| _                  -> []

let eof = function
  [] -> [Result ((), [])]
| _ -> []

let empty st = [Result ((), st)]

let lift p _ = p

let map p f st =
  let rec map p =
    List.map
      (function
       | Delayed p'      -> Delayed (fun () -> map p')
       | Result (x, st') -> Result  (f x, st')
      ) (p ())
  in
  map (fun () -> p st) 
  
let alt p q st = p st @ q st

let seq a b st =  
  let rec seq a b =
    List.concat @@
      List.map (function
          | Result  (x, st') -> b (x) (st')
          | Delayed  a'      -> [Delayed (fun () -> seq a' b)]            
        ) (a ())
  in
  seq (fun () -> a st) b

let rec fix f =
  f (fun st -> [Delayed (fun () -> fix f st)])
        
let rec iterate list =
  let rec pass2 = function
  | []                  -> []
  | Delayed (p)   :: tl -> p () @ pass2 tl
  | x             :: tl -> x :: pass2 tl
  in
  let rec pass = function
  | []                  -> None
  | Result (x, _) :: tl -> Some x
  | _             :: tl -> pass tl
  in
  match pass list with
  | None   -> (match list with []-> None | _ -> iterate (pass2 list)) 
  | Some x -> Some x

let a  = token 'a'
let b  = token 'b'
let c  = token 'c'
let ab = seq a (fun a -> map b (fun b -> [a; b]))

let lmf       = seq (alt ab (map a (fun a -> [a]))) (fun a -> map c (fun c -> a @ [c]))
let ccc  self = alt (seq self (fun p -> map c (fun c -> c :: p))) (map empty (fun _ -> []))
let pali self =
  alt (map empty (fun _ -> []))
      (alt (seq a (fun xa -> seq self (fun m -> map a (fun ya -> [xa] @ m @ [ya]))))
           (alt   (seq b (fun xb -> seq self (fun m -> map b (fun yb -> [xb] @ m @ [yb]))))
                  (seq c (fun xc -> seq self (fun m -> map c (fun yc -> [xc] @ m @ [yc]))))))
             
let parse p s printer =
  match iterate (seq p (fun x -> map eof (fun _ -> x)) s) with
  | None   -> Printf.printf "Oops...\n"
  | Some s -> Printf.printf "%s\n%!" @@ printer s

let printer s =
  let b = Buffer.create 10 in
  List.iter (fun c -> Buffer.add_char b c) s;
  Buffer.contents b
            
let _ = parse (fix ccc)  ['c'; 'c'; 'c']                printer
let _ = parse lmf        ['a'; 'c']                     printer
let _ = parse lmf        ['a'; 'b'; 'c']                printer
let _ = parse (fix pali) []                             printer
let _ = parse (fix pali) ['a'; 'a']                     printer
let _ = parse (fix pali) ['a'; 'b'; 'b'; 'a']           printer
let _ = parse (fix pali) ['a'; 'b'; 'c'; 'c'; 'b'; 'a'] printer
let _ = parse (fix ccc)  ['a']                          printer

module Deep =
  struct
    type _ p =
      Token : char -> _ p                        
    | Seq   : 'a p * 'b p -> 'b p     
    | Alt   : 'a p * 'a p -> 'a p        
    | Empty : _ p
    | Eof   : _ p                
    | Map   : 'a p -> 'b p
(*    | Lift  : *)
        
  end
