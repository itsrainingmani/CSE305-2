datatype expr =  Int of int | Str of string | Plus of expr*expr | Sub of expr*expr | Times of expr*expr | Divs of expr*expr | Mod of expr*expr;

fun eval (Int n) = Int n
  | eval (Plus (e1, e2)) =
    let
        val Int n1 = eval e1
        val Int n2 = eval e2
    in
        Int (n1+n2)
    end
  | eval (Times (e1, e2)) =
    let
        val Int n1 = eval e1
        val Int n2 = eval e2
    in
        Int (n1*n2)
    end
   | eval (Sub (e1, e2)) =
    let
    	val Int n1 = eval e1
    	val Int n2 = eval e2
    in
    	Int(n1-n2)
    end
    | eval (Divs (e1,e2)) = 
    let
    	val Int n1 = eval e1
    	val Int n2 = eval e2
    in
    	Int(n1 div n2)
    end
    | eval (Mod (e1,e2)) = 
    let
    	val Int n1 = eval e1
    	val Int n2 = eval e2
    in
    	Int(n1 mod n2)
    end;

fun printList ([]) = () | 
    printList(a::b) = 
    case a of 
        Int a => (print(Int.toString(a) ^ "\n"); printList(b))
        |  Str a => (print(a ^ "\n"); printList(b))