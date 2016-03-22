open String;

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
    end

fun print_elem([]) = () | print_elem(a::b) = (print(a ^ "\n"); print_elem(b))

fun readlist (infile : string) = 
let
	val ins = TextIO.openIn infile
  	fun loop ins =
   		case TextIO.inputLine ins of
      			SOME line => line :: loop ins
    			| NONE      => []
in
	loop ins before TextIO.closeIn ins
end

fun process ([], stack) = stack | process (s::lines, stack) = 
	if isSubstring ":error:" s then process(lines, Str(":error:")::stack)
	else if isSubstring ":true:" s then process(lines, Str(":true:")::stack)
	else if isSubstring ":false:" s then process(lines, Str(":false:")::stack)
	else if isSubstring "push" s then
		let
			val len = size s
			val y = substring(s, 5, len - 6)
		in
			if isSubstring "." y then process(lines, Str(":error:")::stack)
			else
				let
					val SOME x = Int.fromString(substring(s, 5, len - 6))
				in
					process(lines, Int(x)::stack)
				end 
		end
	else if isSubstring "pop" s then
		let
			val len = length stack
		in
			if len = 0 then process(lines, Str(":error:")::stack)
			else 
				let
					val restOfStack = tl stack
				in
					process(lines, restOfStack)
				end
		end
	else if isSubstring "add" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => process(lines, eval(Plus(Int(x),Int(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "sub" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => process(lines, eval(Sub(Int(x),Int(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "neg" s then
		let
			val len = length(stack)
		in
			if (len = 0) then process(lines, Str(":error:")::stack) 
			else 
				let
					val x = hd (stack)
				in
					case x of
    						Int x => process(lines, eval(Times(Int(x),Int(~1)))::tl(stack))
    						| Str x => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "mul" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => process(lines, eval(Times(Int(x),Int(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "div" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => 
    						if (y = 0) then process(lines, Str(":error:")::stack)
    						else process(lines, eval(Divs(Int(x),Int(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "rem" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => 
    						if (y = 0) then process(lines, Str(":error:")::stack)
    						else process(lines, eval(Mod(Int(x),Int(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, Str(":error:")::stack)
				end
		end
	else if isSubstring "swap" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, Str(":error:")::stack) 
			else 
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(Int x, Int y) => process(lines, Int(y)::Int(x)::(tl (tl stack)))
    						| (Int x, Str y) => process(lines, Str(y)::Int(x)::(tl (tl stack)))
    						| (Str x, Int y) => process(lines, Int(y)::Str(x)::(tl (tl stack)))
    						| (Str x, Str y) => process(lines, Str(y)::Str(x)::(tl (tl stack)))
				end
		end
	else if isSubstring "quit" s then
		process([], stack)
	else stack

fun printList ([], newStack) = [] | printList(a::b, newStack) = 
    case a of 
        Int a => printList(b, Int.toString(a)::newStack) |  Str a => printList(b, a::newStack)

fun removeTilde(inp:string) = 
	if isSubstring "~" inp then ("-" ^ substring(inp, 1, size(inp) - 1)) else inp

fun hw2(infile:string, outFile:string) = 
	let
		val inp = readlist(infile)
		val stack = process(inp, [])
		val outStream = TextIO.openOut outFile
		fun helper([]) = (TextIO.closeOut outStream)
			| helper(a::newStack) = 
			case a of 
				Int a => (TextIO.output(outStream, removeTilde(Int.toString(a)) ^ "\n");
				helper(newStack))
				| Str a => (TextIO.output(outStream, a ^ "\n");
				helper(newStack))
	in
		helper(stack) 
	end