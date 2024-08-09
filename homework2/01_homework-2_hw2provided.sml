(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings
   (returns true if the same string),
   then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1:string, s2:string) =
    s1 = s2;

(* put your solutions for problem 1 here *)

(* string, string list -> list option *)
(* takes a string and a string list.
   Return NONE if the string is not in the list, else return SOME lst *)

(* fun all_except_option (str: string, strList: string list
       			 -> list option) = NONE (* the stub *)  *)

fun all_except_option (str, cs) =
    case cs of
	[] => NONE
      | s::cs =>
	if s = str then SOME cs
	else case all_except_option (str, cs) of
		 SOME resultList => SOME(s::resultList)
	       | _ => NONE

(* (string list) list, string -> string list *)
(* takes a string list list (a list of list of strings,
   the substitutions) and a string s and returns a string list *)

(* fun get_substitutions1 (listStrList: (string list) list, string
       			  -> string list ) = [] (* the stub *) *)
(*
fun get_substitutions1 (listStrList, s) =
    let fun helper ([], acc) = acc
	  | helper (subList::rest, acc) = 
	    case all_except_option(s, subList) of
		NONE => helper (rest, acc)
	      | SOME list_without_s => helper(rest, acc @ list_without_s)
    in helper(listStrList, [])
    end;
*)
fun get_substitutions1 (cs, s) =
    case cs of
	[] => []
      | first::rest => case all_except_option(s, first) of
			   SOME result =>
			   result @ get_substitutions1 (rest, s)
			 | _ => get_substitutions1 (rest, s);

(* get_sub2 *)
fun get_substitutions2 (cs, s) =
    case cs of
	[] => []
      | first::rest => let
	  val tail = get_substitutions2 (rest, s)
      in case all_except_option(s, first) of
	     SOME result => result @ tail
	   | _ => tail end;

(* (string list) list, {string, string, string} -> string list *)
(* string list list of substitutions and eturns a list of full names
   (type {first:string,middle:string,last:string} list) *)

(* fun similar_names (listSList: (string list) list,
       		     fullName:{string, string,string}) = [];
 (* the stub *) *)

fun similar_names (listSList, {first, middle, last}) =
    let val lst = first::get_substitutions2(listSList, first)
	fun create_names ([], acc) = acc
	  | create_names (name::rest, acc) =
	    create_names (rest,
			  acc@[{first=name, middle=middle, last=last}])
    in create_names(lst,[]) end;

fun similar_names1 (listSList, (first, middle, last)) =
    let
	val lst = first :: get_substitutions2(listSList, first)
    in
	map (fn name => {first = name, middle = middle, last = last}) lst
    end	
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove;

(* put your solutions for problem 2 here *)

(* card -> color *)
(* takes a card and returns its color: spades and clubs are black,
diamonds and hearts are red *)

(* fun card_color (r: rank, n: int) = color (* the stub *) *)

fun card_color (s, r) =
    case s of
	Spades => Black
      | Clubs => Black
      | _ => Red;

(* card -> int *)
(* takes a card and returns its value *)

(* fun card_value c = 0 (* the stub *) *)

fun card_value ( _ , r) =
    case r of
	Num int => int
      | Ace => 11
      | _ => 10;

(* card list, card, e -> card list *)
(* takes a list of cards cs, a card c, and an exception e,It returns a
   list that has all the elements of cs except c *)

(* fun remove_card (cardList, card, excep) = [] (* the stub *) *)

fun remove_card (cs, c, e) =
    let	fun rm_cards ([], acc) = raise e
	  | rm_cards (first::rest, acc) =
	    if first = c then acc@rest
	    else rm_cards(rest, first::acc)
    in rm_cards(cs, []) end
	
(* listCard -> Bool *)
(* takes a list of cards, return true if all cards has the same color *)

(* fun all_same_color cs = false (* the stub *) *)


fun all_same_color [] = true
  | all_same_color (first::rest) =
    let fun all_same (cs, one) =
	    case cs of
		[] => true
	      | head::neck => if card_color head = card_color one
			      then all_same_color rest
			      else false
    in all_same(rest,first) end

(*
fun all_same_color cs =
    case cs of
	[(Hearts, _), (Hearts, _)] => true
      | [(Hearts, _), (Diamonds, _)] => true
      | [(Diamonds, _), (Diamonds, _)] => true
      | [(Diamonds, _), (Hearts, _)] => true
      | [(Spades, _), (Spades,_)] => true
      | [(Spades, _), (Clubs,_)] => true
      | [(Clubs, _), (Clubs, _)] => true
      | [(Clubs, _), (Spades, _)] => true
      | _ => false
					
*)				      
				       

(* card list -> int *)
(* takes a list of cards and returns the sum of their values *)

(* fun sum_cards (cardList) = 0 (* the stub *) *)

fun sum_cards cardList =
    let fun sum_cs (cs, acc) =
	    case cs of
		[] => acc
	      | first::rest => sum_cs(rest, acc+ card_value first)
    in sum_cs(cardList, 0) end;

(* card list , int -> int *)
(* takes a card list (the held-cards) and an int (the goal) and computes
   the score.
   Scoring works as follows: Let sum be the sum of the values of the
   held-cards. If sum is greater than goal, the preliminary score is
   three times (sum − goal), else the preliminary score is (goal − sum).
   The score is the preliminary score unless all the held-cards are the
   same color, in which case the score is the preliminary score divided
   by 2 (and rounded down as usual with integer division *)

(* fun score (cs: card list, goal: int) = 0 (* the stub *) *)

fun score1 (cs, gaol) =
    let val sum = sum_cards cs
	val DIV = all_same_color cs
    in case cs of
	   [] => 0
	 | _ => if sum > gaol then if DIV then (3 * (sum - gaol)) div 2
				   else 3 * (sum - gaol) 
		else if DIV then (gaol - sum) div 2 else gaol - sum    
    end;

fun score (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end

(* card list, move list, int -> int *)
(* takes a card list (the card-list) a move list (what the player “does”
   at each point), and an int (the goal) and returns the score at the end
   of the game after processing (some or all of) the moves in the move
   list in order *)

(* fun officiate (cs, ms, gaol) = 0 (* the stub *) *)

fun officiate (cs, ms, gaol) =
    let fun move_list (cs, hcs, ms) =
	    case ms of
		[] => score (hcs, gaol)
	      | (Discard c):: tail =>
		move_list (cs, (remove_card (hcs, c, IllegalMove)), tail)
	      | Draw::tail =>
		case cs of
		    [] => score (hcs, gaol)
		  | first::rest => let
		      val sum = first::hcs
		  in  if score (sum,gaol) > gaol
		      then score (sum, gaol)
		      else move_list (rest, sum, tail) end
    in	move_list (cs, [], ms) end
 (* -> score *)
