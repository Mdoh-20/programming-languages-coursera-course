(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee
   that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: *)
(* All the tests should evaluate to true. For example,
   the REPL should say: val test1 = true : bool *)

use "01_homework-2_hw2provided.sml";

val test10 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option ("string", []) = NONE
val test12 = all_except_option ("string", ["str", "string"])
	     = SOME ["str"]
val test13 = all_except_option ("hello", ["what", "is", "my", "name"])
	     = NONE
val test14 = all_except_option ("what", ["when", "where", "what", "who"])
			       = SOME ["when", "where", "who"]

val test20 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test21 = get_substitutions1 ([], "foo") = []
						  
val test22 = get_substitutions1 ([["fool","foo"],["there"],[]], "foo")
	     = ["fool"]
val test23 = get_substitutions1 ([["foo","faol"],["there"],
				  ["fool","faol"]], "foo")
	     = ["faol"];
val test24 = get_substitutions1([["Fred","Fredrick"],
				 ["Elizabeth","Betty"]
				 ,["Freddie","Fred","F"]], "Fred")
	     = ["Fredrick","Freddie","F"]
val test25 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
				 ["Geoff","Jeff","Jeffrey"]], "Jeff")
	     = ["Jeffrey","Geoff","Jeffrey"];

val test30 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test31 = get_substitutions2 ([], "foo") = []
						  
val test32 = get_substitutions2 ([["fool","foo"],["there"],[]], "foo")
	     = ["fool"]
val test33 = get_substitutions2 ([["foo","faol"],["there"],
				  ["fool","faol"]], "foo")
	     = ["faol"];
val test34 = get_substitutions2 ([["Fred","Fredrick"],
				 ["Elizabeth","Betty"]
				 ,["Freddie","Fred","F"]], "Fred")
	     = ["Fredrick","Freddie","F"]
val test35 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],
				 ["Geoff","Jeff","Jeffrey"]], "Jeff")
	     = ["Jeffrey","Geoff","Jeffrey"];

val test40 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"]
			     ,["Freddie","Fred","F"]],
			    {first="Fred", middle="W", last="Smith"}) =
	     [{first="Fred", last="Smith", middle="W"},
	      {first="Fredrick", last="Smith", middle="W"},
	      {first="Freddie", last="Smith", middle="W"},
	      {first="F", last="Smith", middle="W"}];
val test41 = similar_names ([["Fred","Fredrick"],[]
			     ,["Freddie","Fred","F"]],
			    {first="Fred", middle="W", last="Smith"}) =
	     [{first="Fred", last="Smith", middle="W"},
	      {first="Fredrick", last="Smith", middle="W"},
	      {first="Freddie", last="Smith", middle="W"},
	      {first="F", last="Smith", middle="W"}]
val test42 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"]
			     ,["Freddie","Fred"]],
			    {first="Fred", middle="W", last="Smith"}) =
	     [{first="Fred", last="Smith", middle="W"},
	      {first="Fredrick", last="Smith", middle="W"},
	      {first="Freddie", last="Smith", middle="W"}]
val test43 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"]
			     ,["Freddie","F"]],
			    {first="Fred", middle="W", last="Smith"}) =
	     [{first="Fred", last="Smith", middle="W"},
	      {first="Fredrick", last="Smith", middle="W"}]
val test44 = similar_names ([],{first="Fred", middle="W",
				last="Smith"}) =
	     [{first="Fred", last="Smith", middle="W"}];

val test50 = card_color (Clubs, Num 10) = Black
val test51 = card_color (Spades, Ace) = Black
val test52 = card_color (Diamonds, Num 6) = Red
val test53 = card_color (Hearts, Jack) = Red;

val test60 = card_value (Clubs, Num 10) = 10
val test61 = card_value (Spades, Ace) = 11
val test62 = card_value (Diamonds, Num 6) = 6
val test63 = card_value (Hearts, Jack) = 10;

val test70= remove_card ([(Hearts, Ace),(Hearts, Num 5)],
	 		 (Hearts, Ace), IllegalMove) = [(Hearts, Num 5)];

val test71= remove_card ([(Hearts, King),(Hearts, Ace)],
			 (Hearts, Ace), IllegalMove) = [(Hearts, King)]
val test72= remove_card ([(Clubs, Ace),(Spades, Num 5),(Diamonds, Jack)],
			 (Spades, Num 5), IllegalMove) =
	    [(Clubs, Ace), (Diamonds,Jack)]

(*
val test73= remove_card ([(Hearts, Ace),(Hearts, Num 5)],
			 (Clubs, Ace), IllegalMove)
	    =  IllegalMove
val test74= remove_card ([], (Hearts, Ace), IllegalMove)
	    = IllegalMove 
*)

val test80 = all_same_color [(Hearts, Ace), (Hearts, Num 2)] = true
val test81 = all_same_color [(Spades, King), (Clubs, Num 9)] = true
val test82 = all_same_color [(Spades, Ace), (Clubs, Jack), (Clubs, Ace)]
	     = true
val test83 = all_same_color [(Clubs, King), (Clubs, Jack), (Clubs,Num 3)]
	     = true
val test84 = all_same_color [(Hearts, King), (Clubs, Num 9),(Clubs, Ace)]
	     = false
val test85 = all_same_color [(Clubs, Jack), (Diamonds, Queen)] = false
val test86 = all_same_color [(Diamonds, King), (Spades, King)] = false
val test87 = all_same_color [(Clubs, Jack)] = true


val test90 = sum_cards [(Clubs, Num 2), ( Diamonds, Num 5)] = 7
val test91 = sum_cards [(Hearts, Jack), (Clubs, Num 9)] = 19
val test92 = sum_cards [(Diamonds, Ace),(Spades, Ace)] = 22
val test93 = sum_cards [(Spades, King), (Hearts, Queen)] = 20
val test94 = sum_cards [(Clubs, Num 10)] = 10
val test96 = sum_cards [(Spades, Num 8), (Spades,Num 10)] = 18
val test95 = sum_cards [] = 0

val test100 = score ([(Hearts, Num 2),(Clubs, Queen)],13) = 1
val test101 = score ([(Clubs, King), (Spades, Num 5)],6) = 13
val test102 = score ([(Hearts, Ace), (Diamonds,Num 9)
		      ,(Spades, Jack)], 9) = 63
val test103 = score ([(Diamonds, Ace)], 6) = 7
val test104 = score ([],10) = 0
val test105 = score ([(Spades, Num 9),(Spades, Num 3),(Spades, Num 5)]
		    , 10) = 10

val test110 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test111 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace)
			  ,(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42)
              = 3
val test112 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42); false) handle IllegalMove => true)

val test113 = officiate ([(Diamonds, Num 2), (Hearts, Jack),
			  (Spades, Queen), (Clubs, Ace)],
			 [Draw, Draw, Draw, Draw,
			  Discard(Clubs, Ace)], 25) = 3

