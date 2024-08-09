(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee
 that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file:*)
use "homeWork1 in week 3.sml";
(* All the tests should evaluate to true. For example, the REPL should
 say: val test1 = true : bool *)


val test10 = is_older ((2008,5,10), (2009,3,4)) = true
val test11 = is_older ((2009,2,10), (2009,3,10)) = true
val test12 = is_older ((2020,3,10), (2020,3,19)) = true			
val test13 = is_older ((2010,7,23), (2009,4,5)) = false
val test14 = is_older ((2015,10,3), (2015,9,3)) = false
val test15 = is_older ((2000,1,12), (2000,1,1)) = false;

val test20 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test21 = number_in_month ([(2012,2,20),(2019,2,1),(2024,2,28)],2) = 3
val test22 = number_in_month ([(2012,2,28),(2013,12,1)],5) = 0
val test23 = number_in_month ([],6) = 0;

val test31 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),
				(2011,4,28)],[]) = 0
						       
val test32 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),
				(2011,4,28)],[2,3,4]) = 3
val test33 = number_in_months ([(2012,2,28),(2013,4,1),(2011,3,31),
				(2011,4,28)],[2,3,4]) = 4
							    
val test34 = number_in_months ([(2012,11,28),(2013,12,1),(2011,1,31),
				(2011,5,28)],[2,3,4]) = 0;

val test40 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month ([(2012,2,28),(2013,2,1)], 2) =
	     [(2012,2,28), (2013,2,1)]
val test42 = dates_in_month ([(2012,2,28),(2013,12,1)],5) = []
val test43 = dates_in_month ([],2) = [];

val test50 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),
			       (2011,4,28)],[2,3,4]) =
	     [(2012,2,28),(2011,3,31),(2011,4,28)]
val test51 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),
			       (2011,4,28)],[2,3,12,4]) =
	     [(2012,2,28),(2011,3,31),(2013,12,1),(2011,4,28)]
		 
val test52 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),
			       (2011,4,28)], []) = []
val test53 = dates_in_months ([],[2,3,4]) = []
						
val test60 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test61 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test62 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"
val test63 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test70 = date_to_string (2003, 9, 1) = "September 1, 2003"
val test71 = date_to_string (2002, 2, 1) = "February 1, 2002"
val test72 = date_to_string (1998, 4, 25) = "April 25, 1998"
val test73 = date_to_string (1996, 1, 10) = "January 10, 1996"
						
val test80 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test81 = number_before_reaching_sum (11, [1,2,3,4,5]) = 4
val test82 = number_before_reaching_sum (5, [1,2,3,4,5]) = 2
val test83 = number_before_reaching_sum (7, [1,2,3,4,5]) = 3
val test84 = number_before_reaching_sum (29,[5,5,5,5,5,5,5]) = 5
								   
val test90 = what_month 70 = 3
val test91 = what_month 120 = 4
val test92 = what_month 121 = 5
val test93 = what_month 345 = 12
				  
val test100 = month_range (31, 34) = [1,2,2,2]
val test101 = month_range (59, 65) = [2,3,3,3,3,3,3]
val test102 = month_range (333,336) = [11,11,12,12]
val test103 = month_range (364,365) = [12,12]
val test104 = month_range (34,2) = []
				       
				       
val test110 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)])
	      = SOME (2011,3,31)
val test111 = oldest([(2012,2,28),(2011,3,31),(2009,4,28)])
	      = SOME (2009,4,28)
val test112 = oldest([(2017,2,28),(2011,3,31),(2011,4,28),(1900,2,1)])
	      = SOME (1900,2,1)
val test113 = oldest([(2002,2,28),(2011,3,31),(2011,4,28)])
	      = SOME (2002,2,28)

val test000 = member (2, [2, 3, 4]) = true
val test002 = member (4, [2, 1, 9, 0, 4]) = true
val test001 = member (2, [1, 3, 4]) = false

val test120 = remove_dup [2, 3, 2, 4, 4, 1, 1, 1] = [3, 2, 4, 1]
val test121 = remove_dup [1, 2, 3, 4, 5] = [1 ,2 ,3 ,4 ,5]
val test122 = remove_dup [1, 1, 1, 1, 1] = [1]
val test123 = remove_dup [] = []

				  
val test0000 = dates_in_months_challenge ([(2012,2,28),(2011,3,31),
					   (2011,4,28)], [3, 3]) =
	       [(2011,3,31)]
		   
val test0010 = number_in_months_challenge ([(2012,2,28),(2011,3,31),
					    (2011,4,28)], [3, 3]) = 1;
(*									
val test0100 = reasonable_date(2000, 10, 9) = true
val test0110 = reasonable_date(0000, 12, 2) = false
val test0111 = reasonable_date(2003, 13, 4) = false
val test0112 = reasonable_date(2009, 5, 49) = false
*)
