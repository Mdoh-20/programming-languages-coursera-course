(* int*int*int list, int list -> int *)

(* takes a list of dates and list of months returns the number of dates
 in the list of dates that are in any of the months in the list of *)

fun number_in_months_challenge (dates: (int*int*int) list, ms: int list)
    = if null ms
      then []
      else let
	  fun equal (ms: int list) =
	      if null (tl ms)
	      then hd ms
	      else let val tl_ans = (tl ms)
		   in
		       if (hd ms) = tl_ans
		       then tl_ans
		       else 
