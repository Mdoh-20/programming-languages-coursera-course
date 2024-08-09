
(* int*int*int int*int*int -> bool*)
(*take two dates and return true if the first one is before the second*)
(*the date is "int*int*int" *)
(*where int is the year and int is the month and int is the day*)

(* fun is_older (y1: int*int*int, y2: int*int*int) =
    true (*the stub*)  *)

fun is_older ((y1, m1, d1), (y2, m2, d2)) =
    (y1 < y2) orelse (m1 < m2) andalso (y1 <= y2)
    orelse (d1 < d2) andalso (m1 <= m2) andalso (y1 <= y2)

(* date list, int -> int*)
(* taks list of dates and month to return how many dates in the list are
   in the given month *)

(* fun number_in_month (y: int*int*int list, m: int)= 0 (*the stub*) *)

fun number_in_month1 (ys, m) =
    let fun num_in_month (ys, acc) =
	    case ys of
		[] => acc
	      | x::xs => case x of
			     (_, n, _) =>
			     if m = n
			     then num_in_month (xs, acc + 1)
			     else num_in_month (xs, acc)
    in num_in_month (ys, 0) end;
		    
fun number_in_month (ys, m) =
    length (List.filter (fn (_, n, _) => m = n) ys)
	   
fun number_in_month2 (ys, m) =
    List.foldl (fn ((_, n, _), acc) => if m = n then 1 + acc else acc)
	       0 ys;
	       
(* year list, int list -> int *)
(* taks list of dates and list of months to return how many dates in the
   list are in the given list of months *)

(* fun number_in_months (ys, ms) = 0 (*the stub*) *)

fun number_in_months1 (ys, ms) =
    let fun num_in_months (ys, ms, acc) =
	    case ms of
		[] => acc
	      | x::xs =>
		num_in_months(ys, xs, acc + number_in_month (ys ,x))
    in num_in_months(ys, ms, 0) end;

fun number_in_months (ys, ms) =
    List.foldl (fn (m, acc) => acc + number_in_month (ys, m)) 0 ms;

(* date list, int -> date list *)
(* takes a list of dates and a month and returns a list holding
   the dates from the argument list of dates that are in the month *)

(* fun dates_in_month (ys: int*int*int list, m: int) = [] *)
(* TODO: STILL NEED TO MODIFAY SOME OF THESE FUNCTION *)

fun dates_in_month1 (ys, m) =
    case ys of
	[] => []
      | x::xs => case x of
		     (_, n, _) => if n = m
				  then x::(dates_in_month1(xs, m))
				  else dates_in_month1(xs, m);

fun dates_in_month (ys, m) =
    List.filter(fn (_, n, _) => m = n) ys

(* date list, int list -> date list *)
(* takes a list of dates and a list of months returns a list holding
   the dates from the argument list of dates that are in the months *)

(* fun dates_in_months (ys: (int*intint) list, ms: int list) = [];
(* the stub*) *)

fun dates_in_months1 (ys, ms) =
    case ms of
	[] => []
      | x::xs => dates_in_month (ys, x)@dates_in_months1 (ys, xs);


fun dates_in_months (ys, ms) =
    List.foldl(fn (m, acc) => acc@dates_in_month(ys, m)) [] ms

(* string lsit, int -> int *)
(* takes a list of strings and an int n, returns the nth element of the
   list *)
(*  where the head of the list is 1st *)

(* fun get_nth (ws: string list, n: int) = "" (* the stub*) *)

fun get_nth (ws, n) =
    case ws of
	[] => ""
      | x::xs => case n of
		     1 => x
		   | _ => get_nth(xs, n-1);

(* int*int*int -> string *)

(* takes a date, returns a string of the form January 20, 2013 *)

(* fun date_to_string (y: int*int*int) = ""; (* the stub *) *)

fun date_to_string (d, m, y) =
    let val months = ["January", "February", "March", "April", "May",
		      "June", "July", "August", "September", "October",
		      "November", "December"]
    in
	get_nth (months, m)^ " " ^ Int.toString y ^ ", " ^
	Int.toString d
    end;

(* int , int list -> int *)
(* takes an int and return a how much number in the list before the
   given number*)

(* fun number_before_reaching_sum (sum: int, numbers: int list) = 0
 (* the stub*) *)
exception NoAnswer

fun number_before_reaching_sum (sum, numbers) =
    let	fun counter (current, times, lst) =
	    case lst of
		[] => raise NoAnswer
	      | x::xs => let val now = current + x
			 in if now >= sum then times
			    else counter (now, (times + 1), xs) end
    in counter (0,0,numbers) end;


(* int -> int *)
(* takes a day of year returns what month that day is in *)

(* fun what_month (day: int) = 1 (* the stub *) *)

fun what_month day =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day,days) + 1 end;

(* int, int -> int list *)
(* takes two days and return what months this day in and days between
   them *)

fun month_range (day1, day2) =
    if day1 < day2 then []
    else what_month(day1)::month_range((day1 +1), day2);

(* date list -> date option *)
(* takes a list of dates and evaluates to an (int*int*int) option *)

fun oldest dates =
    case dates of
	[] => NONE
      | x::xs => let fun older_date (x, xs) =
			 case xs of
			     [] => x
			   | x'::xs' => if is_older (x, x')
					then older_date (x, xs')
					else older_date (x', xs')
		 in SOME(older_date (x, xs)) end;
(*
fun oldest dates =
    List.foldl (fn (x::xs, acc) =>
		   (fn x'::xs' => if is_older (x, x')
				  then x
				  else x')) [] dates*)


fun member (i, ms) =
    case ms of
	[] => false
      | x::xs => if x = i then true
		 else member (i, xs);



fun remove_dup ms =
    case ms of
	[] => ms
      | x::xs => if member(x, xs)
		 then remove_dup xs
		 else x::remove_dup xs


fun number_in_months_challenge (dates, ms) =
    number_in_months(dates, remove_dup(ms))


(* int*int*int list, int list -> int *)
(* takes a list of dates and list of months returns the number of dates
 in the list of dates that are in any of the months in the list of *)

fun dates_in_months_challenge (dates, ms)=
    dates_in_months (dates, remove_dup(ms));

(* date -> bool *)
(* takes a date and determines if it describes a real date in the common
   era *)
(* fun reasonable_date date = false (* the stub *) *)
(*
fun reasonable_date (y, m, d)=
    (y > 0) andalso (12 >= m) andalso (m > 0) andalso () andalso
*)
