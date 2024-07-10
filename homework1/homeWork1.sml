
(* int*int*int int*int*int -> bool*)
(*take two dates and return true if the first one is before the second*)
(*the date is "int*int*int" *)
(*where int is the year and int is the month and int is the day*)

(* fun is_older (y1: int*int*int, y2: int*int*int) =
    true (*the stub*)  *)

fun is_older (y1: int*int*int, y2: int*int*int) =
    if #1 y1 < #1 y2
    then true
    else if #2 y1 < #2 y2 andalso #1 y1 = #1 y2
    then true
    else #3 y1 < #3 y2 andalso #2 y1 = #2 y2 andalso #1 y1 = #1 y2;


(* int*int*int list int -> int*)
(* taks list of dates and month
   to return how many dates in the list are in the given month *)

(* fun number_in_month (y: int*int*int list, m: int)= 0 (*the stub*) *)

fun number_in_month (y: (int*int*int) list, m: int) =
    if null y
    then 0
    else if #2 (hd y) = m
    then 1 + number_in_month ((tl y), m)
    else number_in_month ((tl y), m);


(* int*int*int list, int list -> int *)
(* taks list of dates and list of months to return
   how many dates in the list are in the given list of months *)

(* fun number_in_months (ys: (int*int*int) list,
       			 ms: int list) = 0 (*the stub*) *)

fun number_in_months (ys: (int*int*int) list, ms: int list) =
    if null ms
    then 0
    else number_in_month (ys ,(hd ms)) +
	 number_in_months (ys ,(tl ms));
     
(* int*int*int list, int -> int*int*int list *)
(* takes a list of dates and a month and returns a list holding
   the dates from the argument list of dates that are in the month *)

(* fun dates_in_month (ys: int*int*int list, m: int) = [] *)

fun dates_in_month (ys: (int*int*int) list, m: int) =
    if null ys
    then []
    else if #2 (hd ys) = m
    then (hd ys) :: (dates_in_month ((tl ys), m))
    else (dates_in_month ((tl ys), m));

(* int*int*int list, int list -> int*int*int list *)

(* takes a list of dates and a list of months returns a list holding
   the dates from the argument list of dates that are in the months *)

(* fun dates_in_months (ys: (int*intint) list, ms: int list) = [];
 (* the stub*) *)

fun dates_in_months (ys: (int*int*int) list, ms: int list) =
    if null ms
    then []
    else dates_in_month (ys, (hd ms)) @ dates_in_months (ys, (tl ms));

(* string lsit, int -> int *)

(* takes a list of strings and an int n, returns the nth
   element of the list *)
(*  where the head of the list is 1st *)

(* fun get_nth (ws: string list, n: int) = "" (* the stub*) *)

fun get_nth (ws: string list, n: int) =
    if null ws
    then ""
    else  if n = 1
    then hd ws
    else get_nth ((tl ws), (n - 1));

(* int*int*int -> string *)

(* takes a date, returns a string of the form January 20, 2013 *)

(* fun date_to_string (y: int*int*int) = ""; (* the stub *) *)

fun date_to_string (y: int*int*int) =
    let val months = ["January", "February", "March", "April", "May",
		      "June", "July", "August", "September", "October",
		      "November", "December"]
    in
	get_nth (months, (#2 y))^ " " ^
	Int.toString (#3 y) ^ ", " ^
	Int.toString (#1 y)
    end;

(* int , int list -> int *)

(* takes an int and return a how much number in the list before the
   given number*)

(* fun number_before_reaching_sum (sum: int, numbers: int list) = 0
 (* the stub*) *)


fun number_before_reaching_sum (sum: int, numbers: int list) =
    let	fun counter (current: int, times: int,nums: int list ) =
	    let
		val x = current + (hd nums)
		(*val t = times -1*)
	    in
		if x >= sum
		then times
		else counter (x, (times + 1),(tl nums))
	    end
    in
	counter (0,0,numbers)
    end;

(* int -> int *)

(* takes a day of year returns what month that day is in *)

(* fun what_month (day: int) = 1 (* the stub *) *)

fun what_month (day: int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum (day,days)
    end;

(* int, int -> int list *)

(* takes two days and return what months this day in and days between
   them *)

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range ((day1 + 1), day2);

(* int*int*int list -> int*int*int option *)

(* takes a list of dates and evaluates to an (int*int*int) option *)

fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else let
	fun older_date (dates: (int*int*int) list)=
	    if null (tl dates)
	    then hd dates
	    else
		let val tl_ans = older_date (tl dates)
		in
		    if  is_older (hd dates, tl_ans)
		    then hd dates
		    else tl_ans
		end
    in
	SOME (older_date dates)
    end;
		     
fun member (x: int, ms: int list) =
    if null ms
    then false
    else if x = hd ms
    then true
    else member (x, tl ms);

fun remove_dup (ms: int list) =
    if null ms
    then ms
    else let val x = hd ms
	     val y = tl ms
	 in
	     if member (x, y)
	     then remove_dup y
	     else x :: remove_dup y
	 end;

fun number_in_months_challenge (dates, ms) =
    let val lst = remove_dup (ms)
    in number_in_months (dates, lst)
    end;


(* int*int*int list, int list -> int *)

(* takes a list of dates and list of months returns the number of dates
 in the list of dates that are in any of the months in the list of *)

fun dates_in_months_challenge (dates: (int*int*int) list, ms: int list)=
    let val lst = remove_dup (ms)
    in dates_in_months (dates ,lst)
    end;

	
