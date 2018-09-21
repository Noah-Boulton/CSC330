(*  Assignment #1
	Noah Boulton
	V00803598
	Fall 2018	*)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)

(*	Parameters: Two dates 
	Return: 	True if the first argument is a date that comes before the second argument
				If the two dates are the same, the result is false	*)
fun is_older(d1: DATE, d2: DATE): bool =
    if #1 d1 < #1 d2
		then true
    else if #1 d1 > #1 d2
		then false
    else if #2 d1 < #2 d2
		then true (* here year is equal *)
    else if #2 d1 > #2 d2
		then false
    else if #3 d1 < #3 d2
		then true (* here year and month equal *)
    else false (* here the year, month, and day are equal *)

(*	Parameters: A list of dates and a month 
 	Return: 	The number of dates in the list taht are in the month*)
fun number_in_month(months: DATE list, month: int): int =
	if null months
	then 0 (* empty list *)
	else if month < 0 orelse month > 12
	then 0 (* invalid month *)
	else
	let
		val tl_ans = number_in_month(tl months, month);
	in
		if #2 (hd months) = month
		then 1 + tl_ans (* date is in the month, assumes date is valid *)
		else tl_ans
	end

(*	Parameters: A list of dates and a list of months
 	Return: 	The number of dates in the list of dates that 
	 			are in any of the months in the list of months *)
fun number_in_months(months_list: DATE list, months: int list): int =
	if null months_list orelse null months
	then 0
	else number_in_month(months_list, hd months) +
			 number_in_months(months_list, tl months)

(*	Parameters: A list of dates and a month 
 	Return: 	A list holding the dates from the argument 
	 			list of dates that are in the month *)
fun dates_in_month(dates: DATE list, month: int): DATE list =
	if null dates
	then []
	else
	let
		val tl_ans = dates_in_month(tl dates, month)
	in
		if #2 (hd dates) = month
		then hd dates :: tl_ans
		else tl_ans
	end

(*	Parameters: A list of dates and a list of months
 	Return: 	A list holding the dates from the argument 
	 			list of dates that are in any of the months 
				in the list of months *)
fun dates_in_months(dates: DATE list, months: int list): DATE list =
	if null dates orelse null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*	Parameters: A list of strings and an integer n 
 	Return: 	The n-th element of the list where the head of the list is first *)
fun get_nth(strings: string list, n: int): string =
	if n = 0 orelse n > length strings
	then raise InvalidParameter
	else if n = 1
	then hd strings
	else get_nth(tl strings, n-1)

(*	Parameters: A date
 	Return: 	A string of the form "January 20, 2013"*)
fun date_to_string(date: DATE): string =
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
	end

(*	Parameters: An integer sum and an integer list 
 	Return: 	An int n such that the first n elements 
	 			of the list add to less than sum, but the 
				first n + 1 elements of the list add to sum or more *)
fun number_before_reaching_sum(sum: int, l: int list): int =
	if null l
	then 0
	else if sum - (hd l) > 0
	then 1 + number_before_reaching_sum(sum - (hd l), tl l)
	else 0

(*	Parameters: A day of the year
 	Return: 	Which month the day is in *)
fun what_month(day: int): int =
	let
		val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		1 + number_before_reaching_sum(day, days_in_months)
	end

(*	Parameters: Two days of the year day1 and day2 
 	Return: 	An int list [m1,m2,...,mn] where m1 is the month of day1,
	 			m2 is the month of day1+1,..., and mn is the month of day day2 *)
fun month_range(day1: int, day2: int): int list =
	if day1 > day2
	then []
	else (what_month(day1)) :: (month_range(day1+1, day2))

(*	Parameters: A list of dates 
 	Return: 	A DATE option. It evaluates to NONE if
	 			the list has no dates and SOME d if the
				date d is the oldest date in the list *)
fun oldest(dates: DATE list): DATE option =
	if null dates
	then NONE
	else if length dates = 1
	then SOME (hd dates)
	else
		let
			val tl_oldest = oldest(tl dates)
		in
			if is_older((hd dates), valOf(tl_oldest))
			then SOME (hd dates)
			else tl_oldest
		end

(*	Parameters: A date 
 	Return: 	True if the date is a real date in the common era having 
	 			a positive year, a month between 1 and 12, and a day appropriate for the month *)
fun reasonable_date(day: DATE): bool =
	let
		val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val days_in_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

		fun is_leap(day: DATE): bool =
			if #1 day mod 400 = 0 orelse (#1 day mod 4 = 0 andalso #1 day mod 100 <> 0)
			then true
			else false

		fun is_valid(day: DATE): bool =
			if #1 day > 0 andalso #2 day > 0 andalso #2 day <= 12
				 andalso #3 day > 0 andalso #3 day < 32
			then true
			else false

		fun get_nth_int(ints: int list, n: int): int =
			if n = 0 orelse n > length ints
			then raise InvalidParameter
			else if n = 1
			then hd ints
			else get_nth_int(tl ints, n-1)
	in
		if is_valid(day) = false
		then false
		else if is_leap(day) = true andalso #3 day <
						1+get_nth_int(days_in_months_leap, #2 day)
		then true (* handle leap year *)
		else if is_leap(day) = false andalso #3 day <
						1+get_nth_int(days_in_months, #2 day)
		then true (* handle non leap year *)
		else false
	end
