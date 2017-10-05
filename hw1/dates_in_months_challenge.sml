fun dates_in_month (xs : (int*int*int) list, month : int) =
  case xs of
      [] => []
    | (x, y, z)::xs' => if y=month
                        then (x, y, z) :: dates_in_month (xs', month)
                        else dates_in_month (xs', month);

fun dates_in_months (xs : (int*int*int) list, months : int list) =
  case months of
      [] => []
    | month::months' => dates_in_month (xs, month)
                        @ dates_in_months (xs, months');

fun dates_in_months_challenge (xs, months) =
  let
      fun in_that_list (mlist : int list, month : int) =
        case mlist of
            [] => false
          | m::mlist' => month=m orelse in_that_list (mlist', month)
      fun helper (f, months, acc) =
        case months of
            [] => acc
          | month::months' => helper (f,
                                      months',
                                      if f(acc, month) then acc else acc @ (month::[]))
  in
      dates_in_months (xs, helper(in_that_list, months, []))
  end;

val date1 = (2000, 1, 12);
val date2 = (1994, 7, 16);
val date3 = (1995, 4, 9);
val date4 = (2016, 9, 20);
val date5 = (1998, 7, 22);
val date6 = (2016, 7, 16);
val date7 = (1995, 4, 19);
val date8 = (2012, 7, 19);

val date_list = [date1, date2, date3, date4, date5, date6, date7, date8];
val month_list = [1, 3, 5, 7, 9, 1, 1, 7, 7, 9, 9];

val matching_dates_list = dates_in_months_challenge (date_list, month_list);
