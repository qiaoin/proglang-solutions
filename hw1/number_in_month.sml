(*
    xs : (int * int * int) list, a list of dates
    month : an int
    number_in_month : date list * int -> int
          returns how many dates in the list are in the given month
*)
fun number_in_month (xs : (int*int*int) list, month : int) =
  case xs of
      [] => 0
    | (_, x, _) :: xs' => if x=month
                          then number_in_month (xs', month) + 1
                          else number_in_month (xs', month);

(*
    the same as the above function, I add an =acc= to implement tail recursion
*)
fun number_in_month2 (xs : (int*int*int) list, month : int) =
  let fun helper (xs, acc) =
        case xs of
            [] => acc
          | (_, x, _) :: xs' => if x=month
                                then helper (xs', acc+1)
                                else helper (xs', acc)
  in
      helper(xs, 0)
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
val month = 7;
val how_many_7_month = number_in_month (date_list, month);
val how_many_7_month2 = number_in_month2 (date_list, month)
