use "what_month.sml";

fun month_range (day1, day2) =
  if day1 > day2
  then []
  else what_month day1 :: month_range (day1+1, day2);

val r = month_range (27, 33);
