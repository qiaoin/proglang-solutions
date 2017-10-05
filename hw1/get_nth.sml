fun get_nth (xs, n) =
  let
      (* 这一实现借鉴于 SICP 第一章 *)
      fun get_nth_iter (xs, n, counter) =
        (* 'a list * int -> 'a *)
        case xs of
          x::xs' => if n=counter
                    then x
                    else get_nth_iter (xs', n, counter+1)
         | [] => raise List.Empty
          (* don't worry about this case *)
  in
      get_nth_iter (xs, n, 1)
  end;

val str_list = ["abc", "def", "ghi", "jkl", "mn"];
val first = get_nth (str_list, 1);
val second = get_nth (str_list, 2);
val third = get_nth (str_list, 3);
val fourth = get_nth (str_list, 4);
