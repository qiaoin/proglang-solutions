fun only_capitals xs =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs;

fun longest_string1 xs =
  List.foldl
      (fn (x, acc) => if String.size x > String.size acc
                      then x
                      else acc)
      ""
      xs;

fun longest_string2 xs =
  List.foldl
      (fn (x, acc) => if String.size x >= String.size acc
                      then x
                      else acc)
      ""
      xs;

(* test for 1, 2, 3 *)
val xs = ["Hello", "My", "Namme", "is", "Liu", "Bing", "nice", "to", "Meet", "you"];

val a = only_capitals xs;
val b = longest_string1 xs;
val c = longest_string2 xs;

fun longest_string_helper f xs =
  List.foldl
      (fn (x, acc) => if f (String.size x, String.size acc)
                    then x
                    else acc)
      ""
      xs;

val longest_string3 = longest_string_helper
                          (fn (x, y) => x > y);
val longest_string4 = longest_string_helper
                          (fn (x, y) => x >= y);
val d = longest_string3 xs;
val e = longest_string4 xs;

val longest_capitalized = (longest_string1 o only_capitals);
val f = longest_capitalized xs;

val rev_string = (String.implode o rev o String.explode);

val g = rev_string "Hello";

