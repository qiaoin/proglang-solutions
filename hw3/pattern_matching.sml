use "first_and_all_answers.sml";

datatype pattern = WildcardP
		 | VariableP of string
		 | UnitP
		 | ConstantP of int
		 | ConstructorP of string * pattern
		 | TupleP of pattern list;

datatype valu = Constant of int
	      | Unit
	      | Constructor of string * valu
	      | Tuple of valu list;

fun g f1 f2 p =
    let
	      val r = g f1 f2
    in
	      case p of
	          WildcardP         => f1 ()
	        | VariableP x       => f2 x
	        | ConstructorP(_,p) => r p
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | _                 => 0
    end;

(* 9 *)
val count_wildcards = g (fn () => 1) (fn x => 0);

val count_wild_and_variable_lengths =
    g (fn () => 1) (fn x => String.size x);

fun count_some_var (s, p) =
  g (fn () => 0) (fn x => if s=x then 1 else 0) p;

(* 10 *)
fun check_pat p =
  let
      fun pattern_to_list ps =
        case ps of
            VariableP x => x::[]
          | ConstructorP(_, ps') => pattern_to_list ps'
          | TupleP ps' => List.foldl (fn (p', i) => (i @ pattern_to_list(p')))
                                     []
                                     ps'
          | _ => []
      fun is_repeat xs =
        case xs of
            [] => false
          | x::xs' => if List.exists (fn s => s=x) xs'
                      then true
                      else is_repeat xs'
                     (*
                     检查一个列表中是否有重复元素：每次检查第一个元素是否在剩余元素中有重复
                     1. 如果其在剩余元素中出现过，返回True;
                     2. 如果未出现，那在接下来的比较中，可以仅比较剩余列表中是否有重复元素
                     *)
  in
      not((is_repeat o pattern_to_list) p)
  end;

val a = ConstructorP ("hello", VariableP "world");
val b = VariableP "my";
val c = WildcardP;
val d = VariableP "name";
val f = VariableP "world";
val h = ConstructorP ("world", VariableP "world");
val e = ConstantP 12;

val p_tuple = TupleP [a, b, c, d, e];

val v_a = Constructor ("hello", Unit);
val v_b = v_a;
val v_c = Unit;
val v_d = v_a;
val v_e = Constant 12;
val v_tuple = Tuple [v_a, v_b, v_c, v_d, v_e];

val check_pat1 = check_pat (TupleP [a, b, c, d, e]);
val check_pat2 = check_pat (TupleP [a, b, c, d, f]);
val check_pat3 = check_pat (TupleP [a, b, c, d, e, h]);

(* 11 *)
fun match (value, p) =
  case (value, p) of
      (_, WildcardP) => SOME []
    | (v, VariableP s) => SOME ((s, v)::[])
    | (Unit, UnitP) => SOME []
    | (Constant v2, ConstantP v1) => if v1=v2 then SOME [] else NONE
    | (Constructor(s2, v), ConstructorP(s1, p)) =>
      let
          val remain = match (v, p)
          val next = case remain of
                         NONE => false
                       | SOME _ => true
      in
          if s1=s2 andalso next
          then remain
          else NONE
      end
    | (Tuple vlist, TupleP plist) => all_answers
                                        match
                                        (ListPair.zip(vlist, plist))
    | _ => NONE;

val v = Constructor ("world", Constant 10);
val hello = match (v, h);

val twelve = match (Constant 12, ConstantP 12);

val test_tuple = match (v_tuple, p_tuple);

(* 12 *)
fun first_match (value, ps) =
  let
      val result = (first_answer
                       match
                       (List.foldl
                            (fn (p, acc) => (value, p)::acc)
                            []
                            ps)
                   ) handle NoAnswer =>
                                        (print "exception occur\n"; [])
  in
      case result of
          [] => NONE
        | _ => SOME result
  end;

val p_list = [a, b, d, e, h];
val test_first = first_match (Constant 12, p_list);
val test_variable = first_match (Constructor("world", Constant 12), p_list);

val p_list_2 = [UnitP, UnitP, ConstantP 12];
val pair_list = (List.foldl
                        (fn (p, acc) => (Constant 12, p)::acc)
                        []
                        p_list_2);
val test_two = first_answer match pair_list;

val combine = first_match (Constant 13, p_list_2);

