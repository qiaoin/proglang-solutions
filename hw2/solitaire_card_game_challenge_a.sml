datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = rank * suit;

datatype color = Red | Black;
datatype move = Discard of card | Draw;

exception IllegalMove;

fun card_color (c : card) =
  case c of
      (_, Clubs)    => Black
    | (_, Diamonds) => Red
    | (_, Hearts)   => Red
    | (_, Spades)   => Black;

fun card_value (c : card) =
  case c of
      (Num i, _) => i
    | (Ace, _)      => 11
    | _             => 10;

fun remove_card (cs : card list, c : card, e) =
  let
      fun helper (target : card, tested : card list, remainded : card list) =
        if target = c
        then tested @ remainded
        else case remainded of
                 [] => raise e
               | x::remainded' => helper (x, tested@(target::[]), remainded')
  in
      case cs of
          [] => raise e
        | x::cs' => helper (x, [], cs')
  end;

fun all_same_color (cs : card list) =
  case cs of
      [] => true
    | _::[] => true
    | head::(next::rest) => (card_color(head) = card_color(next)) andalso all_same_color (next::rest);

fun sum_cards (cs : card list) =
  let
      fun sum_all (cs : card list, produce : int) =
        case cs of
            [] => produce
          | c::cs' => sum_all (cs', produce+card_value(c))
  in
      sum_all (cs, 0)
  end;

fun score (cs : card list, goal : int) =
  let
      val sum = sum_cards cs
      val preliminary_score = if sum > goal
                              then 3 * (sum - goal)
                              else goal - sum
  in
      if all_same_color cs
      then
          let
              val q = preliminary_score div 2
          in
              if preliminary_score mod 2 = 1
              then q + 1
              else q
          end
      else preliminary_score
  end;

fun officiate (cs : card list, processing : move list, goal : int) =
  let
      fun helper_state (card_list, processing_list, held_cards) =
        case processing_list of
            [] => score (held_cards, goal)
          | p::plist' => case p of
                           Discard c => helper_state (card_list, plist', remove_card(held_cards, c, IllegalMove))
                         | Draw => case card_list of
                                      [] => score (held_cards, goal)
                                    | c::cs' => if sum_cards (c::held_cards) > goal
                                                then score (held_cards, goal)
                                                else helper_state (cs', plist', c::held_cards)
  in
      helper_state (cs, processing, [])
  end;

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
  let
      val cards = [(Jack,Clubs),(Num(8),Spades)]
	    val moves = [Draw,Discard(Jack,Hearts)]
  in
	    officiate(cards,moves,42)
  end;

fun provided_test2 () = (* correct behavior: return 5 *)
  let
      val cards = [(Ace,Clubs),(Ace,Spades),(Ace,Clubs),(Ace,Spades)]
	    val moves = [Draw,Draw,Draw,Draw,Draw]
  in
 	    officiate(cards,moves,42)
  end;

val test_officiate2 = provided_test2 ();

val test_officiate1 = provided_test1 ();
