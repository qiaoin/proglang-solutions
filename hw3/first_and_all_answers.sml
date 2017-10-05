exception NoAnswer;

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
                    NONE => first_answer f xs'
                  | SOME v => v;

fun all_answers f xs =
  let
      fun helper f xs acc =
        case xs of
            [] => acc
          | x::xs' => case f x of
                          NONE => []
                        | SOME v => helper f xs' (acc@v)
  in
      case xs of
          [] => SOME []
        | _  => (* xs is not empty *)
          let
              val result = helper f xs []
          in
              case result of
                  [] => NONE
                | _  => SOME result
          end
  end;
