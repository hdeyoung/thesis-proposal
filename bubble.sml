local
  fun tostring [] = ""
    | tostring [x] =
        Int.toString x
    | tostring (x::(l as _::_)) =
        (Int.toString x) ^ "," ^ tostring l
in
  fun print l = TextIO.print ("[" ^ (tostring l) ^ "]\n")
end

local
  fun select [] = []
    | select (x::l) =
        (x, l)::(List.map (fn (y, l') => (y, x::l'))
                          (select l))

  val flatten = List.foldr (op @) []
in
  fun perms ([] : int list) = [[]]
    | perms (l as _::_) =
      let
        val s = select l
      in
        flatten (List.map (fn (x, xs) => List.map (fn ys => x::ys) (perms xs))
                          s)
      end
end

(* swap : int list -> int list option *)
(* Find the first pair of neighboring elements
   in list l that are out of order and swap them.
   Return SOME of the list after swapping;
   return NONE if no such pair exists. *)
fun swap [] = NONE
  | swap [x] = NONE
  | swap (x::y::ys) =
      if x > y
      then
        SOME (y::x::ys)
      else
        case swap (y::ys) of
          SOME l => SOME (x::l)
        | NONE => NONE

(* sort : int list -> int list *)
fun sort l =
      (print l;
       case swap l of
        SOME l' => sort l'
      | NONE => (TextIO.print "---\n"; l))
