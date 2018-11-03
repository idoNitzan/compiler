
#use "reader.ml";;
  
module Atomic: sig
  val read_bool : char list -> sexpr * char list (*TODO - remove *)
end
= struct
  
let read_bool =
  let _hash_ = char '#' in
  let _true_val_ = disj (char 't') (char 'T') in
  let _false_val_ = disj (char 'f') (char 'F') in
  let _combined_ = caten _hash_ (disj _true_val_ _false_val_) in
  pack _combined_ (fun (h, v) -> match v with
                                  | 'T' -> Bool true
                                  | 't' -> Bool true
                                  | 'F' -> Bool false
                                  | 'f' -> Bool false
                                  | _ -> raise X_this_should_not_happen)

end;; (* struct Reader *)
