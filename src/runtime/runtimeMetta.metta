(= (max $A $B)
    (if (gt $A $B) $A $B)
)

(= (min $A $B)
    (if (lt $A $B) $A $B)
)

(= (not true) false)
(= (not false) true)

(= (and true true) true)
(= (and true false) false)
(= (and false true) false)
(= (and false false) false)

(= (or true true) true)
(= (or true false) true)
(= (or false true) true)
(= (or false false) false)

(= (xor true true) false)
(= (xor true false) true)
(= (xor false true) true)
(= (xor false false) false)
