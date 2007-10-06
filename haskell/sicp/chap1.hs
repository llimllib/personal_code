module Chap1 where

-- exercise 1.2
onepoint2 = (5 + 4 + (2 - (3 - 6 + (4 / 5))))
            / (3 * (6 - 2) * (2 - 7))

-- exercise 1.3
square_biggest a b c = if a > b 
                       then if b > c 
                            then a**2 + b**2
                            else a**2 + c**2
                       else if a > c
                            then a**2 + b**2
                            else b**2 + c**2

-- exercise 1.4
a_plus_abs_b :: (Num a, Ord a) => a -> a-> a
a_plus_abs_b a b = (if b > 0
                    then (+)
                    else (-)) a b

-- excercise 1.7
sqrt x = newtons 1.0 0.0 x

newtons guess oldguess x = if good_enough guess oldguess
                           then guess
                           else newtons (improve guess x) guess x

improve guess x = (guess + (x / guess)) / 2

good_enough guess oldguess = abs ((guess - oldguess) / guess) < 0.001

--exercise 1.8
cbrt x = newtons3 1.0 0.0 x

newtons3 guess oldguess x = if good_enough guess oldguess
                            then guess
                            else newtons3 (improve3 guess x) guess x

improve3 guess x = (x / guess**2 + 2*guess) / 3

--just for fun, the Ackermann's in Haskell:
ack x 0 = 0
ack 0 y = 2 * y
ack x 1 = 2
ack x y = ack (x - 1) (ack x (y - 1))
--pretty! but kind of worthless. ack 2 5 == 2**2**16 stack overflows in no time,
-- scheme takes a few seconds to return a result.


