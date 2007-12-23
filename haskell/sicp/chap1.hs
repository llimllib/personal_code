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

-- Exercise 1.11
f n | n < 3 = n
f n         = f (n-1) + 2 * f (n-2) + 3 * f (n-3)

f2 n 
  | n < 3     = n
  | otherwise = f2_iter n 0 1 2
    where 
      f2_iter n a b c 
        | n < 3     = c
        | otherwise = f2_iter (n-1) b c (c + 2*b + 3*a)

-- here's what I know: (sum . take 2) [1,2,3] = 3
-- ((: []) . sum . take 2) [1,2,3] = [3]
-- 
--a direct translation is easy:
pascal row col
  | col == 0   = 1
  | row == col = 1
  | otherwise  = pascal (row-1) (col-1) + pascal (row-1) col
--and much prettier than the scheme, certainly. What's frustrating here is that
--I'd like to add a condition for bad input, where col > row, and just have
--it print out "invalid result" or something similar. That's not simple, and
--it certainly feels to me like it should be.

-- more interesting is a generator of whole rows in the triangle:
pasc 0 = [1]
pasc 1 = [1,1]
pasc row = rowgen $ pasc (row-1)
  where
    rowgen row            = 1 : sumpairs row ++ [1]
    sumpairs row          = addtwo row []
    addtwo (x:y:xs) accum = addtwo (y:xs) (x+y : accum)
    addtwo _        accum = accum

-- writing this has made me want to punch haskell errors in the face. But it was
-- fun nonetheless. (Num a) has no instance my ass!
-- And another side note: whenever writing haskell, I can't avoid the feeling
-- that someone, somewhere, could write my code in 1/5 the lines. Not sure
-- how I feel about that.
