import Test.QuickCheck

--Part 1

--The power function when given the arguments n k takes k+1 steps to complete.

--Part 2

--In order to make testing easier, we specified that any negative k value will return -1.

power :: Integer -> Integer -> Integer
power n k | k < 0 =  (-1) --error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = (-1) --error "power: negative argument" 
power1 n 0 = 1
power1 n k = product(replicate (fromInteger k) n)

--Part 3

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = (-1) --error "power: negative argument"
power2 n 0 = 1
power2 n k 
           | even k = power2 (n*n) (div k 2)
           | odd k  = n * power2 n (k-1)

--Part 4

{-
A: First we make some tests to make sure they work with legal input.

n = 2 k = 8, should give 256
n = 10 k = 10, should give 10000000000
n = 5 k = 2, should give 25

We then test that the edge cases work as intended

n = 2 k = 0, should give 1
n = 0 k = 0, should give 1
n = 0 k = 2, should give 0

We then test if the three functions behave the same way when given negative k values.

n = 2 k = -1, should give error
-}

--B

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power n k == power2 n k

--C

testCases = [(prop_powers 2 8), 
             (prop_powers 10 10), 
             (prop_powers 5 7),
             (prop_powers 5 2), 
             (prop_powers 2 0), 
             (prop_powers 0 0), 
             (prop_powers 0 2), 
             (prop_powers 2 (-1))] 

test_cases = and testCases

--D

--quickCheck worked with the original prop_powers function.