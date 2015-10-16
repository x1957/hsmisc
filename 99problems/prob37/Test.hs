import Solution
import Test.HUnit
test_list name = TestList . (map (TestLabel name))

brute_force_phi m = length $ filter f [1..m] where f k = gcd m k == 1

test_range l r = [TestCase (assertEqual ("test phi " ++ show m) (brute_force_phi m) (phi m)) |
                  m <- [l .. r] ]

step = 100
t n = test_list ("test " ++ show l ++ " ~ " ++ show r) $ test_range l r
      where r = n * step
            l = r - step + 1

main = mapM_ runTestTT $ map t [1 .. 20]
