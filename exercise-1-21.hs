
smallest_divisor n = find_divisor n 2
    where divides a b = b `mod` a == 0
          square x = x * x
          find_divisor n test | square test > n = n
                              | divides test n = test
                              | otherwise = find_divisor n (test + 1)

print_sd n = do
  putStr $ "smallest_divisor " ++ (show n) ++ " = "
  putStrLn $ show (smallest_divisor n)

main = do
    print_sd 199
    print_sd 1999
    print_sd 19999
