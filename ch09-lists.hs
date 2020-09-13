-- 2. filter letters

hello :: () -> String
hello () = [ x | x <- "HbEfLrLx0", is Upper x ]

  -- 3. Capitalize

  capitalize :: String -> string
  capitalize [] = []
  capitalize (x:xs) = (toUpper x) : xs
