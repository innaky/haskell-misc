palindromes :: String -> String
palindromes = unlines . map f . lines
  where isPalindrome xs = xs == reverse xs
        f xs = if isPalindrome xs then "palindrome" else "not a palindrome"

main = interact palindromes
