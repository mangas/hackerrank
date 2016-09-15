import Control.Applicative

main :: IO ()
main = do
    [n,q] <- map (read :: String -> Int) . words <$> getLine
    xs <- words <$> getLine
    putStrLn . unwords $ (drop q xs) ++ (take q xs)
