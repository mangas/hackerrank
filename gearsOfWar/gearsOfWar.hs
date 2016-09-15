import Control.Applicative

main :: IO ()
main = do
    n <- read <$> getLine
    xs <- map (\x -> if (rem x 2) == 0 then "Yes" else "No") . take n . map (read :: String -> Int) . lines <$> getContents
    putStrLn $ unlines xs
