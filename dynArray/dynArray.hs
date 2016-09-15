import Data.Bits
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as M

data Sequence = Sequence !(M.IntMap Int) !Int

type SequenceList = M.IntMap Sequence
type SequenceSize = Int
type LastAnswer   = Int
type Output       = [String]
type Commands     = [[Int]]

main :: IO ()
main = do
    [n,q] <- map read . words <$> getLine
    let seqList = M.fromList $ zip [0..n] (map (const (Sequence M.empty 0)) [0..n])
    arr_temp <- take q . map (map (read :: String -> Int ) . words) . lines <$> getContents

    putStrLn . unlines . reverse $ execute seqList n arr_temp 0 []

replaceNth :: SequenceList -> Int -> Sequence -> SequenceList
replaceNth seqList i newList = M.adjust (const newList) i seqList

getIndex :: Int -> Int -> Int -> Int
getIndex i lastAns = mod (xor i lastAns)

execute :: SequenceList -> SequenceSize -> Commands -> LastAnswer -> Output -> Output
-- execute seqList seqSize ([1,i,y]:xs) lastAns output = Result newSeqList seqSize lastAns output
execute seqList seqSize ([1,i,y]:xs) lastAns output = execute newSeqList seqSize xs lastAns output
                    where
                        listIndex = getIndex i lastAns seqSize
                        (Sequence m sz) = seqList M.! listIndex
                        -- newList = V.snoc (seqList M.! listIndex) y
                        newList = Sequence (M.insert sz y m) (sz+1)
                        newSeqList = replaceNth seqList listIndex newList

-- execute seqList seqSize ([2,i,y]:xs) lastAns output = Result seqList seqSize newLastAns (output ++ [ show newLastAns ])
execute seqList seqSize ([2,i,y]:xs) lastAns output =  execute seqList seqSize xs newLastAns ( (show newLastAns): output )
                    where
                        listIndex = getIndex i lastAns seqSize
                        (Sequence m sz) = seqList M.! listIndex
                        index = mod y sz
                        newLastAns = m M.! index

-- execute seqList seqSize [] lastAns output = Result seqList seqSize lastAns output
execute _ _ [] _ output = output
execute _ _ _ _ _ = error "Shouldn't reach this line"
