import Control.Monad
import Control.Monad.Random
import System.Directory
import System.FilePath
import System.Random
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S


splitter :: String -> String -> [String]
splitter buf "" = wrapBuffer buf
splitter buf (x:xs)
	|x `elem` delims = wrapBuffer buf ++ ([x] : splitter "" xs)
	|otherwise = splitter (buf ++ [x]) xs
	where
		delims = symbols ++ spaces
		symbols = "!\"#$%&'=~|-^\\,./?()[]<>{}"
		spaces = " \t\n"

wrapBuffer "" = []
wrapBuffer buf = [buf]


count2Gram :: Ord a => [a] -> M.Map (a, a) Int
count2Gram xs = foldr (\gram m -> M.insertWith (+) gram 1 m)
	M.empty $ grams xs
	where
		grams (x0:x1:xs) = (x0, x1) : grams (x1:xs)
		grams _ = []

-- | Generate rank-1 markov sequence from 2-gram frequency table.
genMC1 :: RandomGen g => M.Map (String, String) Int -> Rand g [String]
genMC1 freq2 = sampleAll "import"
	where
		sampleAll prev = do
			m_next <- sampleNext prev
			case m_next of
				Nothing -> return [prev]
				Just next -> return (prev :) `ap` (sampleAll next)

		sampleNext :: RandomGen g => String -> Rand g (Maybe String)
		sampleNext prev = case M.lookup prev shiftProbTable of
			Nothing -> return Nothing
			Just table -> return (Just . sampleRussian table) `ap`
				(getRandomR (0.0, 1.0))

		sampleRussian :: [(Double, a)] -> Double -> a
		sampleRussian [(_, x)] _ = x
		sampleRussian ((p, x):elems) z
			|z < p = x
			|otherwise = sampleRussian elems (z - p)

		shiftProbTable :: M.Map String [(Double, String)]
		shiftProbTable = foldr
			(\((from, to), count) m -> M.insertWith (++) from
				[(fromIntegral count / fromIntegral (targetCount M.! from), to)] m)
			M.empty $ M.assocs freq2

		targetCount :: M.Map String Int
		targetCount = foldr
			(\((from, to), count) m -> M.insertWith (+) from count m)
			M.empty $ M.assocs freq2

data CodeMonkey = CodeMonkey {
	freq2 :: M.Map (String, String) Int
	}

instance Show CodeMonkey where
	show (CodeMonkey freq2) =
		printf "2-gram table size: %d" (M.size freq2)

newbieCodeMonkey :: CodeMonkey
newbieCodeMonkey = CodeMonkey M.empty

learn :: FilePath -> CodeMonkey -> IO CodeMonkey
learn path cm = do
	code <- readFile path
	writeFile "/dev/null" (show $ length code)

	let syms = splitter "" code ++ ["_____EOF_____"]
	let delta = count2Gram syms
	return $ (length code) `seq` cm{freq2 = M.unionWith (+) delta $ freq2 cm}

getCodePathsIn :: FilePath -> IO [FilePath]
getCodePathsIn dir_path = do
	es <- getDirectoryContents dir_path
	return $ map (dir_path </>) (filter (\x -> takeExtensions x == ".py") es)

main = do
	code_paths <- return (take 15) `ap` getCodePathsIn "python-xyx"
	cm' <- foldM (flip learn) newbieCodeMonkey code_paths
	print cm'
	putStrLn . concat =<< evalRandIO (genMC1 $ freq2 cm')
