import Control.Monad
import Control.Monad.Random
import System.Random

import qualified Data.Map as M
import qualified Data.Set as S


voc = ["import", "class", "def", "\n", " ", "\t",
	"Manager", "Web", "Server", "Client", "View", "Model",
	"for",
	"dict", "list",
	"[", "]", ":", "(", ")"]

randomCode :: RandomGen g => Rand g String
randomCode = do
	n <- getRandomR (1,50)
	liftM concat $ replicateM n $ do
		ix <- getRandomR (0, length voc - 1)
		return $ voc !! ix

--randomCode :: RandomGen g => g -> (String, g)
--randomCode gen
--	|z < 0.5 = 
--	where
--		(z, gen') = randomR (0 :: Double, 1) gen'
--		(ix, gen'') = randomR (0, length voc - 1) gen'
--		w = voc !! ix


learn code = return ()

	

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
	

main = do
	c <- readFile "python-xyx/ylex.py"
	let syms = splitter "" c ++ ["_____EOF_____"]
	let s_syms = S.fromList syms
	{-
	print (length syms)
	print (S.size s_syms)
	print $ count2Gram syms
	-}
	let freq2 = count2Gram syms

	putStrLn . concat =<< evalRandIO (genMC1 freq2)

--	mapM_ print $ splitter "" c

	-- putStrLn =<< evalRandIO randomCode
