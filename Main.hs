import Control.Monad
import Control.Monad.Random
import System.Random


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

main = do
	putStrLn =<< evalRandIO randomCode
