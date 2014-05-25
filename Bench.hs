import Control.Monad.Trans.Free
import Data.IORef
import Pipes
-- import System.CPUTime
import Data.Time.Clock

import Common (Step(..), search, parameters)

main = do
    let params =
            map (\(rmsd, filePath, _) -> (rmsd, 100, 0, filePath)) parameters
    search "127.0.0.1" params $ \ps -> do
        t0   <- getCurrentTime
        tRef <- newIORef t0
        iterT (step tRef) ps
  where
    step tRef (Step filePath p) = do
      putStrLn ("Testing: " ++ filePath)
      m <- runEffect (for p discard)
      t1 <- readIORef tRef
      t2 <- getCurrentTime
      writeIORef tRef t2
      let diff = realToFrac (t2 `diffUTCTime` t1) * 1000 :: Double
      putStrLn ("Time: " ++ show diff)
      m
