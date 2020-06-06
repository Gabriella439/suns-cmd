import Control.Monad.Trans.Free hiding (Pure)
import Data.Maybe (fromMaybe)
import Data.IORef
import Options.Applicative
import Pipes
import Pipes.Internal
import Data.Time.Clock

import Common (Step(..), search, parameters)

options :: Parser (String, Maybe Double, Int)
options = (,,)
    <$> strOption (mconcat
        [ long "hostname"
        , value "suns.degradolab.org"
        , showDefaultWith id
        , metavar "STRING"
        , help "Search engine address"
        ] )
    <*> optional (option auto (mconcat
        [ short 'r'
        , long "rmsd"
        , value 1.0
        , showDefault
        , metavar "DOUBLE"
        , help "Override RMSD cutoff"
        ] ))
    <*> option auto (mconcat
        [ short 'n'
        , long "num"
        , value 100
        , showDefault
        , metavar "INT"
        , help "Number of results"
        ] )

parserInfo :: ParserInfo (String, Maybe Double, Int)
parserInfo = info (helper <*> options) $ mconcat
    [ fullDesc
    , header "suns-cmd: The Suns search command line client"
    , progDesc "Send search requests and store results as PDB files"
    , footer "Report bugs to suns-search@googlegroups.com"
    ]

main :: IO ()
main = do
    (hostName, rmsd', numResults) <- execParser parserInfo

    putStrLn ("Host name: " ++ hostName)
    putStrLn ("RMSD Override: " ++ show rmsd')
    putStrLn ("Max results: " ++ show numResults)

    let params =
            map (\(rmsd, filePath, _) ->
                    (fromMaybe rmsd rmsd', numResults, 0, filePath) )
                parameters
    search hostName params $ \ps -> do
        t0   <- getCurrentTime
        tRef <- newIORef t0
        iterT (step tRef) ps
  where
    step tRef (Step filePath p) = do
      putStrLn ("Testing: " ++ filePath)
      (n, m) <- _length p
      t1 <- readIORef tRef
      t2 <- getCurrentTime
      writeIORef tRef t2
      let diff = realToFrac (t2 `diffUTCTime` t1) * 1000 :: Double
      putStrLn ("Number of results: " ++ show n)
      putStrLn ("Time: " ++ show diff)
      m

_length :: Monad m => Producer a m r -> m (Int, r)
_length p = go p 0
  where
    go (Pure      r) n = return (n, r)
    go (Respond _ k) n = go (k ()) $! n + 1
    go (Request _ k) n = go (k ()) n
    go (M         m) n = m >>= \p' -> go p' n
