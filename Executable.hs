{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

import Control.Monad.Trans.Free (iterT)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>), (<.>))
import qualified Filesystem as F
import Network (withSocketsDo)
import Options.Applicative
import Pipes
import Common (search, Step(..))

data Options = Options
    { _hostName   :: String
    , _rmsd       :: Double
    , _numStruct  :: Int
    , _seed       :: Int
    , _directory  :: F.FilePath
    , _inputFiles :: [F.FilePath]
    }

options :: Parser Options
options = Options
    <$> strOption (mconcat
	[ long "hostname"
	, value "suns.degradolab.org"
	, showDefaultWith id
	, metavar "STRING"
	, help "Search engine address"
	] )
    <*> option (mconcat
        [ short 'r'
        , long "rmsd"
        , value 1.0
        , showDefault
        , metavar "DOUBLE"
        , help "RMSD cutoff"
        ] )
    <*> option (mconcat
        [ short 'n'
        , long "num"
        , value 100
        , showDefault
        , metavar "INT"
        , help "Number of results"
        ] )
    <*> option (mconcat
        [ short 's'
        , long "seed"
        , value 0
        , showDefault
        , metavar "INT"
        , help "Randomization seed"
        ] )
    <*> nullOption (mconcat
        [ short 'd'
        , long "directory"
        , value "."
        , showDefaultWith F.encodeString
        , metavar "DIRECTORY"
        , help "Results directory"
        , reader (Right . F.decodeString)
        ] )
    <*> many (argument (Just . F.decodeString) (mconcat
        [ metavar "PDBFILE"
        , help "Input file"
        ] ))

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> options) $ mconcat
    [ fullDesc
    , header "suns-cmd: The Suns search command line client"
    , progDesc "Send search requests and store results as PDB files"
    , footer "Report bugs to suns-search@googlegroups.com"
    ]

main :: IO ()
main = withSocketsDo $ do
    Options hostName rmsd numStruct seed directory inputFiles <-
        execParser parserInfo

    let step (Step filePath p) = do
            let filePath' = F.decodeString filePath
                base      = F.basename filePath'
                dirName = directory </> base

            F.createDirectory True dirName
            m <- runEffect $ for p $ \(pdbID, n, pdbStr) -> liftIO $ do
                let fileName =
                            dirName
                        </> F.decodeString (T.unpack pdbID ++ "_" ++ show n)
                        <.> "pdb"

                F.writeTextFile fileName pdbStr
            m

        params =
            map
                (\inputFile ->
                    (rmsd, numStruct, seed, F.encodeString inputFile) )
                inputFiles

    search hostName params (iterT step)
