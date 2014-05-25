{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>), (<.>))
import qualified Filesystem as F
import Network (withSocketsDo)
import Options.Applicative
import Pipes
import Common (search)

data Options = Options
    { _hostName  :: String
    , _rmsd      :: Double
    , _numStruct :: Int
    , _seed      :: Int
    , _directory :: F.FilePath
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
        , metavar "FILEPATH"
        , help "Results directory"
        , reader (Right . F.decodeString)
        ] )

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> options) $ mconcat
    [ fullDesc
    , header "suns-cmd: The Suns search command line client"
    , progDesc "Send search requests and store results as PDB files"
    , footer "Report bugs to suns-search@googlegroups.com"
    ]

main :: IO ()
main = withSocketsDo $ do
    Options hostName rmsd numStruct seed directory <- execParser parserInfo
    query <- TIO.getContents
    search hostName rmsd numStruct seed query $ \results -> runEffect $ do
        for results $ \(pdbID, n, pdbStr) -> liftIO $ do
            let fileName =
                        directory
                    </> F.decodeString (T.unpack pdbID ++ "_" ++ show n)
                    <.> "pdb"
            F.writeTextFile fileName pdbStr
