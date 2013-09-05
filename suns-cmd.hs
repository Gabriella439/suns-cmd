{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Error (runScript, (??), scriptIO, errLn)
import Control.Exception (bracket)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Network (withSocketsDo)
import Network.AMQP
import Network.AMQP.Types (FieldTable(FieldTable))
import Options.Applicative
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path ((</>), (<.>))
import qualified Filesystem as F

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

requestExchange :: T.Text
requestExchange = "suns-exchange-requests"

responseExchange :: T.Text
responseExchange = "suns-exchange-responses"

main :: IO ()
main = withSocketsDo $ do
    Options hostName rmsd numStruct seed directory <- execParser parserInfo
    bracket
        (openConnection hostName "suns-vhost" "suns-client" "suns-client")
        closeConnection
        $ \connection -> runScript $ do
            channel <- scriptIO $ openChannel connection
            scriptIO $ declareExchange channel $ ExchangeOpts
                responseExchange  -- exchangeName
                "direct"          -- exchangeType
                True              -- exchangePassive
                True              -- exchangeDurable
                False             -- exchangeAutoDelete
                False             -- exchangeInternal
            scriptIO $ declareExchange channel $ ExchangeOpts
                requestExchange  -- exchangeName
                "direct"         -- exchangeType
                True             -- exchangePassive
                True             -- exchangeDurable
                False            -- exchangeAutoDelete
                False            -- exchangeInternal
            (qName, _, _) <- scriptIO $ declareQueue channel $ QueueOpts
                ""                    -- queueName
                False                 -- queuePassive
                False                 -- queueDurable
                True                  -- queueExclusive
                True                  -- queueAutoDelete
                (FieldTable M.empty)  -- queueHeaders
            scriptIO $ bindQueue channel qName responseExchange qName
            pdb  <- scriptIO B.getContents
            uID  <- scriptIO nextRandom
            mvar <- scriptIO newEmptyMVar
            used <- scriptIO $ newIORef M.empty
            let uIDtxt = T.pack (show uID)
            scriptIO $ bracket
                (consumeMsgs
                     channel qName Ack (callback uIDtxt mvar used directory) )
                (cancelConsumer channel)
                $ \_ -> runScript $ do
                    let msg = newMsg
                            { msgBody = encode $ object
                                [ "rmsd_cutoff"    .= rmsd
                                , "num_structures" .= numStruct
                                , "random_seed"    .= seed
                                , "atoms"          .= pdb
                                ]
                            , msgReplyTo = Just qName
                            , msgCorrelationID = Just uIDtxt
                            }
                    scriptIO $ publishMsg channel requestExchange "1.0.0" msg
                    scriptIO $ takeMVar mvar

callback
    :: T.Text
    -> MVar ()
    -> IORef (M.Map B.ByteString Int)
    -> F.FilePath
    -> (Message, Envelope)
    -> IO ()
callback uIDtxt mvar used directory (message, _envelope) = runScript $
    case (msgCorrelationID message) of
        Nothing                        -> return ()
        Just corrID | corrID /= uIDtxt -> return ()
                    | otherwise -> do
            let body = B.concat (BL.toChunks (msgBody message))
            (tag, rest) <- B.uncons body ?? emptyErr
            case tag of
                '0' -> scriptIO $ putMVar mvar ()
                '1' -> do
                    let (pdbID, match) = B.splitAt 4 rest
                    m <- scriptIO $ readIORef used
                    let n = maybe 0 id (M.lookup pdbID m)
                    scriptIO $ modifyIORef used (M.insertWith (+) pdbID 1)
                    let fileName =
                                directory
                            </> F.decodeString (B.unpack pdbID ++ "_" ++ show n)
                            <.> "pdb"
                    scriptIO $ F.writeFile fileName match
                '2' -> scriptIO $ do
                     errLn "Server time limit exceeded"
                     putMVar mvar ()
                '3' -> scriptIO $ do
                     errLn $ "Server Error: " ++ B.unpack rest
                     putMVar mvar ()
                _   -> scriptIO $ do
                     errLn $ invalidErr body
                     putMVar mvar ()

emptyErr :: String
emptyErr = "\
 \suns-cmd received an invalid empty message from the Suns server.  Contact\n\
 \the Suns server administrator and provide them with this error message to\n\
 \fix this problem\n"

invalidErr :: B.ByteString -> String
invalidErr bs = "\
 \suns-cmd received an invalid response from the Suns server.  Contact the\n\
 \Suns server administrator and provide them with this error message and the\n\
 \following data:\n"
 ++ show (B.take 256 bs)
 ++ if (B.length bs > 256) then "..." else ""
