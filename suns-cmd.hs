{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Error (runScript, fmapL, (!?), (??), scriptIO, tryRight, errLn)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import Network (withSocketsDo)
import Network.AMQP
import Options.Applicative
import qualified Filesystem.Path.CurrentOS as F

data Options = Options
    { hostName  :: String
    , rmsd      :: Double
    , numStruct :: Int
    , seed      :: Int
    , directory :: F.FilePath
    }

options :: Parser Options
options = Options
    <$> strOption (mconcat
	[ short 'h'
        , long "hostname"
	, value "suns.degradolab.org"
	, showDefaultWith id
	, metavar "STRING"
	, help "Suns search engine to query"
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
    , footer "Report bugs to suns.maintainers@gmail.com"
    ]

requestExchange :: T.Text
requestExchange = "suns-exchange-requests"

responseExchange :: T.Text
responseExchange = "suns-exchange-responses"

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
                ""     -- queueName
                False  -- queuePassive
                False  -- queueDurable
                True   -- queueExclusive
                True   -- queueAutoDelete
            scriptIO $ bindQueue channel qName responseExchange qName
            pdb  <- scriptIO B.getContents
            uID  <- scriptIO nextRandom
            mvar <- scriptIO newEmptyMVar
            used <- scriptIO $ newIORef M.empty
            let uIDtxt = T.pack (show uID)
            scriptIO $ bracket
                (consumeMsgs channel qName Ack (callback uIDtxt mvar used))
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
    -> (Message, Envelope)
    -> IO ()
callback uIDtxt mvar used (message, envelope) = runScript $
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
                    let fileName = B.unpack pdbID ++ "_" ++ show n ++ ".pdb"
                    scriptIO $ B.writeFile fileName match
                '2' -> scriptIO $ errLn "Server time limit exceeded"
                '3' -> scriptIO $ errLn $ "Server Error: " ++ B.unpack rest
                _   -> scriptIO $ errLn $ invalidErr body

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

