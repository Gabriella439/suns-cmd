{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes #-}

module Common where

import Control.Error (errLn)
import Control.Monad (void)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.V4 (nextRandom)
import Network.AMQP
import Network.AMQP.Types (FieldTable(FieldTable))
import Pipes
import Pipes.Safe
import Pipes.Concurrent

requestExchange :: T.Text
requestExchange = "suns-exchange-requests"

responseExchange :: T.Text
responseExchange = "suns-exchange-responses"

search
    :: (MonadSafe m, Base m ~ IO)
    => String
    -> Double
    -> Int
    -> Int
    -> T.Text
    -> Producer' (T.Text, Integer, T.Text) m ()
search hostName rmsd numStruct seed pdb = do
    (output, input, seal) <- liftIO $ spawn' Unbounded
    bracket
        (openConnection hostName "suns-vhost" "suns-client" "suns-client")
        closeConnection
        $ \connection -> do
            channel <- liftIO $ openChannel connection
            liftIO $ declareExchange channel $ ExchangeOpts
                responseExchange  -- exchangeName
                "direct"          -- exchangeType
                True              -- exchangePassive
                True              -- exchangeDurable
                False             -- exchangeAutoDelete
                False             -- exchangeInternal
                (FieldTable M.empty)  -- queueHeaders
            liftIO $ declareExchange channel $ ExchangeOpts
                requestExchange  -- exchangeName
                "direct"         -- exchangeType
                True             -- exchangePassive
                True             -- exchangeDurable
                False            -- exchangeAutoDelete
                False            -- exchangeInternal
                (FieldTable M.empty)  -- queueHeaders
            (qName, _, _) <- liftIO $ declareQueue channel $ QueueOpts
                ""                    -- queueName
                False                 -- queuePassive
                False                 -- queueDurable
                True                  -- queueExclusive
                True                  -- queueAutoDelete
                (FieldTable M.empty)  -- queueHeaders
            liftIO $ bindQueue channel qName responseExchange qName
            uID  <- liftIO nextRandom
            used <- liftIO $ newIORef M.empty
            let uIDtxt = T.pack (show uID)
            bracket
                (consumeMsgs
                     channel
                     qName
                     Ack
                     (callback uIDtxt used output (atomically seal)) )
                (cancelConsumer channel)
                $ \_ -> do
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
                    liftIO $ publishMsg channel requestExchange "1.0.0" msg
                    fromInput input

callback
    :: T.Text
    -> IORef (M.Map B.ByteString Integer)
    -> Output (T.Text, Integer, T.Text)
    -> IO ()
    -> (Message, Envelope)
    -> IO ()
callback uIDtxt used output done (message, _envelope) =
    case (msgCorrelationID message) of
        Nothing                        -> return ()
        Just corrID | corrID /= uIDtxt -> return ()
                    | otherwise -> do
            let body = B.concat (BL.toChunks (msgBody message))
            case (B.uncons body) of
                Nothing -> do
                     errLn emptyErr
                     done
                Just (tag, rest) -> do
                    case tag of
                        '0' -> done
                        '1' -> do
                            let (pdbID, match) = B.splitAt 4 rest
                            m <- readIORef used
                            let n = maybe 0 id (M.lookup pdbID m)
                            modifyIORef used (M.insertWith (+) pdbID 1)
                            void $ atomically $ send output
                                ( TE.decodeUtf8 pdbID
                                , n
                                , TE.decodeUtf8 match
                                )
                        '2' -> do
                             errLn "Server time limit exceeded"
                             done
                        '3' -> do
                             errLn $ "Server Error: " ++ B.unpack rest
                             done
                        _   -> do
                             errLn $ invalidErr body
                             done

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
