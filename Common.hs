{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Concurrent.Async (withAsync, wait)
import Control.Error (errLn)
import Control.Exception (bracket)
import Control.Monad (void, forM_)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.V4 (nextRandom)
import qualified Data.Text.IO as TIO
import Network.AMQP
import Network.AMQP.Types (FieldTable(FieldTable))
import Pipes
import Pipes.Group
import Pipes.Concurrent

requestExchange :: T.Text
requestExchange = "suns-exchange-requests"

responseExchange :: T.Text
responseExchange = "suns-exchange-responses"

search
    :: String
    -> [(Double, Int, Int, FilePath)]
    -> (FreeT (Step (T.Text, Integer, T.Text) IO) IO () -> IO r)
    -> IO r
search hostName params k = do
    let filePaths = map (\(_, _, _, filePath) -> filePath) params
    (output, input, _seal) <- spawn' Unbounded
    bracket
        (openConnection hostName "suns-vhost" "suns-client" "suns-client")
        closeConnection
        $ \connection -> do
            channel <- openChannel connection
            declareExchange channel $ ExchangeOpts
                responseExchange  -- exchangeName
                "direct"          -- exchangeType
                True              -- exchangePassive
                True              -- exchangeDurable
                False             -- exchangeAutoDelete
                False             -- exchangeInternal
                (FieldTable M.empty)  -- queueHeaders
            declareExchange channel $ ExchangeOpts
                requestExchange  -- exchangeName
                "direct"         -- exchangeType
                True             -- exchangePassive
                True             -- exchangeDurable
                False            -- exchangeAutoDelete
                False            -- exchangeInternal
                (FieldTable M.empty)  -- queueHeaders
            (qName, _, _) <- declareQueue channel $ QueueOpts
                ""                    -- queueName
                False                 -- queuePassive
                False                 -- queueDurable
                True                  -- queueExclusive
                True                  -- queueAutoDelete
                (FieldTable M.empty)  -- queueHeaders
            bindQueue channel qName responseExchange qName
            uID  <- nextRandom
            used <- newIORef M.empty
            let uIDtxt = T.pack (show uID)
            bracket
                (consumeMsgs
                     channel
                     qName
                     Ack
                     (callback
                         uIDtxt
                         used
                         output
--                       (atomically seal)
                         (void $ atomically $ send output Nothing) ) )
                (cancelConsumer channel)
                $ \_ -> do
                    let io = k (partition filePaths (fromInput input))
                    withAsync io $ \a -> do
                        forM_ params $ \(rmsd, numStruct, seed, filePath) -> do
                            pdb <- TIO.readFile filePath
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
                            publishMsg channel requestExchange "1.0.0" msg
                        wait a

callback
    :: T.Text
    -> IORef (M.Map B.ByteString Integer)
    -> Output (Maybe (T.Text, Integer, T.Text))
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
                            void $ atomically $ send output $ Just
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

data Step a m x = Step FilePath (Producer a m x)

instance Monad m => Functor (Step a m) where
    fmap f (Step filePath producer) = Step filePath (fmap f producer)

partition
    :: Monad m => [FilePath] -> Producer (Maybe a) m () -> FreeT (Step a m) m ()
partition []                   _ = return ()
partition (filePath:filePaths) p = FreeT $ do
    x <- catMaybes p
    case x of
        Left  r -> return $ Pure r
        Right p' -> return $ Free $ Step filePath $ do
            p'' <- p'
            return (partition filePaths p'')

catMaybes
    :: Monad m
    => Producer (Maybe a) m r
    -> m (Either r (Producer a m (Producer (Maybe a) m r)))
catMaybes p0 = do
    x <- next p0
    case x of
        Left   r       -> return $ Left r
        Right (ma, p1) -> return $ Right $ go (yield ma >> p1)
  where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (ma, p') -> case ma of
                Nothing -> return p'
                Just a  -> do
                    yield a
                    go p'

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
