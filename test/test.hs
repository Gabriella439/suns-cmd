{-# LANGUAGE TypeFamilies #-}

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BLI
import Crypto.Types (BitLength)
import Data.Digest.Pure.MD5
import Data.Serialize (encode)
import Data.Tagged (Tagged(unTagged))
import Pipes
import Pipes.Lift (evalStateP)
import qualified Pipes.Prelude as P
import Pipes.Parse
import Pipes.Safe
import Pipes.Safe.Prelude (withFile)
import qualified System.IO as IO
import Prelude hiding (readFile, take)
import System.Directory (readable, getPermissions, doesDirectoryExist)
import System.FilePath
import System.Posix (openDirStream, readDirStream, closeDirStream)

fromHandle :: (MonadIO m) => IO.Handle -> Producer' BS.ByteString m ()
fromHandle = hGetSome BLI.defaultChunkSize

hGetSome :: (MonadIO m) => Int -> IO.Handle -> Producer' BS.ByteString m ()
hGetSome size h = go where
    go = do
        eof <- liftIO (IO.hIsEOF h)
        if eof
            then return ()
            else do
                bs <- liftIO (BS.hGetSome h size)
                yield bs
                go

readFile
    :: (MonadSafe m, Base m ~ IO) => FilePath -> Producer' BS.ByteString m ()
readFile file = withFile file IO.ReadMode fromHandle

childOf :: FilePath -> ListT (SafeT IO) FilePath
childOf path = Select $ do
    canRead <- lift $ lift $ fmap readable $ getPermissions path
    when canRead $ bracket (openDirStream path) closeDirStream $ \dirp -> do
        let loop = do
                file <- lift $ lift $ readDirStream dirp
                case file of
                    [] -> return ()
                    _  -> do
                        when (file /= "." && file /= "..") $
                            yield (path </> file)
                        loop
        loop

foldMD5 :: (Monad m) => Producer BS.ByteString m () -> m BS.ByteString
foldMD5 =
    P.fold md5Update md5InitialContext (\c -> encode $ md5Finalize c BS.empty)

take
    :: (Monad m, Integral a)
    => a
    -> Pipe BS.ByteString BS.ByteString (StateT (Producer BS.ByteString m r) m) ()
take n0 = go n0 where
    go n
        | n <= 0 = return ()
        | otherwise = do
            bs <- await
            let len = fromIntegral $ BS.length bs
            if (len > n)
                then do
                    let (prefix, suffix) = BS.splitAt (fromIntegral n) bs
                    yield prefix
                    lift (unDraw suffix)
                else do
                    yield bs
                    go (n - len)

splitN
    :: (Monad m)
    => Int -> Producer BS.ByteString m () -> Producer BS.ByteString m ()
splitN n p = evalStateP p $ do
    bs <- lift $
        P.fold (\x b -> b:x) [] (BS.concat . reverse) (input >-> take n)
    yield bs

md5Files :: FilePath -> IO BS.ByteString
md5Files dir =
      runSafeT
    $ foldMD5
    $ splitN (unTagged (blockLength :: Tagged MD5Digest BitLength))
    $ for (every (childOf dir)) readFile
