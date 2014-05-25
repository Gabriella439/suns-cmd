{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Crypto.Types (BitLength)
import Data.Digest.Pure.MD5
import Data.Monoid ((<>))
import Data.Serialize (encode)
import Data.Tagged (Tagged(unTagged))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network (withSocketsDo)
import Pipes
import Pipes.ByteString (chunksOf')
import Pipes.Group
import qualified Pipes.Prelude as Pipes
import System.Exit (exitFailure, exitSuccess)
import Common (search, parameters, Step(..))

foldMD5
    :: Monad m
    => FreeT (Step (a, b, Text) m) m r -> Producer BS.ByteString m r
foldMD5 ps =
    folds
        md5Update
        md5InitialContext
        (\c -> encode $ md5Finalize c BS.empty)
        (maps
            (\(Step _ p) ->
                let p' = p >-> Pipes.map (\(_, _, txt) -> TE.encodeUtf8 txt)
                in  chunksOf' chunkSize p' >-> Pipes.map pad)
            ps )
  where
    chunkSize = unTagged (blockLength :: Tagged MD5Digest BitLength)

    pad bs = bs <> BS.replicate (chunkSize - BS.length bs) 0

test :: (BS.ByteString, (FilePath, BS.ByteString)) -> IO Bool
test (result,  (filePath, expectation)) = do
    putStrLn $ "Testing: " ++ filePath
    putStrLn $ "Expected: " ++ show expectation
    putStrLn $ "Found   : " ++ show result
    return (result == expectation)

main :: IO ()
main = withSocketsDo $ do
    let params' =
            map (\(rmsd, filePath, _) -> (rmsd, 10, 0, filePath)) parameters
        expected' =
            map (\(_, filePath, expect) -> (filePath, expect)) parameters
    search "suns.degradolab.org" params' $ \ps -> do
        let tests = Pipes.zip (foldMD5 ps) (each expected') >-> Pipes.mapM test
        pass <- Pipes.and tests
        if pass
            then do
                putStrLn "All tests passed"
                exitSuccess
            else do
                putStrLn "Test failed"
                exitFailure
