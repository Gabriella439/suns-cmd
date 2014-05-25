{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Crypto.Types (BitLength)
import Data.Digest.Pure.MD5
import Data.Monoid ((<>))
import Data.Serialize (encode)
import Data.Tagged (Tagged(unTagged))
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Network (withSocketsDo)
import Pipes
import Pipes.ByteString (chunksOf')
import qualified Pipes.Prelude as Pipes
import System.Exit
import Common (search)

-- FIXME: This drops the last block of the stream (< 512 bytes)
foldMD5 :: (Monad m) => Producer BS.ByteString m () -> m BS.ByteString
foldMD5 p =
    Pipes.fold
        md5Update
        md5InitialContext
        (\c -> encode $ md5Finalize c BS.empty)
        (chunksOf' chunkSize p >-> Pipes.map pad)
  where
    chunkSize = unTagged (blockLength :: Tagged MD5Digest BitLength)

    pad bs = bs <> BS.replicate (chunkSize - BS.length bs) 0

expect :: FilePath -> Double -> IO BS.ByteString
expect file rmsd = do
    txt <- TIO.readFile file
    search "suns.degradolab.org" rmsd 10 0 txt $ \results -> do
        foldMD5 $ results >-> Pipes.map (\(_, _, pdb) -> TE.encodeUtf8 pdb)

parameters :: [(FilePath, Double, BS.ByteString)]
parameters =
    [ ("figure2/search1.pdb"     , 0.2,
           "\157k'\201\133yo\166\139\211\139*U\n\180K")
    , ("figure2/search2.pdb"     , 0.2,
        "\242\140\DC2-\238\186\196H\155q\SO\201\238p\221\201")
    , ("figure2/search2.pdb"     , 0.3,
        "0\SYNQ\255\152\206\234\229\224\242\US\235\132f\188|")
    , ("figure3and4B/search1.pdb", 0.1,
        "\183\246N\181\169}p\US\241s\169K\209\DLE\214r")
    , ("figure3and4B/search2.pdb", 0.2,
        "~\248U{\170\132\t^\205\221%(\182\137\197{")
    , ("figure3and4B/search3.pdb", 0.2,
        "P\225\130|H6\199\SYN\247DJ\ENQ\SO\130^\EM")
    , ("figure3and4B/search4.pdb", 0.5,
        "\158\243\168\132\167\228D\233Jn\201\174\247q\153\145")
    , ("figure3and4B/search5.pdb", 0.5,
        "\tUxD\244\205\158\&2`\182oO\130\200T\219")
    , ("figure3and4B/search6.pdb", 0.5,
        "VL\171^\SI4G\241\a\r\149\141\208\226:\255")
    , ("figure3and4B/search7.pdb", 0.5,
        "T\EM\210v\226\t\n<\212%[\223\226\208KR")
    , ("figure3and4B/search8.pdb", 0.6,
        "&\a-\222\244\187\ETB\232lH\238\&0\152\205A\199")
    , ("figure4A/search1.pdb"    , 0.7,
        "\137F\133\234U\211\202\\\153\a\213\225#\211\152k")
    ]

tests :: Producer Bool IO ()
tests = forM_ parameters $ \(relPath, rmsd, expectation) -> do
    liftIO $ putStrLn $ "Testing: " ++ relPath
    bs <- liftIO $ expect ("test/" ++ relPath) rmsd
    liftIO $ putStrLn $ "Expected: " ++ show expectation
    liftIO $ putStrLn $ "Found   : " ++ show bs
    yield (bs == expectation)

main :: IO ()
main = withSocketsDo $ do
    pass <- Pipes.and tests
    if pass
        then do
            putStrLn "All tests passed"
            exitSuccess
        else do
            putStrLn "Test failed"
            exitFailure
