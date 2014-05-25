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
import Common (search, Step(..))

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

-- Saved from a known good run where the results have been manually inspected
params :: [(Double, FilePath, BS.ByteString)]
params = map (\(x, filePath, y) -> (x, "test/" ++ filePath, y))
    [ (0.2, "figure2/search1.pdb",
        "\157k'\201\133yo\166\139\211\139*U\n\180K"            )
    , (0.2, "figure2/search2.pdb",
        "\242\140\DC2-\238\186\196H\155q\SO\201\238p\221\201"  )
    , (0.3, "figure2/search2.pdb",
        "0\SYNQ\255\152\206\234\229\224\242\US\235\132f\188|"  )
    , (0.1, "figure3and4B/search1.pdb",
        "\183\246N\181\169}p\US\241s\169K\209\DLE\214r"        )
    , (0.2, "figure3and4B/search2.pdb",
        "~\248U{\170\132\t^\205\221%(\182\137\197{"            )
    , (0.2, "figure3and4B/search3.pdb",
        "P\225\130|H6\199\SYN\247DJ\ENQ\SO\130^\EM"            )
    , (0.5, "figure3and4B/search4.pdb",
        "\158\243\168\132\167\228D\233Jn\201\174\247q\153\145" )
    , (0.5, "figure3and4B/search5.pdb",
        "\tUxD\244\205\158\&2`\182oO\130\200T\219"             )
    , (0.5, "figure3and4B/search6.pdb",
        "VL\171^\SI4G\241\a\r\149\141\208\226:\255"            )
    , (0.5, "figure3and4B/search7.pdb",
        "T\EM\210v\226\t\n<\212%[\223\226\208KR"               )
    , (0.6, "figure3and4B/search8.pdb",
        "&\a-\222\244\187\ETB\232lH\238\&0\152\205A\199"       )
    , (0.7, "figure4A/search1.pdb",
        "\137F\133\234U\211\202\\\153\a\213\225#\211\152k"     )
    ]

test :: (BS.ByteString, (FilePath, BS.ByteString)) -> IO Bool
test (result,  (filePath, expectation)) = do
    putStrLn $ "Testing: " ++ filePath
    putStrLn $ "Expected: " ++ show expectation
    putStrLn $ "Found   : " ++ show result
    return (result == expectation)

main :: IO ()
main = withSocketsDo $ do
    let params' = map (\(rmsd, filePath, _) -> (rmsd, 10, 0, filePath)) params
        expected' = map (\(_, filePath, expect) -> (filePath, expect)) params
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
