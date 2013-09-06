{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Crypto.Types (BitLength)
import Data.Digest.Pure.MD5
import Data.Serialize (encode)
import Data.Tagged (Tagged(unTagged))
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Network (withSocketsDo)
import Pipes
import Pipes.Lift (evalStateP)
import qualified Pipes.Prelude as P
import Pipes.Parse
import Pipes.Safe
import Prelude hiding (readFile, take)
import System.Exit
import Common (search)

take
    :: (Monad m, Integral a)
    => a
    -> Pipe
        BS.ByteString BS.ByteString (StateT (Producer BS.ByteString m r) m) ()
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
    => Int
    -> Producer BS.ByteString m ()
    -> Producer BS.ByteString m ()
splitN n p = evalStateP p $ do
    bs <- lift $
        P.fold (\x b -> b:x) [] (BS.concat . reverse) (input >-> take n)
    yield bs

-- FIXME: This drops the last block of the stream (< 512 bytes)
foldMD5 :: (Monad m) => Producer BS.ByteString m () -> m BS.ByteString
foldMD5 p =
    P.fold
        md5Update
        md5InitialContext
        (\c -> encode $ md5Finalize c BS.empty)
        (splitN (unTagged (blockLength :: Tagged MD5Digest BitLength)) p)

expect :: FilePath -> Double -> IO BS.ByteString
expect file rmsd = do
    txt <- TIO.readFile file
    runSafeT $ foldMD5 $
            search "suns.degradolab.org" rmsd 10 0 txt
        >-> P.map (\(_, _, pdb) -> TE.encodeUtf8 pdb)

parameters :: [(FilePath, Double, BS.ByteString)]
parameters =
    [ ("figure2/search1.pdb"     , 0.2,
           "\SUB\228s\150\222{T}\182\239\168\158\249p'+"          )
    , ("figure2/search2.pdb"     , 0.2,
        "\218\230\&2@\173&+\136\146\152\145\162\165SzC"           )
    , ("figure2/search2.pdb"     , 0.3,
        "'5+q\248\"\207.\243\221\204[\ESC\138\188\161"            )
    , ("figure3and4B/search1.pdb", 0.1,
        "\251\193\GS\128Oz\224X\197>\200\223\188*\190n"           )
    , ("figure3and4B/search2.pdb", 0.2,
        "*-\190=y:\255\209\157\239Y\229\195`+\190"                )
    , ("figure3and4B/search3.pdb", 0.2,
        "\158\151\203\252\DC3\EOT~/\tt\247\152\ESC\NAK\vN"        )
    , ("figure3and4B/search4.pdb", 0.5,
        "\249\236=*\130\167\155\n./3$mn\148}"                     )
    , ("figure3and4B/search5.pdb", 0.5,
        "\167\192\241*\178\138\134\246\226\234\139\n:\129\&1\133" )
    , ("figure3and4B/search6.pdb", 0.5,
        "P-%\GS\216G\249\184\133\NUL$j\EOT\219*j"                 )
    , ("figure3and4B/search7.pdb", 0.5,
        "\221Q\n\148\159\135\SOw\\\174k`\223J\131\191"            )
    , ("figure3and4B/search8.pdb", 0.6,
        "\199\216\253B\168\DC1\166\254|\195\147\178\181X\148\SUB" )
    , ("figure4A/search1.pdb"    , 0.7,
        "\163\148\147\234&\134\253x\DC2T\142\134\134\157\SO\160"  )
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
    pass <- P.and tests
    if pass
        then do
            putStrLn "All tests passed"
            exitSuccess
        else do
            putStrLn "Test failed"
            exitFailure
