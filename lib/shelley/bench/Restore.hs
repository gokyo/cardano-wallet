{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark measuring how long restoration takes for different wallets.
--
-- Easiest run using
-- @
--     $ export NODE_DB="node-db-testnet"
--     $ nix-build -A benchmarks.cardano-wallet.restore -o restore && ./restore/bin/restore testnet
-- @
--
-- or
-- @
--     $ ./.buildkite/bench-restore.sh shelley testnet
-- @
--
-- since it relies on lots of configuration most most easily retrieved with nix.
--
-- You can also connect to an already-running node using:
-- @
--      stack bench cardano-wallet:bench:restore
--          --ba 'mainnet -c $CONFIGURATION_DIR
--          --running-node $CARDANO_NODE_SOCKET_PATH
-- @
--
-- This makes iteration easy, but requires you to have the configuration
-- directory layout setup correctly, and to know how to start a node.

module Main where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.DB.Sqlite
    ( destroyDBLayer )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToMnemonic )
import Cardano.Wallet
    ( WalletLayer (..), WalletLog (..) )
import Cardano.Wallet.Api.Types
    ( toApiUtxoStatistics )
import Cardano.Wallet.BenchShared
    ( RestoreBenchArgs (..)
    , Time
    , argsNetworkDir
    , bench
    , execBenchWithNode
    , runBenchmarks
    )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, newDBLayer )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network
    ( FollowLog (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal (..)
    , Passphrase (..)
    , PaymentAddress
    , PersistPrivateKey
    , WalletKey
    , digest
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery, GenChange (..), IsOurs, IsOwned, KnownAddresses )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndAnyState, mkRndAnyState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap
    , SeqAnyState (..)
    , mkAddressPoolGap
    , mkSeqAnyState
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip, getState, totalUTxO )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, currentRelativeTime, neverFails )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..), mkSyncTolerance, syncProgress )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , SlotNo (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletName (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxOStatistics (..), computeUtxoStatistics, log10 )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( HasNetworkId (..), NodeVersionData, emptyGenesis, fromCardanoBlock )
import Cardano.Wallet.Shelley.Launch
    ( CardanoNodeConn, NetworkConfiguration (..), parseGenesisData )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTagFor (..), newTransactionLayer )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy, unsafeMkPercentage, unsafeRunExceptT )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Control.Tracer
    ( Tracer (..), traceWith )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.Aeson
    ( ToJSON (..), genericToJSON, (.=) )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime, getCurrentTime, utcTimeToPOSIXSeconds )
import Fmt
    ( Buildable
    , blockListF'
    , build
    , genericF
    , nameF
    , pretty
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Numeric
    ( showFFloat )
import Say
    ( sayErr )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hFlush, withFile )
import UnliftIO.Concurrent
    ( forkIO, threadDelay )
import UnliftIO.Exception
    ( bracket, evaluate, throwString )
import UnliftIO.Temporary
    ( withSystemTempFile )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = execBenchWithNode argsNetworkConfig cardanoRestoreBench

{-------------------------------------------------------------------------------
                                Shelley benchmarks
-------------------------------------------------------------------------------}

argsNetworkConfig :: RestoreBenchArgs -> NetworkConfiguration
argsNetworkConfig args = case argNetworkName args of
    "mainnet" ->
        MainnetConfig
    _ ->
        TestnetConfig (argsNetworkDir args </> "genesis-byron.json")

-- | Run all available benchmarks.
cardanoRestoreBench :: Trace IO Text -> NetworkConfiguration -> CardanoNodeConn -> IO ()
cardanoRestoreBench tr c socketFile = do
    (SomeNetworkDiscriminant networkProxy, np, vData, _b)
        <- unsafeRunExceptT $ parseGenesisData c

    let network = networkDescription networkProxy
    sayErr $ "Network: " <> network

    prepareNode (trMessageText tr) networkProxy socketFile np vData

    let benchRestoreMultipleWallets nWallets target = do
            let targetStr = T.pack $ showFFloat Nothing (fromRational @Double $ getPercentage target) ""
            bench_restoration @_ @ShelleyKey
                networkProxy
                (trMessageText tr)
                socketFile
                np
                vData
                (""+|nWallets|+"-wallets-to-"+|targetStr|+"")
                (map (\i -> walletSeq
                    ("w"+|i|+"") $ mkSeqAnyState' (Proxy @0) networkProxy)
                    [1..nWallets :: Int])
                False -- Don't write progress to .timelog file(s)
                target
                benchmarksSeq

    let benchRestoreRndWithOwnership p = do
            let benchname = showPercentFromPermille p <> "-percent-rnd"
            bench_restoration
                networkProxy
                (trMessageText tr)
                socketFile
                np
                vData
                benchname
                [walletRnd benchname
                    $ mkRndAnyState' p networkProxy]
                True -- Write progress to .timelog file
                (unsafeMkPercentage 1)
                benchmarksRnd

    let benchRestoreSeqWithOwnership p = do
            let benchname = showPercentFromPermille p <> "-percent-seq"
            bench_restoration
                networkProxy
                (trMessageText tr)
                socketFile
                np
                vData
                benchname
                [walletSeq benchname
                    $ mkSeqAnyState' p networkProxy]
                True -- Write progress to .timelog file
                (unsafeMkPercentage 1)
                benchmarksSeq

    runBenchmarks
        [ -- We restore /to/ a percentage that is low enough to be fast,
          -- but high enough to give an accurate enough indication of the
          -- to-100% time.
          benchRestoreMultipleWallets 1 (unsafeMkPercentage 0.1)
        , benchRestoreMultipleWallets 10 (unsafeMkPercentage 0.01)
        , benchRestoreMultipleWallets 20 (unsafeMkPercentage 0.01)
        , benchRestoreMultipleWallets 40 (unsafeMkPercentage 0.01)
        , benchRestoreMultipleWallets 80 (unsafeMkPercentage 0.01)
        , benchRestoreMultipleWallets 100 (unsafeMkPercentage 0.01)

        , benchRestoreSeqWithOwnership (Proxy @0)
        , benchRestoreSeqWithOwnership (Proxy @1)
        , benchRestoreSeqWithOwnership (Proxy @2)
        , benchRestoreSeqWithOwnership (Proxy @4)

        , benchRestoreRndWithOwnership (Proxy @0)
        , benchRestoreRndWithOwnership (Proxy @1)
        , benchRestoreRndWithOwnership (Proxy @2)
        , benchRestoreRndWithOwnership (Proxy @4)
        ]
  where
    walletRnd
        :: Text
        -> (ByronKey 'RootK XPrv -> Int -> s)
        -> (WalletId, WalletName, s)
    walletRnd wname mkState =
        let
            seed = dummySeedFromName wname
            xprv = Byron.generateKeyFromSeed seed mempty
            wid = WalletId $ digest $ publicKey xprv
            rngSeed = 0
            s = mkState xprv rngSeed
        in
            (wid, WalletName wname, s)

    walletSeq
        :: Text
        -> ((ShelleyKey 'RootK XPrv, Passphrase "encryption") -> AddressPoolGap -> s)
        -> (WalletId, WalletName, s)
    walletSeq wname mkState =
        let
            seed = dummySeedFromName wname
            xprv = Shelley.generateKeyFromSeed (seed, Nothing) mempty
            wid = WalletId $ digest $ publicKey xprv
            Right gap = mkAddressPoolGap 20
            s = mkState (xprv, mempty) gap
        in
            (wid, WalletName wname, s)

    mkSeqAnyState'
        :: forall (p :: Nat) (n :: NetworkDiscriminant). ()
        => Proxy p
        -> Proxy n
        -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
        -> AddressPoolGap
        -> SeqAnyState n ShelleyKey p
    mkSeqAnyState' _ _ credentials = mkSeqAnyState @p @n credentials purposeCIP1852


    mkRndAnyState'
        :: forall (p :: Nat) (n :: NetworkDiscriminant). ()
        => Proxy p
        -> Proxy n
        -> ByronKey 'RootK XPrv
        -> Int
        -> RndAnyState n p
    mkRndAnyState' _ _ = mkRndAnyState

    networkDescription :: forall n. (NetworkDiscriminantVal n) => Proxy n -> Text
    networkDescription _ = networkDiscriminantVal @n

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

data SomeBenchmarkResults where
    SomeBenchmarkResults
        :: forall result. Buildable result
        => result
        -> SomeBenchmarkResults

instance Buildable SomeBenchmarkResults where
    build (SomeBenchmarkResults results) = build results

data WalletOverview = WalletOverview
    { utxo :: UTxOStatistics
    , addresses :: Word
    , transactions :: Word
    } deriving (Show, Generic)

instance Buildable WalletOverview where
    build WalletOverview{utxo,addresses,transactions} =
        blockListF' "" id
            [ nameF "number of addresses" (build addresses)
            , nameF "number of transactions" (build transactions)
            , build utxo
            ]

instance ToJSON WalletOverview where
    toJSON WalletOverview{utxo,addresses,transactions} = Aeson.object
        [ "utxo" .= toApiUtxoStatistics utxo
        , "addresses" .= addresses
        , "transactions" .= transactions
        ]

data BenchRndResults = BenchRndResults
    { benchName :: Text
    , restoreTime :: Time
    , readWalletTime :: Time
    , listAddressesTime :: Time
    , listTransactionsTime :: Time
    , importOneAddressTime :: Time
    , importManyAddressesTime :: Time
    , estimateFeesTime :: Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchRndResults where
    build = genericF

instance ToJSON BenchRndResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksRnd
    :: forall (n :: NetworkDiscriminant) s k p.
        ( s ~ RndAnyState n p
        , k ~ ByronKey
        , PaymentAddress n k
        , NetworkDiscriminantVal n
        , KnownNat p
        )
    => Proxy n
    -> WalletLayer s k
    -> WalletId
    -> WalletName
    -> Text
    -> Time
    -> IO BenchRndResults
benchmarksRnd _ w wid wname benchname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet w wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $ do
        pure $ computeUtxoStatistics log10 (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses w wid (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k w wid Nothing Nothing Nothing Descending

    (_, estimateFeesTime) <- bench "estimate tx fee" $ do
        let out = TxOut (dummyAddress @n) (TokenBundle.fromCoin $ Coin 1)
        runExceptT $ withExceptT show $ W.estimateFeeForPayment @_ @s @k
            w wid (out :| []) (Quantity 0) Nothing

    oneAddress <- genAddresses 1 cp
    (_, importOneAddressTime) <- bench "import one addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses @_ @s @k w wid oneAddress

    manyAddresses <- genAddresses 1000 cp
    (_, importManyAddressesTime) <- bench "import many addresses" $ do
        runExceptT $ withExceptT show $
            W.importRandomAddresses @_ @s @k w wid manyAddresses

    let walletOverview = WalletOverview{utxo,addresses,transactions}

    pure BenchRndResults
        { benchName = benchname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
        , estimateFeesTime
        , importOneAddressTime
        , importManyAddressesTime
        , walletOverview
        }
  where
    genAddresses :: Int -> Wallet s -> IO [Address]
    genAddresses n cp = evaluate $ fst $ foldl'
        (\(xs, s) _ -> first (:xs) $ genChange (xprv, mempty) s)
        ([], getState cp)
        [1..n]
      where
        seed = dummySeedFromName $ getWalletName wname
        xprv = Byron.generateKeyFromSeed seed mempty

data BenchSeqResults = BenchSeqResults
    { benchName :: Text
    , restoreTime :: Time
    , readWalletTime :: Time
    , listAddressesTime :: Time
    , listTransactionsTime :: Time
    , estimateFeesTime :: Time
    , walletOverview :: WalletOverview
    } deriving (Show, Generic)

instance Buildable BenchSeqResults where
    build = genericF

instance ToJSON BenchSeqResults where
    toJSON = genericToJSON Aeson.defaultOptions

benchmarksSeq
    :: forall (n :: NetworkDiscriminant) s k p.
        ( s ~ SeqAnyState n k p
        , k ~ ShelleyKey
        , PaymentAddress n k
        , NetworkDiscriminantVal n
        , KnownNat p
        )
    => Proxy n
    -> WalletLayer s k
    -> WalletId
    -> WalletName
    -> Text -- ^ Bench name
    -> Time
    -> IO BenchSeqResults
benchmarksSeq _ w wid _wname benchname restoreTime = do
    ((cp, pending), readWalletTime) <- bench "read wallet" $ do
        (cp, _, pending) <- unsafeRunExceptT $ W.readWallet w wid
        pure (cp, pending)

    (utxo, _) <- bench "utxo statistics" $ do
        pure $ computeUtxoStatistics log10 (totalUTxO pending cp)

    (addresses, listAddressesTime) <- bench "list addresses"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listAddresses w wid (const pure)

    (transactions, listTransactionsTime) <- bench "list transactions"
        $ fmap (fromIntegral . length)
        $ unsafeRunExceptT
        $ W.listTransactions @_ @s @k w wid Nothing Nothing Nothing Descending

    (_, estimateFeesTime) <- bench "estimate tx fee" $ do
        let out = TxOut (dummyAddress @n) (TokenBundle.fromCoin $ Coin 1)
        runExceptT $ withExceptT show $ W.estimateFeeForPayment @_ @s @k
            w wid (out :| []) (Quantity 0) Nothing

    let walletOverview = WalletOverview{utxo,addresses,transactions}

    pure BenchSeqResults
        { benchName = benchname
        , restoreTime
        , readWalletTime
        , listAddressesTime
        , listTransactionsTime
        , estimateFeesTime
        , walletOverview
        }

{- HLINT ignore bench_restoration "Use camelCase" -}
bench_restoration
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> * -> *) s results.
        ( IsOurs s Address
        , IsOurs s RewardAccount
        , IsOwned s k
        , WalletKey k
        , NFData s
        , Show s
        , PersistState s
        , CompareDiscovery s
        , KnownAddresses s
        , PersistPrivateKey (k 'RootK)
        , NetworkDiscriminantVal n
        , HasNetworkId n
        , TxWitnessTagFor k
        , Buildable results
        , ToJSON results
        )
    => Proxy n
    -> Tracer IO (BenchmarkLog n)
    -> CardanoNodeConn  -- ^ Socket path
    -> NetworkParameters
    -> NodeVersionData
    -> Text -- ^ Benchmark name (used for naming resulting files)
    -> [(WalletId, WalletName, s)]
    -> Bool -- ^ If @True@, will trace detailed progress to a .timelog file.
    -> Percentage -- ^ Target sync progress
    -> (Proxy n -> WalletLayer s k -> WalletId -> WalletName -> Text -> Time -> IO results)
    -> IO SomeBenchmarkResults
bench_restoration proxy tr socket np vData benchname wallets traceToDisk targetSync benchmarks = do
    putStrLn $ "*** " ++ T.unpack benchname
    let networkId = networkIdVal proxy
    let tl = newTransactionLayer @k networkId
    withNetworkLayer nullTracer np socket vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        let ti = neverFails "bench db shouldn't forecast into future"
                $ timeInterpreter nw
        withBenchDBLayer @s @k ti $ \db -> do
            withWalletLayerTracer $ \wlTr -> do
                let w = WalletLayer
                        wlTr
                        (emptyGenesis gp, np, mkSyncTolerance 3600)
                        nw
                        tl
                        db

                forM_ wallets $ \(wid, wname, s) -> do
                    _ <- unsafeRunExceptT $ W.createWallet w wid wname s
                    void $ forkIO $ unsafeRunExceptT $ W.restoreWallet @_ @s @k w wid

                -- NOTE: This is now the time to restore /all/ wallets.
                (_, restorationTime) <- bench "restoration" $ do
                    waitForWalletsSyncTo
                        targetSync
                        tr
                        proxy
                        w
                        (map fst' wallets)
                        gp
                        vData

                let (wid0, wname0, _) = head wallets
                results <- benchmarks proxy w wid0 wname0 benchname restorationTime
                Aeson.encodeFile resultsFilepath results
                forM_ wallets $ \(wid, _, _) ->
                    unsafeRunExceptT (W.deleteWallet w wid)
                pure $ SomeBenchmarkResults results
  where
    fst' (x,_,_) = x
    timelogFilepath = T.unpack benchname <> ".timelog"
    resultsFilepath = T.unpack benchname <> ".json"

    withWalletLayerTracer act
        | traceToDisk =
            withFile timelogFilepath WriteMode $ \h -> do
                -- Use a custom tracer to output (time, blockHeight) to a file
                -- each time we apply blocks.
                let fileTr = Tracer $ \msg -> do
                        liftIO . B8.hPut h . T.encodeUtf8 . (<> "\n") $ msg
                        hFlush h
                act $ traceProgressForPlotting fileTr
        | otherwise   = act nullTracer

dummyAddress
    :: forall (n :: NetworkDiscriminant). NetworkDiscriminantVal n
    => Address
dummyAddress
    | networkDiscriminantVal @n == networkDiscriminantVal @'Mainnet =
        Address $ BS.pack $ 0 : replicate 56 0
    | otherwise =
        Address $ BS.pack $ 1 : replicate 56 0

-- | Hash the given text, and construct a mnemonic from it.
dummySeedFromName :: Text -> SomeMnemonic
dummySeedFromName = SomeMnemonic @24
    . entropyToMnemonic
    . unsafeMkEntropy @256
    . blake2b256
    . T.encodeUtf8

traceProgressForPlotting :: Tracer IO Text -> Tracer IO WalletLog
traceProgressForPlotting tr = Tracer $ \case
    MsgFollow (MsgApplyBlocks bs) -> do
        let tip = pretty . getQuantity . blockHeight . NE.last $ bs
        time <- pretty . utcTimeToPOSIXSeconds <$> getCurrentTime
        traceWith tr (time <> " " <> tip)
    _ -> return ()

withBenchDBLayer
    :: forall s k a.
        ( IsOwned s k
        , NFData s
        , Show s
        , PersistState s
        , IsOurs s RewardAccount
        , IsOurs s Address
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        )
    => TimeInterpreter IO
    -> (DBLayer IO s k -> IO a)
    -> IO a
withBenchDBLayer ti action =
    withSystemTempFile "bench.db" $ \dbFile _ -> do
        let before = newDBLayer nullTracer migrationDefaultValues (Just dbFile) ti
        let after = destroyDBLayer . fst
        bracket before after $ \(_ctx, db) -> action db
  where
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        , Sqlite.defaultDesiredNumberOfPool = 0
        , Sqlite.defaultMinimumUTxOValue = Coin 0
        , Sqlite.defaultHardforkEpoch = Nothing
        , Sqlite.defaultKeyDeposit = Coin 0
        }

prepareNode
    :: forall n. (NetworkDiscriminantVal n)
    => Tracer IO (BenchmarkLog n)
    -> Proxy n
    -> CardanoNodeConn
    -> NetworkParameters
    -> NodeVersionData
    -> IO ()
prepareNode tr proxy socketPath np vData = do
    traceWith tr $ MsgSyncStart proxy
    sl <- withNetworkLayer nullTracer np socketPath vData $ \nw' -> do
        let gp = genesisParameters np
        let convert = fromCardanoBlock gp
        let nw = convert <$> nw'
        waitForNodeSync tr nw
    traceWith tr $ MsgSyncCompleted proxy sl

-- | Regularly poll the wallets to monitor syncing progress. Block until all
-- wallets reach the given percentage.
waitForWalletsSyncTo
    :: forall s k n. (NetworkDiscriminantVal n)
    => Percentage
    -> Tracer IO (BenchmarkLog n)
    -> Proxy n
    -> WalletLayer s k
    -> [WalletId]
    -> GenesisParameters
    -> NodeVersionData
    -> IO ()
waitForWalletsSyncTo targetSync tr proxy walletLayer wids gp vData = do
    let tolerance = mkSyncTolerance 3600
    now <- currentRelativeTime ti
    posixNow <- utcTimeToPOSIXSeconds <$> getCurrentTime
    progress <- forM wids $ \wid -> do
        w <- fmap fst' <$> unsafeRunExceptT $ W.readWallet walletLayer wid
        let Quantity bh = blockHeight $ currentTip w
        prog <- syncProgress
            tolerance
            (neverFails "syncProgress never forecasts into the future"
                $ timeInterpreter nl)
            (currentTip w)
            now
        return (prog, bh)
    traceWith tr $ MsgRestorationTick posixNow (map fst progress)
    threadDelay 1000000

    if all ((> Syncing (Quantity targetSync)) . fst) progress
    then return ()
    else waitForWalletsSyncTo targetSync tr proxy walletLayer wids gp vData
  where
    WalletLayer _ _ nl _ _ = walletLayer
    ti = timeInterpreter nl
    fst' (x,_,_) = x

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: forall n. (NetworkDiscriminantVal n)
    => Tracer IO (BenchmarkLog n)
    -> NetworkLayer IO Block
    -> IO SlotNo
waitForNodeSync tr nw = loop 10
  where
    loop :: Int -> IO SlotNo
    loop retries = do
        nodeTip <- currentNodeTip nw
        if slotNo nodeTip /= 0 then do
            let tolerance = mkSyncTolerance 300
            let ti = neverFails "syncProgress never forecasts into the future"
                    $ timeInterpreter nw
            now <- currentRelativeTime ti
            prog <- syncProgress tolerance ti nodeTip now
            traceWith tr $ MsgNodeTipTick nodeTip prog
            if prog == Ready
                then pure (slotNo nodeTip)
                else do
                    -- 2 seconds poll interval
                    threadDelay 2000000
                    loop retries
        else
            if retries > 0 then do
                 let delay = 15000000
                 traceWith tr $ MsgRetryShortly delay
                 threadDelay delay
                 loop (retries - 1)
            else throwString "Gave up waitForNodeSync"

data BenchmarkLog (n :: NetworkDiscriminant)
    = MsgNodeTipTick BlockHeader SyncProgress
    | MsgRestorationTick POSIXTime [SyncProgress]
    | MsgSyncStart (Proxy n)
    | MsgSyncCompleted (Proxy n) SlotNo
    | MsgRetryShortly Int
    deriving (Show, Eq)

instance HasPrivacyAnnotation (BenchmarkLog n)
instance HasSeverityAnnotation (BenchmarkLog n) where
    getSeverityAnnotation = \case
        MsgNodeTipTick{} -> Info
        MsgRestorationTick{} -> Info
        MsgSyncStart{} -> Info
        MsgSyncCompleted{} -> Info
        MsgRetryShortly{} -> Warning

instance NetworkDiscriminantVal n => ToText (BenchmarkLog n) where
    toText = \case
        MsgNodeTipTick tip progress ->
            "Initial node synchronization: " +| progress |+ " " +| tip ||+""
        MsgRestorationTick _posixTime progressList ->
            "Restoring: "+|progressList |+""
        MsgSyncStart _ ->
            "Syncing "+| networkDiscriminantVal @n |+" node... "
        MsgSyncCompleted _ tip ->
            "Completed sync of "+| networkDiscriminantVal @n |+" up to "+|| tip ||+""
        MsgRetryShortly delay ->
            "Fetching tip failed, retrying in " +|| (delay `div` 1000) ||+ "ms"


-- | Format a type-level per-mille number as percent
--
-- >>> showPercentFromPermille (Proxy @1)
-- "0.1"
--
-- >>> showPercentFromPermille (Proxy @0)
-- "0"
showPercentFromPermille :: forall (p :: Nat) . KnownNat p => Proxy p -> Text
showPercentFromPermille =
    T.pack . display . (/10) . fromRational . toRational . natVal
  where
    -- I cannot find a haskell way to format a rational with a /minimum/ number
    -- of decimals, so this will do.
    display :: Double -> String
    display 0 = "0"
    display x = show x
