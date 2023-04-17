module Utils.Helpers
  ( StakingStrategy (IgnoreStaking, UseStakingKey),
    ErrorMsgContains (ErrorMsgContains),
    findUniqueUtxo,
    findUniqueUtxo',
    findUtxos,
    findAllUtxosAt,
    checkFailWithMsg,
    checkFails,
    checkNoFail,
    assertLimits,
    initScriptRef,
    getFirstRefScript,
    minLovelacesPerUtxo,
    unwrapAppendStaking,
  )
where

import Control.Monad (unless, void)
import Control.Monad.State.Strict (MonadState (get, put))
import Data.List (isInfixOf)
import GHC.Stack (HasCallStack)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Plutus.Model
  ( AppendStaking,
    HasAddress,
    HasDatum (DatumType),
    IsValidator,
    checkErrors,
    getMainUser,
    loadRefScript,
    refScriptAt,
    sendTx,
    signTx,
    spend,
    userSpend,
  )
import Plutus.Model.V2
  ( FailReason (GenericFail),
    Log (Log),
    MockConfig,
    Run,
    TxBox,
    TypedValidator,
    boxAt,
    getFails,
    logError,
    mustFail,
    mustFailLog,
    skipLimits,
    testLimits,
    testNoErrors,
    txBoxDatum,
    txBoxOut,
    txBoxRef,
  )
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.Prelude hiding
  ( any,
    error,
    mconcat,
    mempty,
    pure,
    unless,
    (<$>),
  )
import Test.Tasty
import Prelude (String, any, error, mconcat, mempty, pure, show, (<$>))

data StakingStrategy
  = IgnoreStaking
  | UseStakingKey Ledger.PubKeyHash

newtype ErrorMsgContains = ErrorMsgContains String

minLovelacesPerUtxo :: Integer
minLovelacesPerUtxo = 2_000_000

findUniqueUtxo ::
  (HasCallStack, HasDatum script, HasAddress script) =>
  script ->
  (TxBox script -> Bool) ->
  Run (V2.TxOutRef, V2.TxOut, DatumType script)
findUniqueUtxo validator selector =
  findUniqueUtxo' validator selector
    >>= ( \case
            Nothing -> do
              errs <- checkErrors
              error ("Either no or more than 1 utxos found. Previous errors: " <> show errs)
            Just x0 -> pure x0
        )

findUniqueUtxo' ::
  (HasCallStack, HasDatum script, HasAddress script) =>
  script ->
  (TxBox script -> Bool) ->
  Run (Maybe (V2.TxOutRef, V2.TxOut, DatumType script))
findUniqueUtxo' validator selector = do
  boxes <- boxAt validator
  let res = [(txBoxRef a, txBoxOut a, txBoxDatum a) | a <- boxes, selector a]
  case res of
    [(oref, o, datum)] -> pure $ Just (oref, o, datum)
    _ -> pure Nothing

findUtxos ::
  (V2.FromData a, V2.ToData a, V2.FromData b, V2.ToData b) =>
  TypedValidator a b ->
  (TxBox (TypedValidator a b) -> Bool) ->
  Run [(V2.TxOutRef, V2.TxOut, a)]
findUtxos validator selector = do
  boxes <- boxAt validator
  pure [(txBoxRef a, txBoxOut a, txBoxDatum a) | a <- boxes, selector a]

findAllUtxosAt ::
  (V2.FromData a, V2.ToData a, V2.FromData b, V2.ToData b) =>
  TypedValidator a b ->
  Run [(V2.TxOutRef, V2.TxOut, a)]
findAllUtxosAt validator = do
  boxes <- boxAt validator
  pure [(txBoxRef a, txBoxOut a, txBoxDatum a) | a <- boxes]

checkFails :: MockConfig -> Ledger.Value -> String -> Run () -> TestTree
checkFails cfg funds msg act =
  testNoErrors funds (skipLimits cfg) msg (mustFail act)

-- | The test case passes when any of the errors
-- contains the expected error message.
-- This checks just the contents of the GenericFail errors.
checkFailWithMsg ::
  MockConfig -> Ledger.Value -> String -> ErrorMsgContains -> Run () -> TestTree
checkFailWithMsg cfg funds msg (ErrorMsgContains errorMsg) act =
  checkNoFail cfg funds msg run
  where
    run :: Run ()
    run = do
      st <- get
      act
      Log errs <- getFails
      put st {mustFailLog = Log mempty}
      unless
        ( any
            ( \case
                (_, GenericFail err) -> errorMsg `isInfixOf` err
                _ -> False
            )
            errs
        )
        ( logError $
            "Test case errors didn't contain any error containing: \""
              <> errorMsg
              <> "\" All errors: "
              <> show errs
        )

checkNoFail :: MockConfig -> Ledger.Value -> String -> Run () -> TestTree
checkNoFail cfg initialFunds = testNoErrors initialFunds (skipLimits cfg)

assertLimits :: Ledger.Value -> MockConfig -> String -> Run () -> TestTree
assertLimits initialFunds cfg msg run =
  testLimits initialFunds cfg msg id (run >> logError "Show stats")

initScriptRef :: IsValidator script => script -> Run ()
initScriptRef script = do
  admin <- getMainUser
  let minAda = Ada.lovelaceValueOf minLovelacesPerUtxo
  sp <- spend admin minAda
  let tx =
        mconcat
          [ userSpend sp,
            loadRefScript script minAda
          ]
  void $ signTx admin tx >>= sendTx

getFirstRefScript :: IsValidator script => script -> Run V2.TxOutRef
getFirstRefScript script =
  fst . head <$> refScriptAt script

mapTxBox :: forall script1 script2. (DatumType script1 -> DatumType script2) -> TxBox script1 -> TxBox script2
mapTxBox f box = box {txBoxDatum = f $ txBoxDatum box}

unwrapAppendStaking :: forall a. TxBox (AppendStaking a) -> TxBox a
unwrapAppendStaking = mapTxBox id
