module Samples.Simple where

import Data.Text (Text)
import Plutarch
import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V2
import Plutarch.Prelude
import PlutusLedgerApi.V2 (ExBudget)
import PlutusTx qualified
import Utils (evalWithArgsT)

gift :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
gift = plam $ \_datm _redm _ctx -> pconstant ()

giftRes :: Either Text (Script, ExBudget, [Text])
giftRes = gift `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]

burn :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
burn = plam $ \_datm _redm _ctx -> perror

burnRes :: Either Text (Script, ExBudget, [Text])
burnRes = burn `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]