module Ch9 where

import Prelude (Unit, class Show, class Eq, ($), discard, show, (==), (&&))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

class Semigroup a where
    -- combine to `a`s to produce a new `a`, meeting the Magma requirement
    append :: a -> a -> a

infixr 5 append as <>

-- every Monoid must also be a Semigroup
class Semigroup a <= Monoid a where
    mempty :: a

-- type whose binary operator will be the logical AND
data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
    show = genericShow

-- only `ATrue` if both operands are `ATrue`
instance semigroupAndBool :: Semigroup AndBool where
    append ATrue ATrue = ATrue
    append _ _ = AFalse

-- `ATrue` is the identity as it does not affect the other value during the AND operation
instance monoidAndBool :: Monoid AndBool where
    mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
    log "Verifying AndBool Semigroup Laws (1 test)"
    log $ show $ ATrue <> (ATrue <> AFalse) == (ATrue <> ATrue) <> AFalse

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
    log "Verifying AndBool Monoid Laws (2 tests)"
    log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue
    log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse

-- type whose binary operator will be the logical OR
data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
    show = genericShow

-- only `OFalse` if both operands are `OFalse`
instance semigroupOrBool :: Semigroup OrBool where
    append OFalse OFalse = OFalse
    append _ _ = OTrue

-- `OFalse` is the identity as it does not affect the other value during the OR operation
instance monoidOrBool :: Monoid OrBool where
    mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
    log "Verifying OrBool Semigroup Laws (1 test)"
    log $ show $ OFalse <> (OTrue <> OFalse) == (OFalse <> OTrue) <> OFalse

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
    log "Verifying OrBool Monoid Laws (2 tests)"
    log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue
    log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <>mempty == OFalse

test :: Effect Unit
test = do
    log "AndBool Semigroup:"
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse

    log "AndBool Monoid:"
    log $ show $ mempty <> ATrue == ATrue
    log $ show $ mempty <> AFalse == AFalse

    verifyAndBoolSemigroup
    verifyAndBoolMonoid

    verifyOrBoolSemigroup
    verifyOrBoolMonoid
