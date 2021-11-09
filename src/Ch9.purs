module Ch9 where

import Prelude (Unit, class Show, class Eq, ($), discard, show, (==), (&&))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
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

data Mod4 = Zero | One | Two | Three
derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _
instance showMod4 :: Show Mod4 where
    show = genericShow

-- have to write it manually for all inputs
instance semigroupMod4 :: Semigroup Mod4 where
    append Zero y = y
    append x Zero = x

    append One One   = Two
    append One Two   = Three
    append One Three = Zero

    append Two One   = Three
    append Two Two   = Zero
    append Two Three = One

    append Three One   = Zero
    append Three Two   = One
    append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
    mempty = Zero

class Monoid a <= Group a where
    ginverse :: a -> a

instance groupMod4 :: Group Mod4 where
    ginverse Zero  = Zero
    ginverse One   = Three
    ginverse Two   = Two
    ginverse Three = One

class Semigroup g <= Commutative g

-- since the addition operator for `Mod4` is commutative, an instance can be created for it
instance commutativeMod4 :: Commutative Mod4

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
    log "Verifying Mod4 Semigroup Laws (1 test)"
    log $ show $ One <> (Two <> Three) == (One <> Two) <> Three

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
    log "Verifying Mod4 Monoid Laws (1 test)"
    log $ show $ mempty <> One == One <> mempty && One <> mempty == One

newtype First a = First (Maybe a)
derive instance genericFirst :: Generic (First a) _
instance showFirst :: Show a => Show (First a) where
    show = genericShow

newtype Last a = Last (Maybe a)
derive instance genericLast :: Generic (Last a) _
instance showLast :: Show a => Show (Last a) where
    show = genericShow

-- when `append`ing `First`s, always choose the left operand if it is not `Nothing`
instance semigroupFirst :: Semigroup (First a) where
    append (First Nothing) last = last
    append first _              = first

instance monoidFirst :: Monoid (First a) where
    mempty = First Nothing

-- when `append`ing `Last`s, always choose the right operand if it is not `Nothing`
instance semigroupLast :: Semigroup (Last a) where
    append first (Last Nothing) = first
    append _ last               = last

instance monoidLast :: Monoid (Last a) where
    mempty = Last Nothing

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

    verifyMod4Semigroup
    verifyMod4Monoid

    log "First Semigroup:"
    log $ show $ First Nothing <> First (Just 77)
    log "Last Semigroup:"
    log $ show $ Last (Just 1) <> Last (Just 99)
