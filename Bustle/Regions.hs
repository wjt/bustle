module Bustle.Regions
  (
    Stripe(..)
  , Regions
  , translateRegions

  , RegionSelection (rsCurrent)
  , regionSelectionNew
  , regionSelectionUpdate
  , regionSelectionUp
  , regionSelectionDown
  )
where

import Data.Maybe (maybeToList, isNothing, isJust)

import Data.List (sort)
--import Test.QuickCheck

data Stripe = Stripe { stripeTop :: Double
                     , stripeBottom :: Double
                     }
  deriving
    (Show, Eq, Ord)
type Region a = (Stripe, a)
type Regions a = [Region a]

translateRegions :: Double
                 -> Regions a
                 -> Regions a
translateRegions y = map (\(s, a) -> (translate s, a))
  where
    translate (Stripe y1 y2) = Stripe (y1 + y) (y2 + y)

-- A zipper for selected regions. rsBefore is reversed. If rsCurrent is
-- Nothing, the two lists may still both be non-empty (to keep track of roughly
-- where the user's last click was).
data RegionSelection a =
    RegionSelection { rsBefore :: Regions a
                    , rsLastClick :: Double
                    , rsCurrent :: Maybe (Region a)
                    , rsAfter :: Regions a
                    }
  deriving
    (Show, Eq)

relativeTo :: Double
           -> Stripe
           -> Ordering
relativeTo y (Stripe top bottom)
    | y < top    = LT
    | y > bottom = GT
    | otherwise  = EQ

hits :: Double
     -> (Stripe, a)
     -> Bool
hits y (stripe, _) = y `relativeTo` stripe == EQ

nonOverlapping :: [Stripe]
               -> Bool
nonOverlapping []         = True
nonOverlapping (_:[])     = True
nonOverlapping (s1:s2:ss) =
    stripeBottom s1 <= stripeTop s2 && nonOverlapping (s2:ss)

regionSelectionNew :: Regions a
                   -> RegionSelection a
regionSelectionNew rs
    | sorted /= map fst rs        = error $ "regionSelectionNew: unsorted regions"
    | not (nonOverlapping sorted) = error $ "regionSelectionNew: overlapping regions"
    | otherwise                   = RegionSelection [] 0 Nothing rs
  where
    sorted = sort (map fst rs)

regionSelectionUpdate :: Double
                      -> RegionSelection a
                      -> RegionSelection a
regionSelectionUpdate y rs@(RegionSelection { rsLastClick = lastClick })
    | y <= lastClick = searchUp y rs
    | otherwise      = searchDown y rs

invert :: RegionSelection a
       -> RegionSelection a
invert rs = rs { rsBefore = rsAfter rs, rsAfter = rsBefore rs }

regionSelectionUp :: RegionSelection a
                  -> RegionSelection a
regionSelectionUp rs@(RegionSelection before lastClick current after) =
    case before of
        []     -> rs
        (b:bs) -> RegionSelection bs lastClick (Just b) (maybeToList current ++ after)

regionSelectionDown :: RegionSelection a
                    -> RegionSelection a
regionSelectionDown = invert . regionSelectionUp . invert

searchUp, searchDown :: Double
                     -> RegionSelection a
                     -> RegionSelection a
searchUp y rs = search y rs LT
-- We search down by searching up on an inverted selection
searchDown y rs = search y (invert rs) GT

search :: Double
       -> RegionSelection a
       -> Ordering
       -> RegionSelection a
search y rs' side = go (rs' { rsLastClick = y })
  where
    go rs@(RegionSelection before _ current _)
        -- If there's a currently selected point and we're inside it, we're done.
        | maybe False (hits y) current = rs
        | otherwise =
            case before of
                -- If there's no more above the current point, we're done
                []     -> rs
                (_b@(stripe, _):_bs) ->
                    if y `relativeTo` stripe == side
                        -- If the only regions above the current region are above
                        -- the click, we're done.
                        then rs
                        -- Otherwise rotate and carry on up
                        else go $ regionSelectionUp rs

-- Below this point we have tests.
{-

instance Arbitrary Stripe where
    arbitrary = do
        top <- fmap abs arbitrary
        bottom <- arbitrary `suchThat` (>= top)
        return $ Stripe top bottom

newtype NonOverlappingStripes = NonOverlappingStripes [Stripe]
  deriving
    (Show, Eq, Ord)

instance Arbitrary NonOverlappingStripes where
    arbitrary = do
        stripes <- fmap sort (listOf arbitrary) `suchThat` nonOverlapping
        return $ NonOverlappingStripes stripes

newtype ValidRegions a = ValidRegions (Regions a)
  deriving
    (Show, Eq, Ord)

instance Arbitrary a => Arbitrary (ValidRegions a) where
    arbitrary = do
        NonOverlappingStripes stripes <- arbitrary
        values <- listOf arbitrary
        return $ ValidRegions (zip stripes values)

instance Arbitrary a => Arbitrary (RegionSelection a) where
    arbitrary = do
        ValidRegions rs <- arbitrary
        return $ regionSelectionNew rs

isEmpty :: RegionSelection a
        -> Bool
isEmpty rs = and [ isNothing (rsCurrent rs)
                 , null (rsBefore rs)
                 , null (rsAfter rs)
                 ]

propInitiallyUnselected = \rs -> isNothing $ rsCurrent rs
propUpDoesNothing = \rs -> isNothing $ rsCurrent $ regionSelectionUp rs
propDownDoesNothing = \rs -> isEmpty rs ==> isNothing $ rsCurrent $ regionSelectionDown rs
propDownWorks rs =
    if isEmpty rs
      then isNothing first
      else isJust first
  where
    first = rsCurrent $ regionSelectionDown rs

propUpdateOnce :: (Eq a)
               => ValidRegions a
               -> Property
propUpdateOnce (ValidRegions regions) =
    not (null regions) ==> Just first == rsCurrent (regionSelectionUpdate y rs)
  where
    rs = regionSelectionNew regions
    first@(Stripe top bottom, _) = head regions
    y = (top + bottom) / 2

-}
