{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Data.List (sort)
import Data.Maybe (isNothing)

import Bustle.Regions

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
        -- there is no orderedList1 sadly
        stripes <- fmap sort (listOf1 arbitrary) `suchThat` nonOverlapping
        return $ NonOverlappingStripes stripes

newtype ValidRegions a = ValidRegions (Regions a)
  deriving
    (Show, Eq, Ord)

instance Arbitrary a => Arbitrary (ValidRegions a) where
    arbitrary = do
        NonOverlappingStripes stripes <- arbitrary
        values <- vector (length stripes)
        return $ ValidRegions (zip stripes values)

instance Arbitrary a => Arbitrary (RegionSelection a) where
    arbitrary = do
        ValidRegions rs <- arbitrary
        return $ regionSelectionNew rs

prop_InitiallyUnselected = \rs -> isNothing $ rsCurrent rs
prop_UpDoesNothing = \rs -> isNothing $ rsCurrent $ regionSelectionUp rs

prop_DownDoesNothing vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
        let final = last regions
            rs'   = regionSelectionUpdate (midpoint . fst $ last regions) rs
        in
            rsCurrent (regionSelectionDown rs') == Just final

prop_DownWorks vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
        rsCurrent (regionSelectionDown rs) == Just (head regions)

withRegions :: Testable t
            => ValidRegions a
            -> (RegionSelection a -> t)
            -> t
withRegions (ValidRegions regions) f = f (regionSelectionNew regions)

prop_UpdateToFirst :: (Eq a)
                  => ValidRegions a
                  -> Bool
prop_UpdateToFirst vr@(ValidRegions regions) = withRegions vr $ \rs ->
    Just first == rsCurrent (regionSelectionUpdate y rs) &&
    null (rsBefore rs)
  where
    first@(Stripe top bottom, _) = head regions
    y = (top + bottom) / 2

prop_UpdateToAny :: (Eq a, Show a)
                => ValidRegions a
                -> Property
prop_UpdateToAny vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
    forAll (elements regions) $ \ r@(s, _) ->
        rsCurrent (regionSelectionUpdate (midpoint s) rs) == Just r

shuffled :: [a] -> Gen [a]
shuffled [] = return []
shuffled xs = do
    i <- choose (0, length xs - 1)
    let x    = xs !! i
        pre  = take i xs
        post = drop (i + 1) xs
    xs' <- shuffled (pre ++ post)
    return (x:xs')

prop_UpdateToAll :: (Eq a, Show a)
                => ValidRegions a
                -> Property
prop_UpdateToAll vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
    forAll (shuffled regions) $ \regions' ->
        updateAndForward rs regions'
  where
    updateAndForward rs [] = True
    updateAndForward rs (x:xs) =
        let rs' = regionSelectionUpdate (midpoint (fst x)) rs
        in rsCurrent rs' == Just x && updateAndForward rs' xs

runTests = $quickCheckAll
