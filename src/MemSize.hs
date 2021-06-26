module MemSize
    ( MemSize
    , memSize
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (mapAccumWithKey)
import Data.Set (Set)
import qualified Data.Set as S (elems)

-- A type class to help debug approximately how much memory a given type is using.
class MemSize a where
    memSize :: a -> Word
    memSizeList :: [a] -> Word
    memSizeList x = sum $ map memSize x

instance (MemSize a) => MemSize [a] where
    memSize = memSizeList

instance (MemSize a) => MemSize (Maybe a) where
    memSize Nothing = 1
    memSize (Just x) = memSize x

instance (MemSize a, MemSize b) => MemSize (Map a b) where
    memSize m = fst $ M.mapAccumWithKey (\acc k v -> (acc + memSize k + memSize v, acc)) 0 m

instance (MemSize a) => MemSize (Set a) where
    memSize s = sum $ map memSize $ S.elems s

instance MemSize Bool where
    memSize _ = 1

instance MemSize Word where
    memSize _ = 8

instance MemSize Char where
    memSize _ = 1

instance MemSize Double where
    memSize _ = 8
