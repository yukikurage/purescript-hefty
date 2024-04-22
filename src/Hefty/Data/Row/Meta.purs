module Hefty.Data.Row.Meta where

import Prelude

import Data.List (List)
import Data.List as L
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

type RowMetaInfo =
  { length :: Int
  , symbols ::
      List
        { symbol :: String
        , index :: Int
        }
  }

-- | Get Metadata of Rows
class RowMeta :: forall k. Row k -> Constraint
class RowMeta r where
  rowMeta :: RowMetaInfo

class RowListMeta :: forall k1 k2. RowList k1 -> Row k2 -> Constraint
class RowListMeta rl r | rl -> r where
  rowListMeta :: RowMetaInfo

instance (RowToList r rl, RowListMeta rl r) => RowMeta r where
  rowMeta = rowListMeta @rl

instance RowListMeta RL.Nil () where
  rowListMeta = { length: 0, symbols: L.Nil }

instance (RowListMeta rl r, IsSymbol sym, Cons sym t r r2) => RowListMeta (RL.Cons sym t rl) r2 where
  rowListMeta = { length: prev.length + 1, symbols: L.Cons { symbol: sym, index: 0 } $ map (incrementAt sym) prev.symbols }
    where
    sym = reflectSymbol @sym Proxy
    prev = rowListMeta @rl
    incrementAt s { symbol, index } = { symbol, index: if symbol == s then index + 1 else index }
