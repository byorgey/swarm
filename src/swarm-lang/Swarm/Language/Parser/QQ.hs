-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm polytypes.
module Swarm.Language.Parser.QQ (tyQ) where

import Data.Generics
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Swarm.Language.Parser.Core (runParserTH)
import Swarm.Language.Parser.Lex (sc)
import Swarm.Language.Parser.Type (parsePolytype)
import Swarm.Util (liftText)
import Swarm.Util.Parse (fully)

------------------------------------------------------------
-- Quasiquoters
------------------------------------------------------------

-- | A quasiquoter for Swarm polytypes, so we can conveniently write them
--   down using concrete syntax and have them parsed into abstract
--   syntax at compile time.  This is used, for example, in writing down
--   the concrete types of constants (see "Swarm.Language.Typecheck").
tyQ :: QuasiQuoter
tyQ =
  QuasiQuoter
    { quoteExp = quoteTypeExp
    , quotePat = error "quotePat  not implemented for polytypes"
    , quoteType = error "quoteType not implemented for polytypes"
    , quoteDec = error "quoteDec  not implemented for polytypes"
    }

quoteTypeExp :: String -> TH.ExpQ
quoteTypeExp s = do
  loc <- TH.location
  let pos =
        ( TH.loc_filename loc
        , fst (TH.loc_start loc)
        , snd (TH.loc_start loc)
        )
  parsed <- runParserTH pos (fully sc parsePolytype) s
  dataToExpQ (fmap liftText . cast) parsed
