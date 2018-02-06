module Logoot.Commands where

import Prelude
import Control.Monad.Free (Free, liftF)

data LogootF ident cont a
  = Insert ident cont a
  | Delete ident cont a

type Logoot ident cont = Free (LogootF ident cont)

insert :: forall ident cont. ident -> cont -> Logoot ident cont Unit
insert s c = liftF (Insert s c unit)

delete :: forall ident cont. ident -> cont -> Logoot ident cont Unit
delete s c = liftF (Delete s c unit)

-- | move from to
move :: forall ident cont. ident -> ident -> cont -> Logoot ident cont Unit
move s s' c = do
  delete s c
  insert s' c