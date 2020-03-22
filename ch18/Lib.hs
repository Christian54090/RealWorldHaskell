
-- monad transformers are not a standalone entity. instead, they modify the
-- behavior of an underlying monad.
--    For example, the transformer equivalent of State is StateT; it adds mutable
--    state to an underlying monad

import CountEntries