module Database.SQLite3.Conduit where

-- import 

-- conduit
import Conduit (ConduitT, yield, yieldM)
import Data.Conduit (runConduit)
-- direct-sqlite
import Database.SQLite3 (Database, open, close, Statement, prepare, finalize, step, StepResult(..), columns, columnText, columnInt64, columnDouble)
-- resourcet
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource(..), allocate)
-- text
import Data.Text (Text)


-- | Manage a connection to a database in an exception-safe way (with 'bracket')
-- withDatabase :: Text -- ^ connection string
--              -> (Database -> IO a) -- ^ user program
--              -> IO a
withDatabase :: MonadResource f => Text -> f Database
withDatabase path = snd <$> allocate (open path) close

-- | Bracket 'prepare' and 'finalize'. Useful for stepping multiple times through a 'Statement'
-- withStatement :: Database -- ^ DB connection
--               -> Text -- ^ SQL statement
--               -> (Statement -> IO a) -- ^ User program
--               -> IO a
withStatement :: MonadResource f => Database -> Text -> f Statement
withStatement db sql = snd <$> allocate (prepare db sql) finalize
