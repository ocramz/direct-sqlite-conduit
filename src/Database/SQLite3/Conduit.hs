{-# LANGUAGE LambdaCase #-}
module Database.SQLite3.Conduit (
  sinkRows
  , sourceRows
  -- * helpers
  , withDatabase
  , withStatement
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.List (intercalate)

-- conduit
import Conduit (ConduitT, await, yieldM)
-- import Data.Conduit (runConduit)
-- containers
import qualified Data.Map as M (Map, toList)
-- direct-sqlite
import Database.SQLite3 (Database, open, close, Statement, exec, prepare, finalize, step, StepResult(..), columns, columnText, columnInt64, columnDouble, SQLData(..))
-- resourcet
import Control.Monad.Trans.Resource (MonadResource(..), allocate)
-- text
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)


-- | Manage a connection to a database in an exception-safe way (with 'bracket')
withDatabase :: MonadResource f =>
                String -- ^ DB file path
             -> f Database
withDatabase path = snd <$> allocate (open $ pack path) close

-- | Bracket 'prepare' and 'finalize'. Useful for stepping multiple times through a 'Statement'
withStatement :: MonadResource f => Database -> Text -> f Statement
withStatement db sql = snd <$> allocate (prepare db sql) finalize


-- | Stream out the rows resulting from a SELECT statement
sourceRows :: MonadIO m => Statement -> ConduitT i [SQLData] m ()
sourceRows s = liftIO (step s) >>= \r -> case r of
  Row -> yieldM $ liftIO (columns s)
  _ -> pure ()

-- | Sink the rows into the DB with an INSERT statement
--
-- INSERT INTO table_name (column1, column2) VALUES (value1, value2);
sinkRows :: MonadIO m =>
            Database -- ^ table name
         -> String
         -> ConduitT (M.Map String SQLData) o m ()
sinkRows db tname = sinkRowsWith (insertStmt tname) db

sinkRowsWith :: MonadIO m =>
                (t -> Text) -- ^ serialize a row  "INSERT INTO .."
             -> Database -> ConduitT t o m ()
sinkRowsWith f conn = await >>= \r -> case r of
  Just row -> liftIO (exec conn $ f row)
  Nothing -> pure ()



insertStmt :: String -- ^ table name
           -> M.Map String SQLData -- ^ row data
           -> Text
insertStmt tname kvs =
  pack $ unwords ["INSERT INTO", tname, (tupled ks), "VALUES", tupled (map fromSQLData vs)]
  where
    (ks, vs) = unzip $ M.toList kvs

tupled :: [String] -> String
tupled xs = "(" <> intercalate "," xs <> ")"

fromSQLData :: SQLData -> String
fromSQLData = \case
  SQLInteger i -> show i
  SQLFloat f -> show f
  SQLText t -> unpack t
  SQLNull -> "NULL"
  SQLBlob bs -> unpack $ decodeUtf8 bs
