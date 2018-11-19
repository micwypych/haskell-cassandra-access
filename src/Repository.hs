module Repository where

import qualified Data.Text.Lazy as LT
import Data.Text (Text)
--import Data.Text.Lazy
import Data.Functor.Identity
import Database.CQL.IO as Client
import Database.CQL.Protocol as Protocol

import Data.UUID (UUID)
import Data.Either.Combinators
import Control.Monad.IO.Class
import qualified System.Random as Rand
import qualified Data.UUID as Uuid
import qualified System.Logger as Logger

import Debug.Trace

data State = State UUID Text deriving (Eq, Show)


toState :: Either String Row -> Either String State
toState monadicRow = do
     row <- trace (show monadicRow) monadicRow
    -- trace (show row) (return ())
     uuid <- fromRow 0 row
--     trace (show uuid) (return ())
     value <- fromRow 1 row
     return $ State (fromJust uuid) (fromJust value)


class StateRepository repository where
  save :: MonadIO m => State -> repository -> m ()
  exists :: MonadIO m => UUID -> repository -> m Bool
  findById :: MonadIO m => UUID -> repository -> m (Maybe State)
  

data CassandraStateRepository = CassandraStateRepository {
        connection::ClientState, 
        findByIdQuery :: QueryString R (Identity UUID) (Row), 
        toFindByIdQueryParams :: UUID -> QueryParams (Identity UUID),
        existsQuery :: QueryString R (Identity UUID) (Identity UUID), 
        existsQueryParams :: UUID -> QueryParams (Identity UUID),
        updateQuery :: QueryString W (Text, UUID) (), 
        updateQueryParams :: UUID -> Text -> QueryParams (Text, UUID),
        saveQuery :: QueryString W (UUID, Text) (), 
        saveQueryParams :: UUID -> Text -> QueryParams (UUID, Text)}

new :: MonadIO m => m CassandraStateRepository
new = do 
    logger <- Logger.new Logger.defSettings 
    connection <- Client.init logger Client.defSettings 
    return $ CassandraStateRepository connection findByIdQuery findByIdQueryParams existsQuery existsQueryParams updateQuery updateQueryParams saveQuery saveQueryParams
   where

    findByIdQuery = QueryString . LT.pack $ "SELECT * from alvis_test.stored_system_state WHERE id = ?"
    findByIdQueryParams uuid = defQueryParams One (Identity uuid)

    existsQuery = QueryString . LT.pack $ "SELECT id from alvis_test.stored_system_state WHERE id = ?"
    existsQueryParams uuid = defQueryParams One (Identity uuid)

    updateQuery = QueryString . LT.pack $ "UPDATE  alvis_test.stored_system_state SET value = ? WHERE id = ?"
    updateQueryParams uuid value = defQueryParams One (value, uuid)

    saveQuery = QueryString . LT.pack $ "INSERT INTO alvis_test.stored_system_state (id, value) VALUES (?, ?)"
    saveQueryParams uuid value = defQueryParams One (uuid, value)

destroy :: MonadIO m => CassandraStateRepository -> m ()
destroy (CassandraStateRepository connection _ _ _ _ _ _ _ _) = shutdown connection

instance StateRepository (CassandraStateRepository) where
  findById id (CassandraStateRepository connection findByIdQuery findByIdQueryParams _ _ _ _ _ _) = do
    let params = findByIdQueryParams id
    maybeRow <- runClient connection (query1 findByIdQuery params)   
    return . rightToMaybe . toState . maybeToRight "the state was not found" $ maybeRow

  exists id (CassandraStateRepository connection _ _ existsQuery existsQueryParams _ _ _ _) = do
    let params = existsQueryParams id
    maybeRow <- runClient connection (query1 existsQuery params)   
    return . isJust $ maybeRow

  save (State uuid value) (CassandraStateRepository connection _ _ _ _ updateQuery updateQueryParams saveQuery saveQueryParams) = do
    case Uuid.null uuid of
      True -> do
        newUuid <- generateUuid
        let params = saveQueryParams newUuid value
        runClient connection (write saveQuery params)   
      False -> do
        let params = updateQueryParams uuid value
        runClient connection (write updateQuery params)   

generateUuid :: MonadIO m => m UUID
generateUuid = liftIO $ Rand.getStdRandom Rand.random

fromJust :: Maybe a -> a
fromJust (Just v) = v
fromJust Nothing = error "it is not possible to extract value from Nothing"
  
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
