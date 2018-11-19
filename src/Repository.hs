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
import qualified System.Logger as Logger

data State = State UUID Text 


toState :: Either String Row -> Either String State
toState monadicRow = do
     row <- monadicRow
     uuid <- fromRow 0 row
     value <- fromRow 1 row
     return $ State uuid value


class StateRepository repository where
  save :: State -> repository -> State
  exists :: State -> repository -> Bool
  findById :: MonadIO m =>  UUID -> repository -> m (Maybe State)
  

data CassandraStateRepository = CassandraStateRepository {connection::ClientState, findByIdQuery :: QueryString R (Identity UUID) (Row), toFindByIdQueryParams :: UUID -> QueryParams (Identity UUID)}

new :: MonadIO m => m CassandraStateRepository
new = do 
    logger <- Logger.new Logger.defSettings 
    connection <- Client.init logger Client.defSettings 
    return $ CassandraStateRepository connection findByIdQuery findByIdQueryParams
   where
    findByIdQuery = QueryString . LT.pack $ "select * from alvis.stored_system_state WHERE id = ?"
    findByIdQueryParams uuid = defQueryParams One (Identity uuid)

instance StateRepository (CassandraStateRepository) where
  findById id (CassandraStateRepository connection findByIdQuery findByIdQueryParams) = do
    let params = findByIdQueryParams id
    maybeRow <- runClient connection (query1 findByIdQuery params)
    return . rightToMaybe . toState . maybeToRight "the state was not found" $ maybeRow
  

