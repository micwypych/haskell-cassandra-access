module Lib
    ( someFunc,
      connectToCassandraRepositoryAndFindStates
    ) where

import qualified Repository
import qualified Data.UUID as Uuid
import Data.Text (Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

connectToCassandraRepositoryAndFindStates :: IO ()
connectToCassandraRepositoryAndFindStates = do
    repository <- Repository.new
    first <- Repository.findById (uuid "35f32256-9217-4475-bd99-fc8c3ef225e5") repository
    second <- Repository.findById (uuid "a343ef97-da23-4f58-bf5c-cd9d33224e12") repository    
    print first
    print second
    existsSecond <- Repository.exists (uuid "a343ef97-da23-4f58-bf5c-cd9d33224e12") repository  
    print existsSecond
    let updatedSecond = withValue "nowa wartosc ktora dopiero teraz powinna byc widoczna" (fromJust second)
    Repository.save updatedSecond repository
    Repository.destroy repository


fromJust :: Maybe a -> a
fromJust (Just v) = v
fromJust Nothing = error "it is not possible to extract value from Nothing"

uuid :: String -> Uuid.UUID
uuid = fromJust . Uuid.fromString

withValue :: Text -> Repository.State -> Repository.State
withValue newValue (Repository.State uuid _) = Repository.State uuid newValue
