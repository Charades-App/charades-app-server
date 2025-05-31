module Main where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.UUID                   (UUID)
import qualified Data.UUID.V4                as UUID
import           GHC.Generics                (Generic)
import           Network.HTTP.Types.Status   (ok200)
import           Web.Scotty

newtype UserName = MkUsername Text
newtype UserId = MkUserId UUID
  deriving newtype ToJSON
newtype RoomId = MkRoomId UUID
  deriving newtype ToJSON
newtype OwnerId = MkOwnerId UUID

data Player = MkPlayer
  { playerUsername :: !UserName
  , playerId       :: !UserId
  }

data Room = MkRoom
  { roomId            :: !RoomId
  , roomOwnerId       :: !UserId
  , roomPlayers       :: ![Player]
  , roomCurrentPlayer :: !(Maybe Player)
  , roomCurrentWord   :: !(Maybe String)
  }

data Server = MkServer
  { serverRooms   :: ![(OwnerId, Room)]
  , serverPlayers :: ![Player]
  }

emptyServer :: IO (TVar Server)
emptyServer = newTVarIO $ MkServer [] []

-- * API Endpoints

-- * Admin Endpoints

-- | Create a room
-- This endpoint needs the owner's username and returns a generated UserId and RoomId.
-- It also adds the created room to the server and the owner to the room's and server's player list.
createRoom :: NewRoom -> Server -> IO (Server, NewRoomResponse)
createRoom (MkNewRoom ownerUsername) server = do
  ownerId@(MkUserId rawId) <- MkUserId <$> UUID.nextRandom
  roomId' <- MkRoomId <$> UUID.nextRandom
  let userName = MkUsername ownerUsername
      owner = MkPlayer userName ownerId
      room = MkRoom roomId' ownerId [owner] Nothing Nothing
      server' = server
                { serverRooms = (MkOwnerId rawId, room) : serverRooms server
                , serverPlayers = owner : serverPlayers server
                }
  return (server', MkNewRoomResponse roomId' ownerId)

data NewRoom = MkNewRoom { newRoomOwner :: !Text }
  deriving stock Generic
  deriving anyclass FromJSON

data NewRoomResponse = MkNewRoomResponse
  { newRoomId      :: !RoomId
  , newRoomOwnerId :: !UserId
  }
  deriving stock Generic
  deriving anyclass ToJSON

-- Share room link

-- Set player for current round

-- Set word for current round

-- Start current round

-- Stop current round

-- * Player Endpoints

-- Join room

-- See who is the current player

-- If not current player, see current word

main :: IO ()
main = do
  srv <- emptyServer

  scotty 3000 $ do
    get "/api/v1/health" $ status ok200

    post "/api/v1/rooms" $ do
      newRoom <- jsonData
      response <- liftIO $ withServer srv (createRoom newRoom)
      json response


withServer :: MonadIO m => (TVar Server) -> (Server -> m (Server, a)) -> m a
withServer srvT action = do
  srv <- liftIO $ readTVarIO srvT
  (srv', result) <- action srv
  liftIO $ atomically $ writeTVar srvT srv'
  return result
