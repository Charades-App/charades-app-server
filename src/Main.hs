module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson
import           Data.List                 (find)
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import           Data.UUID                 (UUID)
import qualified Data.UUID.V4              as UUID
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status (ok200)
import           Web.Scotty                hiding (Handler)

newtype UserName = MkUsername Text

newtype UserId = MkUserId UUID
  deriving newtype (Eq, FromJSON, ToJSON)

newtype RoomId = MkRoomId UUID
  deriving newtype (Eq, FromJSON, ToJSON, Read)
instance Parsable RoomId where parseParam = readEither

newtype OwnerId = MkOwnerId UUID
  deriving newtype (Eq, FromJSON, ToJSON)

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

type Handler a r = a -> Server -> IO (Server, r)

-- * API Endpoints

-- * Admin Endpoints

-- | Create a room
-- This endpoint needs the owner's username and returns a generated UserId and RoomId.
-- It also adds the created room to the server and the owner to the room's and server's player list.
createRoom :: Handler NewRoom NewRoomResponse
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
-- Just get the room ID from the response of the create room endpoint.

-- | Set player for current round
-- This endpoint needs the room ID and the player ID, as well as the owner ID for access control.
setPlayerForRound :: Handler SetPlayerForRound ()
setPlayerForRound (MkSetPlayerForRound roomId' userId' ownerId) srv = do
  let
    -- Check that room exists
    rooms = filter ((==roomId') . roomId . snd) (serverRooms srv)
  case map snd rooms of
    [room] | isJust $ lookup ownerId rooms {- Check that owner is valid -} ->
      let
          -- Check that user is playing in the room
          player = find ((==userId') . playerId) (roomPlayers room)
          -- Update the room's current player
          newRoom = room { roomCurrentPlayer = player }
          -- NOTE: Here we are being _extra_ lazy and just appending to the list of rooms.
          -- This can lead to running out of memory. We're also relying on lookup to scan the list from head to tail.
          srv' = srv { serverRooms = (ownerId, newRoom) : serverRooms srv } in
      return (srv', ())
    _ -> return (srv, ())

data SetPlayerForRound = MkSetPlayerForRound
  { setPlayerRoomId  :: !RoomId
  , setPlayerUserId  :: !UserId
  , setPlayerOwnerId :: !OwnerId
  }

data SetPlayerForRoundR = MkSetPlayerForRoundR
  { setPlayerUserIdR  :: !UserId
  , setPlayerOwnerIdR :: !OwnerId
  }
  deriving stock Generic
  deriving anyclass FromJSON

-- Set word for current round

-- Start current round
-- This endpoint does nothing: the players will automatically see updates to the room status.
-- NOTE: This means an owner MUST set the current player before settting the current word.

-- Stop current round
-- Same as starting a round, this endpoint does nothing.

-- * Player Endpoints

-- Join room

-- See who is the current player
-- If not current player, see current word
-- Let's fuse this endpoints and just return the room state.

main :: IO ()
main = do
  srv <- emptyServer

  scotty 3000 $ do
    get "/api/v1/health" $ status ok200

    post "/api/v1/rooms" $ do
      newRoom <- jsonData
      response <- liftIO $ withServer srv (createRoom newRoom)
      json response

    post "/api/v1/rooms/:roomId/players/current" $ do
      roomId' <- pathParam "roomId"
      (MkSetPlayerForRoundR playerId ownerId) <- jsonData
      response <- liftIO $ withServer srv (setPlayerForRound (MkSetPlayerForRound roomId' playerId ownerId))
      json response

withServer :: MonadIO m => (TVar Server) -> (Server -> m (Server, a)) -> m a
withServer srvT action = do
  srv <- liftIO $ readTVarIO srvT
  (srv', result) <- action srv
  liftIO $ atomically $ writeTVar srvT srv'
  return result
