import { Username } from "../types/username.ts";

/// Allow operations on rooms.
export interface RoomService {
  /// Creates a new room and associates it with the
  /// given username.
  /// @return The ID of the created room.
  createRoom(username: Username): Promise<string>;

  /// Attempt to join a new user to the specified room.
  /// Returns an error if the room doesn't exist, and null if successful.
  joinRoom(username: Username, roomId: string): Promise<string | null>;
}
