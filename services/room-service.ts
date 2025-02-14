import { Username } from "../types/username.ts";

/// Allow operations on rooms.
export interface RoomService {
  /// Creates a new room and associates it with the
  /// given username.
  /// @return The ID of the created room.
  createRoom(username: Username): Promise<string>;
}
