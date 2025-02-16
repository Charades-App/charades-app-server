import { type RoomService } from './room-service.ts'
import { type IdGenerationService } from './id-generation-service.ts'
import { type RedisClient } from '../types/redis.ts';
import { Username } from "../types/username.ts";

export class RedisRoomService implements RoomService {
  constructor(
    private redis: RedisClient,
    private idGen: IdGenerationService,
  ) { }

  async joinRoom(username: Username, roomId: string): Promise<string | null> {
    const key = `rooms:${roomId}:owner`;

    const roomExists = await this.redis.exists(key);
    if (!roomExists) {
      return 'A room with the given ID was not found';
    }

    const membersSetKey = `rooms:${roomId}:members`;
    const usernameTaken = await this.redis.sIsMember(membersSetKey, username.username);
    if (usernameTaken) {
      return 'That username is already taken';
    }

    await this.redis.sAdd(membersSetKey, username.username);
    return null;
  }

  async createRoom(username: Username): Promise<string> {
    const roomId = await this.idGen.generateId();
    const key = `rooms:${roomId}:owner`
    await this.redis.set(key, username.username);
    await this.joinRoom(username, roomId);
    return roomId;
  }
}
