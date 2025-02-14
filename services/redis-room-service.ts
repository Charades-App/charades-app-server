import { type RoomService } from './room-service.ts'
import { type IdGenerationService } from './id-generation-service.ts'
import { type RedisClient } from '../types/redis.ts';
import { Username } from "../types/username.ts";

export class RedisRoomService implements RoomService {
  constructor(
    private redis: RedisClient,
    private idGen: IdGenerationService,
  ) { }

  async createRoom(username: Username): Promise<string> {
    const roomId = await this.idGen.generateId();
    const key = `rooms:${roomId}:owner`
    await this.redis.set(key, username.username);
    return roomId;
  }
}
