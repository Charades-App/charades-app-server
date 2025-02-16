import { RoomController } from './controllers/rooms.ts'
import { RedisRoomService } from './services/redis-room-service.ts'
import { UuidGenerationService } from './services/uuid-generation-service.ts'

import { createClient } from "redis";

const redisClient = await createClient()
  // TODO: Use a proper logging library.
  .on('error', (err) => console.log('redis error', err))
  .connect();

const idGenerator = new UuidGenerationService();

const roomService = new RedisRoomService(redisClient, idGenerator);
const roomController = new RoomController(roomService);

const server = Bun.serve({
  idleTimeout: 60,
  async fetch(req: Request) {
    const url = new URL(req.url);
    if (req.method === "POST" && url.pathname === "/api/rooms") {
      return await roomController.createRoom(req);
    } else if (req.method === "POST" && url.pathname === "/api/rooms/join") {
      const roomId = url.searchParams.get('roomId');
      if (roomId !== null) {
        return await roomController.joinRoom(roomId, req);
      }
    }
    return new Response(null, { status: 404 });
  },
  error: (err) => {
    // TODO: Put some proper logging here.
    console.log('server error:', err)
    if (err.name === "SyntaxError") {
      return new Response(JSON.stringify({
        error: "Bad payload"
      }), {
        status: 400,
        headers: {
          'Content-Type': 'application/json'
        }
      });
    }
    return new Response(null, { status: 500 });
  }
})

console.log(`server running on port: ${server.port}`)
