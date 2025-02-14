import { type RoomService } from "../services/room-service.ts"
import { Username } from "../types/username.ts"

export type CreateRoom = {
  username: string;
}

export class RoomController {
  constructor(private roomService: RoomService) { }

  async createRoom(req: Request): Promise<Response> {
    const { username } = await req.json() as CreateRoom
    const roomId = await this.roomService.createRoom(new Username(username));
    return new Response(JSON.stringify({ roomId }), {
      status: 201,
      headers: {
        'Content-Type': 'application/json'
      }
    });
  }
}
