import { type RoomService } from "../services/room-service.ts"
import { Username } from "../types/username.ts"
import { ControllerBase } from "./controller-base.ts"
import { SessionData } from "./session-data.ts"

export type CreateRoom = {
  username: string;
}

export type JoinRoom = {
  username: string;
}

export class RoomController extends ControllerBase {
  constructor(private roomService: RoomService) {
    super();
  }

  async createRoom(req: Request): Promise<Response> {
    const { username } = await req.json() as CreateRoom
    const roomId = await this.roomService.createRoom(new Username(username));
    const headers = new SessionData(username, roomId).asHeaders();
    return this.jsonResponse({ roomId }, 201, headers);
  }

  async joinRoom(roomId: string, req: Request): Promise<Response> {
    const { username } = await req.json() as JoinRoom;
    const error = await this.roomService.joinRoom(new Username(username), roomId);
    if (error !== null) {
      return this.jsonResponse({ error }, 400);
    } else {
      const headers = new SessionData(username, roomId).asHeaders();
      return this.jsonResponse(null, 200, headers);
    }
  }

  async getMembers(req: Request, roomId: string): Promise<Response> {
    const sessionData = SessionData.fromRequest(req);
    if (sessionData !== null) {
      const result = await this.roomService.getMembers(sessionData.username, roomId);
      switch (result) {
        case "NOT_ROOM_OWNER":
          return new Response(null, { status: 403 });
        default:
          return this.jsonResponse(result);
      }
    } else {
      return new Response(null, { status: 401 });
    }
  }
}
