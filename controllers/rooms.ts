import { type RoomService } from "../services/room-service.ts"
import { Username } from "../types/username.ts"
import cookie from "cookie"
import { ControllerBase } from "./controller-base.ts"

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
    const headers = this.mkResponseHeaders(username, roomId);
    return new Response(JSON.stringify({ roomId }), {
      status: 201,
      headers,
    });
  }

  async joinRoom(roomId: string, req: Request): Promise<Response> {
    const { username } = await req.json() as JoinRoom;
    const error = await this.roomService.joinRoom(new Username(username), roomId);
    if (error !== null) {
      return new Response(JSON.stringify({ error }), {
        status: 400,
        headers: {
          'Content-Type': 'application/json'
        }
      })
    } else {
      return new Response(null, {
        headers: this.mkResponseHeaders(username, roomId)
      });
    }
  }

  async getMembers(req: Request, roomId: string): Promise<Response> {
    const members = await this.roomService.getMembers(roomId);
    return this.jsonResponse(members);
  }

  private mkResponseHeaders(username: string, roomId: string): Headers {
    const headers = new Headers();
    const sessionInfo = btoa(JSON.stringify({ username, roomId }))
    headers.set('Content-Type', 'application/json');
    headers.set('Set-Cookie', cookie.serialize('charades-session', sessionInfo))
    return headers;
  }
}
