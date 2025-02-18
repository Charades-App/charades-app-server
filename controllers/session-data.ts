import cookie from "cookie"

export class SessionData {
  public static SESSION_COOKIE_NAME = "charades-session";

  constructor(
    public readonly username: string,
    public readonly roomId: string
  ) { }

  static fromRequest(req: Request): SessionData | null {
    const cookie = req.headers.get("Cookie")
      ?.split(";")
      .map(cookie => cookie.split("="))
      .find(cookie => cookie.length === 2 && cookie[0] === SessionData.SESSION_COOKIE_NAME);

    if (cookie) {
      const data = cookie[1];
      const decodedCookie = Buffer.from(decodeURIComponent(data), 'base64').toString();
      const sessionData = JSON.parse(decodedCookie);
      return sessionData;
    } else {
      return null;
    }
  }

  asHeaders(): Headers {
    const headers = new Headers();
    const sessionData = JSON.stringify({ username: this.username, roomId: this.roomId });
    const encoded = Buffer.from(sessionData).toString('base64');
    headers.set('Set-Cookie', cookie.serialize(SessionData.SESSION_COOKIE_NAME, encoded))
    return headers;
  }
}
