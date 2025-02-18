import cookie from "cookie"

export class SessionData {
  public static SESSION_COOKIE_NAME = "charades-session";

  constructor(
    public readonly username: string,
    public readonly roomId: string
  ) { }

  static fromRequest(req: Request): SessionData | null {
    const cookies = req.headers.get("Cookie")
    if (!cookies) return null;

    const cookie = (cookies.includes(';') ? cookies.split(";") : [cookies])
      .map(cookie => cookie.split("="))
      .find(cookie => cookie.length === 2 && cookie[0] === SessionData.SESSION_COOKIE_NAME);
    if (!cookie) return null;

    const data = cookie[1];
    const decodedCookie = Buffer.from(decodeURIComponent(data), 'base64').toString();
    const sessionData = JSON.parse(decodedCookie);
    return sessionData;
  }

  toCookie(): string {
    const sessionData = JSON.stringify({ username: this.username, roomId: this.roomId });
    const encoded = Buffer.from(sessionData).toString('base64');
    return cookie.serialize(SessionData.SESSION_COOKIE_NAME, encoded);
  }

  asHeaders(): Headers {
    const headers = new Headers();
    headers.set('Set-Cookie', this.toCookie())
    return headers;
  }
}
