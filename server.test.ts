import { expect, test } from 'bun:test';
import { server } from "./index.ts"
import { SessionData } from './controllers/session-data.ts'
import { faker } from "@faker-js/faker"

test("creating a room returns the new room ID and sets the session cookie", async () => {
  const username = faker.internet.username();
  const url = new URL('/api/rooms', server.url);

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify({ username })
  })

  const resBody = await res.json();
  const sessionData = extractSessionData(res);

  expect(res.status).toBe(201);
  expect(sessionData).not.toBeNull();
  expect(sessionData!.username).toBe(username);
  expect(resBody).toContainKeys(['roomId']);
})

test("joining a room that doesn't exist returns an error", async () => {
  const username = faker.internet.username();
  const roomId = faker.string.uuid();
  const url = new URL('/api/rooms/join', server.url);
  url.searchParams.append('roomId', roomId);

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify({ username })
  })

  expect(res.status).toBe(400);
})

test("joining a room sets the session cookie", async () => {
  const [owner, roomId] = await createRoom();
  const joiner = faker.internet.username();

  const url = new URL('/api/rooms/join', server.url);
  url.searchParams.append('roomId', roomId);

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify({ username: joiner })
  })

  const sessionData = extractSessionData(res);

  expect(res.status).toBe(200);
  expect(sessionData).not.toBeNull();
  expect(sessionData!.username).toBe(joiner);
  expect(sessionData!.roomId).toBe(roomId);
})

test.todo("fetching members for a non-existent room returns an empty list", async () => { })

test.todo("fetching members for an existing room returns the list of members", async () => { })

const createRoom = async (): Promise<[string, string]> => {
  const username = faker.internet.username();
  const url = new URL('/api/rooms', server.url);

  const res = await fetch(url, {
    method: 'POST',
    body: JSON.stringify({ username })
  })

  const { roomId } = await res.json();

  return [username, roomId];
}

const extractSessionData = (res: Response): SessionData | null => {
  const cookie = res.headers
    .getSetCookie()
    .map(setCookie => setCookie.split("="))
    .find(cookie => cookie[0] === SessionData.SESSION_COOKIE_NAME)
    ?.[1];
  if (cookie) {
    const decoded = Buffer.from(decodeURIComponent(cookie), 'base64').toString();
    const { username, roomId } = JSON.parse(decoded);
    return new SessionData(username, roomId);
  } else {
    return null;
  }
}
