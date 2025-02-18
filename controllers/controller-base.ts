export class ControllerBase {
  jsonResponse<T>(data: T, statusCode: number = 200, headers: Headers = new Headers()): Response {
    headers.set('Content-Type', 'application/json');
    return new Response(JSON.stringify(data), {
      status: statusCode,
      headers,
    })
  }
}
