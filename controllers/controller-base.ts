export class ControllerBase {

  jsonResponse<T>(data: T, statusCode: number = 200): Response {
    return new Response(JSON.stringify(data), {
      status: statusCode,
      headers: {
        'Content-Type': 'application/json'
      }
    })
  }

}
