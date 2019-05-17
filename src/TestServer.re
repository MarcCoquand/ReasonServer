open Server;

let myApp = (request): Response.t =>
  Builder.start(request) |> Builder.sendText("Hello");