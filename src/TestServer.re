open Server

let myApp = request: Response.t =>
    Builder.start(request)
    |>Builder.sendText("Hello world")


start(3000,myApp)


