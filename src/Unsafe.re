module Request = {
  [@bs.deriving {jsConverter: newType}]
  type t = {
    body: string,
    path: string,
    headers: Js.Dict.t(string),
    statusCode: int,
    method: string,
  };
};

module Response = {
  [@bs.deriving {jsConverter: newType}]
  type t = {
    bodyString: option(string),
    bodyBuffer: option(Node.Buffer.t),
    headers: Js.Array.t((string, string)),
    statusCode: int,
    encoding: string,
    statusMessage: string
  };
};

type application = Request.abs_t => Response.abs_t;

[@bs.module "./rawCreateServer.js"] [@bs.val] external server : int => application => unit = "rawCreateServer";

[@bs.module "./rawCreateServer.js"] [@bs.val] external secureServer : int => string => string => application => unit = "rawSecureCreateServer";
