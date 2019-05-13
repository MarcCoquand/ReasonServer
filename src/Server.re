type method =
  | GET
  | POST
  | PUT
  | UPDATE
  | DELETE
  | HEAD
  | OPTION
  | CONNECT
  | TRACE
  | PATCH;

let convertNodeMethod = (code: string) =>
  switch (code) {
  | "GET" => Some(GET)
  | "POST" => Some(POST)
  | "PUT" => Some(PUT)
  | "UPDATE" => Some(UPDATE)
  | "DELETE" => Some(DELETE)
  | "HEAD" => Some(HEAD)
  | "OPTION" => Some(OPTION)
  | "CONNECT" => Some(CONNECT)
  | "TRACE" => Some(TRACE)
  | "PATCH" => Some(PATCH)
  | _ => None
  };

module Header =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type request = {
  body: string,
  path: string,
  headers: Header.t(string),
  method: method,
};

type application('a) = request => Response.response;

let makeHeaders = (headers: Header.t('a)): Js.Dict.t(string) =>
    Header.bindings(headers)->Js.Dict.fromList

let convertResponse = (safeRes: Response.response): Node.unsafeResponse('a) =>
    ({
        bodyString: safeRes.body,
        bodyBuffer: None,
        headers: makeHeaders(safeRes.headers),
        statusCode: safeRes.code->Status.toInt,
        encoding: safeRes.encoding->Encoding.toString,
        statusMessage: string
    })
let handleApplication =
    (nodeReq: Node.unsafeRequest('a), app: application('b))
    : Node.unsafeApplication('a, 'b) => {
  let convertedHeaders: Header.t(string) =
    Js.Dict.entries(nodeReq.headers)->Array.reduce(
      (dict, el) =>
        Header.add(
          Js_array.unsafe_get(el, 0),
          Js_array.unsafe_get(el, 1),
          dict,
        ),
      Header.empty,
    );
  let maybeMethod = convertNodeMethod(nodeReq.method)
  switch(maybeMethod) {
    | Some(method) => app({
        body: nodeReq.body,
        path: nodeReq.path,
        headers: convertedHeaders,
        method: method
    });
    | None => {
        body: nodeReq.body,
        path: nodeReq.path,
        headers: convertedHeaders,
        method: GET
    }
  }


  }

let create = (port: int, server: application) => {};