type t =
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

let fromString = (code: string) =>
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
