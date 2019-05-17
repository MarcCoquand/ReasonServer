type t = {
  body: string,
  path: string,
  headers: Header.Map.t(string),
  method: HttpMethod.t,
  isSecure: bool,
};