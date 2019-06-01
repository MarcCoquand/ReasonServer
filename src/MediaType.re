type t =
  | Html
  | Json
  | Plain
  | Xml
  | Other(string);

let toString = mediaType =>
  switch (mediaType) {
  | Html => "application/html"
  | Xml => "application/xml"
  | Plain => "text/plain"
  | Json => "application/json"
  | Other(s) => s
  };
let fromString = str =>
  switch (str) {
  | "application/html" => Html
  | "application/xml" => Xml
  | "text/plain" => Plain
  | "application/json" => Json
  | s => Other(s)
  };