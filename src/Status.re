type code =
  | Ok200
  | Created201
  | Accepted202
  | NonAuthoritative203
  | NoContent204
  | ResetContent205
  | Partial206
  | MultipleChoices300
  | MovedTemporarily301
  | Found302
  | SeeOther303
  | NotModified304
  | UseProxy305
  | TemporaryRedirect307
  | PermanentRedirect308
  | BadRequest400
  | Unauthorized401
  | PaymentRequired402
  | Forbidden403
  | NotFound404
  | MethodNotAllowed405
  | NotAcceptable406
  | ProxyAuthenticationRequired407
  | RequestTimeout408
  | Conflic409
  | Gone410
  | LengthRequired411
  | PreconditionFailed412
  | PayloadTooLarge413
  | RequestURITooLarge414
  | UnsupportedMediaType415
  | RequestedRangeNotSatisfied416
  | ExpectationFailed417
  | ImATeapot418
  | Error500
  | NotImplemented501
  | BadGateway502
  | ServiceUnavailable503
  | GatewayTimeout504
  | HTTPVersionNotSupported505
  | BandwithLimitExceeded509;
let toInt = (status: code): int =>
  switch (status) {
  | Ok200 => 200
  | Created201 => 201
  | Accepted202 => 202
  | NonAuthoritative203 => 203
  | NoContent204 => 204
  | ResetContent205 => 205
  | Partial206 => 206
  | MultipleChoices300 => 300
  | MovedTemporarily301 => 301
  | Found302 => 302
  | SeeOther303 => 303
  | NotModified304 => 304
  | UseProxy305 => 305
  | TemporaryRedirect307 => 307
  | PermanentRedirect308 => 308
  | BadRequest400 => 400
  | Unauthorized401 => 401
  | PaymentRequired402 => 402
  | Forbidden403 => 403
  | NotFound404 => 404
  | MethodNotAllowed405 => 405
  | NotAcceptable406 => 406
  | ProxyAuthenticationRequired407 => 407
  | RequestTimeout408 => 408
  | Conflic409 => 409
  | Gone410 => 410
  | LengthRequired411 => 411
  | PreconditionFailed412 => 412
  | PayloadTooLarge413 => 413
  | RequestURITooLarge414 => 414
  | UnsupportedMediaType415 => 415
  | RequestedRangeNotSatisfied416 => 416
  | ExpectationFailed417 => 417
  | ImATeapot418 => 418
  | Error500 => 500
  | NotImplemented501 => 501
  | BadGateway502 => 502
  | ServiceUnavailable503 => 503
  | GatewayTimeout504 => 504
  | HTTPVersionNotSupported505 => 505
  | BandwithLimitExceeded509 => 509
  };

let message = (status: code) =>
  switch (status) {
  | Ok200 => "OK"
  | Created201 => "Created"
  | Accepted202 => "Accepted"
  | NonAuthoritative203 => "Non-Authoritative Information"
  | NoContent204 => "No Content"
  | ResetContent205 => "Reset Content"
  | Partial206 => "Partial Content"
  | MultipleChoices300 => "Multiple Choices"
  | MovedTemporarily301 => "Moved Temporarily"
  | Found302 => "Found"
  | SeeOther303 => "See Other"
  | NotModified304 => "Not Modified"
  | UseProxy305 => "Use Proxy"
  | TemporaryRedirect307 => "Temporary Redirect"
  | PermanentRedirect308 => "Permanent Redirect"
  | BadRequest400 => "Bad Request"
  | Unauthorized401 => "Unauthorized"
  | PaymentRequired402 => "Payment Required"
  | Forbidden403 => "Forbidden"
  | NotFound404 => "Not Found"
  | MethodNotAllowed405 => "Method Not Allowed"
  | NotAcceptable406 => "Not Acceptable"
  | ProxyAuthenticationRequired407 => "Proxy Authentication Required"
  | RequestTimeout408 => "Request Timeout"
  | Conflic409 => "Conflict"
  | Gone410 => "Gone"
  | LengthRequired411 => "Length Required"
  | PreconditionFailed412 => "Precondition Failed"
  | PayloadTooLarge413 => "Payload Too Large"
  | RequestURITooLarge414 => "Request-URI Too Large"
  | UnsupportedMediaType415 => "Unsupported Media Type"
  | RequestedRangeNotSatisfied416 => "Requested Range Not Satisfied"
  | ExpectationFailed417 => "Expectation Failed"
  | ImATeapot418 => "I'm a teapot"
  | Error500 => "Internal Server Error"
  | NotImplemented501 => "Not Implemented"
  | BadGateway502 => "Bad Gateway"
  | ServiceUnavailable503 => "Service Unavailable"
  | GatewayTimeout504 => "Gateway Timeout"
  | HTTPVersionNotSupported505 => "HTTP Version Not Supported"
  | BandwithLimitExceeded509 => "Bandwith Limit Exceeded"
  };