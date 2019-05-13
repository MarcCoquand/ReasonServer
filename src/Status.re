module Success = {
  type code =
    | Status200
    | Status201
    | Status202
    | Status203
    | Status204
    | Status205
    | Status206
    | Status300
    | Status301
    | Status302
    | Status303
    | Status304
    | Status305
    | Status307
    | Status308;
  let toInt = (status: code): int =>
    switch (status) {
    | Status200 => 200
    | Status201 => 201
    | Status202 => 202
    | Status203 => 203
    | Status204 => 204
    | Status205 => 205
    | Status206 => 206
    | Status300 => 300
    | Status301 => 301
    | Status302 => 302
    | Status303 => 303
    | Status304 => 304
    | Status305 => 305
    | Status307 => 307
    | Status308 => 308
    };
  let message = (status: code) =>
    switch (status) {
    | Status200 => "OK"
    | Status201 => "Created"
    | Status202 => "Accepted"
    | Status203 => "Non-Authoritative Information"
    | Status204 => "No Content"
    | Status205 => "Reset Content"
    | Status206 => "Partial Content"
    | Status300 => "Multiple Choices"
    | Status301 => "Moved Temporarily"
    | Status302 => "Found"
    | Status303 => "See Other"
    | Status304 => "Not Modified"
    | Status305 => "Use Proxy"
    | Status307 => "Temporary Redirect"
    | Status308 => "Permanent Redirect"
    };
};

module Failure = {
  type code =
    | Status400
    | Status401
    | Status402
    | Status403
    | Status404
    | Status405
    | Status406
    | Status407
    | Status408
    | Status409
    | Status410
    | Status411
    | Status412
    | Status413
    | Status414
    | Status415
    | Status416
    | Status417
    | Status418
    | Status500
    | Status501
    | Status502
    | Status503
    | Status504
    | Status505
    | Status509
  let toInt = (status: code): int =>
    switch (status) {
    | Status400 => 400
    | Status401 => 401
    | Status402 => 402
    | Status403 => 403
    | Status404 => 404
    | Status405 => 405
    | Status406 => 406
    | Status407 => 407
    | Status408 => 408
    | Status409 => 409
    | Status410 => 410
    | Status411 => 411
    | Status412 => 412
    | Status413 => 413
    | Status414 => 414
    | Status415 => 415
    | Status416 => 416
    | Status417 => 417
    | Status418 => 418
    | Status500 => 500
    | Status501 => 501
    | Status502 => 502
    | Status503 => 503
    | Status504 => 504
    | Status505 => 505
    | Status509 => 509
    };

  let message = (status: code): string =>
    switch (status) {
    | Status400 => "Bad Request"
    | Status401 => "Unauthorized"
    | Status402 => "Payment Required"
    | Status403 => "Forbidden"
    | Status404 => "Not Found"
    | Status405 => "Method Not Allowed"
    | Status406 => "Not Acceptable"
    | Status407 => "Proxy Authentication Required"
    | Status408 => "Request Timeout"
    | Status409 => "Conflict"
    | Status410 => "Gone"
    | Status411 => "Length Required"
    | Status412 => "Precondition Failed"
    | Status413 => "Payload Too Large"
    | Status414 => "Request-URI Too Large"
    | Status415 => "Unsupported Media Type"
    | Status416 => "Requested Range Not Satisfied"
    | Status417 => "Expectation Failed"
    | Status418 => "I'm a teapot"
    | Status500 => "Internal Server Error"
    | Status501 => "Not Implemented"
    | Status502 => "Bad Gateway"
    | Status503 => "Service Unavailable"
    | Status504 => "Gateway Timeout"
    | Status505 => "HTTP Version Not Supported"
    | Status509 => "Bandwith Limit Exceeded"
    };
};

type code = Success(Success.code) | Fail(Failure.code);

let fail = (code) => Fail(code)

let toInt = code => switch(code){
    | Success(code) => Success.toInt(code)
    | Fail(code) => Failure.toInt(code)
    }


let message = code => switch(code){
    | Success(code) => Success.message(code)
    | Fail(code) => Failure.message(code)
    }
