// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var $$Map = require("bs-platform/lib/js/map.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Router$Cause = require("./Router.bs.js");
var Status$Cause = require("./Status.bs.js");
var Unsafe$Cause = require("./Unsafe.bs.js");
var Encoding$Cause = require("./Encoding.bs.js");
var HttpMethod$Cause = require("./HttpMethod.bs.js");

var compare = Caml_obj.caml_compare;

var Header = $$Map.Make(/* module */[/* compare */compare]);

var $$Request = /* module */[];

function fail(code, header, message) {
  return /* record */[
          /* code */Status$Cause.fail(code),
          /* headers */header,
          /* body */message,
          /* encoding : Utf8 */7
        ];
}

function makeUnsafeHeaders(headers) {
  return $$Array.of_list(Curry._3(Header[/* fold */10], (function (key, value, l) {
                    return /* :: */[
                            /* tuple */[
                              key,
                              value
                            ],
                            l
                          ];
                  }), headers, /* [] */0));
}

function convert(safeRes) {
  return Unsafe$Cause.$$Response[/* tToJs */0](/* record */[
              /* bodyString */safeRes[/* body */2],
              /* bodyBuffer */undefined,
              /* headers */makeUnsafeHeaders(safeRes[/* headers */1]),
              /* statusCode */Status$Cause.toInt(safeRes[/* code */0]),
              /* encoding */Encoding$Cause.toString(safeRes[/* encoding */3]),
              /* statusMessage */Status$Cause.message(safeRes[/* code */0])
            ]);
}

var $$Response = /* module */[
  /* fail */fail,
  /* makeUnsafeHeaders */makeUnsafeHeaders,
  /* convert */convert
];

function start(request) {
  return /* Constructing */Block.__(0, [
            request[/* headers */2],
            request[/* body */0]
          ]);
}

function failWith(code, header, message) {
  return /* Finish */Block.__(1, [fail(code, header, message)]);
}

function andThen(applyWith, $staropt$star, failureMessage, $staropt$star$1, theValue) {
  var failureCode = $staropt$star !== undefined ? $staropt$star : /* Status500 */19;
  var failureContentType = $staropt$star$1 !== undefined ? $staropt$star$1 : "text/plain";
  if (theValue.tag) {
    return /* Finish */Block.__(1, [theValue[0]]);
  } else {
    var result = Curry._1(applyWith, theValue[1]);
    if (result !== undefined) {
      return /* Constructing */Block.__(0, [
                theValue[0],
                Caml_option.valFromOption(result)
              ]);
    } else {
      return failWith(failureCode, Curry._2(Header[/* singleton */4], "Content-Type", failureContentType), failureMessage);
    }
  }
}

function map(mapWith, theValue) {
  if (theValue.tag) {
    return /* Finish */Block.__(1, [theValue[0]]);
  } else {
    return /* Constructing */Block.__(0, [
              theValue[0],
              Curry._1(mapWith, theValue[1])
            ]);
  }
}

function setHeader(headerType, value, calc) {
  if (calc.tag) {
    return /* Finish */Block.__(1, [calc[0]]);
  } else {
    return /* Constructing */Block.__(0, [
              Curry._3(Header[/* add */3], headerType, value, calc[0]),
              calc[1]
            ]);
  }
}

function setContentType(contentType, calc) {
  return setHeader("Content-Type", contentType, calc);
}

function setContentLength(contentLength, builder) {
  return setHeader("Content-Length", contentLength, builder);
}

function send(success, $staropt$star, body) {
  var encoding = $staropt$star !== undefined ? $staropt$star : /* Utf8 */7;
  if (body.tag) {
    return body[0];
  } else {
    return /* record */[
            /* code : Success */Block.__(0, [success]),
            /* headers */body[0],
            /* body */body[1],
            /* encoding */encoding
          ];
  }
}

function parse(parser, $staropt$star, failureContentType, calc) {
  var failureMessage = $staropt$star !== undefined ? $staropt$star : "Parsing Failed";
  return andThen(parser, /* Status400 */0, failureMessage, failureContentType, calc);
}

function parseJson(decoder, $staropt$star, $staropt$star$1, calc) {
  var failureMessage = $staropt$star !== undefined ? $staropt$star : "Invalid JSON format.";
  var failureContentType = $staropt$star$1 !== undefined ? $staropt$star$1 : "application/json";
  return parse(decoder, failureMessage, failureContentType, calc);
}

function sendJson($staropt$star, body) {
  var code = $staropt$star !== undefined ? $staropt$star : /* Status200 */0;
  return (function (eta) {
              return send(code, undefined, eta);
            })(setHeader("Content-Type", "application/json", map((function (value) {
                        return value;
                      }), map((function (prim) {
                            return JSON.stringify(prim);
                          }), body))));
}

function sendHtml($staropt$star, body) {
  var code = $staropt$star !== undefined ? $staropt$star : /* Status200 */0;
  return (function (eta) {
              return send(code, undefined, eta);
            })(setHeader("Content-Type", "text/html", body));
}

function sendText(text, $staropt$star, body) {
  var code = $staropt$star !== undefined ? $staropt$star : /* Status200 */0;
  return (function (eta) {
              return send(code, undefined, eta);
            })(setHeader("Content-Type", "text/plain", map((function (param) {
                        return text;
                      }), body)));
}

var Builder = /* module */[
  /* start */start,
  /* failWith */failWith,
  /* andThen */andThen,
  /* map */map,
  /* setHeader */setHeader,
  /* setContentType */setContentType,
  /* setContentLength */setContentLength,
  /* send */send,
  /* parse */parse,
  /* parseJson */parseJson,
  /* sendJson */sendJson,
  /* sendHtml */sendHtml,
  /* sendText */sendText
];

function unsafeHandle(app, nodeReq) {
  var nodeReq$1 = Unsafe$Cause.$$Request[/* tFromJs */1](nodeReq);
  var convertedHeaders = $$Array.fold_left((function (dict, param) {
          return Curry._3(Header[/* add */3], param[0], param[1], dict);
        }), Header[/* empty */0], Js_dict.entries(nodeReq$1[/* headers */2]));
  var maybeMethod = HttpMethod$Cause.fromString(nodeReq$1[/* method */4]);
  if (maybeMethod !== undefined) {
    return convert(Curry._1(app, /* record */[
                    /* body */nodeReq$1[/* body */0],
                    /* path */nodeReq$1[/* path */1],
                    /* headers */convertedHeaders,
                    /* method */maybeMethod,
                    /* isSecure */false
                  ]));
  } else {
    return convert(fail(/* Status400 */0, convertedHeaders, "No method"));
  }
}

function fromRouter(router, $staropt$star, request) {
  var notFound = $staropt$star !== undefined ? $staropt$star : "<h1>404 - Not found</h1>";
  var __x = Router$Cause.parseUrl(router, request[/* method */3], request[/* path */1]);
  var __x$1 = Belt_Option.map(__x, (function (handler) {
          return Curry._1(handler, request);
        }));
  return Belt_Option.getWithDefault(__x$1, fail(/* Status404 */4, Curry._2(Header[/* singleton */4], "Content-Type", "application/html"), notFound));
}

function start$1(port, server) {
  return Unsafe$Cause.server(port, (function (param) {
                return unsafeHandle(server, param);
              }));
}

function startSecure(port, keyFilepath, certificateFilepath, server) {
  return Unsafe$Cause.secureServer(port, keyFilepath, certificateFilepath, (function (param) {
                return unsafeHandle(server, param);
              }));
}

var App = /* module */[
  /* unsafeHandle */unsafeHandle,
  /* fromRouter */fromRouter,
  /* start */start$1,
  /* startSecure */startSecure
];

exports.Header = Header;
exports.$$Request = $$Request;
exports.$$Response = $$Response;
exports.Builder = Builder;
exports.App = App;
/* Header Not a pure module */
