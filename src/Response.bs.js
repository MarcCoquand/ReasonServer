// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Json = require("@glennsl/bs-json/src/Json.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Header$Cause = require("./Header.bs.js");

function compose(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function error($staropt$star, $staropt$star$1, $staropt$star$2) {
  var message = $staropt$star !== undefined ? $staropt$star : "Internal server error";
  var code = $staropt$star$1 !== undefined ? $staropt$star$1 : /* Error500 */34;
  var method_ = $staropt$star$2 !== undefined ? $staropt$star$2 : /* Html */0;
  return /* record */[
          /* code */code,
          /* headers */Header$Cause.$$Map[/* empty */0],
          /* contentType */method_,
          /* body */(function (param) {
              return message;
            }),
          /* encoding : Ascii */0
        ];
}

function map(f, response) {
  return /* record */[
          /* code */response[/* code */0],
          /* headers */response[/* headers */1],
          /* contentType */response[/* contentType */2],
          /* body */Curry._1(f, response[/* body */3]),
          /* encoding */response[/* encoding */4]
        ];
}

function contramap(cf, response) {
  var partial_arg = response[/* body */3];
  return /* record */[
          /* code */response[/* code */0],
          /* headers */response[/* headers */1],
          /* contentType */response[/* contentType */2],
          /* body */(function (param) {
              return Curry._1(partial_arg, Curry._1(cf, param));
            }),
          /* encoding */response[/* encoding */4]
        ];
}

function encode(value, response) {
  return /* record */[
          /* code */response[/* code */0],
          /* headers */response[/* headers */1],
          /* contentType */response[/* contentType */2],
          /* body */Curry._1(response[/* body */3], value),
          /* encoding */response[/* encoding */4]
        ];
}

function fromResult(maybeResponse) {
  if (maybeResponse.tag) {
    return error(maybeResponse[0], maybeResponse[1], /* Html */0);
  } else {
    return maybeResponse[0];
  }
}

function id(x) {
  return x;
}

var lift_001 = /* headers */Header$Cause.$$Map[/* empty */0];

var lift = /* record */[
  /* code : Ok200 */0,
  lift_001,
  /* contentType : Plain */2,
  /* body */id,
  /* encoding : Ascii */0
];

function setCode(code, response) {
  return /* record */[
          /* code */code,
          /* headers */response[/* headers */1],
          /* contentType */response[/* contentType */2],
          /* body */response[/* body */3],
          /* encoding */response[/* encoding */4]
        ];
}

function setContentType(mediaType, response) {
  return /* record */[
          /* code */response[/* code */0],
          /* headers */response[/* headers */1],
          /* contentType */mediaType,
          /* body */response[/* body */3],
          /* encoding */response[/* encoding */4]
        ];
}

var setEncoder = map;

function json(encoder, response) {
  return setContentType(/* Json */1, contramap(encoder, contramap(Json.stringify, response)));
}

exports.compose = compose;
exports.error = error;
exports.map = map;
exports.contramap = contramap;
exports.encode = encode;
exports.fromResult = fromResult;
exports.id = id;
exports.lift = lift;
exports.setCode = setCode;
exports.setContentType = setContentType;
exports.setEncoder = setEncoder;
exports.json = json;
/* Header-Cause Not a pure module */