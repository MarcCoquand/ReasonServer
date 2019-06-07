// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Id = require("bs-platform/lib/js/belt_Id.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Chomp$Cause = require("./Chomp.bs.js");
var Header$Cause = require("./Header.bs.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");

var cmp = Caml_obj.caml_compare;

var MediaComparer = Belt_Id.MakeComparable(/* module */[/* cmp */cmp]);

function id(x) {
  return x;
}

function queries(url, _offset, _length, _set) {
  while(true) {
    var set = _set;
    var length = _length;
    var offset = _offset;
    var match = Chomp$Cause.segment(url, offset, length);
    var offsetKey = match[1];
    if (offsetKey === offset) {
      return set;
    } else {
      var key = Chomp$Cause.extractValue(url, offset, match[0]);
      var match$1 = Chomp$Cause.segment(url, offsetKey, match[2]);
      var nextOffset = match$1[1];
      if (nextOffset === offsetKey) {
        return set;
      } else {
        var queryValue = Chomp$Cause.extractValue(url, offsetKey, match$1[0]);
        var newSet = Belt_MapString.set(set, key, queryValue);
        _set = newSet;
        _length = match$1[2];
        _offset = nextOffset;
        continue ;
      }
    }
  };
}

function map(f, content) {
  return /* record */[
          /* url */content[/* url */0],
          /* offset */content[/* offset */1],
          /* length */content[/* length */2],
          /* arguments */Curry._1(f, content[/* arguments */3]),
          /* queries */content[/* queries */4],
          /* contentType */content[/* contentType */5],
          /* headers */content[/* headers */6],
          /* accept */content[/* accept */7],
          /* method */content[/* method */8],
          /* rawBody */content[/* rawBody */9],
          /* encoding */content[/* encoding */10]
        ];
}

function pure(value, request) {
  return /* record */[
          /* url */request[/* url */0],
          /* offset */request[/* offset */1],
          /* length */request[/* length */2],
          /* arguments */value,
          /* queries */request[/* queries */4],
          /* contentType */request[/* contentType */5],
          /* headers */request[/* headers */6],
          /* accept */request[/* accept */7],
          /* method */request[/* method */8],
          /* rawBody */request[/* rawBody */9],
          /* encoding */request[/* encoding */10]
        ];
}

function apply(value, request) {
  return /* record */[
          /* url */request[/* url */0],
          /* offset */request[/* offset */1],
          /* length */request[/* length */2],
          /* arguments */Curry._1(request[/* arguments */3], value),
          /* queries */request[/* queries */4],
          /* contentType */request[/* contentType */5],
          /* headers */request[/* headers */6],
          /* accept */request[/* accept */7],
          /* method */request[/* method */8],
          /* rawBody */request[/* rawBody */9],
          /* encoding */request[/* encoding */10]
        ];
}

function applyCombine(request, value) {
  return /* record */[
          /* url */value[/* url */0],
          /* offset */value[/* offset */1],
          /* length */value[/* length */2],
          /* arguments */Curry._1(request[/* arguments */3], value[/* arguments */3]),
          /* queries */value[/* queries */4],
          /* contentType */value[/* contentType */5],
          /* headers */value[/* headers */6],
          /* accept */value[/* accept */7],
          /* method */value[/* method */8],
          /* rawBody */value[/* rawBody */9],
          /* encoding */value[/* encoding */10]
        ];
}

function compose(f, x) {
  return Curry._1(f, x);
}

function decodeBody(accepts, request) {
  var __x = Belt_Map.get(accepts, request[/* contentType */5]);
  var __x$1 = Belt_Option.flatMap(__x, (function (decoder) {
          return Curry._1(decoder, request[/* rawBody */9]);
        }));
  return Belt_Option.map(__x$1, (function (decodedBody) {
                return apply(decodedBody, request);
              }));
}

function query(str, parse, req) {
  var queries$1 = Belt_Option.getWithDefault(req[/* queries */4], queries(req[/* url */0], req[/* offset */1], req[/* length */2], Belt_MapString.empty));
  var match = Belt_MapString.get(queries$1, str);
  if (match !== undefined) {
    return /* record */[
            /* url */"",
            /* offset */req[/* offset */1],
            /* length */0,
            /* arguments */Curry._1(req[/* arguments */3], Curry._1(parse, match)),
            /* queries */Caml_option.some(queries$1),
            /* contentType */req[/* contentType */5],
            /* headers */req[/* headers */6],
            /* accept */req[/* accept */7],
            /* method */req[/* method */8],
            /* rawBody */req[/* rawBody */9],
            /* encoding */req[/* encoding */10]
          ];
  } else {
    return /* record */[
            /* url */"",
            /* offset */req[/* offset */1],
            /* length */0,
            /* arguments */Curry._1(req[/* arguments */3], undefined),
            /* queries */Caml_option.some(queries$1),
            /* contentType */req[/* contentType */5],
            /* headers */req[/* headers */6],
            /* accept */req[/* accept */7],
            /* method */req[/* method */8],
            /* rawBody */req[/* rawBody */9],
            /* encoding */req[/* encoding */10]
          ];
  }
}

function string(s) {
  return Caml_option.some(s);
}

var Optional = /* module */[
  /* string */string,
  /* int */Belt_Int.fromString
];

function mock(uri, body, method_) {
  return /* record */[
          /* url */uri,
          /* offset */0,
          /* length */uri.length,
          /* arguments */id,
          /* queries */undefined,
          /* contentType : Plain */2,
          /* headers */Header$Cause.$$Map[/* empty */0],
          /* accept : Plain */2,
          /* method */method_,
          /* rawBody */body,
          /* encoding : Ascii */0
        ];
}

function mockPost(uri, body) {
  return /* record */[
          /* url */uri,
          /* offset */0,
          /* length */uri.length,
          /* arguments : NoHandler */0,
          /* queries */undefined,
          /* contentType : Plain */2,
          /* headers */Header$Cause.$$Map[/* empty */0],
          /* accept : Plain */2,
          /* method : POST */1,
          /* rawBody */body,
          /* encoding : Ascii */0
        ];
}

function mockGet(uri) {
  return /* record */[
          /* url */uri,
          /* offset */0,
          /* length */uri.length,
          /* arguments */id,
          /* queries */undefined,
          /* contentType : Plain */2,
          /* headers */Header$Cause.$$Map[/* empty */0],
          /* accept : Plain */2,
          /* method : GET */0,
          /* rawBody */"",
          /* encoding : Ascii */0
        ];
}

exports.MediaComparer = MediaComparer;
exports.id = id;
exports.queries = queries;
exports.map = map;
exports.pure = pure;
exports.apply = apply;
exports.applyCombine = applyCombine;
exports.compose = compose;
exports.decodeBody = decodeBody;
exports.query = query;
exports.Optional = Optional;
exports.mock = mock;
exports.mockPost = mockPost;
exports.mockGet = mockGet;
/* MediaComparer Not a pure module */
