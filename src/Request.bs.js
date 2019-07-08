// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
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

function compose(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function decodeBody(accepts, request) {
  var __x = Belt_Map.get(accepts, request[/* contentType */4]);
  return Belt_Option.flatMap(__x, (function (decoder) {
                return Curry._1(decoder, request[/* rawBody */8]);
              }));
}

function parseQueries(req) {
  var match = req[/* queries */3];
  if (typeof match === "number" || match.tag) {
    return req;
  } else {
    return /* record */[
            /* url */req[/* url */0],
            /* offset */req[/* offset */1],
            /* length */req[/* length */2],
            /* queries : Parsed */Block.__(1, [queries(match[0], req[/* offset */1], req[/* length */2], Belt_MapString.empty)]),
            /* contentType */req[/* contentType */4],
            /* headers */req[/* headers */5],
            /* accept */req[/* accept */6],
            /* method */req[/* method */7],
            /* rawBody */req[/* rawBody */8],
            /* encoding */req[/* encoding */9]
          ];
  }
}

function splitAtQuestionMark(param) {
  return param.split("?", 1);
}

function query(_str, parse, _req) {
  while(true) {
    var req = _req;
    var str = _str;
    var match = req[/* queries */3];
    if (typeof match === "number") {
      return undefined;
    } else if (match.tag) {
      var match$1 = Belt_MapString.get(match[0], str);
      if (match$1 !== undefined) {
        return Curry._1(parse, match$1);
      } else {
        return undefined;
      }
    } else {
      _req = parseQueries(req);
      _str = match[0];
      continue ;
    }
  };
}

function string(s) {
  return Caml_option.some(s);
}

var Optional = /* module */[
  /* string */string,
  /* int */Belt_Int.fromString
];

function mockGet(uri) {
  return /* record */[
          /* url */uri,
          /* offset */0,
          /* length */uri.length,
          /* queries : NoQuery */0,
          /* contentType : Plain */2,
          /* headers */Header$Cause.$$Map[/* empty */0],
          /* accept : Plain */2,
          /* method : GET */0,
          /* rawBody */"hi",
          /* encoding : Ascii */0
        ];
}

exports.MediaComparer = MediaComparer;
exports.id = id;
exports.queries = queries;
exports.compose = compose;
exports.decodeBody = decodeBody;
exports.parseQueries = parseQueries;
exports.splitAtQuestionMark = splitAtQuestionMark;
exports.query = query;
exports.Optional = Optional;
exports.mockGet = mockGet;
/* MediaComparer Not a pure module */
