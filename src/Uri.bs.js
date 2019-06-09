// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Chomp$Cause = require("./Chomp.bs.js");
var Result$Cause = require("./Result.bs.js");
var Request$Cause = require("./Request.bs.js");

function concatMap(f, l) {
  return List.concat(List.map(f, l));
}

function mapHelp(func, state) {
  return /* record */[
          /* url */state[/* url */0],
          /* offset */state[/* offset */1],
          /* length */state[/* length */2],
          /* arguments */Curry._1(func, state[/* arguments */3]),
          /* queries */state[/* queries */4],
          /* contentType */state[/* contentType */5],
          /* headers */state[/* headers */6],
          /* accept */state[/* accept */7],
          /* method */state[/* method */8],
          /* rawBody */state[/* rawBody */9],
          /* encoding */state[/* encoding */10]
        ];
}

function attempt(route, state) {
  if (typeof route === "number") {
    if (route === 0) {
      if (state[/* length */2] === 0) {
        return /* :: */[
                state,
                /* [] */0
              ];
      } else {
        return /* [] */0;
      }
    } else {
      var match = Chomp$Cause.$$int(state[/* url */0], state[/* offset */1], state[/* length */2]);
      var newOffset = match[0];
      if (newOffset <= state[/* offset */1]) {
        return /* [] */0;
      } else {
        return /* :: */[
                /* record */[
                  /* url */state[/* url */0],
                  /* offset */newOffset,
                  /* length */match[1],
                  /* arguments */Curry._1(state[/* arguments */3], match[2]),
                  /* queries */state[/* queries */4],
                  /* contentType */state[/* contentType */5],
                  /* headers */state[/* headers */6],
                  /* accept */state[/* accept */7],
                  /* method */state[/* method */8],
                  /* rawBody */state[/* rawBody */9],
                  /* encoding */state[/* encoding */10]
                ],
                /* [] */0
              ];
      }
    }
  } else {
    switch (route.tag | 0) {
      case 0 : 
          var match$1 = Chomp$Cause.exact(route[0], state[/* url */0], state[/* offset */1], state[/* length */2]);
          var newOffset$1 = match$1[0];
          if (newOffset$1 === -1) {
            return /* [] */0;
          } else {
            return /* :: */[
                    /* record */[
                      /* url */state[/* url */0],
                      /* offset */newOffset$1,
                      /* length */match$1[1],
                      /* arguments */state[/* arguments */3],
                      /* queries */state[/* queries */4],
                      /* contentType */state[/* contentType */5],
                      /* headers */state[/* headers */6],
                      /* accept */state[/* accept */7],
                      /* method */state[/* method */8],
                      /* rawBody */state[/* rawBody */9],
                      /* encoding */state[/* encoding */10]
                    ],
                    /* [] */0
                  ];
          }
      case 1 : 
          var match$2 = Chomp$Cause.segment(state[/* url */0], state[/* offset */1], state[/* length */2]);
          var endOffset = match$2[0];
          if (endOffset === state[/* offset */1]) {
            return /* [] */0;
          } else {
            var subString = $$String.sub(state[/* url */0], state[/* offset */1], endOffset - state[/* offset */1] | 0);
            var match$3 = Curry._1(route[0], subString);
            if (match$3 !== undefined) {
              return /* :: */[
                      /* record */[
                        /* url */state[/* url */0],
                        /* offset */match$2[1],
                        /* length */match$2[2],
                        /* arguments */Curry._1(state[/* arguments */3], Caml_option.valFromOption(match$3)),
                        /* queries */state[/* queries */4],
                        /* contentType */state[/* contentType */5],
                        /* headers */state[/* headers */6],
                        /* accept */state[/* accept */7],
                        /* method */state[/* method */8],
                        /* rawBody */state[/* rawBody */9],
                        /* encoding */state[/* encoding */10]
                      ],
                      /* [] */0
                    ];
            } else {
              return /* [] */0;
            }
          }
      case 2 : 
          var after = route[1];
          var l = attempt(route[0], state);
          var f = function (param) {
            return attempt(after, param);
          };
          return List.concat(List.map(f, l));
      case 3 : 
          var partial_arg = state[/* arguments */3];
          return List.map((function (param) {
                        return mapHelp(partial_arg, param);
                      }), attempt(route[1], /* record */[
                          /* url */state[/* url */0],
                          /* offset */state[/* offset */1],
                          /* length */state[/* length */2],
                          /* arguments */route[0],
                          /* queries */state[/* queries */4],
                          /* contentType */state[/* contentType */5],
                          /* headers */state[/* headers */6],
                          /* accept */state[/* accept */7],
                          /* method */state[/* method */8],
                          /* rawBody */state[/* rawBody */9],
                          /* encoding */state[/* encoding */10]
                        ]));
      case 4 : 
          return List.concat(List.map((function (p) {
                            return attempt(p, state);
                          }), route[0]));
      
    }
  }
}

function is(str) {
  return /* Exact */Block.__(0, [str]);
}

var text = /* Custom */Block.__(1, [(function (value) {
        return value;
      })]);

function custom(f) {
  return /* Custom */Block.__(1, [f]);
}

function map(toMap, route) {
  return /* Map */Block.__(3, [
            toMap,
            route
          ]);
}

function oneOf(l) {
  return /* OneOf */Block.__(4, [l]);
}

function $great$neg(a, b) {
  return /* Slash */Block.__(2, [
            a,
            b
          ]);
}

function $eq$eq$great(a, b) {
  return /* Map */Block.__(3, [
            b,
            a
          ]);
}

function parseHelp(_results) {
  while(true) {
    var results = _results;
    if (results) {
      var state = results[0];
      if (state[/* length */2] === 0) {
        return state;
      } else {
        _results = results[1];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function parse(route, req) {
  var firstDropped = $$String.sub(req[/* url */0], 1, req[/* url */0].length - 1 | 0);
  return Result$Cause.attempt("Not found", /* NotFound404 */19, /* Plain */2, (function (v) {
                return parseHelp(attempt(route, /* record */[
                                /* url */firstDropped,
                                /* offset */v[/* offset */1],
                                /* length */v[/* length */2] - 1 | 0,
                                /* arguments */v[/* arguments */3],
                                /* queries */v[/* queries */4],
                                /* contentType */v[/* contentType */5],
                                /* headers */v[/* headers */6],
                                /* accept */v[/* accept */7],
                                /* method */v[/* method */8],
                                /* rawBody */v[/* rawBody */9],
                                /* encoding */v[/* encoding */10]
                              ]));
              }), req);
}

function id(x) {
  return x;
}

function primitiveParse(router, uri) {
  var firstDropped = $$String.sub(uri, 1, uri.length - 1 | 0);
  var request = Request$Cause.mockGet(uri);
  var result = parseHelp(attempt(router, /* record */[
            /* url */firstDropped,
            /* offset */request[/* offset */1],
            /* length */uri.length - 1 | 0,
            /* arguments */request[/* arguments */3],
            /* queries */request[/* queries */4],
            /* contentType */request[/* contentType */5],
            /* headers */request[/* headers */6],
            /* accept */request[/* accept */7],
            /* method */request[/* method */8],
            /* rawBody */request[/* rawBody */9],
            /* encoding */request[/* encoding */10]
          ]));
  return Belt_Option.map(result, (function (req) {
                return req[/* arguments */3];
              }));
}

var top = /* Top */0;

var $$int = /* Integer */1;

exports.concatMap = concatMap;
exports.mapHelp = mapHelp;
exports.attempt = attempt;
exports.top = top;
exports.is = is;
exports.$$int = $$int;
exports.text = text;
exports.custom = custom;
exports.map = map;
exports.oneOf = oneOf;
exports.$great$neg = $great$neg;
exports.$eq$eq$great = $eq$eq$great;
exports.parseHelp = parseHelp;
exports.parse = parse;
exports.id = id;
exports.primitiveParse = primitiveParse;
/* Request-Cause Not a pure module */
