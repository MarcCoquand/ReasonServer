// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function $great$eq$great(f, g, x) {
  var res1 = Curry._1(f, x);
  if (res1.tag) {
    return /* Failed */Block.__(1, [
              res1[0],
              res1[1],
              res1[2]
            ]);
  } else {
    return Curry._1(g, res1[0]);
  }
}

function pure(x) {
  return /* Ok */Block.__(0, [x]);
}

function $great$great$eq(x, f) {
  if (x.tag) {
    return /* Failed */Block.__(1, [
              x[0],
              x[1],
              x[2]
            ]);
  } else {
    return Curry._1(f, x[0]);
  }
}

function $great$great$great(param, param$1) {
  var g = param$1[0];
  var f = param[0];
  return /* Run */[(function (param) {
              return $great$eq$great(f, g, param);
            })];
}

function arr(f) {
  return /* Run */[f];
}

function first(arrow) {
  return /* Run */[(function (param) {
              var d = param[1];
              return $great$great$eq(Curry._1(arrow[0], param[0]), (function (c) {
                            return /* Ok */Block.__(0, [/* tuple */[
                                        c,
                                        d
                                      ]]);
                          }));
            })];
}

function second(arrow) {
  return /* Run */[(function (param) {
              var d = param[0];
              return $great$great$eq(Curry._1(arrow[0], param[1]), (function (c) {
                            return /* Ok */Block.__(0, [/* tuple */[
                                        d,
                                        c
                                      ]]);
                          }));
            })];
}

function $star$star$star(f, g) {
  return $great$great$great(first(f), second(g));
}

function $unknown$unknown$unknown(f, g) {
  return $great$great$great(/* Run */[(function (b) {
                  return /* Ok */Block.__(0, [/* tuple */[
                              b,
                              b
                            ]]);
                })], $star$star$star(f, g));
}

function merge(f) {
  return /* Run */[(function (param) {
              return /* Ok */Block.__(0, [Curry._2(f, param[0], param[1])]);
            })];
}

function $caret$great$great(f, a) {
  return $great$great$great(/* Run */[f], a);
}

function $great$great$caret(f, a) {
  return $great$great$great(a, /* Run */[f]);
}

function eitherFunc(f, g, v) {
  if (v.tag) {
    return Curry._1(f, v[0]);
  } else {
    return Curry._1(g, v[0]);
  }
}

function $pipe$pipe$pipe(fA, gA) {
  var g = gA[0];
  var f = fA[0];
  return /* Run */[(function (param) {
              return eitherFunc(f, g, param);
            })];
}

function $plus$plus$plus(f, g) {
  return $pipe$pipe$pipe($great$great$great(f, /* Run */[(function (v) {
                      return /* Ok */Block.__(0, [/* Left */Block.__(1, [v])]);
                    })]), $great$great$great(g, /* Run */[(function (v) {
                      return /* Ok */Block.__(0, [/* Right */Block.__(0, [v])]);
                    })]));
}

exports.$great$eq$great = $great$eq$great;
exports.pure = pure;
exports.$great$great$eq = $great$great$eq;
exports.$great$great$great = $great$great$great;
exports.arr = arr;
exports.first = first;
exports.second = second;
exports.$star$star$star = $star$star$star;
exports.$unknown$unknown$unknown = $unknown$unknown$unknown;
exports.merge = merge;
exports.$caret$great$great = $caret$great$great;
exports.$great$great$caret = $great$great$caret;
exports.eitherFunc = eitherFunc;
exports.$pipe$pipe$pipe = $pipe$pipe$pipe;
exports.$plus$plus$plus = $plus$plus$plus;
/* No side effect */
