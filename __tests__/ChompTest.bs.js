// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var $$String = require("bs-platform/lib/js/string.js");
var Chomp$Cause = require("../src/Chomp.bs.js");

Jest.describe("Chompers chomp", (function (param) {
        return Jest.test("segment chomps up until stop", (function (param) {
                      var url = "hello&end";
                      var match = Chomp$Cause.segment(url, 0, url.length);
                      var subString = $$String.sub(url, 0, match[0]);
                      return Jest.Expect[/* toEqual */12]("hello", Jest.Expect[/* expect */0](subString));
                    }));
      }));

/*  Not a pure module */