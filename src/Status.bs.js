// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");

function toInt(status) {
  switch (status) {
    case 0 : 
        return 200;
    case 1 : 
        return 201;
    case 2 : 
        return 202;
    case 3 : 
        return 203;
    case 4 : 
        return 204;
    case 5 : 
        return 205;
    case 6 : 
        return 206;
    case 7 : 
        return 300;
    case 8 : 
        return 301;
    case 9 : 
        return 302;
    case 10 : 
        return 303;
    case 11 : 
        return 304;
    case 12 : 
        return 305;
    case 13 : 
        return 307;
    case 14 : 
        return 308;
    
  }
}

function message(status) {
  switch (status) {
    case 0 : 
        return "OK";
    case 1 : 
        return "Created";
    case 2 : 
        return "Accepted";
    case 3 : 
        return "Non-Authoritative Information";
    case 4 : 
        return "No Content";
    case 5 : 
        return "Reset Content";
    case 6 : 
        return "Partial Content";
    case 7 : 
        return "Multiple Choices";
    case 8 : 
        return "Moved Temporarily";
    case 9 : 
        return "Found";
    case 10 : 
        return "See Other";
    case 11 : 
        return "Not Modified";
    case 12 : 
        return "Use Proxy";
    case 13 : 
        return "Temporary Redirect";
    case 14 : 
        return "Permanent Redirect";
    
  }
}

var Success = /* module */[
  /* toInt */toInt,
  /* message */message
];

function toInt$1(status) {
  switch (status) {
    case 0 : 
        return 400;
    case 1 : 
        return 401;
    case 2 : 
        return 402;
    case 3 : 
        return 403;
    case 4 : 
        return 404;
    case 5 : 
        return 405;
    case 6 : 
        return 406;
    case 7 : 
        return 407;
    case 8 : 
        return 408;
    case 9 : 
        return 409;
    case 10 : 
        return 410;
    case 11 : 
        return 411;
    case 12 : 
        return 412;
    case 13 : 
        return 413;
    case 14 : 
        return 414;
    case 15 : 
        return 415;
    case 16 : 
        return 416;
    case 17 : 
        return 417;
    case 18 : 
        return 418;
    case 19 : 
        return 500;
    case 20 : 
        return 501;
    case 21 : 
        return 502;
    case 22 : 
        return 503;
    case 23 : 
        return 504;
    case 24 : 
        return 505;
    case 25 : 
        return 509;
    
  }
}

function message$1(status) {
  switch (status) {
    case 0 : 
        return "Bad Request";
    case 1 : 
        return "Unauthorized";
    case 2 : 
        return "Payment Required";
    case 3 : 
        return "Forbidden";
    case 4 : 
        return "Not Found";
    case 5 : 
        return "Method Not Allowed";
    case 6 : 
        return "Not Acceptable";
    case 7 : 
        return "Proxy Authentication Required";
    case 8 : 
        return "Request Timeout";
    case 9 : 
        return "Conflict";
    case 10 : 
        return "Gone";
    case 11 : 
        return "Length Required";
    case 12 : 
        return "Precondition Failed";
    case 13 : 
        return "Payload Too Large";
    case 14 : 
        return "Request-URI Too Large";
    case 15 : 
        return "Unsupported Media Type";
    case 16 : 
        return "Requested Range Not Satisfied";
    case 17 : 
        return "Expectation Failed";
    case 18 : 
        return "I'm a teapot";
    case 19 : 
        return "Internal Server Error";
    case 20 : 
        return "Not Implemented";
    case 21 : 
        return "Bad Gateway";
    case 22 : 
        return "Service Unavailable";
    case 23 : 
        return "Gateway Timeout";
    case 24 : 
        return "HTTP Version Not Supported";
    case 25 : 
        return "Bandwith Limit Exceeded";
    
  }
}

var Failure = /* module */[
  /* toInt */toInt$1,
  /* message */message$1
];

function fail(code) {
  return /* Fail */Block.__(1, [code]);
}

function toInt$2(code) {
  if (code.tag) {
    return toInt$1(code[0]);
  } else {
    return toInt(code[0]);
  }
}

function message$2(code) {
  if (code.tag) {
    return message$1(code[0]);
  } else {
    return message(code[0]);
  }
}

exports.Success = Success;
exports.Failure = Failure;
exports.fail = fail;
exports.toInt = toInt$2;
exports.message = message$2;
/* No side effect */
