type method =
    GET
  | POST
  | PUT
  | UPDATE
  | DELETE
  | HEAD
  | OPTION
  | CONNECT
  | TRACE
  | PATCH;
let tap: 'a => 'a;
let convertNodeMethod: string => option(method);
module Header:
  {
    type key = string;
    type t(+'a);
    let empty: t('a);
    let is_empty: t('a) => bool;
    let mem: (key, t('a)) => bool;
    let add: (key, 'a, t('a)) => t('a);
    let singleton: (key, 'a) => t('a);
    let remove: (key, t('a)) => t('a);
    let merge:
      ((key, option('a), option('b)) => option('c), t('a), t('b)) => t('c);
    let compare: (('a, 'a) => int, t('a), t('a)) => int;
    let equal: (('a, 'a) => bool, t('a), t('a)) => bool;
    let iter: ((key, 'a) => unit, t('a)) => unit;
    let fold: ((key, 'a, 'b) => 'b, t('a), 'b) => 'b;
    let for_all: ((key, 'a) => bool, t('a)) => bool;
    let exists: ((key, 'a) => bool, t('a)) => bool;
    let filter: ((key, 'a) => bool, t('a)) => t('a);
    let partition: ((key, 'a) => bool, t('a)) => (t('a), t('a));
    let cardinal: t('a) => int;
    let bindings: t('a) => list((key, 'a));
    let min_binding: t('a) => (key, 'a);
    let max_binding: t('a) => (key, 'a);
    let choose: t('a) => (key, 'a);
    let split: (key, t('a)) => (t('a), option('a), t('a));
    let find: (key, t('a)) => 'a;
    let map: ('a => 'b, t('a)) => t('b);
    let mapi: ((key, 'a) => 'b, t('a)) => t('b);
  };
module Request:
  {
    type t = {
      body: string,
      path: string,
      headers: Header.t(string),
      method: method,
    };
  };
module Response:
  {
    type t = {
      code: MyNewProject.Status.code,
      headers: Header.t(string),
      body: option(string),
      encoding: MyNewProject.Encoding.t,
    };
    let fail:
      (MyNewProject.Status.Failure.code, Header.t(string),
      ~message: string=?) => t;
  };
module Builder:
  {
    type t('a) = Constructing(Header.t(string), 'a) | Finish(Response.t);
    let start: Request.t => t(string);
    let setHeader: (Header.key, string, t('a)) => t('a);
    let setContentType: (string, t('a)) => t('a);
    let setContentLength: (string, t('a)) => t('a);
    let failWith:
      (MyNewProject.Status.Failure.code, Header.t(string),
      ~message: string=?) => t('a);
    let andThen:
      ('a => option('b), ~failureCode: MyNewProject.Status.Failure.code=?,
      ~failureMessage: string=?, ~failureContentType: string=?, t('a)) =>
      t('b);
    let map: ('a => 'b, t('a)) => t('b);
    let send:
      (MyNewProject.Status.Success.code,
      ~encoding: MyNewProject.Encoding.t=?, t(option(string))) => Response.t;
    let parse:
      ('b => option('b), ~failureMessage: string=?,
      ~failureContentType: string, t('b)) => t('b);
    let parseJson:
      (Js.Json.t => option(Js.Json.t), ~failureMessage: string=?,
      ~failureContentType: string=?, t(Js.Json.t)) => t(Js.Json.t);
    let sendJson:
      (~code: MyNewProject.Status.Success.code=?, t(Js.Json.t)) => Response.t;
    let sendHtml:
      (~code: MyNewProject.Status.Success.code=?, t(option(string))) =>
      Response.t;
    let sendText:
      (string, ~code: MyNewProject.Status.Success.code=?, t('a)) =>
      Response.t;
  };
module App:
  {
    type t = Request.t => Response.t;
    let router:
      (MyNewProject.Router.route(t => t, t), ~notFound: string=?,
      Request.t) => Response.t;
    let start: (int, t) => unit;
  };