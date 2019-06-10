type t('a, 'b) =
  | Top: t('a, 'a)
  | Exact(string): t('a, 'a)
  | Custom(string => option('a)): t('a => 'b, 'b)
  | Slash(t('a, 'b), t('b, 'c)): t('a, 'c)
  | Map('a, t('a, 'b)): t('b => 'c, 'c)
  | Integer: t(int => 'a, 'a)
  | OneOf(list(t('a, 'b))): t('a, 'b)
  | Method(HttpMethod.t): t('a, 'a);

type state('result) = ('result, Request.t);

let first = ((a, b)) => a;
let second = ((a, b)) => b;
let id: type x. x => x = x => x;

let concatMap = (f, l) => List.map(f, l) |> List.concat;
//------------------------------------------------------------------------------
// HELPERS
let mapHelp = (f, state: state('a)) =>
  (f(first(state)), second(state)) |> (value => value);

//------------------------------------------------------------------------------
// PARSING
let rec attempt: type a b. (t(a, b), state(a)) => list(state(b)) =
  (route, state) => {
    let (arg, st) = state;
    switch (route) {
    | Top =>
      if (st.length == 0) {
        [state];
      } else {
        [];
      }

    | Exact(str) =>
      let (newOffset, newLength) =
        Chomp.exact(str, st.url, st.offset, st.length);
      if (newOffset == (-1)) {
        [];
      } else {
        [(arg, {...st, offset: newOffset, length: newLength})];
      };
    | Integer =>
      let (newOffset, newLength, number) =
        Chomp.int(st.url, st.offset, st.length);
      if (newOffset <= st.offset) {
        [];
      } else {
        [(arg(number), {...st, length: newLength, offset: newOffset})];
      };
    | Custom(checker) =>
      let (endOffset, newOffset, newLength) =
        Chomp.segment(st.url, st.offset, st.length);
      if (endOffset == st.offset) {
        [];
      } else {
        let subString = String.sub(st.url, st.offset, endOffset - st.offset);
        switch (checker(subString)) {
        | None => []
        | Some(nextValue) => [
            (arg(nextValue), {...st, offset: newOffset, length: newLength}),
          ]
        };
      };
    | Slash(before, after) =>
      concatMap(attempt(after), attempt(before, state))

    | Method(ofType) =>
      if (st.method == ofType) {
        [state];
      } else {
        [];
      }

    | Map(subValue, subParser) =>
      List.map(mapHelp(arg), attempt(subParser, (subValue, st)))
    | OneOf(routes) => concatMap(p => attempt(p, state), routes)
    };
  };

let top = Top;
let is = (str: string) => Exact(str);
let int = Integer;
let text = Custom(value => Some(value));

let custom = (f: string => option('a)): t('a => 'b, 'b) => Custom(f);
let map = (toMap: 'a, route: t('a, 'b)): t('b => 'c, 'c) =>
  Map(toMap, route);
"user";
let oneOf = (l: list(t('a, 'b))) => OneOf(l);
let (-/-) = (a, b) => Slash(a, b);
let (==>) = (a, b) => Map(b, a);

module Method = {
  let get = Method(HttpMethod.GET);
  let post = Method(HttpMethod.POST);
  let delete = Method(HttpMethod.DELETE);
  let put = Method(HttpMethod.PUT);
  let update = Method(HttpMethod.UPDATE);
  let head = Method(HttpMethod.HEAD);
  let option = Method(HttpMethod.OPTION);
  let connect = Method(HttpMethod.CONNECT);
  let trace = Method(HttpMethod.TRACE);
  let patch = Method(HttpMethod.PATCH);
};
let rec parseHelp = (results: list(state('a))) => {
  switch (results) {
  | [] => None
  | [(arg, st), ...restStates] =>
    if (st.length == 0) {
      Some((arg, st));
    } else {
      parseHelp(restStates);
    }
  };
};

let parse: type a. (t(a => a, a), Request.t) => Result.t(state(a)) =
  (route, req) => {
    let firstDropped = String.sub(req.url, 1, String.length(req.url) - 1);
    Result.attempt(
      ~message="Not found",
      ~code=Status.NotFound404,
      ~contenttype=MediaType.Plain,
      ((a, r): state(a => a)) =>
        attempt(route, (a, {...r, url: firstDropped, length: r.length - 1}))
        |> parseHelp,
      (id, req),
    );
  };

let id: type a. a => a = x => x;

let primitiveParse: type a b. (t(a => a, a), string) => option(a) =
  (router, uri) => {
    let firstDropped = String.sub(uri, 1, String.length(uri) - 1);
    Request.mockGet(uri)
    |> (
      (request: Request.t) =>
        attempt(
          router,
          (
            id,
            {...request, url: firstDropped, length: String.length(uri) - 1},
          ),
        )
    )
    |> parseHelp
    |> (result => Belt.Option.map(result, (st: state(a)) => first(st)));
  };