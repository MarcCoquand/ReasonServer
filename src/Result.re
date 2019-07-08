// Implementation of John Hughe's Arrow for our Server. Arrow's in this case
// create a DSL where we can change the response, it's encoders and apply arguments
// to the request.
type t('a) =
  | Ok('a)
  | Failed(string, Status.code, MediaType.t);

let map = (f: 'a => 'b, t: t('a)): t('b) =>
  switch (t) {
  | Ok(a) => Ok(f(a))
  | Failed(m, c, t) => Failed(m, c, t)
  };

// Arrow
type computation('a, 'b) = 'a => t('b);

let evaluate = (f, value) => {
  f(value);
};

let (>=>) = (f, g, x) => {
  let res1 = f(x);
  switch (res1) {
  | Ok(y) => g(y)
  | Failed(s, c, m) => Failed(s, c, m)
  };
};
let pure = x => Ok(x);
let (>>=) = (x, f) =>
  switch (x) {
  | Ok(a) => f(a)
  | Failed(s, c, m) => Failed(s, c, m)
  };

let toOption = a =>
  switch (a) {
  | Ok(a) => Some(a)
  | Failed(_, _, _) => None
  };

let dimap = (f, g, computation) =>
  (value => Ok(f(value))) >=> computation >=> (result => Ok(g(result)));
let id: type a. a => a = x => x;

let lmap = (f, computation) => dimap(f, id, computation);

let rmap = (g, computation) => dimap(id, g, computation);

// Run an operation that might fail and invoke the correct error response.
let attempt =
    (
      ~message="Internal server error",
      ~code=Status.Error500,
      ~contenttype=MediaType.Html,
      f,
      value,
    ) =>
  f(value)
  |> (
    res =>
      switch (res) {
      | Some(success) => Ok(success)
      | None => Failed(message, code, contenttype)
      }
  );

let runFailsafe = (f, s) => Ok(f(s));

let first: type b c d. computation(b, c) => computation((b, d), (c, d)) =
  (f, (b, d)) => {
    f(b) >>= (c => pure((c, d)));
  };

let second = (f, (d, b)) => {
  f(b) >>= (c => pure((d, c)));
};

let invalidParse =
  Failed("Parse failed", Status.BadRequest400, MediaType.Plain);

let (<|>) = (f: computation('a, t('b)), g: computation('a, t('c)), x: 'a) => {
  let r1 = f(x);
  switch (r1) {
  | Ok(success) => success
  | Failed(_, _, _) => g(x)
  };
};

// (***) operator from arrows
let both = (f, g) => first(f) >=> second(g);

let split: computation('a, ('a, 'a)) = b => Ok((b, b));
// (&&&) operator from arrows
let branch:
  (computation('a, 'b), computation('a, 'c)) => computation('a, ('b, 'c)) =
  (f, g) => (b => Ok((b, b))) >=> both(f, g);

let merge = computation => first(computation) |> rmap(((f, x)) => f(x));

let strong = (f: ('a, 'b) => 'c, x: computation('a, 'b)) =>
  dimap(a => (a, a), ((b, a)) => f(a, b), first(x));

// ArrowChoice
// When we parse the url we want to test all paths before throwing an error.

type either('a, 'b) =
  | Right('a)
  | Left('b);

let choose = (fa, fb, v) =>
  switch (fa(v)) {
  | Ok(success) => Ok(Right(success))
  | Failed(_, _, _) =>
    fb(v)
    |> (
      res =>
        switch (res) {
        | Ok(success) => Ok(Left(success))
        | Failed(m, c, t) => Failed(m, c, t)
        }
    )
  };

let eitherFunc = (f, g, v) =>
  switch (f, g, v) {
  | (f, _, Left(x)) => f(x)
  | (_, g, Right(y)) => g(y)
  };

let (|||) = (fA, gA) => {
  eitherFunc(fA, gA);
};

let (+++) = (f, g) =>
  f >=> (v => Ok(Left(v))) ||| (g >=> (v => Ok(Right(v))));

let combine =
    (f: computation('a, 'b), g: computation('a, 'b)): computation('a, 'b) =>
  (req: 'a) => {
    let r1 = f(req);
    let r2 = g(req);

    switch (r1) {
    | Ok(success) => Ok(success)
    | Failed(_, _, _) => r2
    };
  };

let rec selectFirstR: type a. list(t(a)) => t(a) =
  (f) => (
    switch (f) {
    | [c1, ...rest] =>
      switch (c1) {
      | Ok(success) => Ok(success)
      | Failed(_, _, _) => selectFirstR(rest)
      }
    | [] => Failed("Not found", Status.NotFound404, MediaType.Html)
    }:
      t(a)
  );

let rec selectFirst = (f: list(computation('a, 'b))): computation('a, 'b) =>
  switch (f) {
  | [c1, c2, ...rest] =>
    let result = combine(c1, c2);
    selectFirst([result, ...rest]);
  | [c1] => c1
  | [] => (_ => Failed("Not found", Status.NotFound404, MediaType.Html))
  };

let parseInt = (req: Request.t) => {
  let (newOffset, newLength, number) =
    Chomp.int(req.url, req.offset, req.length);
  if (newOffset <= req.offset) {
    invalidParse;
  } else {
    Ok((number, {...req, length: newLength, offset: newOffset}));
  };
};

let parseCustom = (checker, req: Request.t) => {
  let (endOffset, newOffset, newLength) =
    Chomp.segment(req.url, req.offset, req.length);
  if (endOffset == req.offset) {
    invalidParse;
  } else {
    let subStr = String.sub(req.url, req.offset, endOffset - req.offset);
    switch (checker(subStr)) {
    | None => invalidParse
    | Some(nextValue) =>
      Ok((nextValue, {...req, offset: newOffset, length: newLength}))
    };
  };
};
let parseExact = (str, req: Request.t) => {
  let (newOffset, newLength) =
    Chomp.exact(str, req.url, req.offset, req.length);
  if (newOffset == (-1)) {
    invalidParse;
  } else {
    Ok({...req, offset: newOffset, length: newLength});
  };
};
let requestUsesMethod = (ofType, req: Request.t): bool =>
  req.method == ofType;