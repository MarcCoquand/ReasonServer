// Implementation of John Hughe's Arrow for our Server. Arrow's in this case
// create a DSL where we can change the response, it's encoders and apply arguments
// to the request.
type t('a) =
  | Ok('a)
  | Failed(string, Status.code, MediaType.t);

// Arrow
type computation('a, 'b) =
  | Run('a => t('b));

let evaluate = (computation, value) => {
  let Run(f) = computation;
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

let dimap = (f, g, computation) => {
  let Run(m) = computation;

  Run((value => Ok(f(value))) >=> m >=> (result => Ok(g(result))));
};
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

// (>>>) operator from arrows
let andThen:
  type i n o. (computation(n, o), computation(i, n)) => computation(i, o) =
  (Run(g), Run(f)) => Run(f >=> g);

// arr
let run = f => Run(f);

let runFailsafe = f => Run(s => Ok(f(s)));

let first: type b c d. computation(b, c) => computation((b, d), (c, d)) =
  arrow =>
    Run(
      ((b, d)) => {
        let Run(f) = arrow;
        f(b) >>= (c => pure((c, d)));
      },
    );

let second = arrow =>
  Run(
    ((d, b)) => {
      let Run(f) = arrow;
      f(b) >>= (c => pure((d, c)));
    },
  );

// (***) operator from arrows
let both = (f, g) => first(f) |> andThen(second(g));

// (&&&) operator from arrows
let branch = (f, g) => run(b => pure((b, b))) |> andThen(both(f, g));

let merge = computation => first(computation) |> rmap(((f, x)) => f(x));

let strong = (f: ('a, 'b) => 'c, x: computation('a, 'b)) =>
  dimap(a => (a, a), ((b, a)) => f(a, b), first(x));

// ArrowChoice
// When we parse the url we want to test all paths before throwing an error.

type either('a, 'b) =
  | Right('a)
  | Left('b);

let eitherFunc = (f, g, v) =>
  switch (f, g, v) {
  | (f, _, Left(x)) => f(x)
  | (_, g, Right(y)) => g(y)
  };

let (|||) = (fA, gA) => {
  let Run(f) = fA;
  let Run(g) = gA;
  Run(eitherFunc(f, g));
};

let (+++) = (f, g) =>
  f
  |> andThen(run(v => Ok(Left(v))))
  ||| (g |> andThen(run(v => Ok(Right(v)))));