// Implementation of John Hughes Arrow for our Server. Arrow's in this case
// create a DSL where we can change the response and apply arguments
// to the request.
type result('a) =
  | Ok('a)
  | Failed(string, Status.code, MediaType.t);

type t('a, 'b) =
  | Run('a => result('b));

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

let (>>>): type i n o. (t(i, n), t(n, o)) => t(i, o) =
  (Run(f), Run(g)) => Run(f >=> g);

let arr = f => Run(f);

let first: type b c d. t(b, c) => t((b, d), (c, d)) =
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

let ( *** ) = (f, g) => first(f) >>> second(g);

let (&&&) = (f, g) => arr(b => pure((b, b))) >>> f *** g;

let merge = f => arr(((x, y)) => Ok(f(x, y)));

let (^>>) = (f, a) => arr(f) >>> a;
let (>>^) = (f, a) => a >>> arr(f);

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
  f >>> arr(v => Ok(Left(v))) ||| (g >>> arr(v => Ok(Right(v))));