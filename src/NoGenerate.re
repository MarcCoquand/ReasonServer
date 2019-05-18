// type t('a) =
//   | Top: t('a);

let map: type a b. (a => b, list(a)) => list(b) = (f, l) => List.map(f, l);