module Map =
  Map.Make({
    type t = string;
    let compare = compare;
  });