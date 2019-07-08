//----------------------------------------------------------------------------
// CHOMP EXACT
let rec isSubString = (small, big, offset, i, smallLen) =>
  if (i == smallLen) {
    true;
  } else if (small.[i] == big.[offset + i]) {
    isSubString(small, big, offset, i + 1, smallLen);
  } else {
    false;
  };

let exact = (small, big, offset, length) => {
  let smallLen = String.length(small);
  if (length < smallLen || smallLen == 0) {
    ((-1), length);
  } else if (!isSubString(small, big, offset, 0, smallLen)) {
    ((-1), length);
  } else {
    let newOffset = offset + smallLen;
    let newLength = length - smallLen;
    if (newLength == 0) {
      (newOffset, newLength);
    } else if (big.[newOffset] == '/'
               || big.[newOffset] == '&'
               || big.[newOffset] == '='
               || big.[newOffset] == '?') {
      (newOffset + 1, newLength - 1);
    } else {
      ((-1), length);
    };
  };
};

//----------------------------------------------------------------------------
// CHOMP INT
let charDigitToInt = (str): option(int) =>
  switch (str) {
  | '0'..'9' => Some(int_of_char(str) - 48)
  | _ => None
  };

let rec intHelp = (url, offset, length, n) =>
  if (length == 0) {
    (offset, length, n);
  } else {
    let word = url.[offset];
    switch (charDigitToInt(word)) {
    | Some(i) => intHelp(url, offset + 1, length - 1, n * 10 + i)
    | None =>
      word == '/' || word == '&' || word == '=' || word == '?'
        ? (offset + 1, length - 1, n) : (offset, length, n)
    };
  };

let int = (url, offset, length) =>
  if (length == 0) {
    (offset, length, 0);
  } else {
    let word = url.[offset];
    switch (charDigitToInt(word)) {
    | Some(i) => intHelp(url, offset + 1, length - 1, i)
    | None => (offset, length, 0)
    };
  };

//----------------------------------------------------------------------------
// CHOMP SEGMENT
let rec segment = (url, offset, length) =>
  if (length == 0) {
    (offset, offset, length);
  } else if (url.[offset] == '/'
             || url.[offset] == '&'
             || url.[offset] == '='
             || url.[offset] == '?') {
    (offset, offset + 1, length - 1);
  } else {
    segment(url, offset + 1, length - 1);
  };
let extractValue = (str, start, upto) =>
  String.sub(str, start, upto - start);

//----------------------------------------------------------------------------
// CHOMP QUERIES
// Query parameters are unordered while argument list is ordered.
// Thus we need to parse all of them before checking the values are of a
// correct type. Queries are also always at the end of the router and thus we
// need to chomp from right to left.

// chomps from right to left instead of left to right.
let segmentReverse = (url, offset, length) =>
  if (length == 0) {
    (offset, offset, length);
  } else if (url.[offset] == '/'
             || url.[offset] == '&'
             || url.[offset] == '='
             || url.[offset] == '?') {
    (offset, offset - 1, length - 1);
  } else {
    segment(url, offset - 1, length - 1);
  };