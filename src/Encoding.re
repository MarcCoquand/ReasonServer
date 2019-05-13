

type t =
  | Ascii
  | Base64
  | Binary
  | Hex
  | Latin1
  | Ucs2
  | Utf16le
  | Utf8

let toString = enc =>
    switch(enc) {
    | Ascii => "Ascii"
    | Base64 => "Base64"
    | Binary => "Binary"
    | Hex => "Hex"
    | Latin1 => "Latin1"
    | Ucs2 => "Ucs2"
    | Utf16le => "Utf16le"
    | Utf8 => "Utf8"
    }
