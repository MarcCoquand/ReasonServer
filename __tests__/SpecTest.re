open Jest;
open Spec;
open Uri;

let id: type a. a => a = x => x;

module Book = {
  type book = {
    title: string,
    author: string,
    year: int,
    id: int,
  };

  module Encoders = {
    let json = myBook => {
      open! Json.Encode;
      Json.Encode.(
        object_([
          ("title", string(myBook.title)),
          ("year", int(myBook.year)),
          ("author", string(myBook.author)),
          ("id", int(myBook.id)),
        ])
      );
    };
    let plain = books =>
      List.fold_right((book, str) => str ++ book.title, books, "");
  };
  module Decoders = {
    let json = json => {
      open! Json.Decode;
      Json.Decode.{
        title: json |> field("title", string),
        year: json |> field("year", int),
        author: json |> field("author", string),
        id: json |> field("id", int),
      };
    };
  };

  let mockGetById =
      (books: list(book), year: option(int), author: option(string))
      : Result.t(list(book)) =>
    // Do some api fetching
    Result.Ok(
      List.map(
        book =>
          {
            ...book,
            year: Belt.Option.getWithDefault(year, book.year),
            author: Belt.Option.getWithDefault(author, book.author),
          },
        books,
      ),
    );

  module Api = {
    open Spec;
    let get = fetchBookList =>
      accept([
        Contenttype.json(Json.Encode.list(Encoders.json)),
        Contenttype.plain(Encoders.plain),
      ])
      |: query("year", Belt.Int.fromString)
      |: query("author", s => Some(s))
      |> handle(Status.Ok200, fetchBookList);
  };
};
let echoInt = (b: option(int), c: option(int)) =>
  Result.Ok(Belt.Int.toString(Belt.Option.getWithDefault(b, 0)));

let writeHi = Result.Ok("Hello");

let writeHi =
  accept([
    Contenttype.plain((s: string) => s),
    Contenttype.json(Json.Encode.string),
  ])
  |> handle(Status.Partial206, writeHi);

let echoIntRoute =
  query("world", Belt.Int.fromString)
  |: query("worl", Belt.Int.fromString)
  |> handle(Status.Ok200, echoInt);