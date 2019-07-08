open Jest;

let id: type a. a => a = x => x;

module Book = {
  type t = {
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
    let plain = book => book.title;
    let plainList = books =>
      List.fold_right((book, str) => str ++ plain(book), books, "");
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

  let mockFromList =
      (books: list(t), year: option(int), author: option(string))
      : Result.t(list(t)) =>
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

  let asList =
    mockFromList([{title: "Harry", author: "Jk", year: 1995, id: 5}]);

  let mockById = (id: int): Result.t(t) =>
    Result.Ok({title: "Harry", author: "Jk", year: 1995, id});

  let insert = (book: t): Result.t(string) =>
    // Add to database
    Result.Ok("Added to database");

  module Api = {
    open Spec.Router;
    let router: type a. Spec.Router.t((a, string) => string, string) =
      is("hello")
      >- (
        Method.get
        >- accept([Spec.Contenttype.json(Encoders.json)])
        >- int
        |> handler(mockById, Status.Ok200)
        <|> (
          Method.post
          >- contenttype([Spec.Accept.json(Decoders.json)])
          >- accept([(MediaType.Plain, s => s)])
          |> handler(insert, Status.Created201)
        )
      );
  };
} /*... Later o*/;