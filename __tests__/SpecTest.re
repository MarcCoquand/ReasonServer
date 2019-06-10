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
    let fromQuery = asList =>
      Spec.endpoint(~handler=asList)
      |> Spec.accept([
           Spec.Contenttype.json(Json.Encode.list(Encoders.json)),
           Spec.Contenttype.plain(Encoders.plainList),
         ])
      |> Spec.query("year", Belt.Int.fromString)
      |> Spec.query("author", s => Some(s))
      |> Spec.success;

    let fromSpecific = (specific, id) =>
      Spec.endpoint(~handler=specific(id))
      |> Spec.accept([
           Spec.Contenttype.json(Encoders.json),
           Spec.Contenttype.plain(Encoders.plain),
         ])
      |> Spec.success;

    let create = addToDatabase =>
      Spec.endpoint(~handler=addToDatabase)
      |> Spec.accept([
           Spec.Contenttype.json(s =>
             Json.Encode.object_([("message", Json.Encode.string(s))])
           ),
           Spec.Contenttype.plain(id),
         ])
      |> Spec.contentType([Spec.Accept.json(Decoders.json)])
      |> Spec.success(~code=Status.Created201);
    open Uri;
    let router: Uri.t(Spec.endpoint => Spec.endpoint, Spec.endpoint) =
      oneOf([
        Method.get
        -/- oneOf([
              int ==> fromSpecific(mockById),
              Method.get ==> fromQuery(asList),
            ]),
        Method.post ==> create(insert),
      ]);
  };
};

//... Later on
module Server = {
  open Uri;

  //Extend with more afterwards, like is("user") -/- User.spec
  let router = oneOf([is("book") -/- Book.Api.router]);
};