open Server;

module User = {
  type user = {name: string};

  let decoder = json => {
    name: json |> Json.Decode.field("name", Json.Decode.string),
  };
  let postHandler = (int, user) =>
    // Do stuff with int, user
    Builder.sendText(~code=Status.Created201, "updated user!");

  module Spec = {
    open Spec;
    let api: App.spec = int >- jsonBody(decoder) |> post(postHandler);
  };
};

module Books = {
  type book = {
    name: string,
    published: int,
    author: string,
  };

  let decoder = json => {
    name: json |> Json.Decode.field("name", Json.Decode.string),
    published: json |> Json.Decode.field("published", Json.Decode.int),
    author: json |> Json.Decode.field("author", Json.Decode.string),
  };
  let books = (published, author) =>
    // Do stuff with published and author
    Builder.sendText("Here are your books!");

  module Spec = {
    open Spec;
    let api: App.spec =
      query("published", Query.int)
      >- query("author", Query.text)
      |> get(books);
  };
};

module Specification = {
  open Spec;
  let myApp: App.spec = is("api") >- oneOf([is("user") >- User.Spec.api]);
};