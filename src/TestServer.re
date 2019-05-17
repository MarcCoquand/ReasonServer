open Server;

module User = {
  type user = {name: string};

  let decoder = json => {
    name: json |> Json.Decode.field("name", Json.Decode.string),
  };
  let postHandler = (int, user) =>
    // Do stuff with int, user
    Builder.sendText("updated user!", ~code=Status.Success.Status200);

  module Spec = {
    open Spec;
    let api: App.spec =
      is("user") >- int >- jsonBody(decoder) |> post(postHandler);
  };
};

module Specification = {
  open Spec;
  let myApp: App.spec = is("api") >- oneOf([User.Spec.api]);
};