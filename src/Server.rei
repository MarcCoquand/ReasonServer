module Response: {
  type t = {
    code: Cause.Status.code,
    headers: Cause.Header.Map.t(string),
    body: option(string),
    encoding: Cause.Encoding.t,
  };
/** A response sent from the server. You should never need to manually
create this unless it is for testing purposes. Instead use Builders.*/
};
module Builder: {
  /** Builders allow us to construct responses with automatic failure handling.
  If any step in the builder fails it will show the failure and skip
  the rest of the operations. */
  type t('a);
  let andThen:
    (
      ~failureCode: Cause.Status.code=?,
      ~failureMessage: string=?,
      ~failureContentType: string=?,
      'a => option('b),
      t('a)
    ) =>
    t('b);
  /** [andThen failureCode=500, failureMessage="Internal Server Error", failureContentType="text/plain", operation]
      Attempt an operation which might fail. This allows us to wire together steps
      that might fail. For example:
      // Database.get(id) => option(User)
      let userFromId = (id): Builder(User) =>
        Builder.andThen(~failureCode=Status.NotFound404, ~failureMessage="User not found", Database.get(id))

      ... Later on in your handler

      let getUser(id) =
        userFromId(id)
        |> Builder.sendJson
      */
  let map: ('a => 'b, t('a)) => t('b);
  /** [map transformer builder] modify the value with a given transformer. If
      a prior computation has failed it will not perform any transformation but
      instead just return the failed request. See also List.map and Belt.Option.map

      For example:

      let getUser(id) =
        userFromId(id) // Should this fail then it won't perform hidePassword
        |> Builder.map(hidePassword)
        |> Builder.sendJson

       */
  let setHeader: (Cause.Header.Map.key, string, t('a)) => t('a);
  /** Sets a given header to a given value. Will override if keys are the same */
  let setContentType: (string, t('a)) => t('a);
  let setContentLength: (string, t('a)) => t('a);
  let send:
    (Cause.Status.code, ~encoding: Cause.Encoding.t=?, t(option(string))) =>
    Response.t;
  let sendJson: (~code: Cause.Status.code=?, t(Js.Json.t)) => Response.t;
  /** Set content type to application/json and returns the resopnse. */
  let sendHtml:
    (~code: Cause.Status.code=?, string,t('a) => Response.t;
  /** Set content type to application/html and returns the resopnse. */
  let sendText: (~code: Cause.Status.code=?, string, t('a)) => Response.t;
  /** Set content type to text/plain and returns the resopnse. */
};
module App: {
  type t = Cause.Request.t => Response.t;
  type responseBuilder;
  /** [responseBuilder] constructs a response using Builder.*/
  type spec = Cause.Spec.t(responseBuilder => responseBuilder, responseBuilder);
  /** This type represent the specification for
      routing and parsing of request information. It ensures that the handler gets all
      the correct information. This allows you to piece together smaller apis
      into one big program. Note that ReasonML can not deduce this type and it
      needs to be explicitly annotate. For example:
      // User API for JSON
      open Spec
      let getUserHandler: id: int => Response.t(Json.t)
      let postUserHandler: id: option(int) => user: User.t => Response.t(Jsont.)
      let deleteUserHandler: id: int => Response.t(Json.t)
      let putUserHandler: id: int => user: User.t => Response.t(Json.t)

      // Note userSpec is explicitly annotatedby writing userSpec: App.spec
      let userSpec: App.spec = Require.oneOf(
           [Require.int |> get(getUserHandler),
            query("id", Query.int) >- Require.jsonBody |> post(postUserHandler),
            Require.int |> delete(deleteUserHandler),
            Require.int >- Require.jsonBody |> put(putUserHandler)
            ])

      // Note myApi is explicitly annotated by writing myApi: App.spec
      let myApi: App.spec =
        Require.is("api")
        >- Require.oneOf(["user" >- userSpec, ...Add your other apis here])

      (>-) operator is a spec composer. This operator is used to build bigger
      specs from smaller ones. It makes it easy to test each in isolation.

      Handlers need to match the EXACT same arguments as the spec, otherwise
      they will not compile. For example:

      // Throws error because handler does not take string as an argument!
      let handler = (i: int) => .... // No string in the argument list!
      let s: spec = Require.int >- Require.string |> handler

      A spec will call the function with the arguments in the SAME ORDER as the
      spec. So if a spec is Require.int >- Require.string the function needs to
      have the order (i: int, s: string) or else it will not compile.
      Error messages from invalid parameters are cryptic, thefirst thing I would
      check for if I get an error in my spec is if all the parameters are correct.

      For more information on how to write Specs see Spec.rei.
      */
  let runRequest:(Request.t, spec) => Response.t
  /** Run a given request against a spec. Useful for testing. */
  let makeApp: (spec, ~notFound: string=?) => t;
  /** [makeApp fromSpec ~notFound="<h1>Not found</h1>"]
       Create an application from a given specification. If a route is not found
       it will display notFound.  */
  let start: (~port: int, t) => unit;
  /** Starts a sever on a given port (Default is 3000).

  For example:
  open Spec

  let echoApp = makeApp(Require.text |> get(string => "Received: " ++ string))
  start(~port=8000, echoApp)
  Js.log("Running server on localhost:8000")
  */;
  let startSecure:
    (~port: int, ~keyFilepath: string, ~certificateFilepath: string, t) => unit;
  /** Starts a secure sever on a given port (Default is 3000). Will throw error
  if keyFilepath or certificateFilepath is not found. */;
};