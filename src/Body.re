open Status
type unparsed = string;

type parser('a) = unparsed => 'a;

let parseJson = (parser, body) =>
    Response.fromOption(parser(body), Failure.Status400, ~message="Invalid JSON format.", body);
