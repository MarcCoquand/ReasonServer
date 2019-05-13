
type unsafeRequest('b) = {
    body: string,
    path: string,
    headers : Js.Dict.t(string),
    statusCode: int,
    method: string
    }


type unsafeResponse('b) = {
    bodyString: option(string),
    bodyBuffer: option(Node_buffer.t),
    headers: Js.Dict.t(string),
    statusCode: int,
    encoding: string,
    statusMessage: string
}
type unsafeApplication('b,'c) = unsafeRequest('b) => unsafeResponse('c)

let unsafeCreateServer = (port: int, makeResponse: unsafeApplication('b,'c) ): unit => [%bs.raw
  {|
 function (port, makeResponse) {
    const querystring = require('querystring');
    const http = request('http')
    const httpServer = (http.createServer((request, response) => {
        const chunks = [];
        const headers = request.headers
        if (headers.["charset"] === 'utf-8' || headers["Content-Type"] === 'application/json')
            request.setEncoding('utf8')
        request.on('data', data => chunks.push(data));
        request.on('end', () => {
            const data = Buffer.concat(chunks);
            console.log('Data: ', data);
            let payload = data
            if (headers["application/json])
                payload = JSON.parse(payload);
            if (headers["application/x-www-form-urlencoded"])
                payload = querystring.parse(buffer.toString())
            if (headers["text/plain"])
                payload = buffer.toString()

            const requestContent =
                {  body: payload,
                   path: request.url,
                   headers: request.headers,
                   method: request.method
                };
            const result = requestHandle(requestContent)
            result.headers.forEach([a,b] =>
                response.setHeader(a,b))
            response.statusCode = result.statusCode
            response.statusMessage = result.statusMessage
            if (result.bodyString !== undefined)
                response.write(result.bodyString,encoding)
            else if(result.bodyBuffer !== undefined)
                response.write(result.bodyBuffer,encoding)
            response.end
        });
    }).listen(port)
};
|}
];
