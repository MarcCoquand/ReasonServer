
const handleApp = makeResponse => (request, response) => {
    const querystring = require('querystring');
    const chunks = [];
    const headers = request.headers
    if (headers["charset"] === 'utf-8' || headers["Content-Type"] === 'application/json')
        request.setEncoding('utf8')
    request.on('data', data => chunks.push(data));
    request.on('end', () => {
        const data = Buffer.concat(chunks);
        let payload = data
        if (headers["application/json"])
            payload = data.toString();
        if (headers["application/x-www-form-urlencoded"])
            payload = data.toString()
        if (headers["text/plain"])
            payload = data.toString()

        const requestContent =
            {  body: payload,
            path: request.url,
            headers: request.headers,
            method: request.method
            };
        const result = makeResponse(requestContent)
        result.headers.forEach(([a,b]) =>
            response.setHeader(a,b))
        response.statusCode = result.statusCode
        response.statusMessage = result.statusMessage
        if (result.bodyString !== undefined) {
            console.log(result)
            response.write(result.bodyString)
            response.end()
        }
        else if(result.bodyBuffer !== undefined) {
            response.write(result.bodyBuffer,encoding)
            response.end("done")
        }
})}

function rawSecureCreateServer(port, key, cert, makeResponse) {
    const https = require('https');
    const fs = require('fs');

    const options = {
      key: fs.readFileSync(key),
      cert: fs.readFileSync(cert)
    };
    let app = https.createServer(options,handleApp(makeResponse));

    app.listen(port, '127.0.0.1');

}

function rawCreateServer(port, makeResponse) {
    const http = require('http');

    let app = http.createServer(handleApp(makeResponse));

    app.listen(port, '127.0.0.1');
}

exports.rawCreateServer = rawCreateServer