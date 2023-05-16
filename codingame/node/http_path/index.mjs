// JavaScript code​​​​​​‌​‌​​​​‌‌​‌​‌​‌​‌​‌​​‌‌​‌ below
import fs  from "fs"
import http from "http"
import path from "path"
import { pipeline } from "stream"

/**
 *
 * @param {string} folderPath
 * @returns {http.Server}
 */

const fileNotFound = (res, err = 'File not found') => {
    res.writeHead(404, {
        'Content-Type': 'text/plain'
    })
    res.end(err)
}

const genericError = (res, err = 'Something went wrong') => {
    res.writeHead(500, {
        'Content-Type': 'text/plain'
    })
    res.end(err)
}

const serveFolder = (folderPath) => {
    const server = http.createServer()

    server.on('request', (req, res) => {        
        if (req.method !== 'GET') {
            res.writeHead(500, {
                'Content-Type': 'text/plain'
            })
            res.write(JSON.stringify({
                error: {
                    message: 'Please use GET method'
                }
            }))
            res.end()
            return 
        }

        const filePath = path.join(folderPath, req.url)
        const readStream = fs.createReadStream(filePath)

        readStream.on('error', (err) => {
            if (err.code === 'ENOENT') {
                return fileNotFound(res, err.code || err.message)
            } else {
                return genericError(res, err.code || err.message)
            }
        })

        res.status = 200
        pipeline([
            readStream,
            res
        ], () => {})
    })

    return server
};


const server = serveFolder('/home/mehdi/projects/tmp/node_pg')

server.listen(8084)