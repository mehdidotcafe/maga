import https from 'node:https'

const options = {
  hostname: 'google.com',
  port: 443,
  path: '/',
  method: 'GET',
};

const req = https.request(options, (res) => {
  console.log('statusCode:', res.statusCode)
  console.log('headers:', res.headers)

  res.on('data', (d) => {
    process.stdout.write(d)
  })
})

req.end()