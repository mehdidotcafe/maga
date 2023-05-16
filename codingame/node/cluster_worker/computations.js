const cluster = require('node:cluster')

const startThread = (workerPath) => (input) => new Promise((resolve, reject) => {
  cluster.setupPrimary({
    exec: workerPath,
    args: [ input ]
  });      

  const worker  = cluster.fork()

  worker.on('message', (message) => {
    worker.disconnect()

    resolve(message)
  })
  worker.on('error', (err) => {
    worker.disconnect()

    reject(err)
  })
})

function doComputations(inputs, workerPath) {
    return Promise.all(inputs.map(startThread(workerPath)))
}

module.exports = doComputations


