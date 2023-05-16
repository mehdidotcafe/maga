const cluster = require('node:cluster')

if (!cluster.isPrimary) {
  if (process.argv[2] === '1') {
    setTimeout(() => {
      process.send(process.argv[2] * 2);
    }, 5000)
  } else {
    process.send( process.argv[2] * 2);
  }
}
