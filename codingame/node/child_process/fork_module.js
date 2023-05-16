
console.log('Forked process', process.argv[2])

process.on('message', (m) => console.log(m))
