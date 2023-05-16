import { Transform, pipeline } from "node:stream"

const filterStream = (inputStream, pattern) => {
  const outputStream = new Transform({
    transform: (data, _, done) => {
      done(null, pattern.test(data) ? data : null)
    }
  })

  return pipeline(inputStream, outputStream, (err) => {
    if (err) {
      console.log('ERROR', err)
    }
  })
}

export default filterStream