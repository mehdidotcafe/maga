import filterStream from './filter.mjs'

import { Readable } from 'node:stream';

const chunks = ['aaa', 'bbb', 'ccc', 'aaa', 'ddd', 'aaa']
let count = 0

const pattern = /aaa|bbb/i

const inputStream = new Readable({
  read() {
    if (count < chunks.length) {
      this.push(chunks[count])
      count++
    } else {
      this.push(null)
    }
  }
})

const outputStream = await filterStream(inputStream, pattern)

outputStream.on('data', (chunk) => {
  console.log('From output', chunk.toString())
})
