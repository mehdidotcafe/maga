import { createHash } from 'node:crypto'
import { createReadStream } from 'node:fs'
import { readdir } from 'node:fs/promises'
import { pipeline } from 'node:stream/promises'

const HASHING_ALGORITHM = 'sha256'

const listFilesInDir = async (dirname) => {
  try {
    const files = await readdir(dirname)

    return files
  } catch (err) {
    console.error(error)
    return []
  }
}

// const readFileAndGetHash = async (filename) => {
//   const hash = createHash(HASHING_ALGORITHM);

//   hash.on('data', (toto) => {
//     console.log(toto.toString('hex'))
//   })

//   pipeline(
//     createReadStream(filename),
//     hash,
//   )


//   console.log(hash.read())

//   return hash.digest('hex')
// }

const readFileAndGetHash = (filename) => new Promise((resolve, reject) => {
  const hash = createHash(HASHING_ALGORITHM);
  const stream = createReadStream(filename)

  stream.on('error', reject)
  stream.on('data', (data) => hash.update(data))
  stream.on('end', () => resolve(hash.digest('hex')))
})

let files = await listFilesInDir('./')

const hashes = await Promise.all(files.map(readFileAndGetHash))

console.log(hashes)