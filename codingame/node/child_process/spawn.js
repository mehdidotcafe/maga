import { spawn } from 'node:child_process'

const childLs = spawn('ls', ['-la'])
const childGrep = spawn('grep', ['-c', 'fork'])

childLs.stdout.on('data', data => {
  childGrep.stdin.write(data.toString())
}) 

childLs.stdout.on('close', () => {
  console.log('ls closed')
  childGrep.stdin.end();
})

childGrep.stdout.on('data', data => {
  console.log('Grep output: ', data.toString())
})