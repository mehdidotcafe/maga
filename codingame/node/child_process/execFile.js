import { execFile } from 'child_process'

execFile('node', ['--version'], (err, stdout) => {
  console.log(err)
  console.log('===========')
  console.log(stdout)
})



execFile('node', ['--wrong_flag'], (err, stdout) => {
  console.log(err)
  console.log('===========')
  console.log(stdout)
})
