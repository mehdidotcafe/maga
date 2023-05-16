import { exec } from 'node:child_process'


// working
exec('ls -la', (err, out) => {
  console.log(err)
  console.log('==============')
  console.log(out)
})

// not working
exec('not_exisiting_command', (err, out) => {
  console.log(err)
  console.log('==============')
  console.log(out)
})
