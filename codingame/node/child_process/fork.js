import { fork } from 'node:child_process'

const child = fork('./fork_module', ['param1', 'param2'])

child.send({data: 'This is some data'})