import {
  setImmediate,
  setInterval,
} from "node:timers/promises"
import EventEmitter from 'events'

const safeCallFunctionAndEmit = (functionToCall, ee) => {
  try {
    const res = functionToCall()

    ee.emit('data', res)
  } catch (err) {
    ee.emit('error', err)
  }
}

const createEe = (interval, signal, fn) => {
  const ee = new EventEmitter()

  ee.on('newListener', (event) => {
    if (event === 'data') {
      setImmediate(undefined, { signal }).then(() => safeCallFunctionAndEmit(fn, ee))
    }
  })

  ;(async () => {
    for await (const _ of setInterval(interval, undefined, { signal })) {
      safeCallFunctionAndEmit(fn, ee)
    }
  })()

  return ee
}

export default createEe