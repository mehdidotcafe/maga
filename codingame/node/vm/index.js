import { createContext, Script, runInContext } from "node:vm"
import { writeFileSync } from "node:fs"

const context = {
  toto: 42,
  console: {
    log: (value) => writeFileSync('./script_output_1', value.toString() )
  }
}

runInContext("console.log(toto + 8)", createContext(context))


const context2 = {
  toto: 1,
  console: {
    log: (value) => writeFileSync('./script_output_2', value.toString() )
  }
}

new Script("console.log(toto + 8)").runInContext(createContext(context2))