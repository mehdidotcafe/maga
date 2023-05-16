const doComputations = require("./computations")

const inputs = [1, 5, 8, 238, 29318]

;(async () => {
  const results = await doComputations(inputs, "./worker.js")

  console.log(JSON.stringify(results) === JSON.stringify(inputs.map(r => r * 2)))
})()

