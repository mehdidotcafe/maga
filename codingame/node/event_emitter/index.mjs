import createEe from "./emitter.mjs";

const abortSignal = AbortSignal.timeout(5000)
const interval = 1000;

let carry = 0

const fn = () => {
  ++carry

  if (carry % 5 === 0) {
    throw new Error(`Error-${carry}`)
  }

  return `Hello-${carry}`
}

const buildAndSubscribeToEe = () => new Promise((resolve, reject) => {
    const ee = createEe(interval, abortSignal, fn);

    ee.on("data", (data) => {
      console.log("Received data:", data);
    });
    ee.on("error", (error) => {
      console.log("Received error:", error);
    });
    ee.on("close", () => {
      console.log("Closed");
      resolve();
    });
  });

(async () => {
  try {
    await buildAndSubscribeToEe();
  } catch (err) {
    console.error(err);
  }
})();
