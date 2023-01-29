"use strict";

process.stdin.resume();
process.stdin.setEncoding("utf-8");
let inputString: string = "";
let inputLines: string[] = [];
let currentLine: number = 0;
process.stdin.on("data", function (inputStdin: string): void {
  inputString += inputStdin;
});

process.stdin.on("end", function (): void {
  inputLines = inputString.split("\n");
  inputString = "";
  main();
});

function readLine(): string {
  return inputLines[currentLine++];
}

function main() {
  const T = parseInt(readLine());
  for (let i = 0; i < T; i++) {
    const age = parseInt(readLine());
    const p = new Person(age);
    p.amIOld();
    for (let j = 0; j < 3; j++) {
      p.yearPasses();
    }
    p.amIOld();
    console.log("");
  }
}

class Person {
  #initialAge: number;
  #currentAge: number;

  constructor(age: number) {
    if (age < 0) {
      console.log("Age is not valid, setting age to 0.");
      this.#initialAge = 0;
    } else {
      this.#initialAge = age;
    }

    this.#currentAge = this.#initialAge;
  }

  yearPasses() {
    ++this.#currentAge;

    return this;
  }

  private oldnessPolicy() {
    if (this.#currentAge < 13) {
      return "You are young.";
    } else if (this.#currentAge < 18) {
      return "You are a teenager.";
    } else {
      return "You are old.";
    }
  }

  amIOld() {
    console.log(this.oldnessPolicy());
  }
}
