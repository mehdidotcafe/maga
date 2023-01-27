'use strict';

process.stdin.resume();
process.stdin.setEncoding('utf-8');
let inputString: string = '';
let inputLines: string[] = [];
let currentLine: number = 0;
process.stdin.on('data', function(inputStdin: string): void {
    inputString += inputStdin;
});

process.stdin.on('end', function(): void {
    inputLines = inputString.split('\n');
    inputString = '';
    main();
});

function readLine(): string {
    return inputLines[currentLine++];
}

function main() {
  const i = 4
  const d = 4.0
  const s = "HackerRank "

  console.log(i + parseInt(readLine()))
  console.log((d + parseFloat(readLine())).toFixed(1))
  console.log(`${s}${readLine()}`)
}