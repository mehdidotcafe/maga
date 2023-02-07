'use strict';

var _input: string = '';
var _inputAsArray: string[] = [];
var _index = 0;
process.stdin.on('data', (data) => { _input += data; });
process.stdin.on('end', () => {
    _inputAsArray = _input.split(new RegExp('[ \n]+'));
    main();    
});
function read() { return _inputAsArray[_index++]; }

function main() {
  const firstName = read()
  const lastName = read()
  const id = +read()
  const numScores = +read()
  const testScores = new Array(numScores)
  
  for (var i = 0; i < numScores; i++) {
      testScores[i] = +read()
  }

  const s = new Student(firstName, lastName, id, testScores)
  s.printPerson()
  s.calculate()
  console.log('Grade: ' + s.calculate())
}

class Person {
  constructor(private firstName: string, private lastName: string, private identification: number) {}
  
  printPerson() {
      console.log(`Name: ${this.lastName}, ${this.firstName}\nID: ${this.identification}`);
  }
}


class Student extends Person {
  constructor(firstName: string, lastName: string, identification: number, private scores: number[]) {
    super(firstName, lastName, identification);
  }

  public calculate(): string {
    const average = this.scores.reduce((a, b) => a + b) / this.scores.length;
    if (average >= 90 && average <= 100) {
      return 'O';
    } else if (average >= 80 && average < 90) {
      return 'E';
    } else if (average >= 70 && average < 80) {
      return 'A';
    } else if (average >= 55 && average < 70) {
      return 'P';
    } else if (average >= 40 && average < 55) {
      return 'D';
    } else {
      return 'T';
    }
  }
}
