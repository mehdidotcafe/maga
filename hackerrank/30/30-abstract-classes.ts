'use strict';

let _input = '';
let _inputAsArray: string[] = [];
let _index = 0;
process.stdin.on('data', (data) => { _input += data; });
process.stdin.on('end', () => {
    _inputAsArray = _input.split(new RegExp('\n'));
    main();    
});
function readLine() { return _inputAsArray[_index++]; }

/**** Ignore above this line. ****/

class Book {
    constructor(protected title: string, protected author: string) {
        if (this.constructor === Book) {
            throw new TypeError('Do not attempt to directly instantiate an abstract class.'); 
        }
    }
    
    display() {
        console.log('Implement the \'display\' method!')
    }
}

class MyBook extends Book {
    constructor(title: string, author: string, private price: number) {
        super(title, author)
    }
    
    display() {
        console.log(`Title: ${this.title}`)
        console.log(`Author: ${this.author}`)
        console.log(`Price: ${this.price}`)
    }
}

function main() {
    const title = readLine()
    const author = readLine()
    const price = +readLine()
    
    const book = new MyBook(title, author, price)
    book.display()
}
