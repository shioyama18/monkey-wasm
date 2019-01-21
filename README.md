# monkey
## About
Rust implementation of Monkey programming language from the book *Writing an Interpreter in Go*.
This interpreter is also compiled into WebAssembly format so it can run on browser.

## Prerequisite
Steps 2 and 3 are unnecessary if you are only running the REPL
1. Install Rust Toolchain  
[Follow these instructions to install Rust Toolchain](https://www.rust-lang.org/tools/install)

2. Install wasm-pack  
[Follow these instructions to install wasm-pack](https://rustwasm.github.io/wasm-pack/installer/)

3. Install npm  
[Follow these instructions to install npm](https://www.npmjs.com/get-npm)

## Usage
### Running the REPL
1. Build the program

```bash
$ cargo build
   Compiling monkey_wasm v0.1.0 (/home/shion/rust/monkey-wasm)
    Finished dev [unoptimized + debuginfo] target(s) in 0.59s
```

2. Start the repl

```bash
$ target/debug/monkey_repl
Welcome to the Monkey programming language!
Feel free to type in commands
>> 1 + 1
2
>> exit
```

### Running it on browsers
1. Build the project using wasm-pack

```bash
$ wasm-pack build
```

2. Install the dependencies

In the *web* directory, run *npm install* to install the local development server and its dependencies.

```bash
$ sudo npm install
```

3. Use the local package in *web*

In the *pkg* directory, run *npm link* so that the local package can be depended upon by other local packages without publishing them to npm.

```bash
$ sudo npm link
```

In the *web* directory, run *npm link monkey_wasm* to use the linked verstion of the monkey_wasm.

```bash
$ sudo npm link monkey_wasm
```

4. Serving Locally

Run the server with *npm run start* and navigate your Web browser to http://localhost:8080/ and you should be able to run monkey language on the browser.

