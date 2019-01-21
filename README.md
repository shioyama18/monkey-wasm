# monkey
## About
Rust implementation of Monkey programming language from the book *Writing an Interpreter in Go*.
This interpreter is also compiled into WebAssembly format so it can run on browser.

## Prerequisite
*wasm-pack* and *npm* are unnecessary if you are only running the REPL
### Install Rust Toolchain

[Follow these instructions to install Rust Toolchain](https://www.rust-lang.org/tools/install)

### Install npm

[Follow these instructions to install npm](https://www.npmjs.com/get-npm)

If you already have npm installed, make sure it is up to date with the command below:

```bash
$ sudo npm install npm@latest -g
```

## Usage
### Running the REPL
#### Build the project using cargo

```bash
$ cargo build --release
	...
    Finished release [optimized] target(s) in 5m 16s 
```

#### Start the repl

```bash
$ target/release/monkey_repl
Welcome to the Monkey programming language!
Feel free to type in commands
>> 1 + 1
2
>> exit
```

### Running it on browsers
#### Install the dependencies

In the *web* directory, run *npm install* to install the local development server and its dependencies.

```bash
$ sudo npm install
```

#### Use the local package in *web*

In the *pkg* directory, run *npm link* so that the local package can be depended upon by other local packages without publishing them to npm.

```bash
$ sudo npm link
```

In the *web* directory, run *npm link monkey_wasm* to use the linked verstion of the monkey_wasm.

```bash
$ sudo npm link monkey_wasm
```

#### Serving Locally

Run the server with *npm run start* and navigate your Web browser to http://localhost:8080/ and you should be able to run monkey language on the browser.

![playground](https://github.com/shioyama18/monkey-wasm/blob/master/assets/playground.jpg)
