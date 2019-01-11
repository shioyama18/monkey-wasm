import * as wasm from "monkey_wasm";

const runButton = document.getElementById('run');
let input = document.getElementById('source');
let output = document.getElementById('output');

runButton.addEventListener("click", event => {
	let source = input.value;
	let evaluated = wasm.monkey_eval(source);

	output.textContent = evaluated;
});

