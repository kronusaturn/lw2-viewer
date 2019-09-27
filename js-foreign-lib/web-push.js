const webPush = require('web-push');
var inputText = "";

process.stdin.on('data', (chunk) => { inputText += chunk });

process.stdin.on('end', () => {
	process.stdout.write(JSON.stringify(eval(inputText)));
});
