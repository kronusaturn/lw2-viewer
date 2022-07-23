var mathjax = require('mathjax-node'),
    chunks = [],
    size = null;

mathjax.config({ MathJax: { loader: {load: ['ui/safe']},
			    extensions: ["Safe.js"] } });

process.stdin.on('data', (chunk) => {
	if(size === null) {
		size = chunk.readUInt32LE();
		onDataChunk(chunk.subarray(4));
	} else {
		onDataChunk(chunk);
	}
});

function onDataChunk(chunk) {
	if(chunk.length > size) {
		var excessChunk = chunk.subarray(size);
		process.stdin.unshift(excessChunk);
		chunk = chunk.subarray(0, size);
	}

	chunks.push(chunk);

	if(chunk.length == size) {
		onMessage(Buffer.concat(chunks).toString());
		size = null;
		chunks = [];
	}
	else {
		size -= remainingChunk.length;
	}
}

function onMessage(input) {
	mathjax.typeset(
		{ math: input,
		  format: "inline-TeX",
		  html: true,
		  css: true
		},
		(data) => {
			var sizeBuf = Buffer.alloc(4);
			if(data.errors) {
				sizeBuf.writeUInt32LE(0);
				process.stdout.write(sizeBuf);
			}
			else {
				var dataBuf = Buffer.from("<style>"+data.css+"</style>"+data.html);
				sizeBuf.writeUInt32LE(dataBuf.length);
				process.stdout.write(sizeBuf);
				process.stdout.write(dataBuf);
			}
		}
	);
}
