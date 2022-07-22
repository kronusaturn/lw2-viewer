var mathjax = require('mathjax-node'),
    input = '';

mathjax.config({ MathJax: { loader: {load: ['ui/safe']} } });

process.stdin.on('data', (chunk) => { input += chunk });

process.stdin.on('end', () =>
	mathjax.typeset( { math: input,
			   format: "inline-TeX",
			   html: true,
			   css: true
			 },
			 (data) => {
				 if(data.errors) {
					 process.exitCode = 1;
				 }
				 process.stdout.write("<style>"+data.css+"</style>");
				 process.stdout.write(data.html);
			 } )
);
