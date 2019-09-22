var showdown  = require('showdown'),
    converter = new showdown.Converter(),
    markdown = '';

process.stdin.on('data', (chunk) => { markdown += chunk });

process.stdin.on('end', () => {
	process.stdout.write(converter.makeHtml(markdown));
});
