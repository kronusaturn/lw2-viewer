[
  // line-breaks after slashes (for long URLs etc.)
  [/\/+/, '\\&\u200b'],

  // triple prime
  [/'''/g, '\u2034'],
  // beginning "
  [/([^A-Za-z0-9_\)]|^)"(\S)/g, '$1\u201c$2'],
  // ending "
  [/(\u201c[^"]*)"([^"]*$|[^\u201c"]*\u201c)/g, '$1\u201d$2'],
  // remaining " at end of word
  [/([^0-9])"/g, '$1\u201d'],
  // double prime as two single quotes
  [/''/g, '\u2033'],
  // beginning '
  [/(\W|^)'(\S)/g, '$1\u2018$2'],
  // conjunction's possession
  [/([a-z])'([a-z])/ig, '$1\u2019$2'],
  // abbrev. years like '93
  [/(\u2018)([0-9]{2}[^\u2019]*)(\u2018([^0-9]|$)|$|\u2019[a-z])/ig, '\u2019$2$3'],
  // ending '
  [/((\u2018[^']*)|[a-z])'([^0-9]|$)/ig, '$1\u2019$3'],
  // backwards apostrophe
  [/(\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/ig, '$1\u2019'],
  // double prime
  [/"/g, '\u2033'],
  // prime
  [/'/g, '\u2032'],
  
  // turn a hyphen surrounded by spaces, between words, into an em-dash
  [/([a-z\u201d]) - ([a-z\u201c])/ig, '$1\u2014$2'],
  // turn a hyphen between a space and a quote, into an em-dash
  [/([a-z]) -(\u201d)/ig, '$1\u2014$2'],
  [/(\u201c)- ([a-z])/ig, '$1\u2014$2'],
  // turn a double hyphen, optionally surrounded by spaces, between words, into an em-dash
  [/([a-z]) ?-- ?([a-z])/ig, '$1\u2014$2'],
  
  // Two spaces after a period is INCORRECT.
  [ /(\w[\.\?\!])  (\w)/g, '$1 $2'],
  
  // ellipsis rectification
  [/(\s)\.\.\./g, '$1…'],
  [/\.\.\.(\s)/g, '…$1'],
  
  // Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign
  [/(\s)-( ?)([0-9])/g, '$1\u2212$2$3'],
  
  // Arrows
  [/(\s)->(\s)/g, '$1\u2192$2'],
  [/(\s)<-(\s)/g, '$1\u2190$2'],
  [/(\s)=>(\s)/g, '$1\u21d2$2'],
  [/(\s)<=(\s)/g, '$1\u21d0$2']
]
