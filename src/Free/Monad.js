var prompt = require('prompt-sync')();

// module Free.Monad

exports.getLine = function() {
  return prompt('> ');
}

exports.putLine = function(str) {
  return function() {
    console.log(str);
  }
}
