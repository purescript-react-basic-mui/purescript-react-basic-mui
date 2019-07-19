/* global exports, require */

exports.allIcons = Object.keys(require('@material-ui/icons'));


var Bagpipe = require('bagpipe');

exports.bagpipe = function(number) {
  return function() {
    return new Bagpipe(number);
  };
};

exports.push = function(bagpipe) {
  return function(eff) {
    return function() {
      bagpipe.push(eff);
    };
  };
};
