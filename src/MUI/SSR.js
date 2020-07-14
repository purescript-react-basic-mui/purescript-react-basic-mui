/* global exports, require */
/* jshint -W097 */

"use strict";

var styles = require('@material-ui/styles');
var coreStyles = require('@material-ui/core/styles');
var reactDOM = require('react-dom/server');

exports.serverStyleSheets = function() {
  var ServerStyleSheets = styles.ServerStyleSheets || styles.default.ServerStyleSheets;
  return new ServerStyleSheets();
};

exports.collect = function(jsx) {
  return function(sheets) {
    return function() {
      return reactDOM.renderToString(sheets.collect(jsx));
    };
  };
};

exports.toString = function(sheets) {
  return function() {
    return sheets.toString();
  };
};

