exports._Button = require("@material-ui/core/Button").default;
exports._eqColorProp = function(left){ return function(right){ return left === right }};
exports._ordColorProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};
exports._eqSizeProp = function(left){ return function(right){ return left === right }};
exports._ordSizeProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};
exports._eqVariantProp = function(left){ return function(right){ return left === right }};
exports._ordVariantProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};