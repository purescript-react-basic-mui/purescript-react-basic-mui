exports._ButtonBase = require("@material-ui/core/ButtonBase").default;
exports._eqTypeProp = function(left){ return function(right){ return left === right }};
exports._ordTypeProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};