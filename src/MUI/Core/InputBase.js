exports._InputBase = require("@material-ui/mui/core/InputBase").default;
exports._eqMarginProp = function(left){ return function(right){ return left === right }};
exports._ordMarginProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};