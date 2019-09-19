exports._IconButton = require("@material-ui/core/IconButton").default;
exports._eqColorProp = function(left){ return function(right){ return left === right }};
exports._ordColorProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};
exports._eqEdgeProp = function(left){ return function(right){ return left === right }};
exports._ordEdgeProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};
exports._eqSizeProp = function(left){ return function(right){ return left === right }};
exports._ordSizeProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};