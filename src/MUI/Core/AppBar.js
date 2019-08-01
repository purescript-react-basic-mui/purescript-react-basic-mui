exports._AppBar = require("@material-ui/core/AppBar").default;
exports._eqColorProp = function(left){ return function(right){ return left === right }};
exports._ordColorProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};
exports._eqPositionProp = function(left){ return function(right){ return left === right }};
exports._ordPositionProp = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};