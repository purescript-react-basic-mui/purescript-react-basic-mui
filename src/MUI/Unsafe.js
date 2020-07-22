exports.reallyUnsafeRefOrd = function(a1) {
  return function(a2) {
    if( a1 > a2) {
      return 1;
    } else if(a2 > a1) {
      return -1;
    }
    return 0;
  };
};
