// module DOM.Util

exports.setTimeout = function(n) {
  return function (f) {
    return function() {
      return setTimeout(f, n);
    };
  };
};
