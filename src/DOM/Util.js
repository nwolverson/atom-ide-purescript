// module DOM.Util

exports.getScrollTop = function(e) {
  return function() {
    return e.scrollTop;
  };
};

exports.setScrollTop = function(e) {
  return function(value) {
    return function() {
      e.scrollTop = value;
    };
  };
};

exports.getScrollHeight = function (e) {
  return function() {
    return e.scrollHeight;
  };
};
