// module Node.Which

var which = require('which');

exports.whichImpl = function(path, cb, errcb) {
  which(path, { all: true }, function(err, resolved) {
    if (err) {
      errcb(err);
    }
    cb(resolved);
  });
}
