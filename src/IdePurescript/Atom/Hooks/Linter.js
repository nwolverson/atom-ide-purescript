// module IdePurescript.Atom.Hooks.Linter

exports.register = function(registry) {
  return function(options) {
    return function() {
      return registry.register(options);
    };
  };
};

exports.deleteMessages = function (linter) {
  return function () {
    linter.deleteMessages();
    return {};
  };
};

exports.setMessages = function (linter) {
  return function (messages) {
    return function () {
      linter.setMessages(messages);
      return {};
    }
  }
}
