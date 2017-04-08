exports.clearMessages = function (linter) {
  return function () {
    linter.clearMessages();
    return {};
  };
};

exports.setAllMessages = function (linter) {
  return function (messages) {
    return function () {
      linter.setAllMessages(messages);
      return {};
    };
  };
};

exports.setMessages = function (linter) {
  return function (filePath) {
    return function (messages) {
      return function () {
        linter.setMessages(filePath, messages);
        return {};
      };
    };
  };
};

exports.getMessages = function (linter) {
  return function () {
    return Array.from(linter.getMessages());
  };
};
