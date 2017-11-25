var AutoLanguageClient = require('atom-languageclient').AutoLanguageClient;

exports.makeLanguageClient = function (clientMixin, translateSettings, onConnection) {
  var client = new AutoLanguageClient();
  var connection;
  return Object.assign(client, clientMixin, {
    preInitialization: function(conn) {
      connection = conn;
      onConnection(conn);
    },
    postInitialization: function(){
      this._disposable.add(
        atom.config.observe("ide-purescript", function (params) {
          connection.didChangeConfiguration({
            settings: { purescript: translateSettings(params) }
          });
        })
      );
    },
    getGrammarScopes: function() { return [ 'source.purescript']; },
    getLanguageName: function() { return 'PureScript'; },
    getServerName: function() { return 'purescript-language-server' },
    startServerProcess: function (projectPath) {
      console.log("Using project path for cwd: " + projectPath);
      return client.spawnChildNode([ require.resolve('purescript-language-server'), '--stdio', "--config", "{}" ],
        {
          cwd: projectPath
        })
    },
    onDidInsertSuggestion: function (arg, item) {
      console.log(item);
      connection.executeCommand(item.command);
    }
  })
}

exports.executeCommand = function (connection, params) {
  connection.executeCommand(params);
};

exports.onCustom = function (connection, method, callback) {
  connection.onCustom(method, callback);
};
