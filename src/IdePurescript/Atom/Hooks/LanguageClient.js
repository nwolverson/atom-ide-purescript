var AutoLanguageClient = require('atom-languageclient').AutoLanguageClient;

exports.makeLanguageClient = function (clientMixin, translateSettings, fixTypo, onConnection) {
  var client = new AutoLanguageClient();
  var connection;
  var baseCodeActions = client.getCodeActions;
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
      return client.spawnChildNode([ require.resolve('purescript-language-server'), '--stdio' ],
        {
          cwd: projectPath
        })
    },
    onDidConvertAutocomplete: function (item, suggestion, request) {
      suggestion.command = item.command;
    },
    onDidInsertSuggestion: function (item) {
      if (item.suggestion.command) {
        connection.executeCommand(item.suggestion.command);
      }
    },
    // TODO this is the worst pile of hacks
    getCodeActions: function (editor, range, diagnostics) {
      return baseCodeActions.call(this, editor, range, diagnostics).then(function (results) {
          return results && results.map(function (result) {
              var apply = result.apply.bind(result);
              return Object.assign(result, {
                apply: function() {
                  return result.getTitle().then(function (title){
                    if (title === "Fix typo/add import") {
                      return fixTypo(connection, range);
                    }
                    return apply();
                  });
                }
              });
            });
          });
    }
  });
}

exports.executeCommandImpl = function (connection, params) {
  return connection.executeCommand(params);
};

exports.onCustomImpl = function (connection, method, callback) {
  connection.onCustom(method, callback);
};
