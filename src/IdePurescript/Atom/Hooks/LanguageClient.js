var AutoLanguageClient = require('atom-languageclient').AutoLanguageClient;
var Disposable = require('atom').Disposable;

exports.makeLanguageClient = function (clientMixin, translateSettings, fixTypo, onConnection, onDispose, commands) {
  var client = new AutoLanguageClient();
  var connection;
  var baseCodeActions = client.getCodeActions;
  var baseGetSuggestions = client.getSuggestions;

  Object.keys(commands).forEach(function (cmd) {
    client._disposable.add(atom.commands.add("atom-workspace", "ide-purescript:"+cmd,
      function() {
        var ed = atom.workspace.getActiveTextEditor();
        if (!ed) {
          console.error("No active text editor");
          return;
        }
        client.getConnectionForEditor(ed)
          .then(commands[cmd], console.error);
        }
    ));
  });

  return Object.assign(client, clientMixin, {
    preInitialization: function(conn) {
      connection = conn;
      onConnection(conn);
    },
    postInitialization: function(server) {
      var disposable = server.disposable;
      disposable.add(
        atom.config.observe("ide-purescript", function (params) {
          connection.didChangeConfiguration({
            settings: { purescript: translateSettings(params) }
          });
        })
      );
      disposable.add(new Disposable(onDispose));
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
    getSuggestions: function (request) {
      this.autoComplete = null;
      return baseGetSuggestions.call(this, request);
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
