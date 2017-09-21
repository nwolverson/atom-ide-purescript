var AutoLanguageClient = require('atom-languageclient').AutoLanguageClient;

exports.makeLanguageClient = function () {
  var client = new AutoLanguageClient();
  atom.config.set('core.debugLSP', true)
  return Object.assign(client, {
    getGrammarScopes: function() { return [ 'source.purescript']; },
    getLanguageName: function() { return 'PureScript'; },
    getServerName: function() { return 'purescript-language-server' },
    startServerProcess: function (projectPath) {
      console.log("Using project path for cwd: " + projectPath);
      return client.spawnChildNode([ require.resolve('purescript-language-server'), '--stdio', "--config", "{}" ],
        {
          cwd: projectPath
        })
    }
  })
}
