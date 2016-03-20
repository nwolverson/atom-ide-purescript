// module IdePurescript.Atom.SelectView

var SelectListView = require('atom-space-pen-views').SelectListView;

exports.selectListViewStaticImpl = function(viewForItem, confirmed, filterKey, items) {
  function PurescriptSelectListView() {
    SelectListView.call(this);
  }

  PurescriptSelectListView.prototype = Object.create(SelectListView.prototype);
  PurescriptSelectListView.prototype.viewForItem = viewForItem;
  PurescriptSelectListView.prototype.show = function() {
    list.storeFocusedElement();
    this.panel = atom.workspace.addModalPanel({item: list, visible: true});
    list.focusFilterEditor();
  };
  PurescriptSelectListView.prototype.confirmed = function(item) {
    confirmed(item);
    this.panel && this.panel.destroy();
  };
  PurescriptSelectListView.prototype.cancelled = function() {
    this.panel && this.panel.destroy();
  };
  PurescriptSelectListView.prototype.getFilterKey = function() {
    return filterKey;
  };

  var list = new PurescriptSelectListView();
  list.setItems(items);
  list.show();
  return {};
}


exports.selectListViewDynamicImpl = function(viewForItem, confirmed, filterKey, filterQuery, getCompletions, changeDelay) {
  function PurescriptDynamicSelectListView() {
    SelectListView.call(this);
  }

  PurescriptDynamicSelectListView.prototype = Object.create(SelectListView.prototype);
  PurescriptDynamicSelectListView.prototype.viewForItem = viewForItem;
  PurescriptDynamicSelectListView.prototype.show = function() {
    list.storeFocusedElement();
    this.panel = atom.workspace.addModalPanel({item: list, visible: true});
    list.focusFilterEditor();
  };
  PurescriptDynamicSelectListView.prototype.confirmed = function(item) {
    confirmed(item);
    this.panel && this.panel.destroy();
  };
  PurescriptDynamicSelectListView.prototype.cancelled = function() {
    this.panel && this.panel.destroy();
  };
  PurescriptDynamicSelectListView.prototype.getFilterKey = function() {
    return filterKey;
  };
  PurescriptDynamicSelectListView.prototype.getFilterQuery = function() {
    var baseQuery = SelectListView.prototype.getFilterQuery.call(this);
    return filterQuery(baseQuery);
  };
  PurescriptDynamicSelectListView.prototype.initialize = function() {
    SelectListView.prototype.initialize.call(this);
    var editor = this[0].firstChild.getModel();
    var buffer = editor.getBuffer();
    buffer.stoppedChangingDelay = changeDelay;
    var that = this;
    buffer.onDidStopChanging(function(text) {
      getCompletions(buffer.getText()).then(function (items) {
        that.setItems(items);
      });
    })
  };

  var list = new PurescriptDynamicSelectListView();
  list.show();
  return {};
}
