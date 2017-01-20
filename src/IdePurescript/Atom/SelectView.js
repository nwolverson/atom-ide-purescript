var SelectListView = require('atom-space-pen-views').SelectListView;

exports.selectListViewStaticInlineImpl = function(viewForItem, confirmed, filterKey, items) {
  function PurescriptSelectListView() {
    SelectListView.call(this);
    this.addClass('overlay');
    this.addClass('ps-inline-overlay');
  }

  PurescriptSelectListView.prototype = Object.create(SelectListView.prototype);
  PurescriptSelectListView.prototype.viewForItem = viewForItem;
  PurescriptSelectListView.prototype.show = function() {
    list.storeFocusedElement();
    var editor = atom.workspace.getActiveTextEditor()
      , marker = editor.getLastCursor().getMarker();
    this.panel = editor.decorateMarker(marker, { type: "overlay", position: "tail", item: this });
    setTimeout(function() {
      list.focusFilterEditor();
    }, 20);
  };
  PurescriptSelectListView.prototype.confirmed = function(item) {
    confirmed(item);
    this.panel && this.panel.destroy();
    this.restoreFocus();
  };
  PurescriptSelectListView.prototype.cancelled = function() {
    this.panel && this.panel.destroy();
    this.restoreFocus();
  };
  PurescriptSelectListView.prototype.getFilterKey = function() {
    return filterKey;
  };

  var list = new PurescriptSelectListView();
  list.setItems(items);
  list.show();
  return {};
}

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
    this.restoreFocus();
  };
  PurescriptSelectListView.prototype.cancelled = function() {
    this.panel && this.panel.destroy();
    this.restoreFocus();
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

    var searchText = "";
    this.getEmptyMessage = function () {
      var curText = buffer.getText();
      if (curText === "") {
        return "Enter text to search";
      } else if (curText !== searchText) {
        // Text changing and query not made yet
        return "";
      } else {
        return "No matches found";
      }
    };

    var editor = this[0].firstChild.getModel();
    var buffer = editor.getBuffer();
    buffer.stoppedChangingDelay = changeDelay;
    var that = this;
    this.setItems([]);

    function commitChanges() {
      var currentSearchText = buffer.getText();
      getCompletions(currentSearchText).then(function (items) {
        searchText = currentSearchText;
        that.setItems(items);
      });
    }

    buffer.onDidStopChanging(commitChanges);
    atom.commands.add(this[0].firstChild, "core:confirm", function (e) {
        e.stopPropagation();
        commitChanges();
    });
  };

  var list = new PurescriptDynamicSelectListView();
  list.show();
  return {};
}
