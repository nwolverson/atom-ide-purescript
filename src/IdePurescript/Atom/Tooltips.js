exports.mkTooltipProvider = function(provide) {
    var HoverTooltips = require('hover-tooltips').HoverTooltips;
    function ToolTipProvider() {
      HoverTooltips.call(this);
      this.syntax = 'source.purescript';
      this.provider = provide;
      this.activate();
    }
    ToolTipProvider.prototype.dispose = function () { this.deactivate(); };
    ToolTipProvider.prototype = Object.create(HoverTooltips.prototype);

    return new ToolTipProvider();
};

exports.showTooltip = function (pos, text) {
  var editor = atom.workspace.getActiveTextEditor();
  var ed = editor;
  var pos = editor.editorElement.pixelPositionForBufferPosition(pos);
  var elt = ed.editorElement.shadowRoot.querySelector("div.scroll-view");
  var bb = elt.getBoundingClientRect();

  var style = "left: " + (pos.left + bb.left) + "px;" + "top: " + (pos.top + bb.top + 30) + "px;"

  var tt = atom.tooltips.add(ed.editorElement, {
    title: text,
    trigger: 'manual',
    placement: 'bottom',
    placement: function (tooltip, trigger) {
      window.setTimeout(function() {
        tooltip.style = style;
      }, 0);
    }
  });

  window.setTimeout(function() {
    dispose();
  }, 5000);
  ed.editorElement.addEventListener('mousemove', dispose);
  var onChangeCursor = ed.onDidChangeCursorPosition(dispose);

  function dispose() {
    ed.editorElement.removeEventListener('mousemove', dispose);
    tt.dispose();
    onChangeCursor.dispose();
  };

  return dispose;
};
