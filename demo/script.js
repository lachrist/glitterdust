
var Instrument = require(__INSTRUMENT__);
var Util = require("../util.js");
var JsBeautify = require('js-beautify').js_beautify

var editors = {};
function editor (name) {
  var editor = ace.edit(name+"-editor");
  editor.setTheme("ace/theme/chrome");
  editor.getSession().setMode("ace/mode/javascript");
  editor.$blockScrolling = Infinity;
  editor.setOption("showPrintMargin", false);
  editor.getSession().setTabSize(2);
  editor.getSession().setUseSoftTabs(true);
  editors[name] = editor;
}

function select (select, object, editor) {
  object[""] = "";
  for (var name in object) {
    var option = document.createElement("option");
    option.textContent = name;
    option.value = name;
    select.appendChild(option);
  }
  select.onchange = function () { editor.setValue(object[select.value], -1) };
  select.value = "";
  select.onchange();
}

module.exports = function () {
  ["master", "target", "instrumented"].forEach(editor);
  editors.instrumented.setReadOnly(true);
  select(document.getElementById("master-select"), __MASTERS__, editors.master);
  select(document.getElementById("target-select"), __TARGETS__, editors.target);
  document.getElementById("run-button").onclick = function () {
    try {
      var instrumented = JsBeautify(Instrument(editors.master.getValue(), editors.target.getValue()));
    } catch (e) {
      alert("Error during instrumentation: "+e);
      throw e;
    }
    editors.instrumented.setValue(instrumented, -1);
    var out1 = Util.benchmark(editors.target.getValue());
    var out2 = Util.benchmark(instrumented);
    document.getElementById("result-span").textContent = Util.print(out1.result);
    document.getElementById("error-span").textContent = Util.print(out1.error);
    document.getElementById("time-span").textContent = Util.round(out1.time)+"ms";
    document.getElementById("instrumented-result-span").textContent = Util.print(out2.result);
    document.getElementById("instrumented-error-span").textContent = Util.print(out2.error);
    document.getElementById("instrumented-time-span").textContent = Util.round(out2.time)+"ms";
    if (out1.error)
      throw out1.error;
    if (out2.error)
      throw out2.error;
  };
};
