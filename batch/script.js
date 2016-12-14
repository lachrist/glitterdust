
var Instrument = require(__INSTRUMENT__);
var Util = require("../util.js");
var dom = {};
var interval = null;

/////////
// DOM //
/////////

function header (content) {
  var th = document.createElement("th");
  th.textContent = content;
  return th;
}

function cell (content) {
  var td = document.createElement("td");
  td.textContent = content;
  return td;
}

module.exports = function () {
  // cache //
  dom.run = document.getElementById("run");
  dom.stop = document.getElementById("stop");
  dom.masters = document.getElementById("masters");
  dom.targets = document.getElementById("targets");
  dom.list = document.getElementById("list");
  // bind //
  dom.stop.onclick = stop;
  dom.run.onclick = run;
  toggle(true);
  benchmarkall(__MASTERS__, __TARGETS__);
}

function toggle (d) {
  dom.masters.disabled = d;
  dom.targets.disabled = d;
  dom.run.disabled = d;
  dom.stop.disabled = !d;
}

/////////////////
// Run && Stop //
/////////////////

function run () {
  toggle(true);
  while (dom.list.firstChild)
    dom.list.removeChild(dom.list.firstChild);
  var masters = {};
  var targets = {};
  var rdv = 0;
  function read (file, dico) {
    rdv++;
    var reader = new FileReader();
    reader.readAsText(file, "UTF-8");
    reader.onload = function () {
      dico[file.name] = reader.result;
      if (--rdv === 0)
        benchmarkall(masters, targets);
    }
  }
  for (var i=0; i<dom.masters.files.length; i++)
    read(dom.masters.files[i], masters);
  for (var i=0; i<dom.targets.files.length; i++)
    read(dom.targets.files[i], targets);
  if (rdv === 0)
    benchmarkall(masters, targets);
}

function stop () {
  clearInterval(interval);
  toggle(false);
}

///////////////
// Benchmark //
///////////////

function benchmarkall (masters, targets) {
  var tks = Object.keys(targets);
  var mks = Object.keys(masters);
  var tidx = -1, midx = mks.length;
  var table = null;
  json = {};
  interval = setInterval(function () {
    if (midx === mks.length) {
      midx = 0;
      tidx++;
      if (tidx === tks.length)
        stop();
      else
        table = target(tks[tidx], targets[tks[tidx]]);
    } else {
      master(mks[midx], masters[mks[midx]], targets[tks[tidx]], table);
      midx++;
    }
  }, 10);
}

function target (name, target) {
  var out = Util.benchmark(target);
  var li = document.createElement("li");
  var table = document.createElement("table");
  li.textContent = name;
  li.appendChild(table);
  dom.list.appendChild(li);
  // Header Row //
  var tr1 = document.createElement("tr");
  tr1.appendChild(header("Master"));
  tr1.appendChild(header("Length"));
  tr1.appendChild(header("Result"));
  tr1.appendChild(header("Error"));
  tr1.appendChild(header("Time[ms]"));
  table.appendChild(tr1);
  // Master row //
  var tr2 = document.createElement("tr");
  tr2.appendChild(cell(""));
  tr2.appendChild(cell(out.length));
  tr2.appendChild(cell(("result" in out) ? Util.print(out.result) : ""));
  tr2.appendChild(cell(("error" in out) ? Util.print(out.error) : ""));
  tr2.appendChild(cell(Util.round(out.time)));
  table.appendChild(tr2);
  return table;
}

function master (name, master, target, table) {
  var tr = document.createElement("tr");
  tr.appendChild(cell(name));
  try {
    var out = Util.benchmark(Instrument(master, target));
    tr.appendChild(cell(out.length));
    tr.appendChild(cell(("result" in out) ? Util.print(out.result) : ""));
    tr.appendChild(cell(("error" in out) ? Util.print(out.error) : ""));
    tr.appendChild(cell(Util.round(out.time)));
  } catch (e) {
    var td = cell("Instrumentation error: "+e);
    td.style.color = "red";
    td.colSpan = "4";
    tr.appendChild(td);
  }
  table.appendChild(tr);
}
