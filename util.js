
exports.round = function (x) { return Math.round(1000*x)/1000 }

exports.print = function (x) { return (typeof x === "string" ? JSON.stringify(x) : String(x)).substring(0,100) }

exports.benchmark = function (code) {
  var o = {length: code.length};
  var t1 = performance.now();
  try { o.result = window.eval(code) }
  catch (e) { o.error = e }
  var t2 = performance.now();
  o.time = t2 - t1;
  return o;
}
