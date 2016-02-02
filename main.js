
var Stream = require("stream");
var Browserify = require("browserify");
var Path = require("path");
var Fs = require("fs");

module.exports = function (options) {

  function collect (path) {
    if (!path)
      return {};
    var names = Fs.readdirSync(path);
    var files = {};
    for (var i=0; i<names.length; i++)
      if (names[i] !== ".DS_Store")
        files[names[i]] = Fs.readFileSync(path+"/"+names[i], {encoding:"utf8"});
    return files;
  }

  function constant (x) { return function () { return x } }

  var basedir = __dirname+"/"+options.mode;
  var js = Fs.readFileSync(basedir+"/script.js", {encoding:"utf8"});
  js = js.replace("__INSTRUMENT__", constant(JSON.stringify(Path.resolve(options.instrument))));
  js = js.replace("__TARGETS__", constant(JSON.stringify(collect(options.targets))));
  js = js.replace("__MASTERS__", constant(JSON.stringify(collect(options.masters))));
  var readable = new Stream.Readable();
  readable.push(js);
  readable.push(null);
  Browserify()
    .require(readable, {basedir:basedir, expose:"glitterdust"})
    .bundle(function (err, buf) {
      if (err)
        throw err;
      var js = buf.toString("utf8")+"\nwindow.onload = require('glitterdust');";
      var css = Fs.readFileSync(__dirname+"/"+options.mode+"/style.css", {encoding:"utf8"});
      var html = Fs.readFileSync(__dirname+"/"+options.mode+"/index.html", {encoding:"utf8"});
      html = html.replace("/* @CSS */", constant(css));
      html = html.replace("/* @JS */", constant(js.replace(/\<\/script\>/g, "<\\/script>")));
      Fs.writeFileSync(options.out, html, {encoding:"utf8"});
    });

};
