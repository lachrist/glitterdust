#!/usr/bin/env node

var Minimist = require("minimist");
var Main = require("./main.js");

var args = Minimist(process.argv.slice(2));

if ("help" in args)
  process.stdout.write([
    "Glitterdust is a tool to test and benchmark JavaScript instrumenters.",
    "Recognized arguments:",
    "  --mode       either ``batch'' or ``demo'', indicate the mode",
    "  --instrument path to a file where ``module.exports'' points to the instrumentation function",
    "  --masters    path to a directory containing example of masters",
    "  --targets    path to a directory containing example of targets",
    "  --out        path to the file to write the bundled html page",
    "  --help       prints this message.",
    ""
  ].join("\n"));

Main(args);
