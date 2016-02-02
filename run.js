#!/usr/bin/env node

var Minimist = require("minimist");
var Main = require("./main.js");

var args = Minimist(process.argv.slice(2));
if ("help" in args)
  process.stdout.write([
    "Glitterdust bundles JavaScript scripts.",
    "Recognized arguments:",
    "  --mode        either ``batch'' or ``demo'', indicate the mode",
    "  --instrument  path to a file where ``module.exports'' points to the instrumentation function",
    "  --targets     path to a directory containing example of targets (must be JavaScript)",
    "  --masters     path to a directory containing example of masters (can be any text)",
    "  --out         path to the file to write the bundled html page",
    "  --help        prints this message.",
    ""
  ].join("\n"));
Main(args);