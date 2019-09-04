const Fs = require("fs");
const meta = Fs.readFileSync(process.argv[2], "utf8")
const base = Fs.readFileSync(process.argv[3], "utf8")
console.log(meta.replace("<INSTRUMENTED CODE>", () => base));