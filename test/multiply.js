
// node ../run.js --mode batch --out ./batch.html --instrument ./multiply.js --masters ./masters --targets ./targets --help

module.exports = function (master, target) { return Array(Number(master)+1).join(target) }
