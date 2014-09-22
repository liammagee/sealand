if (typeof require !== 'undefined') XLSX = require('xlsx');

var _ = require('underscore')
var jStat = require('jStat').jStat


var workbook = XLSX.readFile('data/database.xlsx');

var database = workbook.Sheets["Data"]

console.log(database["A-1"])

_.each([1, 2, 3], console.log);

console.log(jStat.exponential.cdf(Math.random() * 9, 1))

var insuredCosts = _.chain(	
	_(318).times(function(n) { return (database["AD"+(n+1)]) } ) 
)
.filter(function(n) { return !_.isUndefined(n) && !_.isUndefined(n.v) && !_.isUndefined(n.t) && n.t === 'n' })
.map(function(n) { return n.v })
.value()


console.log(insuredCosts)
console.log(insuredCosts.length)
console.log(jStat.mean(insuredCosts))
