'use strict';

exports.formatYearTwoDigits = function(year) {
  if (year < 10) return ("0" + year);
  return (""+year).substr(-2);
}
