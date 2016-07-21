# purescript-formatters
Replacement for numeral.js, moment.js etc

## Number formatters

Formatter has following properties
+ Number of digits before dot
+ Number of digits after dot
+ Should sign be printed for positive numbers
+ Should thousands be separated by comma
+ Should output string have abbreviations (like `K` or `M`)

Number will be padded with zeros to have at least this number of leading zeros.
This doesn't restrict number to have more digits then leading zeros in format string.
+ `0000.0` will show 4 digits: `12 -> "0012.0"`, `1234 -> "1234.0"`
+ `00.0` will show only 2 digits : `12 -> "12.0"`, `1234 -> "1234.0"`

Number of digits after dot is set by number of trailing zeros (note the rounding)
+ `0.000` will show 3 digits: `0.12345 -> "0.123"`, `12.98765 -> "12.988"`
+ `0.0` will show only 1 digit: `0.12345 -> "0.1"`, `12.98765 -> "13.0"`

If number is lesser then zero `-` is always printed. Otherwise you could specify `+` in format string
+ `+0`: `12.0 -> "+12"`, `-34.8 -> "-35"`
+ `0`: `12.0 -> "12"`, `-34.8 -> "-35"`

Thousands separator is specified as `,0` please note that this `0` isn't counted as leading.
+ `00,0`: `1234567890 -> "1,234,567,890.0", `1 -> "1.0"`

For abbreviation one could use `a` flag. In general it tries to find the closest power of thousand and
then use formatter to result of division of input number and that power.
+ `0a`: `1234567 -> "1M"`, `1 -> "1"`

## Date/Time formatters

This is just subset of format/parse string from moment.js library. Currently supported
+ `YYYY`
+ `YY`
+ `MMMM`
+ `MMM`
+ `MM`
+ `DD`
+ `X`
+ `E`
+ `HH`
+ `hh`
+ `a`
+ `mm`
+ `ss`
+ `SSS`
