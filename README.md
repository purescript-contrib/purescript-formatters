# purescript-formatters

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-formatters.svg)](https://github.com/slamdata/purescript-formatters/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-formatters.svg?branch=master)](https://travis-ci.org/slamdata/purescript-formatters)

A PureScript alternative to numeral.js, moment.js, etc.

## Installation

```
bower install purescript-formatters
```

## Number formatters

Formatter has following properties
+ Number of digits before dot
+ Number of digits after dot
+ Should sign be printed for positive numbers
+ Should thousands be separated by comma
+ Should output string have abbreviations (like `K` or `M`)

Number will be padded with zeros to have at least this number of leading zeros.
This doesn't restrict number to have more digits then leading zeros in format string.
+ `0000.0` will show 4 digits: `12 → "0012.0"`, `1234 → "1234.0"`
+ `00.0` will show only 2 digits : `12 → "12.0"`, `1234 → "1234.0"`

Number of digits after dot is set by number of trailing zeros (note the rounding)
+ `0.000` will show 3 digits: `0.12345 → "0.123"`, `12.98765 → "12.988"`
+ `0.0` will show only 1 digit: `0.12345 → "0.1"`, `12.98765 → "13.0"`

If number is lesser then zero `-` is always printed. Otherwise you could specify `+` in format string
+ `+0`: `12.0 → "+12"`, `-34.8 → "-35"`
+ `0`: `12.0 → "12"`, `-34.8 → "-35"`

Thousands separator is specified as `,0` please note that this `0` isn't counted as leading.
+ `00,0`: `1234567890 → "1,234,567,890.0", `1 → "1.0"`

For abbreviation one could use `a` flag. In general it tries to find the closest power of thousand and
then use formatter to result of division of input number and that power.
+ `0a`: `1234567 → "1M"`, `1 → "1"`

## Date/Time formatters

This is a subset of common format/parse strings currently supported.

+ `YYYY` - Full Year      (1999)
+ `YY`   - 2 digit year   (99)
+ `MMMM` - Full Month     (January)
+ `MMM`  - Short Month    (Jan)
+ `DD`   - Padded Day     (02)
+ `D`    - Day of month   (2)
+ `X`    - Unix Timestamp (1506875681)
+ `E`    - Day of Week    (2)
+ `dddd` - DOW Name       (Monday)
+ `ddd`  - DOW Name Short (Mon)
+ `HH`   - 24 Hour        (13)
+ `hh`   - 12 Hour        (1)
+ `a`    - Meridiem       (am/pm)
+ `mm`   - Minutes Padded (02)
+ `m`    - Minutes        (2)
+ `ss`   - Seconds Padded (02)
+ `s`    - Seconds        (2)
+ `S`    - MilliSeconds   (4)
+ `SS`   - MilliSeconds   (04)
+ `SSS`  - MilliSeconds   (004)

Full list is defined [here](https://github.com/slamdata/purescript-formatters/blob/master/src/Data/Formatter/DateTime.purs)

## Documentation

Module documentation is published on Pursuit: [http://pursuit.purescript.org/packages/purescript-formatters](http://pursuit.purescript.org/packages/purescript-formatters)
