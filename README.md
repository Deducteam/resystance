# ReSyStanCe: Rewrite Systems Statistics 'n Counts
ReSyStanCe is a tool to collect statistics on rewrite systems written
in the [lambdapi](https://github.com/deducteam/lambdapi) format.

## Installation
The tool can be compiled with dune with the dependencies
- lambdapi (development version needed)
- yojson
- ppx\_deriving\_yojson
- timed

and then
```
make install
```

## Example
On a rewrite system from lambdapi repository
[miller.dk](https://raw.githubusercontent.com/deducteam/lambdapi/tests/OK/miller.dk),
```
$ resystance miller.dk
Checked [tests/OK/miller.dk]
{
  "sym_cardinal": 13,
  "rul_cardinal": 11,
  "nlr_cardinal": 0,
  "hor_cardinal": 8,
  "rul_size": {
    "percentiles": [
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0
    ],
    "average": NaN,
    "sd": NaN
  },
  "rul_height": {
    "percentiles": [
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0
    ],
    "average": NaN,
    "sd": NaN
  }
}
```
