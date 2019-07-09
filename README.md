# ReSyStanCe: Rewrite Systems Statistics 'n Counts
ReSyStanCe is a tool to collect statistics on rewrite systems written
in the [lambdapi](https://github.com/deducteam/lambdapi) format.

## Installation
The tool can be compiled with dune with the dependencies
- lambdapi (development version needed)
- yojson
- ppx\_deriving\_yojson
- timed

*Lambdapi must be compiled from sources.*  To have the latest version.

and then
```
make install
```

## Output
The program outputs json with this semantics:
- the field `catalogue` contains values that are counted:
  - `sym` number of symbols,
  - `rul` number of rules,
  - `nlr` number of non left linear rules,
  - `hor` number of rules with abstractions;
- `stats` contains statistics coming from distributions,
  - `arul_size` distribution of the size of the rules
  - `arul_height` distribution of the height of the rules.
  
The statistics are
- `percentiles` all the percentiles of the distribution,
- `average` the name says it all,
- `sd` the standard deviation.

## Options
- `--csv` output as a csv line rather than a full json

## Example
On the
[dedukti library](https://github.com/rafoo/dklib/archive/v2.6.zip), 
```
$ shopt -s extglob
$ cd dklib/
$ resystance !(dk_monads_coc.dk)
{
  "catalogue": { "sym": 361, "rul": 258, "nlr": 1, "hor": 0 },
  "stats": {
    "arul_size": {
      "percentiles": [
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5,
        5, 5, 5, 6, 7, 7, 7, 8
      ],
      "average": 2.546511627906977,
      "sd": 1.5018617134922705
    },
    "arul_height": {
      "percentiles": [
        1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 6, 7, 7, 7, 8
      ],
      "average": 3.1705426356589146,
      "sd": 1.4712965911178189
    }
  }
}
```
