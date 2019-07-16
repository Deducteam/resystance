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

Data can be output as csv lines, where the fields are, from left to
right
- file name if used with `--separate`, or `N/A`,
- number of symbols,
- number of rules,
- number of non left linear rules,
- number of rules with abstractions.

## Options
- `--csv` output as a csv line rather than a full json
- `--separate` output one csv line per input file

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
$ resystance !(dk_monads_coc.dk) --separate
slist.dk, 23, 19, 0, 0
dk_type.dk, 15, 6, 0, 0
dk_tuple.dk, 5, 0, 0, 0
dk_string.dk, 5, 4, 0, 0
dk_opt.dk, 6, 2, 0, 0
dk_nat.dk, 47, 39, 0, 0
dk_monads.dk, 12, 12, 1, 0
dk_machine_int.dk, 31, 50, 0, 0
dk_logic.dk, 61, 21, 0, 0
dk_list.dk, 13, 10, 0, 0
dklib.dk, 0, 0, 0, 0
dk_int.dk, 22, 21, 0, 0
dk_fail.dk, 1, 0, 0, 0
dk_char.dk, 70, 0, 0, 0
dk_builtins.dk, 10, 0, 0, 0
dk_bool.dk, 13, 9, 0, 0
dk_binary_nat.dk, 23, 55, 0, 0
cc.dk, 4, 10, 0, 0
```
