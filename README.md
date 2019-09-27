# Resystance: Rewrite Systems Statistics
Resystance is a tool to collect statistics on rewrite systems written
in the [lambdapi](https://github.com/deducteam/lambdapi) format.

## Installation
The tool can be compiled with dune with the dependencies
- lambdapi (development version needed)
- timed

*Lambdapi must be compiled from sources.*  To have the latest version.

and then
```
make install
```

## Usage
Two executables are available
- `dkstats` to compute miscellaneous statistics,
- `dkritic` to find critical pairs in the system.

## Dkstats
### Output description
The program outputs the following statistics:
- `Symbols` number of symbols,
- `Rules` number of rules,
- `NL_rules` number of non left linear rules,
- `HO_rules` number of rules with abstractions;
- statistics of three distributions:
  + `Arity` arity of the root symbol of the rules
  + `Size` number of subterms in rules
  + `Height` height (or depth) of the rules
  
The statistics of distributions are
- `avg` average value
- `25th_pct` 25th percentile
- `med` median or 50th percentile
- `75th_pct` 75th percentile
- `sd` standard deviation.

The CSV separator is the comma ','.  CSV content is preceded by a
header containing the name of the columns.

### Options
- `--csv` output as a csv line
- `--separate` output one csv line per input file

### Example
On the tests,
```
$ cd tests/
$ resystance *
SUMMARY
Symbols         : 17
Rules           : 25
Non linear rules: 0
HO rules        : 0
$ resystance --csv *
File,Symbols,Rules,NL_rules,HO_rulesArity_avg,Arity_25th_pct,Arity_med,Arity_75th_pctHeight_avg,Height_25th_pct,Height_med,Height_75th_pctSize_avg,Size_25th_pct,Size_med,Size_75th_pct
N/A,17,25,0,0,1.920000,2,2,2,1.120000,1,1,2,1.360000,1,1,3
$ resystance --separate *
File,Symbols,Rules,NL_rules,HO_rulesArity_avg,Arity_25th_pct,Arity_med,Arity_75th_pctHeight_avg,Height_25th_pct,Height_med,Height_75th_pctSize_avg,Size_25th_pct,Size_med,Size_75th_pct
nat.lp,9,15,0,0,2.000000,2,2,2,1.333333,1,1,1,1.733333,1,1,2
bool.lp,8,10,0,0,1.800000,2,1,2,0.800000,1,0,1,0.800000,1,0,1
```

## Dkritic [WIP]
