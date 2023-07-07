# Conway's RATS exploration tool 

## Installation

- Clone this repo
- Install glpk (e.g., `apt-get install libglpk-dev`)
- Install [Haskell Stack](https://www.fpcomplete.com/haskell/get-started/) (e.g., `wget -qO- https://get.haskellstack.org/ | sh`)
- Run `stack build`

## Running

Either:
- `stack run -- --help`, or
- `stack install`, `rats --help`

```
rats [OPTIONS] CASEPATH|NUMBER
Run Conway's RATS, base 4.
If given a simple number, follow it forever.
If given a list of cases by letter (as specified in the case4 file),
  evaluate the conditions on the initial digit counts (a=1s,b=2s,...)
  that would lead to this path of cases, and compute the minimum initial
  digit counts necessary.

  -i         --ilp           Find minimum integral solution to constraints (rather than just simplex)
  -t[DEPTH]  --tree[=DEPTH]  Explore a tree of possibilites after specified case path
  -d         --decrease      Allow exploring cases that decrease count
```

## Base 4

Currently everything is hard-coded for base 4, but this can (partially) be changed by editing `Param.hs` and creating the appropriate `caseN` file.
