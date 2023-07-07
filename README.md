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

## Example

```
> rats -t3 -i HECJ
a, b, c	if 	[] 0	a+b+c >= 0 (0,0,0)
2+b-c, -1+2c, 0	if b-c>=0,c>=1,-a>=0	[H] 1	1+b+c >= 2 (0,1,1)
0, 3+b-3c, -2+4c	if 2c>=2,b-3c>=-3,-a>=0,b-c>=0	[HE] 1	1+b+c >= 2 (0,1,1)
1, 1, -3+4c	if 4c>=3,-b+3c>=3,b-c>=0,-a>=0,b-3c>=-3	[HEC] 2	-1+4c >= 5 (0,3,2)
2, 2, -5+4c	if b-3c>=-3,-a>=0,b-c>=0,-b+3c>=3	[HECJ] 2	-1+4c >= 5 (0,3,2)
 4, 4, -9+4c	if 4c>=9,-b+3c>=3,-a>=0,b-3c>=-3	[HECJJ] 2	-1+4c >= 9 (0,6,3)
  0, 0, 8	if -4c>=-9,b-3c>=-3,-a>=0,-b+3c>=3,4c>=9	[HECJJE] 2	8
  8, 8, -17+4c	if 4c>=17,b-3c>=-3,-a>=0,-b+3c>=3	[HECJJJ] 2	-1+4c >= 17 (0,12,5)
   0, 0, 16	if -4c>=-17,-b+3c>=3,-a>=0,b-3c>=-3,4c>=17	[HECJJJE] 2	16
   16, 16, -33+4c	if 4c>=33,-b+3c>=3,-a>=0,b-3c>=-3	[HECJJJJ] 2	-1+4c >= 33 (0,24,9)
   49-4c, -50+8c, 0	if 4c>=25,-4c>=-32,-b+3c>=3,-a>=0,b-3c>=-3	[HECJJJK] 2	-1+4c >= 25 (0,18,7)
   -51+12c, 0, 0	if -4c>=-24,4c>=18,-b+3c>=3,-a>=0,b-3c>=-3	[HECJJJL] -48+8c	-51+12c >= 17 (0,12,5)
  25-4c, -26+8c, 0	if 4c>=13,-4c>=-16,b-3c>=-3,-a>=0,-b+3c>=3	[HECJJK] 2	-1+4c bounded
  -27+12c, 0, 0	if -4c>=-12,4c>=10,b-3c>=-3,-a>=0,-b+3c>=3	[HECJJL] -24+8c	-27+12c bounded
 13-4c, -14+8c, 0	if 4c>=7,-4c>=-8,-b+3c>=3,-a>=0,b-3c>=-3	[HECJK] 2	-1+4c >= 5 (0,3,2)
  0, 13-4c, 0	if -8c>=-14,8c>=14,b-3c>=-3,-a>=0,-b+3c>=3,4c>=7	[HECJKA] 2	13-4c bounded
  0, 27-12c, -28+16c	if 8c>=15,b-3c>=-3,-a>=0,-b+3c>=3,-4c>=-8	[HECJKE] 2	-1+4c bounded
 -15+12c, 0, 0	if -4c>=-6,4c>=5,4c>=6,-b+3c>=3,b-c>=0,-a>=0	[HECJL] -12+8c	-15+12c bounded
```

- We ask what initial conditions can follow the sequence of cases H,E,C,J:
   - Starting with a 1s, b 2s, and c 3s
   - this can expand (using case H) to 2+b-c 1s, 2c-1 2s, 0 cs if b>=c,c>=1,a=0
      - for a total of 1+b+c initial digits
      - and requires at least 2 total initial digits (0 1s, 1 2, 1 3)
   - which can expand (using case E) to 0 1s, 3+b-3c 2s, 4c-2 3s if ...
   - ... which can expand (using case C and the J) to 2 1s, 2 2s, 4c-5 3s if ...
      - and we now have a total of 4c-1 digits
      - which must be at least 5
- And then ask for a tree of all possible cases (that don't decrease, `-d`):
  - cases J, K, and L are possible (HECJJ, HECJK, HECJL)...
  - case HECJL requires a set of initial conditions that are bounded above (an upper bound on the number of possible digits), so the search terminates

## Base 4

Currently everything is hard-coded for base 4, but this can (partially) be changed by editing `Param.hs` and creating the appropriate `caseN` file.
