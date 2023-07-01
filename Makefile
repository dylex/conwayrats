default: rats

RESOLVER=lts-19
PACKAGES_rats=simplex-method math-programming math-programming-glpk

%: %.hs
	stack --resolver $(RESOLVER) ghc $(foreach p,$(PACKAGES_$*),--package $p) -- -O -Wall -rtsopts -threaded --make $@
