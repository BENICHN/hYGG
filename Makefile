main:
	stack install --local-bin-path bin
	# cp res/* bin

aarch64:
	# curl https://www.stackage.org/lts-18.7/cabal.config > cabal.project.freeze
	cabal install --builddir=bin --hsc2hs-options=--cross-compile --configure-option=--host=aarch64-linux-gnu --with-ld=aarch64-linux-gnu-ld --with-gcc=aarch64-linux-gnu-gcc --with-ghc=aarch64-linux-gnu-ghc --with-ghc-pkg=aarch64-linux-gnu-ghc-pkg --with-hsc2hs=aarch64-linux-gnu-hsc2hs

run: main
	bin/hYGG

clean:
	rm -rf .stack-work
	rm -rf bin/hYGG
