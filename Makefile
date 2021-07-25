main:
	stack install --local-bin-path bin
	# cp res/* bin

run: main
	bin/hYGG

clean:
	rm -rf .stack-work
	rm -rf bin/hYGG