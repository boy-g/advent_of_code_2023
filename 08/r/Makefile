run:
	ghc -Wall HauntedWasteland.hs
	cat ./example1 | ./HauntedWasteland 2>&1 | sed 's/{/{\n/g' | sed 's/\[/\[\n/g' | sed 's/,/,\n/g'
