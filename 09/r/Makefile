run:
	ghc -Wall MirageMaintenance.hs
	#cat ./example | ./MirageMaintenance 2>&1 | sed 's/{/{\n/g' | sed 's/\[/\[\n/g' | sed 's/,/,\n/g'
	cat ./input | ./MirageMaintenance 2>&1 | sed 's/{/{\n/g' | sed 's/\[/\[\n/g' | sed 's/,/,\n/g'
