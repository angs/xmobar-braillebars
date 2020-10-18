all: cpubar netbar anybar

netbar: netbar.hs BrailleBar.hs Common.hs
	ghc -O2 --make netbar.hs

cpubar: cpubar.hs BrailleBar.hs Common.hs
	ghc -O2 --make cpubar.hs

anybar: anybar.hs BrailleBar.hs Common.hs

install: netbar cpubar anybar
	cp cpubar /usr/local/bin/
	cp netbar /usr/local/bin/
