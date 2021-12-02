all: cabal

cabal: aoc21.cabal

aoc21.cabal: package.yaml
	hpack

check: aoc21.cabal
	cabal test


clean:
	rm -rf ./dist-newstyle
	rm -rf *.cabal
	rm -rf ~/aoc21.overmind.sock

watch: clean aoc21.cabal
	SHELL=`which bash` \
	overmind start --title aoc21 \
	--procfile Procfile \
	--socket ~/aoc21.overmind.sock \
	--tmux-config /dev/null

new-golden-results:
	rm -rf ./test/golden-results/

watch-new-golden: new-golden-results watch
