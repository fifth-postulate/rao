.PHONY=clean

PAPER=docs/rao.pdf

${PAPER}:
	curl --output $@ https://arxiv.org/pdf/1708.00274.pdf 

pentatiles: pentatiles.tgz
	tar xvf $<

pentatiles.tgz:
	curl --output $@ http://www.arthy.org/pentatiles.tgz


clean:
	rm ${PAPER}