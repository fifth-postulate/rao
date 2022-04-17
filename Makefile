.PHONY=clean

PAPER=docs/rao.pdf

${PAPER}:
	curl --output $@ https://arxiv.org/pdf/1708.00274.pdf 

clean:
	rm ${PAPER}