.PHONY: clean

DOCS=../docs
OUTPUT=${DOCS}/classification-pentagonal-tilings.pdf
ARTIFACT=article.pdf
SOURCES=$(shell find content -name '*.tex')

${OUTPUT}: ${ARTIFACT}
	cp $< $@

${ARTIFACT}: article.tex ${SOURCES}
	pdflatex $<
	pdflatex $<

clean:
	rm ${OUTPUT} ${ARTIFACT}