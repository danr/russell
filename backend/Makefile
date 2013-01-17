PYTHON = python

saldo.xml:
	wget https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldo/saldo.xml

saldom.xml:
	wget https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldom/saldom.xml

saldom-stripped-dups: saldom.xml
	$(PYTHON) saldostripper.py > $@

saldom-stripped: saldom-stripped-dups
	sort -u $(arg1) > $@

.PHONY: all

all: saldom-stripped

arg1 = $(word 1,$+)
