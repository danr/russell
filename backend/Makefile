PYTHON = python

saldo.xml:
	wget https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldo/saldo.xml

saldom.xml:
	wget https://svn.spraakdata.gu.se/sb-arkiv/pub/lmf/saldom/saldom.xml

saldom-dups: saldom.xml saldostripper.py
	$(PYTHON) saldostripper.py > saldom-stripped-dups 2> saldom-trigrams-dups
	touch saldom-dups

saldom-stripped-dups: saldom-dups

saldom-trigram-dups: saldom-dups

saldom-stripped: saldom-stripped-dups
	sort -u $(arg1) > $@

saldom-trigram-count: saldom-trigrams-dups
	sort $(arg1) | uniq -c | sort -rn > $@
	sed -i 's/^ *//' $@

.PHONY: all saldom-dups

all: saldom-stripped saldom-trigram-count

arg1 = $(word 1,$+)
