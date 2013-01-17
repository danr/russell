# -*- coding: utf-8 -*-

"""
Parses an lmf-lexicon into the standard Saldo Morfologiska format,
returning the words that are not of an ignored pos or msd.
"""

import xml.etree.cElementTree as cet
import codecs
import sys

def ignore_pos(pos):
    return pos == "pm" or len(pos) >= 3

def ignore_msd(msd):
    return len(msd) <= 3 and msd not in ["nom","ack"]

OK_SET = set(u"abcdefghijklmnopqrstuvwxyzåäöABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ")

def ignore_word(word):
    diff = set(word).difference(OK_SET)
    # print "word %s, diff: %s" % (word, diff)
    return len(diff) > 0

def read_xml(xml_file='saldom.xml'):

    # "start" needed to save reference to root element
    context = cet.iterparse(xml_file, events=("start", "end"))
    context = iter(context)
    event, root = context.next()

    out = codecs.getwriter('utf-8')(sys.stdout)

    for event, elem in context:
        if event == "end":
            if elem.tag == 'LexicalEntry':

                lem = elem.find("Lemma").find("FormRepresentation")

                pos = findval(lem,"partOfSpeech")

                if not ignore_pos(pos):

                    # There may be several WordForms
                    for forms in elem.findall("WordForm"):
                        word = findval(forms,"writtenForm")
                        msd = findval(forms,"msd")

                        if not (ignore_msd(msd) or ignore_word(word)):
                            out.write(word)
                            out.write('\n')

            # Done parsing section. Clear tree to save memory
            if elem.tag in ['LexicalEntry', 'frame', 'resFrame']:
                elem.clear()

def findval(elems, key):
    def iterfindval():
        for form in elems:
            att = form.get("att","")
            if att == key:
                yield form.get("val")
        yield ""

    return iterfindval().next()

if __name__ == '__main__':
    read_xml()
