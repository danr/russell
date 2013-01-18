# -*- coding: utf-8 -*-

"""
Parses an lmf-lexicon into the standard Saldo Morfologiska format,
returning the words that are not of an ignored pos or msd.
"""

import xml.etree.cElementTree as cet
import codecs
import sys

def ok_pos(pos):
    return pos != "pm" and len(pos) < 3

def ok_msd(msd):
    return len(msd) > 3 or msd in ["nom","ack"]

# We convert words to uppercase, but only these characters should be allowed
OK_SET = set(u"ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ")

def ok_word(word):
    diff = set(word).difference(OK_SET)
    return len(word) > 1 and len(diff) == 0

def read_xml(xml_file='saldom.xml'):

    # "start" needed to save reference to root element
    context = cet.iterparse(xml_file, events=("start", "end"))
    context = iter(context)
    event, root = context.next()

    out = codecs.getwriter('utf-8')(sys.stdout)
    err = codecs.getwriter('utf-8')(sys.stderr)

    for event, elem in context:
        if event == "end":
            if elem.tag == 'LexicalEntry':

                lem = elem.find("Lemma").find("FormRepresentation")

                pos = findval(lem,"partOfSpeech")

                if ok_pos(pos):

                    # There may be several WordForms
                    for forms in elem.findall("WordForm"):
                        word_mix_case = findval(forms,"writtenForm")
                        word = word_mix_case.upper()
                        msd = findval(forms,"msd")

                        if ok_msd(msd) and ok_word(word):
                            out.write(word)
                            out.write('\n')
                            for x in range(0,len(word) - 2):
                                err.write(word[x:x+3])
                                err.write('\n')

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
