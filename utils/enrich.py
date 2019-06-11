#!/bin/python3

##
### TODO: search MWEs from dictionary, glob them, try to automatically tag
### them see mwe (re)tokenizer
### https://www.nltk.org/api/nltk.tokenize.html
##
### TODO: assign POS, assign sense if there's only one sense for that
### lemma and POS
##

from data import *
from nltk.corpus import wordnet as wn
import string
import json
import click
import os


def go_sent(sent):
    def encode_lemma(lemma, pos):
        P = {'n' : 1, 'v' : 2, 'a': 3, 'r' : 4, 's' : 3}
        return '{}%{}'.format(lemma, P[pos])

    def go_tk(tk):
        # TODO: better design, this won't scale with more 'enrichment'
        ## get lemma candidates
        lemmas  = set()
        # existing lemmas
        old_lemmas = tk.get(LEMMAS_KEY, [])
        for lstr in old_lemmas:
            lemmas.add(lstr)
        synsets = set()
        form = tk[FORM_KEY]
        for p in WN_POS:
            lstr = wn.morphy(form.lower(), pos=p)
            if lstr:
                for l in wn.lemmas(lstr, pos=p):
                    lemmas.add(encode_lemma(l.name(), p))
                    sense = l
                    synsets.add(sense.synset())
        lemmas = list(lemmas)
        if lemmas:
            tk[LEMMAS_KEY] = lemmas
        old_senses = tk.get(SENSES_KEY, [])
        # if there currently are no senses, we try to assign one
        # automatically:
        if not old_senses:
            ns = len(synsets)
            if ns == 1:
                tk[SENSES_KEY] = [sense.key()]
                tk[TAG_KEY] = "auto"
            elif ns == 0:
                # ignore words that have no sense in wordnet
                tk[TAG_KEY] = "ignore"
            else:
                tk[TAG_KEY] = tk.get(TAG_KEY, "un")
        return tk

    sent[TOKENS_KEY] = list(map(lambda tk: go_tk(tk), sent[TOKENS_KEY]))
    return sent

def to_json(obj):
    return json.dumps(obj, sort_keys=True, ensure_ascii=False)

def sent_id(doc_id, sent_id):
    return "{}-{}".format(doc_id, sent_id)

def go_doc(fp):
    with open(fp, 'r') as infile:
        for line in infile:
            sent = json.loads(line)
            yield go_sent(sent)

@click.command()
@click.argument('input_files', type=click.Path(exists=True), nargs=-1)
@click.option('--es', 'elasticsearch', default=False, is_flag=True, help="Include elasticsearch indexing command before each sentence.")
def main(input_files, elasticsearch=False):
    for fp in input_files:
        sents = go_doc(fp)
        for sent in sents:
            if elasticsearch:
                click.echo(to_json({'index': {'_id' : sent_id(sent[DOC_ID_KEY], sent[SENT_ID_KEY])}}))
            click.echo(to_json(sent))


if __name__ == '__main__':
    main()
