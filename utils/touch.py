#!/bin/python3

### TODO: search MWEs from dictionary, glob them, try to automatically tag
### them see mwe (re)tokenizer
### https://www.nltk.org/api/nltk.tokenize.html
##
### TODO: assign POS, assign sense if there's only one sense for that
### lemma and POS
##
### TODO: way of specifying sentence ids
from delphin.repp import REPP
from nltk.corpus import wordnet as wn
import json
import click
import os

WN_POS=[wn.NOUN,wn.VERB,wn.ADJ,wn.ADV,wn.ADJ_SAT]

def go_sent(sent, sent_id, tkz):
    def go(form):
        # get lemma candidates
        lemmas = []
        for p in WN_POS:
            l = wn.morphy(form, pos=p)
            if l:
                lemmas.append((l, p))
        # assign sense automatically if desirable
        synsets = set()
        for l_p in lemmas:
            for s in wn.lemmas(l_p[0], pos=l_p[1]):
                sense = s
                synsets.add(s.synset())
        senses = []
        ignore = False
        ns = len(synsets)
        if ns == 1:
            senses = [sense.key()]
        elif ns == 0:
            # ignore words that have no sense in wordnet
            ignore = True
        return (form, lemmas, senses, ignore)

    def tk2dict(tk):
        def encode_lemma(l_p):
            lemma, pos =  l_p
            P = {'n' : 1, 'v' : 2, 'a': 3, 'r' : 4, 's' : 3}
            return '{}%{}'.format(lemma,P[pos])
                
        form, lemmas, senses, ignore = tk
        r = {}
        if senses:
            r['senses'] = senses
            r['tag'] = "auto"
        elif ignore:
            r['tag'] = "ignore"
        else:
            r['tag'] = "un"
        r['form'] = form
        #TODO: convert lemmas to format
        r['lemmas'] = list(map(encode_lemma, lemmas))
        r['kind'] = "wf"
        return r

    def sent2dict(tks):
        r = {}
        r['tokens'] = tks
        r['id'] = sent_id
        r['text'] = sent
        return r
        
    latt = tkz.tokenize(sent)
    tks = map(lambda tk: tk2dict(go(tk['form'])), latt.to_list())
    return sent2dict(list(tks))

def to_json(obj):
    return json.dumps(obj, sort_keys=True, ensure_ascii=False)

def go_doc(fp,tkz):
    fname = os.path.basename(os.path.splitext(fp)[0])
    # if '-' in fname:
    #     raise Error("Filename shouldn't contain '-' character.")
    with open(fp, 'r') as infile:
        for ix, line in enumerate(infile):
            sent_id = '{}-{}'.format(fname, ix)
            yield sent_id, to_json(go_sent(line.rstrip('\n'), sent_id, tkz))

@click.command()
@click.option('-c', '--config', 'tokenization_config', type=click.Path(exists=True), required=True, help = "path to a .set file used by the tokenization algorithm. See https://pydelphin.readthedocs.io/en/latest/api/delphin.repp.html#delphin.repp.REPP.from_config")
@click.argument('input_files', type=click.Path(exists=True), nargs=-1)
@click.option('--es', 'elasticsearch', default=False, is_flag=True, help="Include elasticsearch indexing command before each sentence.")
def main(tokenization_config, input_files,elasticsearch=False):
    tkz = REPP.from_config(tokenization_config)
    for fp in input_files:
        sents = go_doc(fp, tkz)
        for sent_id, sent_json in sents:
            if elasticsearch:
                click.echo(to_json({'index': {'_id' : sent_id}}))
            click.echo(sent_json)


if __name__ == '__main__':
    main()
