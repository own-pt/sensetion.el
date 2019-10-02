#!/bin/python3

### TODO: way of specifying sentence ids, use filename + counter for now
from delphin.repp import REPP
import string
import json
import click
import os
from data import TOKENS_KEY,FORM_KEY,KIND_KEY, SENT_ID_KEY, TEXT_KEY, DOC_ID_KEY


def go_sent(sent, sent_id, doc_id, tkz):
    def tk2dict(form):
        r = {}
        r[FORM_KEY] = form
        r[KIND_KEY] = ["wf"]
        return r

    def sent2dict(tks):
        r = {}
        r[DOC_ID_KEY] = doc_id
        r[SENT_ID_KEY] = sent_id
        r[TEXT_KEY] = sent
        r[TOKENS_KEY] = tks
        r[ID_KEY] = "{}-{}".format(doc_id, sent_id)
        return r
        
    latt = tkz.tokenize(sent)
    tks = map(lambda tk: tk2dict(tk[FORM_KEY]), latt.to_list())
    return sent2dict(list(tks))

def to_json(obj):
    return json.dumps(obj, sort_keys=True, ensure_ascii=False)


def go_doc(fp, tkz):
    doc_id = os.path.basename(os.path.splitext(fp)[0])
    with open(fp, 'r') as infile:
        for sent_id, line in enumerate(infile):
            yield to_json(go_sent(line.rstrip('\n'), sent_id, doc_id, tkz))

@click.command()
@click.option('-c', '--config', 'tokenization_config', type=click.Path(exists=True), required=True, help = "path to a .set file used by the tokenization algorithm. See https://pydelphin.readthedocs.io/en/latest/api/delphin.repp.html#delphin.repp.REPP.from_config")
@click.argument('input_files', type=click.Path(exists=True), nargs=-1)
def main(tokenization_config, input_files):
    tkz = REPP.from_config(tokenization_config)
    for fp in input_files:
        sents = go_doc(fp, tkz)
        for sent_json in sents:
            click.echo(sent_json)


if __name__ == '__main__':
    main()
