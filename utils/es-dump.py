#!/bin/python3
import elasticsearch
from elasticsearch.helpers import scan
import click
import json
import os



@click.command()
@click.option('-a', '--address', type=click.Path(), default='http://localhost', help = "elasticsearch server url")
@click.option('-p', '--port', type=click.INT, default=9200, help = "elasticsearch server port")
@click.option('-i', '--index', type=click.STRING, required=True, help="name of the index to dump")
@click.argument('output-dir', type=click.Path(writable=True, resolve_path=True))
@click.option('-s', '--file_size', type=click.INT, default=1000, help="number of documents per file")
def main(address,port,index,output_dir,file_size):
    # https://elasticsearch-py.readthedocs.io/en/master/helpers.html?highlight=scroll#scan
    def dump_chunk(ix, docs):
        fp = os.path.join(output_dir, '{}.json'.format(ix))
        with open(fp, 'w') as out:
            for doc in docs:
                print(json.dumps(doc['_source'], sort_keys=True, ensure_ascii=False), file=out)

    
    url = '{}:{}/'.format(address, port)
    es = elasticsearch.Elasticsearch(url)
    resp = scan(
        es,
        index=index,
        preserve_order=True,
        query={"query": { "match_all" : {}}}
    )
    ix = 0
    while True:
        docs = [d for _, d in zip(range(file_size), resp)]
        if docs:
            dump_chunk(ix, docs)
            ix += 1
        else:
            break

if __name__ == '__main__':
    main()
