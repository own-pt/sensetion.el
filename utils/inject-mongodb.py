#!/bin/python3

import pymongo
import click
import json


def read_sent(sent_str):
    sent = json.loads(sent_str)
    sent["_id"] = "{}-{}".format(sent["doc_id"], sent["sent_id"])
    return sent

@click.command()
@click.option('-db', '--database-name', 'database_name', default="sensetion-database",
              type=click.STRING, help = "Name of mongoDB database")
@click.option('-sc', '--synset-collection-name', 'synset_collection_name',
              default="synsets", type=click.STRING,
              help = "Name of mongoDB synset collection")
@click.option('-dc', '--document-collection-name', 'document_collection_name',
              default="documents", type=click.STRING,
              help = "Name of mongoDB document collection")
@click.argument('synsets_file', type=click.File(mode='r'), required=True)
@click.argument('document_files', type=click.File(mode='r'), required=True, nargs=-1)
def main(database_name, synset_collection_name, document_collection_name,
         synsets_file, document_files):
    client = pymongo.MongoClient()
    db = client[database_name]
    synset_collection = db[synset_collection_name]
    document_collection = db[document_collection_name]

    synset_collection.delete_many({})
    document_collection.delete_many({})

    synset_collection.insert_many(
        map(json.loads, synsets_file.readlines()))

    for document_file in document_files:
        document_collection.insert_many(
            map(read_sent, document_file.readlines()))


if __name__ == '__main__':
    main()
