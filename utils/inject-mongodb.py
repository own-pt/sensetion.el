#!/bin/python3

import pymongo
import click
import json


@click.command()
@click.argument('synsets_file', type=click.Path(exists=True, dir_okay=False, resolve_path=True), required=True)
@click.argument('documents_files', type=click.Path(exists=True), nargs=-1)
def main(synsets_file, documents_files):
    client = pymongo.MongoClient()
    db = client['sensetion-database']
    synset_collection = db.synsets
    document_collection = db.documents

    synset_collection.delete_many({})
    document_collection.delete_many({})

    with open(synsets_file) as jsonl_file:
        synset_collection.insert_many(
            list(map(json.loads, jsonl_file.readlines())))

    for document_file in documents_files:
        with open(document_file) as jsonl_file:
            document_collection.insert_many(
                list(map(json.loads, jsonl_file.readlines())))


if __name__ == '__main__':
    main()
