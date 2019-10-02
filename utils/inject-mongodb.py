#!/bin/python3

import pymongo
import click
import json


@click.command()
@click.option('-db', '--database', 'database', default="sensetion-database",
              type=click.STRING, help = "Name of mongoDB database", show_default=True)
@click.option('--quiet/--verbose', default=False)
@click.argument('collection', type=click.STRING)
@click.argument('input_files', type=click.File(mode='r'), required=True, nargs=-1)
def main(database, quiet, collection, input_files):
    """Insert documents from INPUT_FILES in COLLECTION at DATABASE.

Each input file must be a JSON-lines document. The COLLECTION at
DATABASE is emptied before insertion.
    """
    client = pymongo.MongoClient()
    db = client[database]
    collection = db[collection]

    collection.delete_many({})

    n_written = 0

    for document_file in input_files:
        result = collection.insert_many(map(json.loads, document_file.readlines()))
        if not result.acknowledged:
            raise RuntimeError("Could not insert documents")
        if not quiet:
            n_written = n_written + len(result.inserted_ids)
    if not quiet:
        print("Successfully inserted {} documents".format(n_written))


if __name__ == '__main__':
    main()
