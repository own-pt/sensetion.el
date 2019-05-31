ES=/home/odanoburu/builds/elasticsearch-7.0.0/bin/elasticsearch
SYNSETS_URL=sensetion-synsets
SYNSETS_PATH=data/synsets.json
DOCS_DIR=data/docs/
DOCS_URL=sensetion-docs
PORT=9200
PUT_SYNSETS="{\
    \"mappings\": {\
        \"properties\": {\
            \"gloss\": {\
                \"enabled\": false\
            },\
            \"terms\": {\
                \"type\": \"keyword\"\
            }\
        }\
    }\
}"
PUT_DOCS="{\
    \"mappings\": {\
        \"properties\": {\
            \"tokens\": {\
                \"properties\": {\
                    \"form\": {\
                        \"enabled\": false\
                    },\
                    \"tag\": {\
                        \"enabled\": false\
                    },\
                    \"kind\": {\
                        \"enabled\": false\
                    },\
                    \"meta\": {\
                        \"enabled\": false\
                    },\
                    \"lemmas\": {\
                        \"type\": \"keyword\"\
                    }\
                },\
                \"type\": \"nested\"\
            },\
            \"text\": {\
                \"enabled\": false\
            },\
            \"id\": {\
                \"type\": \"keyword\"\
            }\
        }\
    }\
}"

synsets:
	curl -i -H Content-Type\:\ application/json -XDELETE http://localhost:${PORT}/${SYNSETS_URL} ; echo -e '\n\n---\n'
	curl -i -H Content-Type\:\ application/json -XPUT http://localhost:${PORT}/${SYNSETS_URL} -d ${PUT_SYNSETS} ; echo -e '\n\n---\n'
	curl -s -H "Content-Type: application/x-ndjson" -XPOST localhost:${PORT}/${SYNSETS_URL}/_bulk --data-binary "@${SYNSETS_PATH}" &> /dev/null ; echo

docs:
	curl -i -H Content-Type\:\ application/json -XDELETE http://localhost:${PORT}/${DOCS_URL} ; echo -e '\n\n---\n'
	curl -i -H Content-Type\:\ application/json -XPUT http://localhost:${PORT}/${DOCS_URL} -d ${PUT_DOCS} ; echo -e '\n\n---\n'
	for i in ${DOCS_DIR}*.json ; do curl -s -H "Content-Type: application/x-ndjson" -XPOST localhost:${PORT}/${DOCS_URL}/_bulk --data-binary "@$$i" &> /dev/null ; done ; echo

all: synsets docs
