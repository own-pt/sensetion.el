db.createCollection( collection, {
   validator:
{
  $jsonSchema: {
     required: [ "doc_id", "sent_id", "text", "tokens" ],
     properties: {
        doc_id: {
            bsonType: "string",
            description: "must be a string and is required"
        },
	 sent_id: {
             bsonType: "double", // actually int but mongo considers
				 // numbers unwrapped with NumberInt
				 // to be floats
	     minimum: 0,
             description: "must be an integer and is required"
         },
	 text: {
             bsonType: "string",
             description: "must be a string and is required"
        },
        tokens: {
            bsonType: "array",
	    items: {
		oneOf: [
		    { // meta tokens have only meta information
			bsonType: "object",
			required:["kind"],
			properties:{
			    kind: {
				bsonType: ["array"],
				minItems: 1,
				items: [
				    {
					enum: ["aux", "classif", "def", "ex", "mwf", "qf"],
					description: "First element of the array must be the name"
				    }
				],
				additionalItems: false
			    }
			}
		    },
		    { // word form or collocation tokens
			// TODO: use regex for sense keys (format), for lemmas
			// (guarantee no spaces)
			bsonType: "object",
			required: ["kind", "form", "tag"],
			properties: {
			    kind: {
				oneOf: [
				    {
					bsonType: ["array"],
					minItems: 2,// cf may be part of more than one glob
					uniqueItems: true,
					items: [
					    {
						enum: ["cf"],
						description: "..."
					    }
					],
					additionalItems: {
					    bsonType: "string",
					    description: "..."
					}
				    },
				    {
					bsonType: ["array"],
					minItems: 1,
					maxItems: 1,
					items: [
					    {
						enum: ["wf"],
						description: "..."
					    }
					],
				    }
				]
			    },
			    form: {
				bsonType: "string",
				description: "must be a string"
			    },
			    lemmas: {
				bsonType: "array",
				items: {
				    pattern: "[^ ]+"
				}
			    },
			    senses:{
				bsonType: "array",
				items: {
				    pattern: "[^ %]+%[1-5]:[0-9]+:[0-9]+:[^ :]*:[0-9]*"
				}
			    },
			    tag: {
				enum: ["auto", "ignore", "man", "un"],
				description: "annotation status"
			    }
			}
		    },
		    { // globs
			bsonType: "object",
			required: ["kind", "lemmas", "glob", "tag"],
			properties: {
			    kind: {
				bsonType: ["array"],
				minItems: 2,
				maxItems: 2,
				items: [
				    {
					enum: ["glob"],
					description: "..."
				    },
				    {
					bsonType: "string",
					description: "..."
				    }
				]
			    },
			    glob: {
				enum: ["auto", "man"],
				description: "whether glob was done automatically or manually"
			    },
			    lemmas: {
				bsonType: "array",
				items: {
				    pattern: "[^ ]+"
				}
			    },
			    senses:{
				bsonType: "array",
				items: {
				    pattern: "[^ %]+%[1-5]:[0-9]+:[0-9]+:[^ :]*:[0-9]*",
				}
			    },
			    tag: {
				enum: ["auto", "man", "un"], // glob can't be ignored
				description: "annotation status"
			    }
			}
		    }
		]
	    }
        }
     }
  }
}

})
