# cl-solid

[![Solid Logo](https://avatars3.githubusercontent.com/u/14262490?v=3&s=200)](https://github.com/solid/solid)

A common lisp library for the Solid framework - under development...

## Usage

```common-lisp
(in-package :cl-solid)
```

Start Solid web service (uses same options as caveman2):

```common-lisp
(start :port 8080)
;->Hunchentoot server is started.
;->Listening on localhost:8080.
```

Create a new webid with new agent and container:
```common-lisp
(make-webid :name "Frederick Gibson")
;->"<http://solid.example.com/node#2>"
```

[![](https://graphmetrix.com/images/solid-webid.png)](https://github.com/gibsonf1/cl-solid)

Graph visualization from make-webid command above

## Installation

This library is being used and tested with Allegrograph graph db.
A free version is available here: https://franz.com/agraph/downloads/

Using the VM Allegrograph version is highly recommended with Virtualbox as you can then install sbcl/emacs/slime on the Ubuntu VM and devlop directly in that environment.

The graph db used requires the following ontologies to be loaded:

* http://www.w3.org/ns/auth/acl
* https://lov.linkeddata.es/dataset/lov/vocabs/ldp
* https://lov.linkeddata.es/dataset/lov/vocabs/rdf
* https://lov.linkeddata.es/dataset/lov/vocabs/rdfs
* https://lov.linkeddata.es/dataset/lov/vocabs/foaf
* https://lov.linkeddata.es/dataset/lov/vocabs/dcterms
* https://lov.linkeddata.es/dataset/lov/vocabs/vann
* https://lov.linkeddata.es/dataset/lov/vocabs/vs
* https://lov.linkeddata.es/dataset/lov/vocabs/cert
* http://www.w3.org/ns/pim/space

Configure your graph db settings in cl-solid/src/config

```common-lisp
(load "path/cl-solid/sl-solid.asd")
(ql:quickload :cl-solid)
```

When first initiating cl-solid with a new Repository, use (initiate-solid) to create the needed triples for your Solid agent and lock.

To switch between configurations:

```common-lisp
(ql:quickload :osicat)
(setf (osicat:environment-variable "APP_ENV") "development")
```

## Authors

* Frederick C Gibson, Mark Watson

## Copyright

Copyright (c) 2018 Frederick C Gibson


