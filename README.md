# cl-solid

[![Solid Logo](https://avatars3.githubusercontent.com/u/14262490?v=3&s=200)](https://github.com/solid/solid)

A common lisp library for the Solid framework - under development...

[![](https://raw.githubusercontent.com/gibsonf1/cl-solid/master/static/images/solid-webid.png)](https://github.com/gibsonf1/cl-solid)

Graph visualization after make-webid command

## Installation

This library is being used and tested with Allegrograph graph db.
A free version is available here: https://franz.com/agraph/downloads/

Using the VM Allegrograph version is highly recommended with Virtualbox as you can then install sbcl/emacs/slime on the Ubuntu VM and devlop directly in that environment.

Configure your graph db settings in cl-solid/src/config - an example development configuration:

```common-lisp
(defconfig |development|
    `(:domain "http://localhost"
	      :user "user"
	      :password "password"
	      :port "10035"
	      :repository "/repositories/Solid"
	      :namespace "http://localhost/node#"
	      :agent-name "Localhost"
	      :graphdb "Allegrograph"
	      ))
```
Until the library is released and added to quicklisp, to load:

```common-lisp
(load "<path>/cl-solid/sl-solid.asd")
(ql:quickload :cl-solid)
```

When first initiating cl-solid:
```common-lisp
(cl-solid:initiate-solid)

Creating new Repository...
Uploading Allegrograph Server Scripts...
Uploading Solid related ontologies...
Loading VCARD...
Loading SCHEMA...
Loading SPACE...
Loading SOLID...
Loading CERT...
Loading VS...
Loading VANN...
Loading DCTERMS...
Loading FOAF...
Loading RDFS...
Loading LDP...
Loading RDF...
Loading ACL...
Creating new Repository Agent and locking ontology...
T
```

To switch between configurations:

```common-lisp
(ql:quickload :osicat)
(setf (osicat:environment-variable "APP_ENV") "development")
```

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

Create a new webid:
```common-lisp
(make-webid "gibsonf1" :name "Frederick Gibson")

Creating Profile...
Creating WebID...
Creating WebID Account...
Adding Web Access Control ACLs...
;->"<http://gibsonf1.example.com/profile/card#me>"
```



## Authors

* Frederick C Gibson, Mark Watson

## Copyright

Copyright (c) 2018 Frederick C Gibson


