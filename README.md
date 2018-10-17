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
;->"<http://solid.example.com/node#3>"
```

[![](https://graphmetrix.com/images/solid-webid.png)](https://github.com/gibsonf1/cl-solid)

Graph visualization from make-webid command above

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

When first initiating cl-solid with a new Repository, to create the needed triples for your Solid agent and lock:
```common-lisp
(cl-solid:initiate-solid)
```
For Allegrograph users, write scripts to the graph db server on first use:
```common-lisp
(cl-solid:update-server-scripts)
```

To switch between configurations:

```common-lisp
(ql:quickload :osicat)
(setf (osicat:environment-variable "APP_ENV") "development")
```

## Authors

* Frederick C Gibson, Mark Watson

## Copyright

Copyright (c) 2018 Frederick C Gibson


