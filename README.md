----------------------------------------
Hunchentoot - The Common Lisp web server
----------------------------------------

Hunchentoot is a web server written in Common Lisp and at the same
time a toolkit for building dynamic websites.  As a stand-alone web
server, Hunchentoot is capable of HTTP/1.1 chunking (both directions),
persistent connections (keep-alive), and SSL.

Hunchentoot provides facilities like automatic session handling (with
and without cookies), logging, customizable error handling, and easy
access to GET and POST parameters sent by the client. It does *not*
include functionality to programmatically generate HTML output. For
this task you can use any library you like,
e.g. [CL-WHO](https://github.com/edicl/cl-who/) or
[HTML-TEMPLATE](https://github.com/edicl/html-template/).

Hunchentoot talks with its front-end or with the client over TCP/IP
sockets and optionally uses multiprocessing to handle several requests
at the same time.  Therefore, it cannot be implemented completely in
[portable Common
Lisp](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm).
It currently works with [LispWorks](http://www.lispworks.com/) and all
Lisps which are supported by the compatibility layers
[usocket](http://common-lisp.net/project/usocket/) and [Bordeaux
Threads](http://common-lisp.net/project/bordeaux-threads/).

Hunchentoot comes with a [BSD-style
license](http://www.opensource.org/licenses/bsd-license.php) so you
can basically do with it whatever you want.

Complete documentation for Hunchentoot including details about how to
install it can be found in the `docs` directory or at the [project
website](https://edicl.github.io/hunchentoot/).
