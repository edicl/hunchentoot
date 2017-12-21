
all:
	xsltproc --stringparam library-version `perl -ne 'print "$$1\n" if (/:version "(.*)"/)' ../hunchentoot.asd` clixdoc.xsl index.xml > ../www/hunchentoot-doc.html
