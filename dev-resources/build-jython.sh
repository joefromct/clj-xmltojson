#!/usr/bin/env bash


# Grab jython-standalone, note it's 2.7
curl --insecure -o ./jython-standalone.jar  https://repo1.maven.org/maven2/org/python/jython-standalone/2.7.2b3/jython-standalone-2.7.2b3.jar 

# compile the xmltodict.py file, in Lib
jython -c "import py_compile; py_compile.compile('Lib/xmltodict.py')"

# this should merge the Lib f older with the existing jar file.
jar uf ./jython-standalone.jar ./Lib

# this should show our new xmltodict classes.
unzip -l ./jython-standalone.jar   | grep -i xmltodict
