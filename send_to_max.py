#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os
import tempfile

import maxCom

CMD_SUFFIX ="\r\n"

def cleanCmdLineString(text):
    """ remove any double backslashes and carriage returns
    that may have beend added from the command line translation."""
    text = text.replace("\\\\","\\")
    text = text.replace("\r","")
    return text

def sendCmdToMax(cmd):
    com = maxCom.getCommunicator()
    com.sendCmdToMax(cmd)

def getOutputFromMax():
    com = maxCom.getCommunicator()
    return com.getOutputFromMax()

def executeFile(fpath):
    name, ext = os.path.splitext(fpath)
    if ext.lower() == ".ms":
        cmd = ('fileIn (@"%s")'+CMD_SUFFIX) % fpath
        sendCmdToMax(cmd)
    elif ext.lower() == ".py":
        cmd = ('python.executefile (@"%s")'+CMD_SUFFIX) % fpath
        sendCmdToMax(cmd)
    else:
        print "Unknown file extension: %s" % ext

def writeTempFile(text, ext):
    tmpdir = tempfile.gettempdir()
    tmpfile = "sendtomaxtempfile" + ext
    fpath = os.path.join(tmpdir, tmpfile)
    with open(fpath, 'w') as f:
        f.write(text)
    return fpath

def executeMaxScript(code):
    """ Execute maxscript code by saving it to a temporary .ms file and
    send a fileIn command to Max.
    When only one line of code is present, execute it directly instead.
    """
    if "\n" in code:
        fpath = writeTempFile(code, ".ms")
        executeFile(fpath)
    else:
        cmd = code+CMD_SUFFIX
        sendCmdToMax(cmd)

def executeMaxPython(code):
    fpath = writeTempFile(code, ".py")
    executeFile(fpath)

def clearListenerOutput():
    com = maxCom.getCommunicator()
    com.clearListenerOutput()

def executeThis():
    if len(sys.argv)>2:
        # when receiving strings through the cmdline, we will have
        # double backslashes and carriage returns, killing syntax.
        if sys.argv[1] == "-ms":
            script = cleanCmdLineString(sys.argv[2])
            executeMaxScript(script)
        elif sys.argv[1] == "-py":
            scritp = cleanCmdLineString(sys.argv[2])
            executeMaxPython(script)
        elif sys.argv[1] == "-f":
            executeFile(sys.argv[2])
    elif len(sys.argv)>1:
        if sys.argv[1] == "-c":
            clearListenerOutput()
        elif sys.argv[1] == "-g":
            res = getOutputFromMax()
            print cleanCmdLineString(res)

if __name__ == "__main__":
    executeThis()
