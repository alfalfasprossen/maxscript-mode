#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os
import tempfile
import ctypes #required for windows ui stuff
import threading

def sendCmdToMax(cmd):
    if connect():
        SendMessage(gMiniMacroRecorder, WM_SETTEXT, 0, cmd)
        SendMessage(gMiniMacroRecorder, WM_CHAR, VK_RETURN, 0)
        disconnect()
    else:
        print MAX_NOT_FOUND

def executeFile(fpath):
    name, ext = os.path.splitext(fpath)
    if ext.lower() == ".ms":
        cmd = 'fileIn (@"%s");' % fpath
        sendCmdToMax(cmd)
    elif ext.lower() == ".py":
        cmd = 'python.executefile (@"%s");' % fpath
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
    fpath = writeTempFile(code, ".ms")
    executeFile(fpath)

def executeMaxPython(code):
    fpath = writeTempFile(code, ".py")
    executeFile(fpath)

def clearListenerOutput():
    print "not implemented yet"

def executeThis():
    if len(sys.argv)>2:
        # when receiving strings through the cmdline, we will have
        # double backslashes and carriage returns, killing syntax.
        if sys.argv[1] == "-ms":
            sys.argv[2] = sys.argv[2].replace("\\\\","\\").replace("\r","")
            executeMaxScript(sys.argv[2])
        elif sys.argv[1] == "-py":
            sys.argv[2] = sys.argv[2].replace("\\\\","\\").replace("\r","")
            executeMaxPython(sys.argv[2])
        elif sys.argv[1] == "-f":
            executeFile(sys.argv[2])
    elif len(sys.argv)>1:
        if sys.argv[1] == "-c":
            clearListenerOutput()


# lots of code in here copied from our m2u-project
#

MAX_TITLE_IDENTIFIER = r"Autodesk 3ds Max"
MAX_NOT_FOUND = r"Could not find a 3ds Max instance."
RECORDER_NOT_FOUND = r"Could not find MAXScript Macro Recorder"

gMaxThreadProcessID = None # the UI-thread of 3DsMax
gMainWindow = None # the 3DsMax Window
gMiniMacroRecorder = None

# windows functions and constants
# stuff for finding and analyzing UI Elements
EnumWindows = ctypes.windll.user32.EnumWindows
EnumWindowsProc = ctypes.WINFUNCTYPE(ctypes.c_bool, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int))
EnumChildWindows = ctypes.windll.user32.EnumChildWindows
FindWindowEx = ctypes.windll.user32.FindWindowExW

GetClassName = ctypes.windll.user32.GetClassNameW
GetWindowText = ctypes.windll.user32.GetWindowTextW
GetWindowTextLength = ctypes.windll.user32.GetWindowTextLengthW
IsWindowVisible = ctypes.windll.user32.IsWindowVisible
GetWindow = ctypes.windll.user32.GetWindow
GetWindowThreadProcessId = ctypes.windll.user32.GetWindowThreadProcessId

PostMessage = ctypes.windll.user32.PostMessageA
SendMessage = ctypes.windll.user32.SendMessageA

WM_SETTEXT = 0x000C
WM_KEYDOWN = 0x0100
WM_KEYUP = 0x0101
WM_CHAR = 0x0102 # the alternative to WM_KEYDOWN
VK_RETURN  = 0x0D # Enter key

# attaching is required for SendMessage and the like to actually work like it should
AttachThreadInput = ctypes.windll.user32.AttachThreadInput


class ThreadWinLParm(ctypes.Structure):
    """lParam object to get a name to and an object back from a windows
    enumerator function.

    .. seealso:: :func:`_getChildWindowByName`
    """
    _fields_=[
        ("name", ctypes.c_wchar_p), # name to find / return
        ("cls", ctypes.c_wchar_p), # class to find / return
        ("hwnd", ctypes.POINTER(ctypes.c_long)), # hwnd to return
        ("enumPos", ctypes.c_int), # enum pos to find / return
        ("_enum", ctypes.c_int), # keep track of current enum step
        ("instance", ctypes.c_int), # instance to find
        ("_instance", ctypes.c_int), # keep track of current instance
        ("exact", ctypes.c_int) # match name (True) or string contains (False)
    ]

def _getChildWindowByName(hwnd, lParam):
    """callback function to be called by EnumChildWindows, see
    :func:`getChildWindowByName`

    :param hwnd: the window handle
    :param lParam: a :ref:`ctypes.byref` instance of :class:`ThreadWinLParam`
    
    if name is None, the cls name is taken,
    if cls is None, the name is taken,
    if both are None, all elements are printed
    if both have values, only the element matching both will fit
    
    """
    length = GetWindowTextLength(hwnd)
    buff = ctypes.create_unicode_buffer(length + 1)
    GetWindowText(hwnd, buff, length + 1)
    param = ctypes.cast(lParam, ctypes.POINTER(ThreadWinLParm)).contents
    param._enum += 1

    length = 255
    cbuff = ctypes.create_unicode_buffer(length + 1)
    GetClassName(hwnd, cbuff, length+1)
    # TODO: implement exact-checking (== instead of in)
    if param.name == None and param.cls != None:
        #print "no name, but cls"
        if param.cls in cbuff.value:# == param.cls:
            param._instance += 1
            if param.instance == 0 or param.instance == param._instance:
                param.hwnd = hwnd
                return False
    elif param.cls == None and param.name != None:
        #print "no cls, but name"
        if buff.value == param.name:
            param._instance += 1
            if param.instance == 0 or param.instance == param._instance:
                param.hwnd = hwnd
                return False
    elif param.cls != None and param.name != None:
        #print "cls and name"
        if buff.value == param.name and param.cls in cbuff.value:# == param.cls:
            if param.instance == 0 or param.instance == param._instance:
                param.hwnd = hwnd
                return False
    else: #both values are None, print the current element
        print "wnd cls: "+cbuff.value+" name: "+buff.value+" enum: "+str(param._enum)
    return True


def getChildWindowByName(hwnd, name=None, cls=None, exact=True, instance=0,
                         loops=1, hwndIsThread=False):
    """find a window by its name or clsName, returns the window's hwnd
    
    :param hwnd: the parent window's hwnd
    :param name: the name/title to search for
    :param cls: the clsName to search for
    :param exact: name/class string has to match (True) or is contained (False)
        NOT IMPLEMENTED YET!
    :param instance: make use of this to get the second, third and so on
        instance of a window. 
    :param loops: try to find a window that is still being created,
        check loops num of times (0.02 seconds sleep inbetween)
        0 = check until found
    :param hwndIsThread: if True, iterate through ThreadWindows, this
        is a more powerful alternative to :func:`getThreadWindowByName`

    :return: the hwnd of the matching child window

    
    if name is None, the cls name is taken,
    if cls is None, the name is taken,
    if both are None, all elements are printed
    if both have values, only the element matching both will fit.

    the instance parameter is useful if there are more than one window with
    the same name or class are present and you don't want to have the first
    instance, but you cannot use :func:`getChildWindowByEnumPos` because the
    enum is not fixed. 0 and 1 will return the first instance found, 2 will
    return the second instance found etc.

    .. note:: When using loops, make sure the execution of this script is not
    blocking the thread which creates the window you are waiting for.
    (detach threads when necessary)
    
    .. seealso:: :func:`_getChildWindowByName`, :func:`getChildWindowByEnumPos`
    
    """
    null_ptr = ctypes.POINTER(ctypes.c_int)()
    param = ThreadWinLParm(hwnd=null_ptr, name=name, cls=cls, _enum=-1,
                           instance=instance, _instance=0, exact=exact)
    lParam = ctypes.byref(param)
    WinIterator = EnumThreadWindows if hwndIsThread else EnumChildWindows
    iters = 0
    while True:
        WinIterator( hwnd, EnumWindowsProc(_getChildWindowByName),lParam)
        if param.hwnd != null_ptr:
            break
        iters += 1
        if loops != 0 and iters >= loops:
            break
        time.sleep(0.02)
    return param.hwnd


def _getWindows(hwnd, lParam):
    """callback function, find the Max Window (and fill the ui element vars)
    
    This is a callback function. Windows itself will call this function for
    every top-level window in EnumWindows iterator function.
    .. seealso:: :func:`connectToUEd`
    """
    if IsWindowVisible(hwnd):
        length = GetWindowTextLength(hwnd)
        buff = ctypes.create_unicode_buffer(length + 1)
        GetWindowText(hwnd, buff, length + 1)
        null_ptr = ctypes.POINTER(ctypes.c_int)()
        if MAX_TITLE_IDENTIFIER in buff.value:
            global gMainWindow, gMiniMacroRecorder
            gMainWindow = hwnd
            attachThreads(gMainWindow)
            
            # get the command line field
            gMiniMacroRecorder = getChildWindowByName(gMainWindow, name = None,
                                                      cls = "MXS_Scintilla")
            if gMiniMacroRecorder == null_ptr:
                print RECORDER_NOT_FOUND
            return False # we found Max, no further iteration required
    return True

def attachThreads(hwnd):
    """tell Windows to attach the program and the 3DsMax threads.
    
    This will give us some benefits in control, for example SendMessage calls to
    the 3DsMax thread will only return when max has processed the message, amazing!
    
    """
    thread = GetWindowThreadProcessId(hwnd, 0) #udk thread
    global gMaxThreadProcessID
    gMaxThreadProcessID = thread
    thisThread = threading.current_thread().ident #program thread
    AttachThreadInput(thread, thisThread, True)

def detachThreads():
    thisThread = threading.current_thread().ident #program thread
    AttachThreadInput(gMaxThreadProcessID, thisThread, False)

def connect():
    EnumWindows(EnumWindowsProc(_getWindows), 0)
    return (gMainWindow is not None)

def disconnect():
    detachThreads()

executeThis()