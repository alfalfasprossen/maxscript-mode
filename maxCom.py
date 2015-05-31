""" Communicate with 3dsMax
"""
import winCom

MAX_TITLE_IDENTIFIER = r"Autodesk 3ds Max"
MAX_SCINTILLA = r"MXS_Scintilla"
MAX_LISTENER_IDENTIFIER = r"MAXScript Listener"
MAX_NOT_FOUND = r"Could not find a 3ds Max instance."
RECORDER_NOT_FOUND = r"Could not find MAXScript Macro Recorder"
OUTPUT_NOT_FOUND = r"Could not find the Listener Output window"

CommunicatorInstance = None

def getCommunicator():
    global CommunicatorInstance
    if not CommunicatorInstance:
        mw = _getMainWindow()
        CommunicatorInstance = MaxCommunicator(mw)
    return CommunicatorInstance

def _getMainWindow():
    mw = winCom.get_window_by_title(MAX_TITLE_IDENTIFIER)
    if not mw:
        raise winCom.WindowNotFoundException(MAX_NOT_FOUND)
    return mw

class MaxCommunicator():
    def __init__(self, maxMainWindow):
        self.mainWindow = maxMainWindow

        self.miniMacroRecorder = self.mainWindow.get_child_window(cls = MAX_SCINTILLA,
                                                             instance = 1)
        if not self.miniMacroRecorder:
            raise winCom.WindowNotFoundException(RECORDER_NOT_FOUND)
        listenerWin = self.mainWindow.get_thread_window(name = MAX_LISTENER_IDENTIFIER)
        self.logWindow = listenerWin.get_child_window(cls = MAX_SCINTILLA,
                                                      instance = 1)
        if not self.logWindow:
            raise winCom.WindowNotFoundException(OUTPUT_NOT_FOUND)

    def sendCmdToMax(self, cmd):
        self._connect()
        self.miniMacroRecorder.set_text(cmd)
        self.miniMacroRecorder.send_return()
        self._disconnect()

    def getOutputFromMax(self):
        self._connect()
        text = self.logWindow.get_text()
        self._disconnect()
        return text

    def clearListenerOutput(self):
        self._connect()
        self.logWindow.set_text(unicode(""))
        self._disconnect()

    def _connect(self):
        winCom.attachThreads( self.mainWindow.hwnd )

    def _disconnect(self):
        winCom.detachThreads( self.mainWindow.hwnd )
