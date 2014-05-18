
class EngineError(Exception): pass

from .engine import Engine, OPCODES, DLGOPS
from .fman import FileManager
from .imgbmp import BMPLoader
from .imgflc import FLCLoader
