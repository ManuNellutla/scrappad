from typing import Dict, List, Optional, Set, Tuple, TypedDict

class DataItem(TypedDict):
    """Structure for COBOL data items."""
    level: int
    type: str
    usage: str
    parent: Optional[str]
    children: List[str]

class FileItem(TypedDict):
    """Structure for COBOL file definitions."""
    record: str
    operations: List[str]

class Procedure(TypedDict):
    """Structure for COBOL procedures."""
    operations: List[str]
    variables_used: Set[str]
    is_main: bool

class Rule(TypedDict):
    """Structure for COBOL business rules."""
    type: str
    details: str
    variables: str
    paragraph: str

class Relationship(TypedDict):
    """Structure for COBOL relationships (e.g., REDEFINES, PERFORM)."""
    source: str
    target: str
    type: str
    
class SplitResult(TypedDict):
    """Result of splitting a COBOL file."""
    split_files: List[str]
    outline_path: str
    manifest_path: str

class CobolSplitterError(Exception):
    """Custom exception for COBOL splitter errors."""
    pass

SplitResult = Dict[str, str | List[str]]