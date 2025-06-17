import os
import gzip
import aiofiles
import logging
from typing import AsyncGenerator, List, Dict, Set
from .types import SplitResult, CobolSplitterError

async def read_file_stream(file_path: str) -> AsyncGenerator[str, None]:
    """
    Read a COBOL file line by line, stripping sequence numbers (columns 1-6).

    Args:
        file_path: Path to the COBOL file.

    Yields:
        Stripped line without sequence numbers.

    Raises:
        CobolSplitterError: If file cannot be read or has encoding errors.
    """
    try:
        async with aiofiles.open(file_path, mode="r", encoding="utf-8") as file:
            async for line in file:
                if len(line) > 6 and line[0:6].isdigit():
                    yield line[6:]
                else:
                    yield line
    except UnicodeDecodeError as e:
        logging.error(f"Encoding error in {file_path}: {e}")
        raise CobolSplitterError(f"Encoding error in {file_path}: {e}")
    except IOError as e:
        logging.error(f"IO error reading {file_path}: {e}")
        raise CobolSplitterError(f"IO error reading {file_path}: {e}")

def read_file_stream_sync(file_path: str) -> List[str]:
    """
    Read a COBOL file line by line synchronously, stripping sequence numbers (columns 1-6).

    Args:
        file_path: Path to the COBOL file.

    Returns:
        List of stripped lines without sequence numbers.

    Raises:
        CobolSplitterError: If file cannot be read or has encoding errors.
    """
    try:
        with open(file_path, mode="r", encoding="utf-8") as file:
            for line in file:
                if len(line) > 6 and line[0:6].isdigit():
                    yield line[6:]
                else:
                    yield line
    except UnicodeDecodeError as e:
        logging.error(f"Encoding error in {file_path}: {e}")
        raise CobolSplitterError(f"Encoding error in {file_path}: {e}")
    except IOError as e:
        logging.error(f"IO error reading {file_path}: {e}")
        raise CobolSplitterError(f"IO error reading {file_path}: {e}")

def save_file(
    program_name: str,
    division: str,
    section: str,
    content: List[str],
    output_dir: str,
    file_counter: int,
    split_files: List[str],
    outline: List[str],
    compress: bool = False,
) -> None:
    """
    Save a split COBOL file, optionally compressed.

    Args:
        program_name: Name of the COBOL program.
        division: Current division (e.g., PROCEDURE).
        section: Current section (e.g., DEFAULT).
        content: Lines to save.
        output_dir: Directory to save the file.
        file_counter: Counter for file naming.
        split_files: List to store output file paths.
        outline: List to store outline entries.
        compress: Whether to compress the file with gzip.

    Raises:
        CobolSplitterError: If file cannot be written.
    """
    filename = f"{program_name.lower()}_{division.lower()}_{section.lower()}_part{file_counter}.cbl"
    if compress:
        filename += ".gz"
    file_path = os.path.join(output_dir, filename)
    
    try:
        if compress:
            with gzip.open(file_path, "wt", encoding="utf-8") as f:
                f.writelines(content)
        else:
            with open(file_path, "w", encoding="utf-8") as f:
                f.writelines(content)
    except IOError as e:
        logging.error(f"Error writing file {file_path}: {e}")
        raise CobolSplitterError(f"Error writing file {file_path}: {e}")
    
    split_files.append(file_path)
    outline.append(f"Part {file_counter}: {os.path.basename(file_path)} ({division} DIVISION {section} SECTION)\n")
    logging.debug(f"Saved file: {file_path}")

async def process_included_file(
    file_path: str,
    data_items: Dict,
    outline: List[str],
    current_division: str,
    included_files: Set[str],
) -> None:
    """
    Process a COBOL copybook file asynchronously.

    Args:
        file_path: Path to the copybook file.
        data_items: Dictionary to store data items.
        outline: List to store outline entries.
        current_division: Current division for context.
        included_files: Set to track processed files.

    Raises:
        CobolSplitterError: If file cannot be read.
    """
    if file_path in included_files or not os.path.exists(file_path):
        return
    
    included_files.add(file_path)
    outline.append(f"Included File: {os.path.basename(file_path)}\n")
    logging.debug(f"Processing included file: {file_path}")
    
    try:
        async for line in read_file_stream(file_path):
            if "01" in line and "PIC" not in line:
                item_name = line.split("01")[1].split(".")[0].strip()
                data_items[item_name] = {
                    "level": 1,
                    "type": "GROUP",
                    "usage": "",
                    "parent": None,
                    "children": [],
                }
                outline.append(f"Data Item (Included): {item_name}\n")
    except CobolSplitterError as e:
        logging.error(f"Error processing included file {file_path}: {e}")
        raise

def process_included_file_sync(
    file_path: str,
    data_items: Dict,
    outline: List[str],
    current_division: str,
    included_files: Set[str],
) -> None:
    """
    Process a COBOL copybook file synchronously.

    Args:
        file_path: Path to the copybook file.
        data_items: Dictionary to store data items.
        outline: List to store outline entries.
        current_division: Current division for context.
        included_files: Set to track processed files.

    Raises:
        CobolSplitterError: If file cannot be read.
    """
    if file_path in included_files or not os.path.exists(file_path):
        return
    
    included_files.add(file_path)
    outline.append(f"Included File: {os.path.basename(file_path)}\n")
    logging.debug(f"Processing included file: {file_path}")
    
    try:
        for line in read_file_stream_sync(file_path):
            if "01" in line and "PIC" not in line:
                item_name = line.split("01")[1].split(".")[0].strip()
                data_items[item_name] = {
                    "level": 1,
                    "type": "GROUP",
                    "usage": "",
                    "parent": None,
                    "children": [],
                }
                outline.append(f"Data Item (Included): {item_name}\n")
    except CobolSplitterError as e:
        logging.error(f"Error processing included file {file_path}: {e}")
        raise