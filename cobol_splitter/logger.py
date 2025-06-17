import os
import logging
from typing import Optional

def setup_logging(output_dir: str, debug: bool = False) -> None:
    """
    Configure logging to file and console.

    Args:
        output_dir: Directory to save log file.
        debug: Enable debug-level logging if True.
    """
    log_level = logging.DEBUG if debug else logging.INFO
    log_file = os.path.join(output_dir, "split_cobol.log")
    
    # Remove existing handlers to prevent duplicates
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    
    # Configure logging
    logging.basicConfig(
        level=log_level,
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler(),
        ],
    )
    logging.debug(f"Logging configured: level={log_level}, file={log_file}")