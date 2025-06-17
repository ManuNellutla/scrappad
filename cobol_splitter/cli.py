import os
import json
import glob
import argparse
import asyncio
import logging
from datetime import datetime
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor
from tqdm import tqdm
from typing import List
from .parser import CobolParser
from .file_handler import read_file_stream, save_file, process_included_file, read_file_stream_sync, process_included_file_sync, CobolSplitterError
from .outline import generate_outline
from .logger import setup_logging
from .types import SplitResult, CobolSplitterError

async def split_cobol_program(
    input_file: str,
    max_size_mb: float = 1.0,
    output_dir: str = "split_cobol",
    split_strategy: str = "division_section",
    compress: bool = False,
    debug: bool = False,
) -> SplitResult:
    """
    Split a COBOL program into smaller files and generate an outline (async).

    Args:
        input_file: Path to the COBOL file.
        max_size_mb: Maximum file size in MB.
        output_dir: Directory to save outputs.
        split_strategy: Split strategy (division_section, paragraph, line_count).
        compress: Compress output files with gzip.
        debug: Enable debug logging.

    Returns:
        Dictionary with split files, outline path, and manifest path.

    Raises:
        CobolSplitterError: If input file is invalid or processing fails.
    """
    if not input_file.endswith(".cbl"):
        raise CobolSplitterError("Input file must have .cbl extension")
    
    setup_logging(output_dir, debug)
    logging.info(f"Starting to process {input_file}")
    
    os.makedirs(output_dir, exist_ok=True)
    
    max_size_bytes = max_size_mb * 1024 * 1024
    program_name = Path(input_file).stem
    file_counter = 1
    split_files: List[str] = []
    outline: List[str] = []
    included_files: set[str] = set()
    current_file_content: List[str] = []
    current_file_size = 0
    
    parser = CobolParser()
    parser.program_name = program_name
    
    file_size = os.path.getsize(input_file)
    with tqdm(total=file_size, unit="B", unit_scale=True, desc=f"Processing {program_name}") as pbar:
        async for line in read_file_stream(input_file):
            current_file_content.append(line)
            current_file_size += len(line.encode("utf-8"))
            pbar.update(len(line.encode("utf-8")))
            
            parser.parse_line(line)
            
            if hasattr(parser, "copy_file"):
                copy_path = os.path.join(os.path.dirname(input_file), f"{parser.copy_file}.cpy")
                await process_included_file(copy_path, parser.data_items, outline, parser.current_division, included_files)
                delattr(parser, "copy_file")
            
            if parser.current_division and current_file_content:
                if split_strategy == "division_section" and parser.current_division != (previous_division := locals().get("previous_division")):
                    save_file(program_name, previous_division or parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = [line]
                    current_file_size = len(line.encode("utf-8"))
                    file_counter += 1
                elif split_strategy == "paragraph" and parser.current_paragraph != (previous_paragraph := locals().get("previous_paragraph")) and current_file_size > max_size_bytes:
                    save_file(program_name, parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = [line]
                    current_file_size = len(line.encode("utf-8"))
                    file_counter += 1
                elif split_strategy == "line_count" and current_file_size > max_size_bytes:
                    save_file(program_name, parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = []
                    current_file_size = 0
                    file_counter += 1
            
            previous_division = parser.current_division
            previous_paragraph = parser.current_paragraph
    
    if current_file_content:
        save_file(program_name, parser.current_division or "UNKNOWN", parser.current_section or "DEFAULT",
                 current_file_content, output_dir, file_counter, split_files, outline, compress)
    
    outline = generate_outline(
        parser.program_name, parser.current_division or "UNKNOWN", parser.current_section,
        parser.data_items, parser.file_items, parser.procedures, parser.rules, parser.relationships,
        parser.main_procedures, parser.conversational_ops, parser.batch_ops, split_files
    )
    
    outline_path = os.path.join(output_dir, f"{program_name}_outline.md")
    try:
        with open(outline_path, "w", encoding="utf-8") as f:
            f.writelines(outline)
    except IOError as e:
        logging.error(f"Error writing outline {outline_path}: {e}")
        raise CobolSplitterError(f"Error writing outline {outline_path}: {e}")
    
    manifest = {
        "program_name": program_name,
        "source_file": input_file,
        "split_files": split_files,
        "created_at": datetime.now().isoformat(),
    }
    manifest_path = os.path.join(output_dir, f"{program_name}_manifest.json")
    try:
        with open(manifest_path, "w", encoding="utf-8") as f:
            json.dump(manifest, f, indent=2)
    except IOError as e:
        logging.error(f"Error writing manifest {manifest_path}: {e}")
        raise CobolSplitterError(f"Error writing manifest {manifest_path}: {e}")
    
    logging.info(f"Completed processing {input_file}")
    return {"split_files": split_files, "outline_path": outline_path, "manifest_path": manifest_path}

def split_cobol_program_sync(
    input_file: str,
    max_size_mb: float = 1.0,
    output_dir: str = "split_cobol",
    split_strategy: str = "division_section",
    compress: bool = False,
    debug: bool = False,
) -> SplitResult:
    """
    Split a COBOL program into smaller files and generate an outline (synchronous).

    Args:
        input_file: Path to the COBOL file.
        max_size_mb: Maximum file size in MB.
        output_dir: Directory to save outputs.
        split_strategy: Split strategy (division_section, paragraph, line_count).
        compress: Compress output files with gzip.
        debug: Enable debug logging.

    Returns:
        Dictionary with split files, outline path, and manifest path.

    Raises:
        CobolSplitterError: If input file is invalid or processing fails.
    """
    if not input_file.endswith(".cbl"):
        raise CobolSplitterError("Input file must have .cbl extension")
    
    setup_logging(output_dir, debug)
    logging.info(f"Starting to process {input_file}")
    
    os.makedirs(output_dir, exist_ok=True)
    
    max_size_bytes = max_size_mb * 1024 * 1024
    program_name = Path(input_file).stem
    file_counter = 1
    split_files: List[str] = []
    outline: List[str] = []
    included_files: set[str] = set()
    current_file_content: List[str] = []
    current_file_size = 0
    
    parser = CobolParser()
    parser.program_name = program_name
    
    file_size = os.path.getsize(input_file)
    with tqdm(total=file_size, unit="B", unit_scale=True, desc=f"Processing {program_name}") as pbar:
        for line in read_file_stream_sync(input_file):
            current_file_content.append(line)
            current_file_size += len(line.encode("utf-8"))
            pbar.update(len(line.encode("utf-8")))
            
            parser.parse_line(line)
            
            if hasattr(parser, "copy_file"):
                copy_path = os.path.join(os.path.dirname(input_file), f"{parser.copy_file}.cpy")
                process_included_file_sync(copy_path, parser.data_items, outline, parser.current_division, included_files)
                delattr(parser, "copy_file")
            
            if parser.current_division and current_file_content:
                if split_strategy == "division_section" and parser.current_division != (previous_division := locals().get("previous_division")):
                    save_file(program_name, previous_division or parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = [line]
                    current_file_size = len(line.encode("utf-8"))
                    file_counter += 1
                elif split_strategy == "paragraph" and parser.current_paragraph != (previous_paragraph := locals().get("previous_paragraph")) and current_file_size > max_size_bytes:
                    save_file(program_name, parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = [line]
                    current_file_size = len(line.encode("utf-8"))
                    file_counter += 1
                elif split_strategy == "line_count" and current_file_size > max_size_bytes:
                    save_file(program_name, parser.current_division, parser.current_section or "DEFAULT",
                             current_file_content, output_dir, file_counter, split_files, outline, compress)
                    current_file_content = []
                    current_file_size = 0
                    file_counter += 1
            
            previous_division = parser.current_division
            previous_paragraph = parser.current_paragraph
    
    if current_file_content:
        save_file(program_name, parser.current_division or "UNKNOWN", parser.current_section or "DEFAULT",
                 current_file_content, output_dir, file_counter, split_files, outline, compress)
    
    outline = generate_outline(
        parser.program_name, parser.current_division or "UNKNOWN", parser.current_section,
        parser.data_items, parser.file_items, parser.procedures, parser.rules, parser.relationships,
        parser.main_procedures, parser.conversational_ops, parser.batch_ops, split_files
    )
    
    outline_path = os.path.join(output_dir, f"{program_name}_outline.md")
    try:
        with open(outline_path, "w", encoding="utf-8") as f:
            f.writelines(outline)
    except IOError as e:
        logging.error(f"Error writing outline {outline_path}: {e}")
        raise CobolSplitterError(f"Error writing outline {outline_path}: {e}")
    
    manifest = {
        "program_name": program_name,
        "source_file": input_file,
        "split_files": split_files,
        "created_at": datetime.now().isoformat(),
    }
    manifest_path = os.path.join(output_dir, f"{program_name}_manifest.json")
    try:
        with open(manifest_path, "w", encoding="utf-8") as f:
            json.dump(manifest, f, indent=2)
    except IOError as e:
        logging.error(f"Error writing manifest {manifest_path}: {e}")
        raise CobolSplitterError(f"Error writing manifest {manifest_path}: {e}")
    
    logging.info(f"Completed processing {input_file}")
    return {"split_files": split_files, "outline_path": outline_path, "manifest_path": manifest_path}

def process_single_file(
    input_file: str,
    max_size_mb: float,
    output_dir: str,
    split_strategy: str,
    compress: bool,
    debug: bool,
) -> SplitResult:
    """
    Process a single COBOL file synchronously for parallel execution.

    Args:
        input_file: Path to the COBOL file.
        max_size_mb: Maximum file size in MB.
        output_dir: Directory to save outputs.
        split_strategy: Split strategy.
        compress: Compress output files.
        debug: Enable debug logging.

    Returns:
        Split result dictionary.

    Raises:
        CobolSplitterError: If processing fails.
    """
    setup_logging(output_dir, debug)
    try:
        return split_cobol_program_sync(
            input_file, max_size_mb, output_dir, split_strategy, compress, debug
        )
    except Exception as e:
        logging.error(f"Error processing {input_file}: {str(e)}")
        raise

def process_directory(
    input_dir: str,
    max_size_mb: float,
    output_dir: str,
    split_strategy: str,
    compress: bool,
    debug: bool,
) -> List[SplitResult]:
    """
    Process all COBOL files in a directory in parallel.

    Args:
        input_dir: Directory containing COBOL files.
        max_size_mb: Maximum file size in MB.
        output_dir: Directory to save outputs.
        split_strategy: Split strategy.
        compress: Compress output files.
        debug: Enable debug logging.

    Returns:
        List of split results.
    """
    cobol_files = glob.glob(os.path.join(input_dir, "*.cbl"))
    if not cobol_files:
        setup_logging(output_dir, debug)
        logging.warning(f"No COBOL files found in {input_dir}")
        return []
    
    results: List[SplitResult] = []
    with ProcessPoolExecutor() as executor:
        futures = [
            executor.submit(
                process_single_file,
                input_file=f,
                max_size_mb=max_size_mb,
                output_dir=output_dir,
                split_strategy=split_strategy,
                compress=compress,
                debug=debug
            ) for f in cobol_files
        ]
        for future in futures:
            try:
                results.append(future.result())
            except Exception as e:
                setup_logging(output_dir, debug)
                logging.error(f"Subprocess error: {str(e)}")
                continue
    
    return results

def main() -> None:
    """Command-line interface for the COBOL splitter."""
    parser = argparse.ArgumentParser(description="Split large COBOL programs.")
    parser.add_argument("input_path", help="COBOL file or directory path")
    parser.add_argument("--max-size", type=float, default=1, help="Max file size in MB")
    parser.add_argument("--output-dir", default="split_cobol", help="Output directory")
    parser.add_argument(
        "--strategy", default="division_section",
        choices=["division_section", "paragraph", "line_count"],
        help="Split strategy"
    )
    parser.add_argument("--compress", action="store_true", help="Compress split files")
    parser.add_argument("--debug", action="store_true", help="Enable debug logging")
    
    args = parser.parse_args()
    
    if os.path.isdir(args.input_path):
        results = process_directory(args.input_path, args.max_size, args.output_dir,
                                  args.strategy, args.compress, args.debug)
        print("\nBatch processing completed!")
        for result in results:
            if result["split_files"]:
                print(f"\nProcessed {os.path.basename(result['outline_path']).replace('_outline.md', '')}:")
                print("Split files created:", *result["split_files"], sep="\n- ")
                print(f"Outline: {result['outline_path']}")
                print(f"Manifest: {result['manifest_path']}")
    else:
        if not args.input_path.endswith(".cbl"):
            print("Error: Input file must have .cbl extension")
            return
        result = asyncio.run(split_cobol_program(
            args.input_path, args.max_size, args.output_dir, args.strategy, args.compress, args.debug
        ))
        print("\nCOBOL program split successfully!")
        print("Split files created:", *result["split_files"], sep="\n- ")
        print(f"Outline: {result['outline_path']}")
        print(f"Manifest: {result['manifest_path']}")

if __name__ == "__main__":
    main()