from typing import Dict, List, Optional

def generate_outline(
    program_name: str,
    current_division: str,
    current_section: Optional[str],
    data_items: Dict[str, Dict],
    file_items: Dict[str, Dict],
    procedures: List[Dict],
    rules: List[Dict],
    relationships: List[Dict],
    main_procedures: List[str],
    conversational_ops: List[str],
    batch_ops: List[str],
    split_files: List[str],
) -> List[str]:
    """
    Generate a detailed outline for a COBOL program.

    Args:
        program_name: Name of the COBOL program.
        current_division: Current division being processed.
        current_section: Current section being processed.
        data_items: Dictionary of data items from WORKING-STORAGE.
        file_items: Dictionary of file items from FILE SECTION.
        procedures: List of procedures with operations.
        rules: List of business rules.
        relationships: List of entity and procedure relationships.
        main_procedures: List of main procedures.
        conversational_ops: List of conversational operations.
        batch_ops: List of batch operations.
        split_files: List of split file paths.

    Returns:
        List of strings forming the outline in Markdown format.
    """
    outline = [f"# Detailed Outline of {program_name.lower()}.cbl\n", f"## Program: {program_name.upper()}\n"]

    # Divisions and Sections
    outline.append(f"### {current_division} DIVISION\n")
    if current_section:
        outline.append(f"- **{current_section} SECTION**\n")

    # Entities
    outline.append("### Entities\n")
    if file_items:
        outline.append("#### File Entities\n")
        for name, item in file_items.items():
            outline.append(f"- **{name}** (Type: {item['type']})\n")
            if item["fields"]:
                outline.append("  Fields:\n")
                for field in item["fields"]:
                    outline.append(f"    - {field['name']} (Level: {field['level']}, PIC: {field['pic']})\n")
    if data_items:
        outline.append("#### Working-Storage Entities\n")
        for name, item in data_items.items():
            outline.append(f"- **{name}** (Type: {item['type']})\n")
            if item["fields"]:
                outline.append("  Fields:\n")
                for field in item["fields"]:
                    outline.append(f"    - {field['name']} (Level: {field['level']}, PIC: {field['pic']})\n")

    # Entity Relationships
    outline.append("### Entity Relationships\n")
    for rel in relationships:
        if rel["type"] == "FILE_ACCESS":
            outline.append(f"- **{rel['source']}** accesses file entity **{rel['target']}**\n")
        elif rel["type"] == "PROCEDURE_CALL":
            outline.append(f"- **{rel['source']}** calls procedure **{rel['target']}**\n")

    # Business Logic
    outline.append("### Business Logic\n")
    fee_rules = [r for r in rules if r["type"] == "FEE_CALCULATION"]
    rate_rules = [r for r in rules if r["type"] == "RATE_CALCULATION"]
    other_rules = [r for r in rules if r["type"] == "ARITHMETIC"]
    if fee_rules:
        outline.append("#### Fee Calculations\n")
        for rule in fee_rules:
            outline.append(f"- {rule['details']} (Paragraph: {rule['paragraph']})\n")
    if rate_rules:
        outline.append("#### Rate Calculations\n")
        for rule in rate_rules:
            outline.append(f"- {rule['details']} (Paragraph: {rule['paragraph']})\n")
    if other_rules:
        outline.append("#### Other Calculations\n")
        for rule in other_rules:
            outline.append(f"- {rule['details']} (Paragraph: {rule['paragraph']})\n")

    # Rules
    outline.append("### Rules\n")
    outline.append("| Type | Details | Variables | Paragraph |\n")
    outline.append("|------|---------|-----------|-----------|\n")
    for rule in rules:
        vars_str = ", ".join(rule["variables"])
        outline.append(f"| {rule['type']} | {rule['details']} | {vars_str} | {rule['paragraph']} |\n")

    # Procedures
    outline.append("### Procedures\n")
    for proc in procedures:
        outline.append(f"- **{proc['name']}** ({proc['purpose']})\n")
        outline.append("  Operations:\n")
        for op in proc["operations"]:
            outline.append(f"    - {op}\n")
        if proc["variables"]:
            outline.append("  Variables Used:\n")
            for var in proc["variables"]:
                outline.append(f"    - {var}\n")

    # Summary
    outline.append("### Summary\n")
    program_type = "Batch" if batch_ops else "Conversational" if conversational_ops else "Unknown"
    outline.append(f"- **Program Type**: {program_type}\n")
    outline.append("- **Functionalities**:\n")
    if batch_ops:
        outline.append("  - File processing (batch operations)\n")
    if fee_rules or rate_rules:
        outline.append("  - Financial calculations (fees, rates)\n")
    if procedures:
        outline.append("  - Structured business logic\n")

    # Split Files
    outline.append("### Split Files\n")
    for file in split_files:
        outline.append(f"- {file}\n")

    return outline