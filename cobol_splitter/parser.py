import re
from typing import Dict, List, Set, Optional

class CobolParser:
    """Parser for COBOL source code."""
    def __init__(self):
        self.program_name: str = ""
        self.current_division: str = ""
        self.current_section: str = ""
        self.current_paragraph: str = ""
        self.data_items: Dict[str, Dict] = {}
        self.file_items: Dict[str, Dict] = {}
        self.procedures: List[Dict] = []
        self.rules: List[Dict] = []
        self.relationships: List[Dict] = []
        self.main_procedures: List[str] = []
        self.conversational_ops: List[str] = []
        self.batch_ops: List[str] = []
        self.copy_file: Optional[str] = None
        self._entity_stack: List[str] = []
        self._rule_set: Set[str] = set()  # For deduplication
        self._current_procedure_ops: List[str] = []
        self._current_procedure_vars: Set[str] = set()

    def parse_line(self, line: str) -> None:
        """Parse a single line of COBOL code."""
        line = line.strip()
        if not line or line.startswith("*"):
            return

        # Division detection
        division_match = re.match(r"(\w+-?\w*) DIVISION\.", line, re.IGNORECASE)
        if division_match:
            self.current_division = division_match.group(1).upper()
            self.current_section = ""
            self.current_paragraph = ""
            return

        # Section detection
        section_match = re.match(r"(\w+-?\w*) SECTION\.", line, re.IGNORECASE)
        if section_match:
            self.current_section = section_match.group(1).upper()
            self.current_paragraph = ""
            return

        # Program ID
        if "PROGRAM-ID." in line.upper():
            self.program_name = line.split("PROGRAM-ID.")[1].split(".")[0].strip()
            return

        # Paragraph detection
        paragraph_match = re.match(r"(\w+-?\w*)\.", line, re.IGNORECASE)
        if paragraph_match and self.current_division == "PROCEDURE" and not line.upper().startswith("END-"):
            self.current_paragraph = paragraph_match.group(1).upper()
            if self._current_procedure_ops or self._current_procedure_vars:
                self._finalize_procedure()
            return

        # Data items in FILE SECTION
        if self.current_division == "DATA" and self.current_section == "FILE":
            level_match = re.match(r"(\d{2})\s+(\w+-?\w*)\s*(PIC.*)?\.", line, re.IGNORECASE)
            if level_match:
                level, name, pic = level_match.groups()
                level = int(level)
                if level == 1:
                    self.file_items[name.upper()] = {
                        "level": level,
                        "type": "GROUP" if not pic else "ELEMENTARY",
                        "pic": pic or "",
                        "fields": [],
                    }
                    self._entity_stack = [name.upper()]
                elif level > 1 and self._entity_stack:
                    parent = self._entity_stack[-1]
                    self.file_items[parent]["fields"].append({
                        "name": name.upper(),
                        "level": level,
                        "type": "ELEMENTARY",
                        "pic": pic or "",
                    })
            return

        # Data items in WORKING-STORAGE SECTION
        if self.current_division == "DATA" and self.current_section == "WORKING-STORAGE":
            level_match = re.match(r"(\d{2})\s+(\w+-?\w*)\s*(PIC.*)?\.", line, re.IGNORECASE)
            if level_match:
                level, name, pic = level_match.groups()
                level = int(level)
                if level == 1:
                    self.data_items[name.upper()] = {
                        "level": level,
                        "type": "GROUP" if not pic else "ELEMENTARY",
                        "pic": pic or "",
                        "fields": [],
                    }
                    self._entity_stack = [name.upper()]
                elif level > 1 and self._entity_stack:
                    parent = self._entity_stack[-1]
                    self.data_items[parent]["fields"].append({
                        "name": name.upper(),
                        "level": level,
                        "type": "ELEMENTARY",
                        "pic": pic or "",
                    })
            return

        # Copybook detection
        if "COPY" in line.upper():
            copy_match = re.search(r"COPY\s+(\w+)\.", line, re.IGNORECASE)
            if copy_match:
                self.copy_file = copy_match.group(1)
            return

        # Procedure operations and rules
        if self.current_division == "PROCEDURE":
            # Skip boilerplate
            if any(op in line.upper() for op in ["STOP RUN", "END-IF", "END-PERFORM"]):
                return

            # Detect file operations
            file_ops = ["OPEN", "READ", "WRITE", "CLOSE"]
            if any(op in line.upper() for op in file_ops):
                self._current_procedure_ops.append(line.strip())
                self.batch_ops.append(line.strip())
                for file in self.file_items:
                    if file in line.upper():
                        self.relationships.append({
                            "source": self.current_paragraph or "MAIN",
                            "target": file,
                            "type": "FILE_ACCESS",
                        })
                return

            # Detect calculations
            if "COMPUTE" in line.upper():
                compute_match = re.search(r"COMPUTE\s+(\w+-?\w*)\s*=\s*(.*)", line, re.IGNORECASE)
                if compute_match:
                    var, expr = compute_match.groups()
                    rule_key = f"COMPUTE {var} = {expr}"
                    if rule_key not in self._rule_set:
                        self._rule_set.add(rule_key)
                        rule_type = "FEE_CALCULATION" if "FEE" in var.upper() or "FEE" in expr.upper() else \
                                    "RATE_CALCULATION" if "RATE" in var.upper() or "RATE" in expr.upper() else \
                                    "ARITHMETIC"
                        self.rules.append({
                            "type": rule_type,
                            "details": f"COMPUTE {var} = {expr}",
                            "variables": [var.upper()] + [v.upper() for v in re.findall(r"\w+-?\w*", expr) if v.upper() in self.data_items or v.upper() in self.file_items],
                            "paragraph": self.current_paragraph or "MAIN",
                        })
                    self._current_procedure_ops.append(line.strip())
                    self._current_procedure_vars.add(var.upper())
                    for v in re.findall(r"\w+-?\w*", expr):
                        if v.upper() in self.data_items or v.upper() in self.file_items:
                            self._current_procedure_vars.add(v.upper())
                    return

            # Detect conditional logic
            if "IF" in line.upper() and not "END-IF" in line.upper():
                self._current_procedure_ops.append(line.strip())
                for var in re.findall(r"\w+-?\w*", line):
                    if var.upper() in self.data_items or var.upper() in self.file_items:
                        self._current_procedure_vars.add(var.upper())
                return

            # Detect PERFORM statements
            if "PERFORM" in line.upper():
                perform_match = re.search(r"PERFORM\s+(\w+-?\w*)", line, re.IGNORECASE)
                if perform_match:
                    target = perform_match.group(1).upper()
                    self.relationships.append({
                        "source": self.current_paragraph or "MAIN",
                        "target": target,
                        "type": "PROCEDURE_CALL",
                    })
                    self._current_procedure_ops.append(line.strip())
                return

    def _finalize_procedure(self) -> None:
        """Finalize the current procedure and store it."""
        if self.current_paragraph and (self._current_procedure_ops or self._current_procedure_vars):
            purpose = "Calculation" if any("COMPUTE" in op.upper() for op in self._current_procedure_ops) else \
                      "File Operation" if any(op in op.upper() for op in ["OPEN", "READ", "WRITE", "CLOSE"] for op in self._current_procedure_ops) else \
                      "Control Flow"
            self.procedures.append({
                "name": self.current_paragraph,
                "operations": self._current_procedure_ops,
                "variables": list(self._current_procedure_vars),
                "purpose": purpose,
            })
            if self.current_paragraph == "MAIN" or "MAIN" in self.current_paragraph.upper():
                self.main_procedures.append(self.current_paragraph)
            self._current_procedure_ops = []
            self._current_procedure_vars = set()