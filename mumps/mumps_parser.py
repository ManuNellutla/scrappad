import re

def parse_mumps_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    business_rules = []
    functional_rules = []
    logic = []
    data_points = set()
    current_routine = None
    
    for line in lines:
        line = line.strip()
        if not line or line.startswith(';'):
            continue
        if line.endswith(';'):
            current_routine = line[:-1].strip()
            logic.append(f"Routine: {current_routine}")
            continue
        
        # Extract globals and locals
        globals_matches = re.findall(r'\^ACCOUNTS\(([^)]+)\)', line)
        for match in globals_matches:
            data_points.add(f"Global: ^ACCOUNTS({match})")
        
        if re.search(r'\b(S|SET)\b', line, re.I):
            var_match = re.search(r'\b(S|SET)\s+([^\s=]+)\s*=', line, re.I)
            if var_match:
                var = var_match.group(2)
                if not var.startswith('^'):
                    data_points.add(f"Local: {var}")
        
        # Extract business rules from IF conditions
        if re.search(r'\b(I|IF)\b', line, re.I):
            if '$D(^ACCOUNTS(CUSTOMER_ID))' in line:
                business_rules.append("Account IDs must be unique.")
                functional_rules.append("Check account existence with $DATA(^ACCOUNTS(CUSTOMER_ID)).")
            if 'AMOUNT<=0' in line:
                business_rules.append("Transaction amounts must be positive.")
                functional_rules.append("Validate AMOUNT > 0.")
            if 'AMOUNT>^ACCOUNTS(CUSTOMER_ID,"BALANCE")' in line:
                business_rules.append("Withdrawals cannot exceed account balance.")
                functional_rules.append("Check sufficient funds with AMOUNT <= ^ACCOUNTS(CUSTOMER_ID,'BALANCE').")
            if 'INITIAL_BALANCE<0' in line:
                business_rules.append("Initial account balance cannot be negative.")
                functional_rules.append("Validate INITIAL_BALANCE >= 0.")
        
        # Extract logic
        if re.search(r'\b(W|WRITE)\b', line, re.I):
            logic.append(f"Output: {line}")
        if re.search(r'\b(R|READ)\b', line, re.I):
            logic.append(f"Input: {line}")
        if re.search(r'\b(D|DO)\b', line, re.I):
            logic.append(f"Call: {line}")
    
    return {
        'overview': 'This MUMPS program implements a banking system for managing customer accounts, supporting account creation, deposits, withdrawals, and balance inquiries using a global array ^ACCOUNTS for persistent storage.',
        'business_rules': business_rules,
        'functional_rules': functional_rules,
        'logic': logic,
        'data_points': list(data_points)
    }

def generate_document(parsed_data, output_path):
    with open(output_path, 'w') as file:
        file.write('# Banking System Analysis\n\n')
        file.write('## Overview\n')
        file.write(parsed_data['overview'] + '\n\n')
        file.write('## Business Rules\n')
        for rule in parsed_data['business_rules']:
            file.write(f"- {rule}\n")
        file.write('\n## Functional Rules\n')
        for rule in parsed_data['functional_rules']:
            file.write(f"- {rule}\n")
        file.write('\n## Logic\n')
        for step in parsed_data['logic']:
            file.write(f"- {step}\n")
        file.write('\n## Data Points\n')
        for point in parsed_data['data_points']:
            file.write(f"- {point}\n")

# Example usage
if __name__ == "__main__":
    parsed_data = parse_mumps_file('BANK.m')
    generate_document(parsed_data, 'banking_analysis.md')
