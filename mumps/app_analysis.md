# Banking System Analysis

## Overview
This MUMPS program implements a banking system for managing customer accounts, supporting account creation, deposits, withdrawals, and balance inquiries using a global array ^ACCOUNTS for persistent storage.

## Business Rules
- Account IDs must be unique.
- Initial account balance cannot be negative.
- Transaction amounts must be positive.
- Withdrawals cannot exceed account balance.

## Functional Rules
- Check account existence with $DATA(^ACCOUNTS(CUSTOMER_ID)).
- Validate INITIAL_BALANCE >= 0.
- Validate AMOUNT > 0.
- Check sufficient funds with AMOUNT <= ^ACCOUNTS(CUSTOMER_ID,"BALANCE").

## Logic
- Routine: BANK
- Output: W !,"Banking System"
- Output: W !,"1. Create Account"
- Output: W !,"2. Deposit"
- Output: W !,"3. Withdraw"
- Output: W !,"4. Check Balance"
- Output: W !,"5. Exit"
- Input: R CHOICE
- Call: D CREATE
- Call: D DEPOSIT
- Call: D WITHDRAW
- Call: D BALANCE
- Call: D BANK
- Routine: CREATE
- Output: W !,"Enter customer ID: "
- Input: R CUSTOMER_ID
- Output: W !,"Account exists"
- Output: W !,"Enter initial balance: "
- Input: R INITIAL_BALANCE
- Output: W !,"Invalid balance"
- Output: W !,"Account created"
- Routine: DEPOSIT
- Output: W !,"Enter customer ID: "
- Input: R CUSTOMER_ID
- Output: W !,"Account not found"
- Output: W !,"Enter deposit amount: "
- Input: R AMOUNT
- Output: W !,"Invalid amount"
- Output: W !,"Deposit successful"
- Routine: WITHDRAW
- Output: W !,"Enter customer ID: "
- Input: R CUSTOMER_ID
- Output: W !,"Account not found"
- Output: W !,"Enter withdrawal amount: "
- Input: R AMOUNT
- Output: W !,"Invalid amount"
- Output: W !,"Insufficient funds"
- Output: W !,"Withdrawal successful"
- Routine: BALANCE
- Output: W !,"Enter customer ID: "
- Input: R CUSTOMER_ID
- Output: W !,"Account not found"
- Output: W !,"Balance: ",^ACCOUNTS(CUSTOMER_ID,"BALANCE")

## Data Points
- Global: ^ACCOUNTS(CUSTOMER_ID)
- Global: ^ACCOUNTS(CUSTOMER_ID,"BALANCE")
- Local: CHOICE
- Local: CUSTOMER_ID
- Local: INITIAL_BALANCE
- Local: AMOUNT
