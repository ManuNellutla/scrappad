BANK ;
 ; Banking System Main Menu
 W !,"Banking System"
 W !,"1. Create Account"
 W !,"2. Deposit"
 W !,"3. Withdraw"
 W !,"4. Check Balance"
 W !,"5. Exit"
 R CHOICE
 I CHOICE=1 D CREATE
 E  I CHOICE=2 D DEPOSIT
 E  I CHOICE=3 D WITHDRAW
 E  I CHOICE=4 D BALANCE
 E  I CHOICE=5 Q
 E  W !,"Invalid choice"
 D BANK

CREATE ;
 W !,"Enter customer ID: "
 R CUSTOMER_ID
 I $D(^ACCOUNTS(CUSTOMER_ID)) W !,"Account exists" Q
 W !,"Enter initial balance: "
 R INITIAL_BALANCE
 I INITIAL_BALANCE<0 W !,"Invalid balance" Q
 S ^ACCOUNTS(CUSTOMER_ID,"BALANCE")=INITIAL_BALANCE
 W !,"Account created"

DEPOSIT ;
 W !,"Enter customer ID: "
 R CUSTOMER_ID
 I '$D(^ACCOUNTS(CUSTOMER_ID)) W !,"Account not found" Q
 W !,"Enter deposit amount: "
 R AMOUNT
 I AMOUNT<=0 W !,"Invalid amount" Q
 S ^ACCOUNTS(CUSTOMER_ID,"BALANCE")=^ACCOUNTS(CUSTOMER_ID,"BALANCE")+AMOUNT
 W !,"Deposit successful"

WITHDRAW ;
 W !,"Enter customer ID: "
 R CUSTOMER_ID
 I '$D(^ACCOUNTS(CUSTOMER_ID)) W !,"Account not found" Q
 W !,"Enter withdrawal amount: "
 R AMOUNT
 I AMOUNT<=0 W !,"Invalid amount" Q
 I AMOUNT>^ACCOUNTS(CUSTOMER_ID,"BALANCE") W !,"Insufficient funds" Q
 S ^ACCOUNTS(CUSTOMER_ID,"BALANCE")=^ACCOUNTS(CUSTOMER_ID,"BALANCE")-AMOUNT
 W !,"Withdrawal successful"

BALANCE ;
 W !,"Enter customer ID: "
 R CUSTOMER_ID
 I '$D(^ACCOUNTS(CUSTOMER_ID)) W !,"Account not found" Q
 W !,"Balance: ",^ACCOUNTS(CUSTOMER_ID,"BALANCE")
