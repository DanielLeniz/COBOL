       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-SYSTEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCOUNT-NUMBER       PIC 9(5) VALUE ZEROS.
       01  ACCOUNT-BALANCE      PIC 9(8)V99 VALUE ZEROS.
       01  DEPOSIT-AMOUNT       PIC 9(8)V99.
       01  WITHDRAW-AMOUNT      PIC 9(8)V99.
       01  USER-CHOICE          PIC 9.
       01  MAX-ACCOUNTS         PIC 9(4) VALUE 100.
       01  CURRENT-ACCOUNT-INDEX PIC 9(4) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'Welcome to Simple Bank System'.
           DISPLAY '1. Open Account'.
           DISPLAY '2. Deposit Money'.
           DISPLAY '3. Withdraw Money'.
           DISPLAY '4. View Balance'.
           DISPLAY '5. Exit'.
           DISPLAY 'Enter your choice (1-5): '.
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM OPEN-ACCOUNT
               WHEN 2
                   PERFORM DEPOSIT-MONEY
               WHEN 3
                   PERFORM WITHDRAW-MONEY
               WHEN 4
                   PERFORM VIEW-BALANCE
               WHEN 5
                   PERFORM EXIT-PROGRAM
               WHEN OTHER
                   DISPLAY 'Invalid choice.' 
                   DISPLAY 'Please enter a number between 1 and 5.'
                   PERFORM MAIN-LOGIC
           END-EVALUATE.

       OPEN-ACCOUNT.
           IF CURRENT-ACCOUNT-INDEX >= MAX-ACCOUNTS
               DISPLAY 'Cannot open more accounts.' 
               DISPLAY 'Maximum limit reached.'
           ELSE
               ADD 1 TO CURRENT-ACCOUNT-INDEX
               MOVE CURRENT-ACCOUNT-INDEX TO ACCOUNT-NUMBER
               DISPLAY 'New account created.'
               DISPLAY 'Your account number is: ' ACCOUNT-NUMBER
           END-IF.
           PERFORM MAIN-LOGIC.

       DEPOSIT-MONEY.
           DISPLAY 'Enter account number: '
           ACCEPT ACCOUNT-NUMBER.
           IF CURRENT-ACCOUNT-INDEX > 0
               DISPLAY 'Enter deposit amount: '
               ACCEPT DEPOSIT-AMOUNT
               ADD DEPOSIT-AMOUNT TO ACCOUNT-BALANCE
               DISPLAY 'Deposit successful.' 
               DISPLAY 'New balance is: ' ACCOUNT-BALANCE
           ELSE
               DISPLAY 'No accounts exist.'
               DISPLAY 'Please open an account first.'
           END-IF.
           PERFORM MAIN-LOGIC.

       WITHDRAW-MONEY.
           DISPLAY 'Enter withdrawal amount: '.
           ACCEPT WITHDRAW-AMOUNT.
           IF WITHDRAW-AMOUNT > ACCOUNT-BALANCE
               DISPLAY 'Error: Insufficient funds.'
           ELSE
               SUBTRACT WITHDRAW-AMOUNT FROM ACCOUNT-BALANCE
               DISPLAY 'Withdrawal successful.' 
               DISPLAY 'New balance is: ' ACCOUNT-BALANCE
           END-IF.
           PERFORM MAIN-LOGIC.

       VIEW-BALANCE.
           DISPLAY 'Your current balance is: ' ACCOUNT-BALANCE.
           PERFORM MAIN-LOGIC.

       EXIT-PROGRAM.
           DISPLAY 'Thank you for using the Simple Bank System.'.
           STOP RUN.
