# Save as: deploy-cobol-programs.ps1

Write-Host "=== Deploying COBOL Programs ===" -ForegroundColor Green

# 1. Customer CRUD Program (customer-crud.cob)
@'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-CRUD.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/customers.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT EVENT-PIPE ASSIGN TO "pipes/crud-events"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC X(10).
           05  CUST-NAME       PIC X(30).
           05  CUST-BALANCE    PIC 9(9)V99.
           05  LAST-UPDATED    PIC X(26).
       
       FD  EVENT-PIPE.
       01  EVENT-MESSAGE       PIC X(500).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS      PIC XX.
       01  WS-OPERATION        PIC X(10).
       01  WS-TIMESTAMP        PIC X(26).
       01  WS-EVENT-JSON       PIC X(500).
       
       LINKAGE SECTION.
       01  LK-OPERATION        PIC X.
       01  LK-CUST-ID          PIC X(10).
       01  LK-CUST-NAME        PIC X(30).
       01  LK-CUST-BALANCE     PIC 9(9)V99.
       
       PROCEDURE DIVISION USING LK-OPERATION LK-CUST-ID 
                                LK-CUST-NAME LK-CUST-BALANCE.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           
           EVALUATE LK-OPERATION
               WHEN "C" PERFORM CREATE-CUSTOMER
               WHEN "R" PERFORM READ-CUSTOMER
               WHEN "U" PERFORM UPDATE-CUSTOMER
               WHEN "D" PERFORM DELETE-CUSTOMER
           END-EVALUATE
           
           GOBACK.
       
       CREATE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           MOVE LK-CUST-NAME TO CUST-NAME
           MOVE LK-CUST-BALANCE TO CUST-BALANCE
           MOVE WS-TIMESTAMP TO LAST-UPDATED
           WRITE CUSTOMER-RECORD
           IF WS-FILE-STATUS = "00"
               MOVE "CREATE" TO WS-OPERATION
               PERFORM SEND-EVENT
           END-IF
           CLOSE CUSTOMER-FILE.
       
       UPDATE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
           IF WS-FILE-STATUS = "00"
               MOVE LK-CUST-NAME TO CUST-NAME
               MOVE LK-CUST-BALANCE TO CUST-BALANCE
               MOVE WS-TIMESTAMP TO LAST-UPDATED
               REWRITE CUSTOMER-RECORD
               MOVE "UPDATE" TO WS-OPERATION
               PERFORM SEND-EVENT
           END-IF
           CLOSE CUSTOMER-FILE.
       
       READ-CUSTOMER.
           OPEN INPUT CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
           IF WS-FILE-STATUS = "00"
               MOVE CUST-NAME TO LK-CUST-NAME
               MOVE CUST-BALANCE TO LK-CUST-BALANCE
               MOVE "READ" TO WS-OPERATION
               PERFORM SEND-EVENT
           END-IF
           CLOSE CUSTOMER-FILE.
       
       DELETE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           DELETE CUSTOMER-FILE RECORD
           IF WS-FILE-STATUS = "00"
               MOVE "DELETE" TO WS-OPERATION
               PERFORM SEND-EVENT
           END-IF
           CLOSE CUSTOMER-FILE.
       
       SEND-EVENT.
           STRING '{"operation":"' WS-OPERATION 
                  '","timestamp":"' WS-TIMESTAMP
                  '","entity":"CUSTOMER","id":"' CUST-ID
                  '","name":"' CUST-NAME
                  '","balance":' CUST-BALANCE
                  ',"status":"' WS-FILE-STATUS '"}' 
                  DELIMITED BY SIZE INTO WS-EVENT-JSON
           
           OPEN OUTPUT EVENT-PIPE
           MOVE WS-EVENT-JSON TO EVENT-MESSAGE
           WRITE EVENT-MESSAGE
           CLOSE EVENT-PIPE.
'@ | Out-File -FilePath "customer-crud.cob" -Encoding UTF8

# 2. Test Driver Program (test-crud.cob)
@'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CRUD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPERATION        PIC X.
       01  WS-CUST-ID          PIC X(10).
       01  WS-CUST-NAME        PIC X(30).
       01  WS-CUST-BALANCE     PIC 9(9)V99.
       
       PROCEDURE DIVISION.
           DISPLAY "Testing CRUD operations..."
           
      *    Create customer
           MOVE "C" TO WS-OPERATION
           MOVE "CUST001" TO WS-CUST-ID
           MOVE "John Doe" TO WS-CUST-NAME
           MOVE 1000.50 TO WS-CUST-BALANCE
           CALL "CUSTOMER-CRUD" USING WS-OPERATION WS-CUST-ID 
                                     WS-CUST-NAME WS-CUST-BALANCE
           
      *    Update customer
           MOVE "U" TO WS-OPERATION
           MOVE 2500.75 TO WS-CUST-BALANCE
           CALL "CUSTOMER-CRUD" USING WS-OPERATION WS-CUST-ID 
                                     WS-CUST-NAME WS-CUST-BALANCE
           
           STOP RUN.
'@ | Out-File -FilePath "test-crud.cob" -Encoding UTF8

# Deploy to container
docker cp customer-crud.cob cobol-metadata-node:/app/cobol_programs/
docker cp test-crud.cob cobol-metadata-node:/app/cobol_programs/

# Compile
docker exec cobol-metadata-node bash -c @'
cd /app
cobc -m -o customer-crud.so cobol_programs/customer-crud.cob
cobc -x -o test-crud cobol_programs/test-crud.cob -L. -lcustomer-crud
'@

Remove-Item "customer-crud.cob", "test-crud.cob"
Write-Host "✅ COBOL programs deployed" -ForegroundColor Green