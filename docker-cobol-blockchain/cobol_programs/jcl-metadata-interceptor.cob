IDENTIFICATION DIVISION.
       PROGRAM-ID. JCL-METADATA-INTERCEPTOR.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JCL-LOG ASSIGN TO "JCLLOG"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  JCL-LOG.
       01  JCL-RECORD          PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-JOB-METADATA.
           05  WS-JOB-NAME     PIC X(8).
           05  WS-STEP-NAME    PIC X(8).
           05  WS-PROGRAM      PIC X(8).
           05  WS-DD-NAME      PIC X(8).
           05  WS-DATASET      PIC X(44).
           05  WS-DISP         PIC X(20).
           05  WS-TIMESTAMP    PIC X(26).
       
       01  WS-BLOCKCHAIN-CALL.
           05  WS-PROGRAM-NAME PIC X(8) VALUE "BLKCHN01".
           05  WS-METADATA-PTR POINTER.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           ACCEPT WS-JOB-NAME FROM JOB-NAME
           ACCEPT WS-STEP-NAME FROM STEP-NAME
           
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           
           PERFORM CAPTURE-DD-STATEMENTS
           PERFORM CALL-BLOCKCHAIN-ADAPTER
           
           GOBACK.
       
       CAPTURE-DD-STATEMENTS.
      *    Parse job step for DD statements and datasets
           MOVE "CREATE" TO WS-DISP
           MOVE "MASTER.CUSTOMER.DATA" TO WS-DATASET
           
      *    Write to metadata log
           OPEN OUTPUT JCL-LOG
           STRING "JOB=" WS-JOB-NAME
                  ",STEP=" WS-STEP-NAME  
                  ",DS=" WS-DATASET
                  ",TIME=" WS-TIMESTAMP
                  DELIMITED BY SIZE
                  INTO JCL-RECORD
           WRITE JCL-RECORD
           CLOSE JCL-LOG.
       
       CALL-BLOCKCHAIN-ADAPTER.
           SET WS-METADATA-PTR TO ADDRESS OF WS-JOB-METADATA
           CALL WS-PROGRAM-NAME USING WS-METADATA-PTR
           END-CALL.