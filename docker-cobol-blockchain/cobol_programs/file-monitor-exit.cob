IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-MONITOR-EXIT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAND          PIC X(200).
       01  WS-RETURN-CODE      PIC S9(9) COMP.
       
       LINKAGE SECTION.
       01  LK-FILE-NAME        PIC X(50).
       01  LK-OPERATION        PIC X(10).
       
       PROCEDURE DIVISION USING LK-FILE-NAME LK-OPERATION.
           STRING "echo '{"
                  '"file":"' DELIMITED BY SIZE
                  LK-FILE-NAME DELIMITED BY SPACE
                  '","op":"' DELIMITED BY SIZE
                  LK-OPERATION DELIMITED BY SPACE
                  '"}' "' >> /tmp/cobol-triggers.pipe"
                  DELIMITED BY SIZE
                  INTO WS-COMMAND
           
           CALL "SYSTEM" USING WS-COMMAND
                        RETURNING WS-RETURN-CODE
           
           EXIT PROGRAM.