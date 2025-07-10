IDENTIFICATION DIVISION.
       PROGRAM-ID. METADATA-CAPTURE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT METADATA-LOG ASSIGN TO "data/metadata.log"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           
           SELECT SEQUENCE-FILE ASSIGN TO "data/sequence.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  METADATA-LOG.
       01  LOG-RECORD          PIC X(200).
       
       FD  SEQUENCE-FILE.
       01  SEQ-RECORD          PIC 9(10).
       
       WORKING-STORAGE SECTION.
       01  WS-TIMESTAMP        PIC X(26).
       01  WS-SEQUENCE-NUM     PIC 9(10) VALUE 0.
       01  WS-RECORD-ID        PIC X(10).
       01  WS-FILE-STATUS      PIC XX.
       01  WS-METADATA.
           05  FILLER          PIC X(16) VALUE '{"source":"COBOL'.
           05  FILLER          PIC X(14) VALUE ',"operation":"'.
           05  WS-OP-TYPE      PIC X(8) VALUE "CREATE".
           05  FILLER          PIC X(16) VALUE '","timestamp":"'.
           05  WS-TIME-STAMP   PIC X(26).
           05  FILLER          PIC X(16) VALUE '","record_id":"'.
           05  WS-REC-ID       PIC X(10).
           05  FILLER          PIC X(20) VALUE '","file":"MASTER.DAT'.
           05  FILLER          PIC X(14) VALUE ',"status":"00"'.
           05  FILLER          PIC X(1)  VALUE '}'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM GET-NEXT-SEQUENCE
           PERFORM GENERATE-RECORD-ID
           PERFORM CAPTURE-METADATA
           STOP RUN.
       
       GET-NEXT-SEQUENCE.
           OPEN I-O SEQUENCE-FILE
           IF WS-FILE-STATUS = "35" OR WS-FILE-STATUS = "05"
               MOVE 1 TO WS-SEQUENCE-NUM
               OPEN OUTPUT SEQUENCE-FILE
               WRITE SEQ-RECORD FROM WS-SEQUENCE-NUM
               CLOSE SEQUENCE-FILE
           ELSE
               READ SEQUENCE-FILE INTO WS-SEQUENCE-NUM
               ADD 1 TO WS-SEQUENCE-NUM
               CLOSE SEQUENCE-FILE
               OPEN OUTPUT SEQUENCE-FILE
               WRITE SEQ-RECORD FROM WS-SEQUENCE-NUM
               CLOSE SEQUENCE-FILE
           END-IF.
       
       GENERATE-RECORD-ID.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           STRING "REC" WS-SEQUENCE-NUM
               DELIMITED BY SIZE
               INTO WS-RECORD-ID.
       
       CAPTURE-METADATA.
           MOVE WS-TIMESTAMP TO WS-TIME-STAMP
           MOVE WS-RECORD-ID TO WS-REC-ID
           
           OPEN EXTEND METADATA-LOG
           MOVE WS-METADATA TO LOG-RECORD
           WRITE LOG-RECORD
           CLOSE METADATA-LOG
           
           DISPLAY "Metadata captured: " WS-REC-ID.