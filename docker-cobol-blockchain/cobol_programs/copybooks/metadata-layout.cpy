* METADATA-LAYOUT.CPY - Common metadata structure
       01  METADATA-RECORD.
           05  META-SOURCE         PIC X(10).
           05  META-TIMESTAMP      PIC X(26).
           05  META-OPERATION      PIC X(10).
           05  META-FILE-NAME      PIC X(30).
           05  META-RECORD-KEY     PIC X(20).
           05  META-USER           PIC X(10).
           05  META-JOB-NAME       PIC X(8).
           05  META-STATUS         PIC XX.
           05  META-HASH           PIC X(64).