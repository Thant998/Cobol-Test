      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT Transaction ASSIGN TO 'D:\Cobol\tr.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD Transaction.
            01 Transaction-File.
                05 BRANCH-CODE     PIC 9(3).
                05 PRODUCT-CODE    PIC 9(2).
                05 QUANTITY        PIC 9(2).
       WORKING-STORAGE SECTION.
            01 WS-TRANSACTION-FILE.
                05 WS-BRANCH-CODE  PIC 9(3).
                05 WS-PRODUCT-CODE PIC 9(2).
                05 WS-QUANTITY     PIC 9(2).
             01 WS-EOF             PIC A(1).

            01 TEMP-TRANSACTION-FILE.
                05 TEMP-BRANCH-CODE  PIC 9(3).
                05 TEMP-PRODUCT-CODE PIC 9(2).
                05 TEMP-QUANTITY     PIC 9(2).
            01 TEMP-RESULT           PIC 9(3).

            77 STRING1 PIC A(12) VALUE "BRANCH CODE".
            77 STRING2 PIC A(16) VALUE "    PRODUCT CODE".
            77 STRING3 PIC A(12) VALUE "   QUANTITY".
            77 STRING4 PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            STRING STRING1,STRING2,STRING3
                 DELIMITED BY SIZE INTO STRING4
            END-STRING.
            DISPLAY STRING4.

             OPEN INPUT Transaction.
               PERFORM UNTIL WS-EOF='Y'
                  READ Transaction INTO WS-TRANSACTION-FILE
                  AT END MOVE 'Y' TO WS-EOF
                  NOT AT END
                  IF TEMP-BRANCH-CODE = ZERO AND
                     TEMP-PRODUCT-CODE= ZERO THEN
                      ADD WS-BRANCH-CODE  TO TEMP-BRANCH-CODE
                      ADD WS-PRODUCT-CODE TO TEMP-PRODUCT-CODE
                      ADD WS-QUANTITY     TO TEMP-QUANTITY
                  END-IF

                   IF WS-BRANCH-CODE =TEMP-BRANCH-CODE  AND
                      WS-PRODUCT-CODE=TEMP-PRODUCT-CODE THEN
                       ADD WS-QUANTITY TO TEMP-RESULT
                   ELSE IF WS-BRANCH-CODE =ZERO AND
                           WS-PRODUCT-CODE=ZERO AND
                           WS-QUANTITY    =ZERO THEN
                      EXIT PROGRAM
                   ELSE
                       DISPLAY TEMP-BRANCH-CODE "              "
                               TEMP-PRODUCT-CODE"              "
                               TEMP-RESULT
                       MOVE WS-TRANSACTION-FILE TO TEMP-TRANSACTION-FILE
                       MOVE WS-QUANTITY         TO TEMP-RESULT
                  END-IF
                  END-READ
               END-PERFORM.
                    IF TEMP-BRANCH-CODE =ZERO AND
                       TEMP-PRODUCT-CODE=ZERO AND
                       TEMP-RESULT      =ZERO THEN
                      EXIT PROGRAM
                    ELSE
                        DISPLAY TEMP-BRANCH-CODE "              "
                                TEMP-PRODUCT-CODE"              "
                                TEMP-RESULT
                    END-IF
            CLOSE Transaction.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
