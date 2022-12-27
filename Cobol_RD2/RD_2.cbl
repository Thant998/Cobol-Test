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
            SELECT OldBalance ASSIGN TO 'D:\Cobol\OldBalance.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TodayTransaction ASSIGN TO
            'D:\Cobol\TodayTransaction.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT NewBalance ASSIGN TO 'D:\Cobol\NewBalance.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD NewBalance.
            01 NEWBALANCE-FILE.
                05 NEW-BRANCH-CODE     PIC 9(3).
                05 NEW-PRODUCT-CODE    PIC 9(2).
                05 NEW-QUANTITY        PIC 9(4).

       FD OldBalance.
            01 OLDBALANCE-FILE.
                05 OLD-BRANCH-CODE     PIC 9(3).
                05 OLD-PRODUCT-CODE    PIC 9(2).
                05 OLD-QUANTITY        PIC 9(4).

       FD TodayTransaction.
            01 TODAYTRANSACTION-FILE.
                05 T-BRANCH-CODE     PIC 9(3).
                05 T-PRODUCT-CODE    PIC 9(2).
                05 T-BUY-SELL        PIC 9(1).
                05 T-QUANTITY        PIC 9(4).

       WORKING-STORAGE SECTION.
            01 WS-OLDBALANCE-FILE.
                05 WS-OLD-BRANCH-CODE  PIC 9(3).
                05 WS-OLD-PRODUCT-CODE PIC 9(2).
                05 WS-OLD-QUANTITY     PIC 9(4).
             01 WS-OLD-EOF             PIC A(1).

            01 TEMP-OLDBALANCE-FILE.
                05 TEMP-OLD-BRANCH-CODE  PIC 9(3).
                05 TEMP-OLD-PRODUCT-CODE PIC 9(2).
                05 TEMP-OLD-QUANTITY     PIC 9(4).

            01 WS-TODAYTRANSACTION-FILE.
                05 WS-T-BRANCH-CODE  PIC 9(3).
                05 WS-T-PRODUCT-CODE PIC 9(2).
                05 WS-T-BUY-SELL    PIC 9(1).
                05 WS-T-QUANTITY     PIC 9(4).
             01 WS-T-EOF             PIC A(1).

            01 TEMP-TODAYTRANSACTION-FILE.
                05 TEMP-T-BRANCH-CODE  PIC 9(3).
                05 TEMP-T-PRODUCT-CODE PIC 9(2).
                 05 TEMP-T-BUY-SELL PIC 9(1).
                05 TEMP-T-QUANTITY     PIC 9(4).
            01 TEMP-RESULT PIC 9(4).

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

             OPEN INPUT OldBalance.
             OPEN EXTEND NewBalance.
               PERFORM UNTIL WS-OLD-EOF='Y'
                  READ OldBalance INTO WS-OLDBALANCE-FILE
                  AT END
                  DISPLAY WS-OLD-BRANCH-CODE "             "
                          WS-OLD-PRODUCT-CODE"                "
                          WS-OLD-QUANTITY
                  MOVE WS-OLD-BRANCH-CODE  TO NEW-BRANCH-CODE
                  MOVE WS-OLD-PRODUCT-CODE TO NEW-PRODUCT-CODE
                  MOVE WS-OLD-QUANTITY     TO NEW-QUANTITY
                  WRITE NEWBALANCE-FILE
                  MOVE 'Y' TO WS-OLD-EOF
                  NOT AT END

                  IF TEMP-OLD-BRANCH-CODE = ZERO AND
                     TEMP-OLD-PRODUCT-CODE= ZERO THEN
                      MOVE WS-OLD-BRANCH-CODE  TO TEMP-OLD-BRANCH-CODE
                      MOVE WS-OLD-PRODUCT-CODE TO TEMP-OLD-PRODUCT-CODE
                      MOVE WS-OLD-QUANTITY     TO TEMP-OLD-QUANTITY
                  END-IF

                  PERFORM TodayTransaction-PARA

               END-READ
              END-PERFORM.
             CLOSE NewBalance.
             CLOSE OldBalance.
            STOP RUN.
            TodayTransaction-PARA.
               OPEN INPUT TodayTransaction.
               PERFORM UNTIL WS-T-EOF='Y'
                  READ TodayTransaction INTO WS-TODAYTRANSACTION-FILE
                  AT END MOVE 'Y' TO WS-T-EOF
                  NOT AT END
                  MOVE WS-T-BRANCH-CODE  TO TEMP-T-BRANCH-CODE
                  MOVE WS-T-PRODUCT-CODE TO TEMP-T-PRODUCT-CODE
                  MOVE WS-T-QUANTITY     TO TEMP-T-QUANTITY

                  IF WS-OLD-BRANCH-CODE=TEMP-T-BRANCH-CODE AND
                     WS-OLD-PRODUCT-CODE=TEMP-T-PRODUCT-CODE AND
                     WS-T-BUY-SELL=2 THEN
                     MOVE WS-OLD-QUANTITY TO TEMP-RESULT
                     COMPUTE TEMP-RESULT=TEMP-RESULT+TEMP-T-QUANTITY
                     DISPLAY WS-OLD-BRANCH-CODE "             "
                             WS-OLD-PRODUCT-CODE"                "
                             TEMP-RESULT
                     MOVE WS-OLD-BRANCH-CODE  TO NEW-BRANCH-CODE
                     MOVE WS-OLD-PRODUCT-CODE TO NEW-PRODUCT-CODE
                     MOVE TEMP-RESULT         TO NEW-QUANTITY
                     WRITE NEWBALANCE-FILE
                   END-IF

                   IF WS-OLD-BRANCH-CODE=TEMP-T-BRANCH-CODE
                    IF NOT WS-OLD-PRODUCT-CODE=TEMP-T-PRODUCT-CODE THEN
                        DISPLAY WS-OLD-BRANCH-CODE "             "
                                TEMP-T-PRODUCT-CODE"                "
                                TEMP-T-QUANTITY
                        MOVE WS-OLD-BRANCH-CODE  TO NEW-BRANCH-CODE
                        MOVE TEMP-T-PRODUCT-CODE TO NEW-PRODUCT-CODE
                        MOVE TEMP-T-QUANTITY     TO NEW-QUANTITY
                        WRITE NEWBALANCE-FILE
                    END-IF
                   END-IF

                    IF WS-OLD-BRANCH-CODE=TEMP-T-BRANCH-CODE AND
                     WS-OLD-PRODUCT-CODE=TEMP-T-PRODUCT-CODE AND
                     WS-T-BUY-SELL=1 THEN
                     COMPUTE TEMP-RESULT=WS-OLD-QUANTITY -
                                         TEMP-T-QUANTITY
                     DISPLAY WS-OLD-BRANCH-CODE "             "
                             WS-OLD-PRODUCT-CODE"                "
                             TEMP-RESULT
                     MOVE WS-OLD-BRANCH-CODE  TO NEW-BRANCH-CODE
                     MOVE WS-OLD-PRODUCT-CODE TO NEW-PRODUCT-CODE
                     MOVE TEMP-RESULT         TO NEW-QUANTITY
                     WRITE NEWBALANCE-FILE
                   END-IF
               END-READ
              END-PERFORM.
              MOVE ' ' TO WS-T-EOF.
             CLOSE TodayTransaction.

       END PROGRAM YOUR-PROGRAM-NAME.
