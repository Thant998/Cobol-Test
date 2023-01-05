      ******************************************************************
      * Author: U HIGH
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT OldBalance1 ASSIGN TO
            'C:\cobolpractice\OldBalance1.txt'
              ORGANIZATION IS LINE SEQUENTIAL.
             SELECT TodayTransaction1 ASSIGN TO
            'C:\cobolpractice\TodayTransaction1.txt'
              ORGANIZATION IS LINE SEQUENTIAL.
              SELECT NewBalance ASSIGN TO
            'C:\cobolpractice\NewBalance.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD NewBalance.
            01   NEW-BALANCE-FILE.
              05 NEW-BRANCH-CODE       PIC 9(3).
              05 FILLER                PIC X(12).
              05 NEW-PRODUCT-CODE      PIC 9(2).
              05 FILLER                PIC X(16).
              05 NEW-QUANTITY          PIC 9(4).
              05 FILLER                PIC X(12).

       FD OldBalance1.
            01   OLD-BALANCE-FILE.
              05 OLD-BRANCH-CODE       PIC 9(3).
              05 OLD-PRODUCT-CODE      PIC 9(2).
              05 OLD-QUANTITY          PIC 9(4).

       FD TodayTransaction1.
            01   TODAY-TRANSACTION-FILE.
              05 T-BRANCH-CODE           PIC 9(3).
              05 T-PRODUCT-CODE          PIC 9(2).
              05 T-BUY-SELL              PIC 9(1).
              05 T-QUANTITY              PIC 9(4).


       WORKING-STORAGE SECTION.
      *>    OLD BALANCE
            01   WS-OLD-BALANCE.
              05 WS-OLD-BRANCH-CODE      PIC 9(3).
              05 WS-OLD-PRODUCT-CODE     PIC 9(2).
              05 WS-OLD-QUANTITY         PIC 9(4).
            01   WS-OLD-EOF              PIC A(1).

            01   WS-TEMP-OLD-BALANCE.
              05 TEMP-OLD-BRANCH-CODE    PIC 9(3).
              05 TEMP-OLD-PRODUCT-CODE   PIC 9(2).
              05 TEMP-OLD-QUANTITY       PIC 9(4).

      *>    ************************************************************
      *>    TODAY TRANSACTION
            01   WS-TODAY-TRANSACTION.
              05 WS-T-BRANCH-CODE        PIC 9(3).
              05 WS-T-PRODUCT-CODE       PIC 9(2).
              05 WS-T-BUY-SELL           PIC 9(1).
              05 WS-T-QUANTITY           PIC 9(4).
            01   WS-T-EOF                PIC A(1).

            01   WS-TEMP-TODAY-TRANSACTION.
              05 WS-TEMP-T-BRANCH-CODE   PIC 9(3).
              05 WS-TEMP-T-PRODUCT-CODE  PIC 9(2).
              05 WS-TEMP-T-BUY-SELL      PIC 9(1).
              05 WS-TEMP-T-QUANTITY      PIC 9(4).
            01   RESULT                  PIC 9(4).
            01   TEMP-T-RESULT           PIC 9(4).

      *>   *************************************************************

            77 STRING1 PIC A(12) VALUE "Branch Code".
            77 STRING2 PIC A(16) VALUE "  Product Code".
            77 STRING3 PIC A(12) VALUE "  Quantity".
            77 STRING4 PIC X(40) VALUE SPACES.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *>    For Column Name
            STRING STRING1,STRING2,STRING3 DELIMITED BY SIZE
            INTO STRING4
            END-STRING

            DISPLAY "Branch Code  "  "Product Code  "
                       "Quantity"
              OPEN INPUT OldBalance1.
                PERFORM UNTIL WS-OLD-EOF='Y'
                   READ OldBalance1 INTO WS-OLD-BALANCE
                   AT END
                   DISPLAY WS-OLD-BRANCH-CODE "     D      "
                           WS-OLD-PRODUCT-CODE"            "
                           WS-OLD-QUANTITY
                   MOVE 'Y' TO WS-OLD-EOF
                   NOT AT END

                   IF TEMP-OLD-BRANCH-CODE = ZERO AND
                      TEMP-OLD-PRODUCT-CODE= ZERO THEN
                       MOVE WS-OLD-BRANCH-CODE  TO TEMP-OLD-BRANCH-CODE
                       MOVE WS-OLD-PRODUCT-CODE TO TEMP-OLD-PRODUCT-CODE
                       MOVE WS-OLD-QUANTITY     TO TEMP-OLD-QUANTITY
                   END-IF

                   PERFORM TODAY-PARA

                END-READ
               END-PERFORM.
              CLOSE OldBalance1.
       STOP RUN.

       TODAY-PARA.
            OPEN INPUT TodayTransaction1.
              PERFORM UNTIL WS-T-EOF='Y'
                READ TodayTransaction1  INTO WS-TODAY-TRANSACTION
                   AT END
                   MOVE 'Y' TO WS-T-EOF
                   NOT AT END
      *>                 IF  WS-TEMP-T-BRANCH-CODE  = ZERO AND
      *>                     WS-TEMP-T-PRODUCT-CODE = ZERO AND
      *>                     WS-TEMP-T-BUY-SELL     = ZERO THEN
                       MOVE WS-T-BRANCH-CODE  TO WS-TEMP-T-BRANCH-CODE
                       MOVE WS-T-PRODUCT-CODE TO WS-TEMP-T-PRODUCT-CODE
                       MOVE WS-T-BUY-SELL     TO WS-TEMP-T-BUY-SELL
                       MOVE WS-T-QUANTITY     TO WS-TEMP-T-QUANTITY
      *>                  END-IF

                    IF WS-T-BRANCH-CODE  = WS-TEMP-T-BRANCH-CODE   AND
                       WS-T-PRODUCT-CODE = WS-TEMP-T-PRODUCT-CODE  AND
                       WS-T-BUY-SELL     = WS-TEMP-T-BUY-SELL      THEN
                       ADD WS-T-QUANTITY TO TEMP-T-RESULT
                     ELSE
                        MOVE WS-TODAY-TRANSACTION TO
                             WS-TEMP-TODAY-TRANSACTION
                        MOVE WS-T-QUANTITY   TO TEMP-T-RESULT
                    END-IF

                      IF WS-OLD-BRANCH-CODE = WS-TEMP-T-BRANCH-CODE  AND
                         WS-OLD-PRODUCT-CODE= WS-TEMP-T-PRODUCT-CODE AND
                         WS-T-BUY-SELL=2                           THEN
                           MOVE WS-OLD-QUANTITY TO TEMP-T-RESULT
                           COMPUTE TEMP-T-RESULT=TEMP-T-RESULT +
                                                 WS-TEMP-T-QUANTITY
                           DISPLAY WS-OLD-BRANCH-CODE "     A      "
                                   WS-OLD-PRODUCT-CODE"            "
                                   TEMP-T-RESULT
                       END-IF

                  IF WS-OLD-BRANCH-CODE=WS-TEMP-T-BRANCH-CODE
                  IF NOT WS-OLD-PRODUCT-CODE=WS-TEMP-T-PRODUCT-CODE THEN
                        DISPLAY WS-OLD-BRANCH-CODE    "     B      "
                                WS-TEMP-T-PRODUCT-CODE"            "
                                WS-TEMP-T-QUANTITY
                  END-IF
                  END-IF

                    IF WS-OLD-BRANCH-CODE=WS-TEMP-T-BRANCH-CODE AND
                     WS-OLD-PRODUCT-CODE=WS-TEMP-T-PRODUCT-CODE AND
                     WS-T-BUY-SELL=1 THEN
                     COMPUTE TEMP-T-RESULT=WS-OLD-QUANTITY -
                                         WS-TEMP-T-QUANTITY
                     DISPLAY WS-OLD-BRANCH-CODE "     C      "
                             WS-OLD-PRODUCT-CODE"            "
                             TEMP-T-RESULT
                    END-IF
                END-READ
             END-PERFORM.
             MOVE ' ' TO WS-T-EOF
            CLOSE TodayTransaction1.
