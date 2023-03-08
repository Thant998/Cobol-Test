      ******************************************************************
      * Author: U HIGH
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLRD2.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT OLDBALANCE ASSIGN TO
           'C:\cobolpractice\OldBalance.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TDYTRANSAICTION ASSIGN TO
           'C:\cobolpractice\TodayTransaction.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NEWBALANCE ASSIGN TO
           'C:\cobolpractice\NewBalance.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OLDBALANCE.
       01 OLDBALANCE-FILE.
           05 BRANCH-CODE PIC 9(3).
           05 PRODUCT-CODE PIC 9(2).
           05 QUANTITY PIC 9(4).

       FD TDYTRANSAICTION.
       01 TDYTRANSACTION-FILE.
           05 TDY-BRANCH-CODE PIC 9(3).
           05 TDY-PRODUCT-CODE PIC 9(2).
           05 OPTION PIC 9.
           05 TDY-QTY PIC 9(4).

       FD NEWBALANCE.
       01 NEWBALANCE-FILE.
           05 NEW-BCODE PIC 9(3).
           05 NEW-PCODE PIC 9(2).
           05 NEW-QUANTITY PIC 9(4).

       WORKING-STORAGE SECTION.
       77 STRING1 PIC A(12) VALUE "PRODUCT CODE".
       77 STRING2 PIC A(16) VALUE "    BRANCH CODE".
       77 STRING3 PIC A(12) VALUE "   QUANTITY".
       77 STRING4 PIC X(40) VALUE SPACES.

       01 WS-OLDBALANCE.
           05 WS-BRANCH-CODE PIC 9(3).
           05 WS-PRODUCT-CODE PIC 9(2).
           05 WS-QUANTITY PIC 9(4).
       01 WS-EOF1 PIC A(1).

       01 WS-TDYTRANSACTION.
           05 WS-TDY-BRANCH-CODE PIC 9(3).
           05 WS-TDY-PRODUCT-CODE PIC 9(2).
           05 WS-TDY-OPTION PIC 9.
           05 WS-TDY-QUANTITY PIC 9(4).
       01 WS-EOF2 PIC A(1).

       01 WS-RESULT PIC 9(4).
       01 WS-SUBRESULT PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            STRING STRING1,STRING2,STRING3
            DELIMITED BY SIZE INTO STRING4
            END-STRING
            DISPLAY STRING4.

            PERFORM OPEN-FILE.
            PERFORM READ-FILE1.
            PERFORM READ-FILE2.
            PERFORM PROCESS-PARA.
            PERFORM CLOSE-FILE.
            STOP RUN.

       OPEN-FILE.
           OPEN INPUT OLDBALANCE TDYTRANSAICTION.
           OPEN OUTPUT NEWBALANCE.

       READ-FILE1.
           READ OLDBALANCE INTO WS-OLDBALANCE
               AT END MOVE 'Y' TO WS-EOF1
           END-READ.

       READ-FILE2.
           READ TDYTRANSAICTION INTO WS-TDYTRANSACTION
               AT END MOVE 'Y' TO WS-EOF2
           END-READ.

       PROCESS-PARA.
           IF WS-EOF2 = 'Y'
               PERFORM READ-NEXT-RECS

           ELSE IF WS-BRANCH-CODE = WS-TDY-BRANCH-CODE AND
               WS-PRODUCT-CODE = WS-TDY-PRODUCT-CODE AND
               WS-TDY-OPTION = 2

               ADD WS-TDY-QUANTITY TO WS-QUANTITY GIVING WS-RESULT
               DISPLAY
               WS-TDY-BRANCH-CODE'             '
               WS-TDY-PRODUCT-CODE'             '
               WS-RESULT'             '

               MOVE WS-TDY-BRANCH-CODE TO NEW-BCODE
               MOVE WS-TDY-PRODUCT-CODE TO NEW-PCODE
               MOVE WS-RESULT TO NEW-QUANTITY
               WRITE NEWBALANCE-FILE
               END-WRITE

               PERFORM READ-FILE2
               PERFORM PROCESS-PARA

           ELSE IF WS-BRANCH-CODE = WS-TDY-BRANCH-CODE AND
               WS-PRODUCT-CODE NOT = WS-TDY-PRODUCT-CODE AND
               WS-TDY-OPTION = 2

               DISPLAY
               WS-TDY-BRANCH-CODE'             '
               WS-TDY-PRODUCT-CODE'             '
               WS-TDY-QUANTITY'             '

               MOVE WS-TDY-BRANCH-CODE TO NEW-BCODE
               MOVE WS-TDY-PRODUCT-CODE TO NEW-PCODE
               MOVE WS-TDY-QUANTITY TO NEW-QUANTITY
               WRITE NEWBALANCE-FILE
               END-WRITE

               PERFORM READ-FILE2
               PERFORM PROCESS-PARA


           ELSE IF WS-BRANCH-CODE = WS-TDY-BRANCH-CODE AND
               WS-PRODUCT-CODE = WS-TDY-PRODUCT-CODE AND
               WS-TDY-OPTION = 1

               SUBTRACT WS-TDY-QUANTITY FROM WS-QUANTITY
               GIVING WS-SUBRESULT

               DISPLAY
               WS-TDY-BRANCH-CODE'             '
               WS-TDY-PRODUCT-CODE'             '
               WS-SUBRESULT'             '

               MOVE WS-TDY-BRANCH-CODE TO NEW-BCODE
               MOVE WS-TDY-PRODUCT-CODE TO NEW-PCODE
               MOVE WS-SUBRESULT TO NEW-QUANTITY
               WRITE NEWBALANCE-FILE
               END-WRITE

               PERFORM READ-FILE2
               PERFORM PROCESS-PARA
           ELSE
               PERFORM READ-FILE1
               PERFORM PROCESS-PARA
           END-IF.

       READ-NEXT-RECS.
            PERFORM UNTIL WS-EOF1 = 'Y'
               READ OLDBALANCE INTO WS-OLDBALANCE
               AT END MOVE 'Y' TO WS-EOF1
               NOT AT END
               DISPLAY
                   WS-BRANCH-CODE'             '
                   WS-PRODUCT-CODE'             '
                   WS-QUANTITY'             '

               MOVE WS-BRANCH-CODE TO NEW-BCODE
               MOVE WS-PRODUCT-CODE TO NEW-PCODE
               MOVE WS-QUANTITY TO NEW-QUANTITY
               WRITE NEWBALANCE-FILE
               END-WRITE

            END-PERFORM.

       CLOSE-FILE.
           CLOSE OLDBALANCE TDYTRANSAICTION NEWBALANCE.
       END PROGRAM COBOLRD2.
