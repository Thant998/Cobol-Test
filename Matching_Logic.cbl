      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM80.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLD-MASTER ASSIGN TO DATA13O.
           SELECT TRANS-FILE ASSIGN TO DATA13T.
           SELECT NEW-MASTER ASSIGN TO DATA13N.
           
       DATA DIVISION.
       FILE SECTION.
       FD OLD-MASTER
       LABEL RECORDS ARE STANDARD.
            01  OLD-MASTER-REC.
             05 M-ACCT-NO  PIC X(5).
             05 AMOUNT-DUE PIC 9(4)V99.
             05            PIC X(89).
             
       FD TRANS-FILE
       LABEL RECORDS ARE STANDARD.
            01  TRANS-REC.
             05 T-ACCT-NO                PIC X(5).
             05 AMT-TRANS-IN-CURRENT-PER PIC 9(4)V99.
             05                          PIC X(89).
             
       FD NEW-MASTER
       LABEL RECORDS ARE STANDARD.
            01   NEW-MASTER-REC.
              05 ACCT-NO-OUT    PIC X(5).
              05 AMOUNT-DUE-OUT PIC 9(4)V99.
              05                PIC X(89).
              
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
             PERFORM 800-INITIALIZATION-RTN.
             PERFORM 600-READ-MASTER.
             PERFORM 700-READ-TRANS.
             PERFORM 200-COMP-RTN
             UNTIL M-ACCT-NO = HIGH-VALUES AND
                   T-ACCT-NO = HIGH-VALUES
             PERFORM 900-END-OF-JOB-RTN.
        STOP RUN.
        
       200-COMP-RTN.
            EVALUATE TRUE
               WHEN T-ACCT-NO = M-ACCT-NO
                 PERFORM 300-REGULAR-UPDATE
               WHEN T-ACCT-NO < M-ACCT-NO
                 PERFORM 400-NEW-ACCOUNT
               WHEN OTHER
                 PERFORM 500-NO-UPDATE
            END-EVALUATE.
                
       300-REGULAR-UPDATE.
            MOVE OLD-MASTER-REC TO NEW-MASTER-REC
            COMPUTE AMOUNT-DUE-OUT = AMT-TRANS-IN-CURRENT-PER
                                   + AMOUNT-DUE
            WRITE NEW-MASTER-REC
            PERFORM 600-READ-MASTER.
            PERFORM 700-READ-TRANS.
            
       400-NEW-ACCOUNT.
            MOVE SPACES TO NEW-MASTER-REC.
            MOVE T-ACCT-NO TO ACCT-NO-OUT.
            MOVE AMT-TRANS-IN-CURRENT-PER TO AMOUNT-DUE-OUT.
            WRITE NEW-MASTER-REC.
            PERFORM 700-READ-TRANS.
            
       500-NO-UPDATE.
            WRITE NEW-MASTER-REC FROM OLD-MASTER-REC.
            PERFORM 600-READ-MASTER.
            
       600-READ-MASTER.
            READ OLD-MASTER
              AT END
              MOVE HIGH-VALUES TO M-ACCT-NO
            END-READ.
                
       700-READ-TRANS.
            READ TRANS-FILE
              AT END
              MOVE HIGH-VALUES TO T-ACCT-NO
            END-READ.
                
       800-INITIALIZATION-RTN.
            OPEN INPUT  OLD-MASTER
                        TRANS-FILE.
            OPEN OUTPUT NEW-MASTER.
            
       900-END-OF-JOB-RTN.
            CLOSE OLD-MASTER
                  TRANS-FILE
                  NEW-MASTER.

