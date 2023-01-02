IDENTIFICATION DIVISION.
PROGRAM-ID.PGM77.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
      SELECT IN-EMPLOYEE-FILE ASSIGN TO EMPDAT1
      ORGANIZATION IS SEQUENTIAL.
      SELECT OUT-REPORT-FILE ASSIGN TO PRINTER1.
      ORGANIZATION IS SEQUENTIAL.
      
DATA DIVISION.
FILE SECTION.
      FD IN-EMPLOYEE-FILE
      LABEL RECORDS ARE STANDARD.
          01     IN-EMPLOYEE-REC.
              05 IN-EMPLOYEE-NO         PIC X(3).
              05 IN-EMPLOYEE-LAST-NAME  PIC X(20).
              05 IN-EMPLOYEE-FIRST-NAME PIC X(10).
              05 IN-RANK                PIC XX.
              05 IN-SALARY              PIC 9(6)V99.
      
      FD OUT-REPORT-FILE
      LABEL RECORDS ARE OMITTED.
          01 OUT-REPORT-REC PIC X(132).
      
WORKING-STORAGE SECTION.
          01 WS-WORK-AREAS.
              05 ARE-THERE-MORE-RECORDS PIC X(3) VALUE 'YES'.
              05 WS-PROFESSOR-CTR       PIC 9(3) VALUE ZEROS.
              05 WS-ASSOCIATE-CTR PIC 9(3) VALUE ZEROS.
              05 WS-ASSISTANT-CTR PIC 9(3) VALUE ZEROS.
              05 WS-INSTRUCTOR-CTR PIC 9(3) VALUE ZEROS.
              05 WS-PROFESSOR-COST PIC 9(7)V99 VALUE ZEROS.
              05 WS-ASSOCIATE-COST PIC 9(7)V99 VALUE ZEROS.
              05 WS-ASSISTANT-COST PIC 9(7)V99 VALUE ZEROS.
              05 WS-INSTRUCTOR-COST PIC 9(7)V99 VALUE ZEROS.
              05 WS-TOTAL-COST PIC 9(9)V99 VALUE ZEROS.
              05 NEW-SAL PIC 9(7)V99 VALUE ZEROS.
          01 HL-HEADER-1.
              05 PIC X(49) VALUE SPACES.
              05 PIC X(25) VALUE 'UNIVERSITY PAYROLL REPORT'.
              05 PIC X(58) VALUE SPACES.
          01 HL-HEADER-2.
               05 PIC X(24) VALUE SPACES.
               05 PIC X(30) VALUE 'RANK'.
               05 PIC X(20) VALUE 'NO OF EMPLOYEES'.
               05 PIC X(25) VALUE 'COST OF PROPOSED INCREASE'.
               05 PIC X(33) VALUE SPACES.
          01 TL-TOTAL-LINE.
               05 PIC X(24) VALUE SPACES.
               05 TL-RANK PIC X(10).
               05 PIC X(26) VALUE SPACES.
               05 TL-NO-OF-EMPLOYEES PIC 9(3).
               05 PIC X(16) VALUE SPACES.
               05 TL-COST PIC $Z,ZZZ,ZZ9.99.
               05 PIC X(41) VALUE SPACES.
          01 TL-FINAL-TOTAL-LINE.
               05 PIC X(32) VALUE SPACES.
               05 PIC X(46) VALUE 'TOTAL UNIVERSITY BUGGEST WILL BE INCREASED BY'.
               05 TL-TOTAL-COST PIC $Z,ZZZ,ZZ9.99.
               05 PIC X(39) VALUE SPACES.
PROCEDURE DIVISION.
100-MAIN-MODULE.
       OPEN INPUT IN-EMPLOYEE-FILE
       OUTPUT OUT-REPORT-FILE.
       PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO'
       READ IN-EMPLOYEE-FILE
       AT END
       MOVE 'NO' TO ARE-THERE-MORE-RECORDS
      NOT AT END
      PERFORM 200-CALC-RTN
      END-READ
END-PERFORM.
PERFORM 300-FINAL-RTN
CLOSE IN-EMPLOYEE-FILE
OUT-REPORT-FILE.
STOP RUN.
200-CALC-RTN.
IF IN-RANK = 'FP'
MULTIPLY IN-SALARY BY .062 GIVING NEW-SAL
ADD NEW-SAL TO WS-PROFESSOR-COST
ADD 1 TO WS-PROFESSOR-CTR
END-IF.
IF IN-RANK = 'AS'
MULTIPLY IN-SALARY BY .081 GIVING NEW-SAL
ADD NEW-SAL TO WS-ASSOCIATE-COST
ADD 1 TO WS-ASSOCIATE-CTR
END-IF.
IF IN-RANK = 'AP'
MULTIPLY IN-SALARY BY .083 GIVING NEW-SAL
ADD NEW-SAL TO WS-ASSISTANT-COST
ADD 1 TO WS-ASSISTANT-CTR
END-IF.
IF IN-RANK = 'AP'
MULTIPLY IN-SALARY BY .102 GIVING NEW-SAL
ADD NEW-SAL TO WS-INSTRUCTOR-COST
ADD 1 TO WS-INSTRUCTOR-CTR
END-IF.
300-FINAL-RTN.
WRITE OUT-REPORT-REC FROM HL-HEADER-1
AFTER ADVANCING PAGE.
WRITE OUT-REPORT-REC FROM HL-HEADER-2
AFTER ADVANCING 5 LINES.
MOVE 'FULL' TO TL-RANK.
MOVE WS-PROFESSOR-CTR TO TL-NO-OF-EMPLOYEES.
MOVE WS-PROFESSOR-COST TO TL-COST.
WRITE OUT-REPORT-REC FROM TL-TOTAL-LINE
AFTER ADVANCING 1 LINE
MOVE 'ASSOCIATE' TO TL-RANK.
MOVE WS-ASSOCIATE-CTR TO TL-NO-OF-EMPLOYEES .
MOVE WS-ASSOCIATE-COST TO TL-COST .
WRITE OUT-REPORT-REC FROM TL-TOTAL-LINE
AFTER ADVANCING 1 LINE
MOVE 'ASSISTANT' TO TL-RANK.
MOVE WS-ASSISTANT-CTR TO TL-NO-OF-EMPLOYEES .
MOVE WS-ASSISTANT-COST TO TL-COST .
WRITE OUT-REPORT-REC FROM TL-TOTAL-LINE
AFTER ADVANCING 1 LINE
MOVE 'INSTRUCTOR' TO TL-RANK.
MOVE WS-INSTRUCTOR-CTR TO TL-NO-OF-EMPLOYEES .
MOVE WS-INSTRUCTOR-COST TO TL-COST .
WRITE OUT-REPORT-REC FROM TL-TOTAL-LINE
AFTER ADVANCING 1 LINE
ADD WS-PROFESSOR-COST , WS-ASSOCIATE-COST,
WS-ASSISTANT-COST , WS-INSTRUCTOR-COST
GIVING WS-TOTAL-COST.
MOVE WS-TOTAL-COST TO TL-TOTAL-COST.
WRITE OUT-REPORT-REC FROM TL-TOTAL-LINE
AFTER ADVANCING 5 LINES.
