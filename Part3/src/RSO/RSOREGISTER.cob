      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGISTER ORDERS | V0.2 | IN UPDATE | 05.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSOREGISTER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY RSOSELECTS.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDERS.
       COPY RSOFD.

       FD  ORDERSKEYS.
       01  FDORDERSKEYS                               PIC 9(005).

       FD  CALENDAR.
       COPY FDCALENDAR.

       FD  SCHOOLS.
       COPY CB-SCHOOLS.

       FD SANDWICHES.
       COPY CB-FD-SR.

       WORKING-STORAGE SECTION.
       COPY RSOWS.
       COPY RSOCONTANTS.
       COPY RSOWSVAR.
       COPY VAR-VALIDDATE.
       COPY RSOTABLES.

      ******************************************************************

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

      ******************************************************************

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 45.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(022) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 26 COL 98.
           05 VALUE MAIN-TEXT1 LINE 25 COL 100 FOREGROUND-COLOR 5.

      ******************************************************************

       01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(082) LINE 10 COL 08.
           05 VALUE ALL " " PIC X(082) LINE 07 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 88 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 31.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 11.
           05 VALUE REGISTER-TEXT-DELIVERY-DATE LINE 15 COL 11.
           05 VALUE REGISTER-TEXT-SCHOOL        LINE 16 COL 11.
           05 VALUE REGISTER-TEXT-SANDWICH      LINE 17 COL 11.
           05 VALUE REGISTER-TEXT-QUANTITY      LINE 18 COL 11.
           05 VALUE REGISTER-TEXT-ORDER-DATE    LINE 19 COL 11.
           05 REG-ID PIC 9(005) LINE 13 COL 35 USING WS-ORDERS-ID.
           05 REG-REC.
              10 REG-DELIVERY-DATE.
                 15 REG-DELIVERY-DAY PIC X(002) LINE 15 COL 35 TO
                    WS-DELIVERY-DAY AUTO REQUIRED.
                 15 LINE 15 COL 37 VALUE "/".
                 15 REG-DELIVERY-MONTH PIC X(002) LINE 15 COL 38 TO
                    WS-DELIVERY-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 40 VALUE "/".
                 15 REG-DELIVERY-YEAR PIC X(004) LINE 15 COL 41 TO
                    WS-DELIVERY-YEAR AUTO REQUIRED.
              10 REG-DELIVERY-TIME.
                 15 LINE 15 COL 46 VALUE "|".
                 15 REG-DELIVERY-HOUR PIC X(002) LINE 15 COL 48 TO
                    WS-DELIVERY-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 50 VALUE ":".
                 15 REG-DELIVERY-MINUTE PIC X(002) LINE 15 COL 51 TO
                    WS-DELIVERY-MINUTE AUTO REQUIRED.
              10 REG-SCHOOL PIC 9(003) LINE 16 COL 35
                 TO WS-ORDERS-SCHOOL-INTERNAL-ID AUTO REQUIRED.
              10 REG-SANDWICH PIC 9(003) LINE 17 COL 35
                 TO WS-ORDERS-SANDWICH-INTERNAL-ID AUTO REQUIRED.
              10 REG-QUANTITY PIC 9(003) LINE 18 COL 35
                 TO WS-ORDERS-QUANTITY AUTO REQUIRED.
              10 REG-ORDERS-DATE.
                 15 REG-ORDERS-DAY PIC 9(002) LINE 19 COL 35 FROM
                    WS-ORDERS-DAY AUTO REQUIRED.
                 15 LINE 19 COL 37 VALUE "/".
                 15 REG-ORDERS-MONTH PIC 9(002) LINE 19 COL 38 FROM
                    WS-ORDERS-MONTH AUTO REQUIRED.
                 15 LINE 19 COL 40 VALUE "/".
                 15 REG-ORDERS-YEAR PIC 9(004) LINE 19 COL 41 FROM
                    WS-ORDERS-YEAR AUTO REQUIRED.

      ******************************************************************

       01  COMMENTS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  INSTRUCTIONS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

      ******************************************************************

       01  SAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SS-SAVE PIC X(002) LINE 25 COL 61
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE SPACES TO FLAG-TRUE, CALENDAR-EXIST
      *>      PERFORM CHECK-SCHOOL-SANDIWICH-FILE
      *>      IF FLAG-TRUE = "N" THEN
      *>         EXIT PROGRAM
      *>      END-IF

           PERFORM CREATE-FILE

           PERFORM LOAD-ALL-TABLES

           PERFORM REGISTER-DOWNTIME
              IF KEYSTATUS = F3 THEN
                 EXIT PROGRAM
              END-IF
           EXIT PROGRAM.

      ******************************************************************

       REGISTER-DOWNTIME SECTION.
           PERFORM GET-ORDER-ID

           ACCEPT WS-ORDERS-DATE FROM DATE YYYYMMDD

           OPEN I-O ORDERS
           MOVE "DD"   TO REG-DELIVERY-DAY
           MOVE "MM"   TO REG-DELIVERY-MONTH
           MOVE "YYYY" TO REG-DELIVERY-YEAR
           MOVE "HH"   TO REG-DELIVERY-HOUR
           MOVE "MM"   TO REG-DELIVERY-MINUTE
           MOVE ZEROS  TO REG-SCHOOL, REG-SANDWICH, REG-QUANTITY

           MOVE FDORDERSKEYS TO WS-ORDERS-ID

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM GET-DELIVERY-DATE
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-SCHOOL-ID
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-SANDWICH-ID
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-QUANTITY
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM WITH TEST AFTER UNTIL SAVE-VALID
              ACCEPT SAVE-SCREEN
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

              IF NOT SAVE-VALID THEN
                 MOVE INVALID-OPTION TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    MOVE SPACES TO SS-SAVE
                    CLOSE ORDERSKEYS
                    CLOSE ORDERS
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

           IF SAVE = "Y" OR "y"
              REWRITE FDORDERSKEYS
              END-REWRITE
              CLOSE ORDERSKEYS
              WRITE FD-ORDERS FROM WS-ORDERS
              END-WRITE
              CLOSE ORDERS
              MOVE MESSAGE-WRITE-YES TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 MOVE SPACES TO SS-SAVE
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF
           ELSE
              IF SAVE = "N" OR "n"
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 MOVE MESSAGE-WRITE-NO TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    MOVE SPACES TO SS-SAVE
                    EXIT SECTION
                 END-IF
              END-IF
           END-IF

           MOVE SPACES TO SS-SAVE
           EXIT SECTION.

      ******************************************************************

       GET-ORDER-ID SECTION.
           OPEN I-O ORDERSKEYS
              READ ORDERSKEYS
                 ADD 1 TO FDORDERSKEYS
           EXIT SECTION.

      ******************************************************************

       GET-DELIVERY-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
           AND FLAG-TRUE = "Y" AND FLAG-CALENDAR = "Y"

              MOVE SPACES TO DATE-VALID, FLAG-TRUE, FLAG-CALENDAR
              MOVE "DD"   TO REG-DELIVERY-DAY
              MOVE "MM"   TO REG-DELIVERY-MONTH
              MOVE "YYYY" TO REG-DELIVERY-YEAR
              MOVE "HH"   TO REG-DELIVERY-HOUR
              MOVE "MM"   TO REG-DELIVERY-MINUTE
              DISPLAY REG-DELIVERY-DATE, REG-DELIVERY-TIME

              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-DELIVERY-DAY
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-MONTH
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-YEAR
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-DELIVERY-DATE TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO WS-DELIVERY-DATE

              IF DATE-VALID = "Y" THEN
                 PERFORM CHECK-BEFORE-3DAYS
              END-IF

              IF DATE-VALID = "Y" AND FLAG-TRUE = "Y" THEN
                 PERFORM DELIVERY-TIME
              END-IF

              IF CALENDAR-EXIST NOT = "N" AND DATE-VALID = "Y"
              AND FLAG-TRUE = "Y" THEN
                 PERFORM CHECK-UNAVAILABILITY
              END-IF

              IF CALENDAR-EXIST = "N" THEN
                 MOVE "Y" TO FLAG-CALENDAR
              END-IF

           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       DELIVERY-TIME SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-DELIVERY-HOUR
           AND VALID-DELIVERY-MINUTE
           AND REG-DELIVERY-HOUR IS NOT EQUALS "HH"
           AND REG-DELIVERY-MINUTE IS NOT EQUALS "MM"

              MOVE "HH"   TO REG-DELIVERY-HOUR
              MOVE "MM"   TO REG-DELIVERY-MINUTE
              DISPLAY REG-DELIVERY-TIME

              MOVE INSTRUCTIONS-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-DELIVERY-HOUR
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-MINUTE
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-DELIVERY-HOUR OR NOT VALID-DELIVERY-MINUTE
              OR REG-DELIVERY-HOUR = "HH"
              OR REG-DELIVERY-MINUTE = "MM" THEN
                 MOVE INVALID-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-SCHOOL-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-SCHOOL-INTERNAL-ID
           NOT EQUALS ALL ZEROS

              MOVE ZEROS TO REG-SCHOOL
              DISPLAY REG-SCHOOL

              MOVE INSTRUCTIONS-SCHOOL TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-SCHOOL
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF WS-ORDERS-SCHOOL-INTERNAL-ID EQUALS ALL ZEROS THEN
                 MOVE INVALID-SCHOOL TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

       EXIT SECTION.

      ******************************************************************

       GET-SANDWICH-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-SANDWICH-INTERNAL-ID
           NOT EQUALS ALL ZEROS

              MOVE ZEROS TO REG-SANDWICH
              DISPLAY REG-SANDWICH

              MOVE INSTRUCTIONS-SANDWICH TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-SANDWICH
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF WS-ORDERS-SANDWICH-INTERNAL-ID EQUALS ALL ZEROS THEN
                 MOVE INVALID-SANDWICH TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

       EXIT SECTION.

      ******************************************************************

       GET-QUANTITY SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-QUANTITY
           NOT EQUALS ALL ZEROS

              MOVE ZEROS TO REG-QUANTITY
              DISPLAY REG-QUANTITY

              MOVE INSTRUCTIONS-QUANTITY TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-QUANTITY
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF WS-ORDERS-QUANTITY EQUALS ALL ZEROS THEN
                 MOVE INVALID-QUANTITY TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

       EXIT SECTION.

      ******************************************************************

       CHECK-SCHOOL-SANDIWICH-FILE SECTION.
           OPEN INPUT SCHOOLS
           IF SCHOOL-FS = 35 THEN
              MOVE SCHOOLS-INEXISTENT TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              MOVE "N" TO FLAG-TRUE
              CLOSE SCHOOLS
              EXIT SECTION
           ELSE
              MOVE 001 TO SCHOOL-INTERNAL-ID
              START SCHOOLS KEY IS GREATER OR EQUAL SCHOOL-INTERNAL-ID
                 INVALID KEY
                    MOVE SCHOOLS-INEXISTENT TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    MOVE "N" TO FLAG-TRUE
                    CLOSE SCHOOLS
                    EXIT SECTION
              END-START
           END-IF
           CLOSE SCHOOLS

           OPEN INPUT SANDWICHES
           IF SANDWICH-FS = 35 THEN
              MOVE SANDWICH-INEXISTENT TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              MOVE "N" TO FLAG-TRUE
              CLOSE SANDWICHES
              EXIT SECTION
           ELSE
              MOVE 001 TO SR-IID
              START SANDWICHES KEY IS GREATER OR EQUAL SR-IID
                 INVALID KEY
                    MOVE SANDWICH-INEXISTENT TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    MOVE "N" TO FLAG-TRUE
                    CLOSE SANDWICHES
                    EXIT SECTION
              END-START
           END-IF
           CLOSE SANDWICHES
           EXIT SECTION.

      ******************************************************************

       CREATE-FILE SECTION.
           OPEN I-O ORDERS
           IF ORDERS-FS = "35"
              OPEN OUTPUT ORDERS
              CLOSE ORDERS
           ELSE
              CLOSE ORDERS
           END-IF

           OPEN I-O ORDERSKEYS
           IF ORDERSKEYS-FS = "35"
              OPEN OUTPUT ORDERSKEYS
                 MOVE 0 TO FDORDERSKEYS
                 WRITE FDORDERSKEYS
                 END-WRITE
              CLOSE ORDERSKEYS
           ELSE
              CLOSE ORDERSKEYS
           END-IF

           OPEN INPUT CALENDAR
           IF CALENDAR-FS = "35"
              MOVE "N" TO CALENDAR-EXIST
           END-IF
           CLOSE CALENDAR
           EXIT SECTION.

      ******************************************************************

       LOAD-ALL-TABLES SECTION.
           IF CALENDAR-EXIST NOT = "N" THEN
              PERFORM FILL-TABLES
              IF CALENDAR-EXIST NOT = "N" THEN
                 PERFORM SORT-ASCENDING
                 PERFORM AGG-TABLE
              END-IF
           END-IF

           EXIT SECTION.

      ******************************************************************

       FILL-TABLES SECTION.
           OPEN INPUT CALENDAR
           MOVE 001 TO FD-DOWNTIME-ID
           START CALENDAR KEY IS GREATER OR EQUAL FD-DOWNTIME-ID
              INVALID KEY
                 MOVE "N" TO CALENDAR-EXIST
                 EXIT SECTION
           END-START

           SET IND-CAL TO 0
           PERFORM UNTIL EOF-DOWNTIME-ID
              READ CALENDAR
                 AT END
                    SET EOF-DOWNTIME-ID TO TRUE
                    MOVE IND-CAL TO MAX-CAL1
                 NOT AT END
                    SET IND-CAL UP BY 1
                    PERFORM LOAD-TABLE
              END-READ
           END-PERFORM
           CLOSE CALENDAR
           EXIT SECTION.

       LOAD-TABLE SECTION.
           STRING FD-START-DOWNTIME FD-START-TIME INTO
           TAB-BEGIN (IND-CAL)
           IF FD-END-DOWNTIME = ZERO THEN
              MOVE "999999999999" TO TAB-END (IND-CAL)
           ELSE
              STRING FD-END-DOWNTIME FD-END-TIME INTO
              TAB-END (IND-CAL)
           END-IF
           EXIT SECTION.

       SORT-ASCENDING SECTION.
           SORT TAB-CAL
           ON ASCENDING TAB-BEGIN
           ON ASCENDING TAB-END
           DUPLICATES
           EXIT SECTION.

       AGG-TABLE SECTION.
           MOVE TAB-CAL (1) TO TAB-AGG (1)
           SET IND-CAL TO 2
           SET IND-AGG TO 1
           PERFORM WITH TEST AFTER UNTIL IND-CAL > MAX-CAL1
              IF TAB-BEGIN (IND-CAL) <= AGG-END (IND-AGG) THEN
                 IF TAB-END (IND-CAL) > AGG-END (IND-AGG) THEN
                    MOVE TAB-END (IND-CAL) TO AGG-END (IND-AGG)
                 END-IF
              ELSE
                 SET IND-AGG UP BY 1
                 MOVE TAB-BEGIN (IND-CAL) TO AGG-BEGIN (IND-AGG)
                 MOVE TAB-END (IND-CAL) TO AGG-END (IND-AGG)
              END-IF
              SET IND-CAL UP BY 1
           END-PERFORM
           MOVE IND-AGG TO MAX-AGG
           EXIT SECTION.

      ******************************************************************

       CHECK-DATE SECTION.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           IF WS-CURRENT-DATE <= WS-VALID-DATE THEN
              IF VALID-YEAR AND VALID-MONTH AND VALID-DAY THEN
                 IF WS-YEAR >= WS-CURRENT-YEAR AND WS-MONTH >=
                 WS-CURRENT-MONTH THEN
                    IF NOT MONTH-FEB AND NOT MONTH-30 THEN
                       MOVE "Y" TO DATE-VALID
                    ELSE
                       IF MONTH-30 AND DAY-30 THEN
                          MOVE "Y" TO DATE-VALID
                       END-IF
                       IF MONTH-FEB THEN
                          PERFORM LEAP-YEAR-CHECK
                          IF LEAP-YEAR-YES AND FEB-LEAP-YEAR THEN
                             MOVE "Y" TO DATE-VALID
                          ELSE
                             IF NOT LEAP-YEAR-YES AND DAY-FEBRUARY THEN
                                MOVE "Y" TO DATE-VALID
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF DATE-VALID NOT = "Y" THEN
              MOVE INVALID-DATE1 TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       LEAP-YEAR-CHECK SECTION.
           MOVE SPACE TO LEAP-YEAR
           IF FUNCTION MOD (WS-YEAR,4) = 0 THEN
              IF FUNCTION MOD (WS-YEAR,100) <> 0 THEN
                 MOVE "Y" TO LEAP-YEAR
              ELSE
                 IF FUNCTION MOD (WS-YEAR,400) = 0 THEN
                    MOVE "Y" TO LEAP-YEAR
                 END-IF
               END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       CHECK-BEFORE-3DAYS SECTION.
           MOVE WS-DELIVERY-DATE TO TEST2
           MOVE WS-ORDERS-DATE TO TEST3

           SUBTRACT TEST2 FROM TEST3 GIVING TEST1

           IF TEST1 < "00000003" THEN
              MOVE INVALID-DATE2 TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           ELSE
              MOVE "Y" TO FLAG-TRUE
           END-IF
           EXIT SECTION.

      ******************************************************************
       CHECK-UNAVAILABILITY SECTION.
           SET IND-AGG TO 1

           PERFORM UNTIL IND-AGG > MAX-AGG
              IF WS-DELIVERY-DATE-TIME < AGG-BEGIN (IND-AGG) THEN
                 MOVE "Y" TO FLAG-CALENDAR
                 MOVE MAX-AGG TO IND-AGG
              ELSE
                 IF WS-DELIVERY-DATE-TIME <= AGG-END (IND-AGG)
                 THEN
                    MOVE "N" TO FLAG-CALENDAR
                    MOVE INVALID-DATE3 TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    MOVE MAX-AGG TO IND-AGG
                 END-IF
              END-IF
              SET IND-AGG UP BY 1
           END-PERFORM

           IF FLAG-CALENDAR = SPACE THEN
              MOVE "Y" TO FLAG-CALENDAR
           END-IF
           EXIT SECTION.

      ******************************************************************

       END PROGRAM RSOREGISTER.
