      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    SEARCH ORDERS | V0.1 | IN UPDATE | 08.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSOSEARCH.

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

       FD  SANDWICHES.
       COPY CB-FD-SR.

       WORKING-STORAGE SECTION.
       COPY RSOWS.
       COPY RSOWSVAR.
       COPY RSOTABLES.
       COPY RSOCONSTANTS.

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

       01  ACCEPT-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 REG-REC.
              10 REG-DELIVERY-DATE.
                 15 REG-DELIVERY-DAY PIC X(002) LINE 15 COL 26 TO
                    WS-DELIVERY-DAY AUTO REQUIRED.
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-DELIVERY-MONTH PIC X(002) LINE 15 COL 29 TO
                    WS-DELIVERY-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-DELIVERY-YEAR PIC X(004) LINE 15 COL 32 TO
                    WS-DELIVERY-YEAR AUTO REQUIRED.
              10 REG-SCHOOL PIC 9(003) LINE 16 COL 26
                 TO WS-ORDERS-SCHOOL-INTERNAL-ID AUTO REQUIRED.
              10 REG-SANDWICH PIC 9(003) LINE 17 COL 26
                 TO WS-ORDERS-SANDWICH-INTERNAL-ID AUTO REQUIRED.

      ******************************************************************

       01  SHOW-REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(088) LINE 10 COL 03.
           05 VALUE ALL " " PIC X(090) LINE 07 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(090) LINE 22 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 91 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 33.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 05.
           05 VALUE REGISTER-TEXT-DELIVERY-DATE LINE 15 COL 05.
           05 VALUE REGISTER-TEXT-SCHOOL        LINE 16 COL 05.
           05 VALUE REGISTER-TEXT-SANDWICH      LINE 17 COL 05.
           05 VALUE REGISTER-TEXT-QUANTITY      LINE 18 COL 05.
           05 VALUE REGISTER-TEXT-ORDER-DATE    LINE 19 COL 05.
           05 REG-ID2 PIC 9(005) LINE 13 COL 26 FROM
              TAB-ORDERS-ID (IND-ORDERS).
           05 REG-REC2.
              10 REG-DELIVERY-DATE2.
                 15 REG-DELIVERY-DAY2 PIC X(002) LINE 15 COL 26 FROM
                    TAB-DELIVERY-DAY (IND-ORDERS).
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-DELIVERY-MONTH2 PIC X(002) LINE 15 COL 29 FROM
                    TAB-DELIVERY-MONTH (IND-ORDERS).
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-DELIVERY-YEAR2 PIC X(004) LINE 15 COL 32 FROM
                    TAB-DELIVERY-YEAR (IND-ORDERS).
              10 REG-DELIVERY-TIME2.
                 15 LINE 15 COL 37 VALUE "|".
                 15 REG-DELIVERY-HOUR2 PIC X(002) LINE 15 COL 39 FROM
                    TAB-DELIVERY-HOUR (IND-ORDERS).
                 15 LINE 15 COL 41 VALUE ":".
                 15 REG-DELIVERY-MINUTE2 PIC X(002) LINE 15 COL 42 FROM
                    TAB-DELIVERY-MINUTE (IND-ORDERS).
              10 REG-SCHOOL2 PIC 9(003) LINE 16 COL 26
                 FROM TAB-ORDERS-SCHOOL-INTERNAL-ID (IND-ORDERS).
              10 VALUE "|" LINE 16 COL 30.
              10 SHOW-SCHOOL-NAME2 PIC X(050) LINE 16 COL 32
                 FROM TAB-SCHOOL-DESIGNATION (IND-SCHOOL).
              10 REG-SANDWICH2 PIC 9(003) LINE 17 COL 26
                 FROM TAB-ORDERS-SANDWICH-INTERNAL-ID (IND-ORDERS).
              10 VALUE "|" LINE 17 COL 30.
              10 SHOW-SANDWICH-NAME2 PIC X(025) LINE 17 COL 32
                 FROM TAB-SR-S-DESCRIPTION (IND-SANDWICH).
              10 REG-QUANTITY2 PIC 9(003) LINE 18 COL 26
                 FROM TAB-ORDERS-QUANTITY (IND-ORDERS).
              10 REG-ORDERS-DATE2.
                 15 REG-ORDERS-DAY2 PIC 9(002) LINE 19 COL 26 FROM
                    TAB-ORDERS-DAY (IND-ORDERS).
                 15 LINE 19 COL 28 VALUE "/".
                 15 REG-ORDERS-MONTH2 PIC 9(002) LINE 19 COL 29 FROM
                    TAB-ORDERS-MONTH (IND-ORDERS).
                 15 LINE 19 COL 31 VALUE "/".
                 15 REG-ORDERS-YEAR2 PIC 9(004) LINE 19 COL 32 FROM
                    TAB-ORDERS-YEAR (IND-ORDERS).
           05 VALUE VIEW-ORDERS-ONEBYONE LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(064) LINE 07 COL 54
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(064) LINE 22 COL 54
              BACKGROUND-COLOR 7.
           05 TEXT0 PIC X(057)    LINE 08 COL 58 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(064) LINE 09 COL 54.
           05 VALUE ALL "Ä" PIC X(064) LINE 20 COL 54.
           05 TEXT1 PIC X(020)    LINE 21 COL 58 FOREGROUND-COLOR 5.
           05 TEXT2 PIC X(019)    LINE 21 COL 97 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 22 COL 118 BACKGROUND-COLOR 7.

      ******************************************************************

       01  CALENDAR-LIST LINE ILIN COL ICOL.
           05 LIST-BEGIN-DAY   PIC X(002) FROM AGG-BEGIN-DAY (IND-AGG).
           05 VALUE "/".
           05 LIST-BEGIN-MONTH PIC X(002)
              FROM AGG-BEGIN-MONTH (IND-AGG).
           05 VALUE "/".
           05 LIST-BEGIN-YEAR PIC X(004)  FROM AGG-BEGIN-YEAR (IND-AGG).
           05 VALUE " - ".
           05 LIST-BEGIN-HOUR PIC X(002)  FROM AGG-BEGIN-HOUR (IND-AGG).
           05 VALUE ":".
           05 LIST-BEGIN-MIN PIC X(002)   FROM AGG-BEGIN-MIN (IND-AGG).
           05 VALUE UNTIL-LIST.
           05 LIST-END-DAY   PIC X(002)   FROM AGG-END-DAY (IND-AGG).
           05 VALUE "/".
           05 LIST-END-MONTH PIC X(002)   FROM AGG-END-MONTH (IND-AGG).
           05 VALUE "/".
           05 LIST-END-YEAR PIC X(004)    FROM AGG-END-YEAR (IND-AGG).
           05 VALUE " - ".
           05 LIST-END-HOUR PIC X(002)    FROM AGG-END-HOUR (IND-AGG).
           05 VALUE ":".
           05 LIST-END-MIN PIC X(002)     FROM AGG-END-MIN (IND-AGG).

      ******************************************************************

       01  SCHOOL-LIST.
           05 LIST-SCHOOL-ID PIC 9(003)   LINE ILIN COL ICOL
              FROM TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL).
           05 VALUE "|"                   LINE ILIN COL PLUS 2.
           05 LIST-SCHOOL-NAME PIC X(050) LINE ILIN COL PLUS 2
              FROM TAB-SCHOOL-DESIGNATION (IND-SCHOOL).

      ******************************************************************

       01  SANDWICH-LIST.
           05 LIST-SANDWICH-ID PIC 9(003)   LINE ILIN COL ICOL
              FROM TAB-SR-IID (IND-SANDWICH).
           05 VALUE "|"                     LINE ILIN COL PLUS 2.
           05 LIST-SANDWICH-NAME PIC X(025) LINE ILIN COL PLUS 2
              FROM TAB-SR-S-DESCRIPTION (IND-SANDWICH).

      ******************************************************************

       01  CLEAR-LIST.
           05 VALUE ALL " " PIC X(060) LINE 10 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 11 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 12 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 13 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 14 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 15 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 16 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 17 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 18 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 19 COL 56.

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
           MOVE SPACES TO FLAG-TRUE, CALENDAR-EXISTS
           PERFORM CHECK-SCHOOL-SANDWICH-FILE
           IF FLAG-TRUE = "N" THEN
              EXIT PROGRAM
           END-IF

           PERFORM CREATE-FILE

           PERFORM LOAD-ALL-TABLES

      *    FALTA MENU
           PERFORM SEARCH-SCHOOL
           PERFORM SEARCH-SANDWICH
           PERFORM SEARCH-SCHOOL-SANDWICH
           EXIT PROGRAM.

      ******************************************************************

       SEARCH-SCHOOL SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN

           MOVE ZEROS TO COUNTER

           PERFORM WITH TEST AFTER UNTIL SCHOOL-EXISTS = "Y"
              PERFORM LIST-SCHOOLS
              PERFORM CHECK-SCHOOL-EXISTS
           END-PERFORM

           SET IND-ORDERS TO 1
           PERFORM UNTIL IND-ORDERS > MAX-ORDERS
              IF REG-SCHOOL = TAB-ORDERS-SCHOOL-INTERNAL-ID (IND-ORDERS)
              THEN
                 DISPLAY CLEAR-SCREEN
                 DISPLAY MAIN-SCREEN
                 PERFORM GET-SANDWICH-NAME
                 ACCEPT SHOW-REGISTER-SCREEN
              END-IF
              SET IND-ORDERS UP BY 1
           END-PERFORM

           IF COUNTER = 0 THEN
              MOVE ERROR-SEARCH TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       SEARCH-SANDWICH SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN

           MOVE ZEROS TO COUNTER

           PERFORM WITH TEST AFTER UNTIL SANDWICH-EXISTS = "Y"
              PERFORM LIST-SANDWICHS
              PERFORM CHECK-SANDWICH-EXISTS
           END-PERFORM

           SET IND-ORDERS TO 1
           PERFORM UNTIL IND-ORDERS > MAX-ORDERS
              IF REG-SANDWICH =
              TAB-ORDERS-SANDWICH-INTERNAL-ID (IND-ORDERS) THEN
                 ADD 1 TO COUNTER
                 DISPLAY CLEAR-SCREEN
                 DISPLAY MAIN-SCREEN
                 PERFORM GET-SCHOOL-NAME
                 ACCEPT SHOW-REGISTER-SCREEN
              END-IF
              SET IND-ORDERS UP BY 1
           END-PERFORM

           IF COUNTER = 0 THEN
              MOVE ERROR-SEARCH TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       SEARCH-SCHOOL-SANDWICH SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN

           MOVE ZEROS TO COUNTER

           PERFORM WITH TEST AFTER UNTIL SCHOOL-EXISTS = "Y"
              PERFORM LIST-SCHOOLS
              PERFORM CHECK-SCHOOL-EXISTS
           END-PERFORM

           PERFORM WITH TEST AFTER UNTIL SANDWICH-EXISTS = "Y"
              PERFORM LIST-SANDWICHS
              PERFORM CHECK-SANDWICH-EXISTS
           END-PERFORM

           SET IND-ORDERS TO 1
           PERFORM UNTIL IND-ORDERS > MAX-ORDERS
              IF REG-SCHOOL = TAB-ORDERS-SCHOOL-INTERNAL-ID (IND-ORDERS)
              THEN
                 IF REG-SANDWICH =
                 TAB-ORDERS-SANDWICH-INTERNAL-ID (IND-ORDERS) THEN
                    ADD 1 TO COUNTER
                    DISPLAY CLEAR-SCREEN
                    DISPLAY MAIN-SCREEN
                    ACCEPT SHOW-REGISTER-SCREEN
                 END-IF
              END-IF
              SET IND-ORDERS UP BY 1
           END-PERFORM

           IF COUNTER = 0 THEN
              MOVE ERROR-SEARCH TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       CHECK-SCHOOL-SANDWICH-FILE SECTION.
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
      *       DAR ERRO A DIZER QUE FICHEIRO ESTA VAZIO + REPETIR CODIGO DO START
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
      *       DAR ERRO A DIZER QUE FICHEIRO ESTA VAZIO + REPETIR CODIGO DO START
           ELSE
              CLOSE ORDERSKEYS
           END-IF

           OPEN INPUT CALENDAR
           IF CALENDAR-FS = "35"
              MOVE "N" TO CALENDAR-EXISTS
           END-IF
           CLOSE CALENDAR
           EXIT SECTION.

      ******************************************************************

       LOAD-ALL-TABLES SECTION.
           IF CALENDAR-EXISTS NOT = "N" THEN
              PERFORM FILL-TABLE-CAL
              IF CALENDAR-EXISTS NOT = "N" THEN
                 PERFORM SORT-ASCENDING-CAL
                 PERFORM AGG-TABLE-CAL
              END-IF
           END-IF

           PERFORM FILL-TABLE-SCHOOL

           PERFORM FILL-TABLE-SANDWICH

           PERFORM FILL-TABLE-ORDERS
           EXIT SECTION.

      ******************************************************************

       FILL-TABLE-CAL SECTION.
           OPEN INPUT CALENDAR
           MOVE 001 TO FD-DOWNTIME-ID
           START CALENDAR KEY IS GREATER OR EQUAL FD-DOWNTIME-ID
              INVALID KEY
                 MOVE "N" TO CALENDAR-EXISTS
                 EXIT SECTION
           END-START

           SET IND-CAL TO 0
           PERFORM UNTIL EOF-DOWNTIME-ID
              READ CALENDAR
                 AT END
                    SET EOF-DOWNTIME-ID TO TRUE
                    MOVE IND-CAL TO MAX-CAL
                 NOT AT END
                    SET IND-CAL UP BY 1
                    PERFORM LOAD-TABLE-CAL
              END-READ
           END-PERFORM
           CLOSE CALENDAR
           EXIT SECTION.

       LOAD-TABLE-CAL SECTION.
           STRING FD-START-DOWNTIME FD-START-TIME INTO
           TAB-BEGIN (IND-CAL)
           IF FD-END-DOWNTIME = ZERO THEN
              MOVE "999999999999" TO TAB-END (IND-CAL)
           ELSE
              STRING FD-END-DOWNTIME FD-END-TIME INTO
              TAB-END (IND-CAL)
           END-IF
           EXIT SECTION.

       SORT-ASCENDING-CAL SECTION.
           SORT TAB-CAL
           ON ASCENDING TAB-BEGIN
           ON ASCENDING TAB-END
           DUPLICATES
           EXIT SECTION.

       AGG-TABLE-CAL SECTION.
           MOVE TAB-CAL (1) TO TAB-AGG (1)
           SET IND-CAL TO 2
           SET IND-AGG TO 1
           PERFORM WITH TEST AFTER UNTIL IND-CAL > MAX-CAL
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

       FILL-TABLE-SCHOOL SECTION.
           OPEN INPUT SCHOOLS
           SET IND-SCHOOL TO 0
           PERFORM UNTIL EOFSCHOOLS
              READ SCHOOLS
                 AT END
                    SET EOFSCHOOLS TO TRUE
                    MOVE IND-SCHOOL TO MAX-SCHOOL
                 NOT AT END
                    SET IND-SCHOOL UP BY 1
                    PERFORM LOAD-TABLE-SCHOOL
              END-READ
           END-PERFORM
           CLOSE SCHOOLS
           EXIT SECTION.

       LOAD-TABLE-SCHOOL SECTION.
           MOVE SCHOOL-INTERNAL-ID TO
           TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL)
           MOVE SCHOOL-DESIGNATION1 TO
           TAB-SCHOOL-DESIGNATION (IND-SCHOOL)
           EXIT SECTION.

      ******************************************************************

       FILL-TABLE-SANDWICH SECTION.
           OPEN INPUT SANDWICHES
           SET IND-SANDWICH TO 0
           PERFORM UNTIL SR-EOF
              READ SANDWICHES
                 AT END
                    SET SR-EOF TO TRUE
                    MOVE IND-SANDWICH TO MAX-SANDWICH
                 NOT AT END
                    SET IND-SANDWICH UP BY 1
                    PERFORM LOAD-TABLE-SANDWICH
              END-READ
           END-PERFORM
           CLOSE SANDWICHES
           EXIT SECTION.

       LOAD-TABLE-SANDWICH SECTION.
           MOVE SR-IID TO TAB-SR-IID (IND-SANDWICH)
           MOVE SR-S-DESCRIPTION TO TAB-SR-S-DESCRIPTION (IND-SANDWICH)
           EXIT SECTION.

      ******************************************************************

       FILL-TABLE-ORDERS SECTION.
           OPEN INPUT ORDERS
           SET IND-ORDERS TO 0
           PERFORM UNTIL EOFORDERS
              READ ORDERS
                 AT END
                    SET EOFORDERS TO TRUE
                    MOVE IND-ORDERS TO MAX-ORDERS
                 NOT AT END
                    SET IND-ORDERS UP BY 1
                    PERFORM LOAD-TABLE-ORDERS
              END-READ
           END-PERFORM
           CLOSE ORDERS
           EXIT SECTION.

       LOAD-TABLE-ORDERS SECTION.
           MOVE FD-ORDERS TO TAB-ORDERS (IND-ORDERS)
           EXIT SECTION.

      ******************************************************************

       LIST-CALENDAR SECTION.
           MOVE LIST-FRAME2 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-AGG TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-AGG >= MAX-AGG
              SET IND-AGG UP BY 1
              DISPLAY CALENDAR-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-DELIVERY-DATE
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY CALENDAR-LIST
                    MOVE 10 TO ILIN
                    SET IND-AGG DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY CALENDAR-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-AGG >= MAX-AGG
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-DELIVERY-DATE
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY CALENDAR-LIST
                    MOVE 10 TO ILIN
                    SET IND-AGG DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       LIST-SCHOOLS SECTION.
           DISPLAY CLEAR-LIST
           MOVE LIST-FRAME1 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-SCHOOL TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-SCHOOL >= MAX-SCHOOL
              SET IND-SCHOOL UP BY 1
              DISPLAY SCHOOL-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SCHOOL
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY SCHOOL-LIST
                    MOVE 10 TO ILIN
                    SET IND-SCHOOL DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY SCHOOL-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-SCHOOL >= MAX-SCHOOL THEN
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SCHOOL
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY SCHOOL-LIST
                    MOVE 10 TO ILIN
                    SET IND-SCHOOL DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       CHECK-SCHOOL-EXISTS SECTION.
           MOVE SPACES TO SCHOOL-EXISTS
           SET IND-SCHOOL TO 1
           PERFORM UNTIL IND-SCHOOL > MAX-SCHOOL
              IF WS-ORDERS-SCHOOL-INTERNAL-ID =
              TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL) THEN
                 MOVE "Y" TO SCHOOL-EXISTS
                 EXIT SECTION
              ELSE
                 SET IND-SCHOOL UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       LIST-SANDWICHS SECTION.
           DISPLAY CLEAR-LIST
           MOVE LIST-FRAME1 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-SANDWICH TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-SANDWICH >= MAX-SANDWICH
              SET IND-SANDWICH UP BY 1
              DISPLAY SANDWICH-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SANDWICH
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY SANDWICH-LIST
                    MOVE 10 TO ILIN
                    SET IND-SANDWICH DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY SANDWICH-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-SANDWICH >= MAX-SANDWICH
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SANDWICH
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY SANDWICH-LIST
                    MOVE 10 TO ILIN
                    SET IND-SANDWICH DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       CHECK-SANDWICH-EXISTS SECTION.
           MOVE SPACES TO SANDWICH-EXISTS
           SET IND-SANDWICH TO 1
           PERFORM UNTIL IND-SANDWICH > MAX-SANDWICH
              IF WS-ORDERS-SANDWICH-INTERNAL-ID =
              TAB-SR-IID (IND-SANDWICH) THEN
                 MOVE "Y" TO SANDWICH-EXISTS
                 EXIT SECTION
              ELSE
                 SET IND-SANDWICH UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-SANDWICH-NAME SECTION.
           SET IND-SANDWICH TO 1
           PERFORM UNTIL IND-SANDWICH > MAX-SANDWICH
              IF TAB-ORDERS-SANDWICH-INTERNAL-ID (IND-ORDERS) =
              TAB-SR-IID (IND-SANDWICH) THEN
                 EXIT SECTION
              ELSE
                 SET IND-SANDWICH UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-SCHOOL-NAME SECTION.
           SET IND-SCHOOL TO 1
           PERFORM UNTIL IND-SCHOOL > MAX-SCHOOL
              IF TAB-ORDERS-SCHOOL-INTERNAL-ID (IND-ORDERS) =
              TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL) THEN
                 EXIT SECTION
              ELSE
                 SET IND-SCHOOL UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       END PROGRAM RSOSEARCH.
