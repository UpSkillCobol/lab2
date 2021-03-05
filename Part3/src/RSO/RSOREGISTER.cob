      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGISTER ORDERS | V0.1 | IN UPDATE | 04.03.2021
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
           SELECT ORDERS ASSIGN TO "ORDERSFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-ORDERS-ID
              FILE STATUS IS ORDERS-FS.

           SELECT ORDERSKEYS ASSIGN TO "ORDERSKEYSFILE"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS ORDERSKEYS-FS.

           SELECT CALENDAR ASSIGN TO "CALENDARFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-DOWNTIME-ID
              ALTERNATE KEY IS FD-START-DOWNTIME WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDERS.
       COPY RSOFD.

       FD  ORDERSKEYS.
       01  FDORDERSKEYS                               PIC 9(005).

       FD  CALENDAR.
       COPY FDCALENDAR.

       WORKING-STORAGE SECTION.
       COPY RSOWS.
       COPY RSOCONTANTS.
       COPY RSOWSVAR.
       COPY VAR-VALIDDATE.
       COPY VAR-SPACEUPPER.

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
           05 SS-SAVE PIC X(002) LINE 25 COL 62
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
       REGISTER-DOWNTIME SECTION.
           PERFORM CREATE-FILE

           PERFORM DOWNTIME-ID

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

           PERFORM DELIVERY-DATE
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

       EXIT PROGRAM.

      ******************************************************************

       DOWNTIME-ID SECTION.
           OPEN I-O ORDERSKEYS
              READ ORDERSKEYS
                 ADD 1 TO FDORDERSKEYS
           EXIT SECTION.

      ******************************************************************

       DELIVERY-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
           AND FLAG-TRUE = "Y"
              MOVE SPACES TO DATE-VALID, FLAG-TRUE
              MOVE "DD"   TO REG-DELIVERY-DAY
              MOVE "MM"   TO REG-DELIVERY-MONTH
              MOVE "YYYY" TO REG-DELIVERY-YEAR

              DISPLAY REG-DELIVERY-DATE
              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-DELIVERY-DAY
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-MONTH
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-YEAR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-DELIVERY-DATE TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO WS-DELIVERY-DATE

              IF DATE-VALID = "Y" THEN
                 MOVE WS-DELIVERY-DATE TO TEST2
                 MOVE WS-ORDERS-DATE TO TEST3

                 SUBTRACT TEST2 FROM TEST3 GIVING TEST1

                 IF TEST1 < 3 THEN
                    MOVE INVALID-DATE2 TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                 ELSE
                    MOVE "Y" TO FLAG-TRUE
                 END-IF
              END-IF

              IF DATE-VALID = "Y" AND FLAG-TRUE = "Y" THEN
                 PERFORM DELIVERY-TIME
              END-IF

      *       AQUI SERÁ A VERIFICAÇÃO DA INDISPONIBILIDADE.

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
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-MINUTE
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-DELIVERY-HOUR OR NOT VALID-DELIVERY-MINUTE
              OR REG-DELIVERY-HOUR = "HH"
              OR REG-DELIVERY-MINUTE = "MM" THEN
                 MOVE INVALID-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
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
              IF KEYSTATUS = 1003 THEN
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

       END PROGRAM RSOREGISTER.
