      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    ADD MODULE | V0.6 | IN UPDATE | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAM-ADD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALENDAR ASSIGN TO "CALENDARFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-DOWNTIME-ID
              FILE STATUS IS CALENDAR-TEST.

           SELECT KEYS ASSIGN TO "KEYSFILE"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS KEYS-TEST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
       COPY FDCALENDAR.

       FD  KEYS.
       01  FDKEYS                               PIC 9(003).

       WORKING-STORAGE SECTION.
       COPY LANGUAGE.
       COPY WSCALENDAR.
       COPY WSVAR.

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 26 COL 98.
           05 VALUE MAIN-TEXT1 LINE 25 COL 99 FOREGROUND-COLOR 5.

       01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(080) LINE 10 COL 18.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 18
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 18
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 96 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT             LINE 09 COL 48.
           05 VALUE REGISTER-TEXT-ID          LINE 13 COL 21.
           05 VALUE REGISTER-TEXT-DATE        LINE 15 COL 21.
           05 VALUE REGISTER-TEXT-DATE1       LINE 16 COL 21.
           05 VALUE REGISTER-TEXT-DESCRIPTION LINE 18 COL 21.
           05 REG-ID PIC 9(003) LINE 13 COL 45 USING WS-DOWNTIME-ID.
           05 REG-REC.
              10 REG-START-DATE.
                 15 REG-START-DAY PIC 9(002) LINE 15 COL 45 TO
                    WS-START-DT-DAY BLANK WHEN ZERO AUTO REQUIRED.
                 15 LINE 15 COL 47 VALUE "/".
                 15 REG-START-MONTH PIC 9(002) LINE 15 COL 48 TO
                    WS-START-DT-MONTH BLANK WHEN ZERO AUTO REQUIRED.
                 15 LINE 15 COL 50 VALUE "/".
                 15 REG-START-YEAR PIC 9(004) LINE 15 COL 51 TO
                    WS-START-DT-YEAR BLANK WHEN ZERO AUTO REQUIRED.
              10 REG-END-DATE.
                 15 REG-END-DAY PIC 9(002) LINE 16 COL 45 TO
                    WS-END-DT-DAY BLANK WHEN ZERO AUTO.
                 15 LINE 16 COL 47 VALUE "/".
                 15 REG-END-MONTH PIC 9(002) LINE 16 COL 48 TO
                    WS-END-DT-MONTH BLANK WHEN ZERO AUTO.
                 15 LINE 16 COL 50 VALUE "/".
                 15 REG-END-YEAR PIC 9(004) LINE 16 COL 51 TO
                    WS-END-DT-YEAR BLANK WHEN ZERO AUTO.
              10 REG-DESCRIPTION.
                 15 REG-DESCRIPTION1 PIC X(050) LINE 18 COL 45
                    TO WS-DOWNTIME-DESCRIPTION1 AUTO.
                 15 REG-DESCRIPTION2 PIC X(050) LINE 19 COL 45
                    TO WS-DOWNTIME-DESCRIPTION2 AUTO.

       01  INVALID-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INVALID-TEXT LINE 25 COL 03 PIC X(085)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

       01  INSTRUCTIONS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  SAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 10
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SC-SAVE LINE 25 COL 69
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

       PROCEDURE DIVISION.
       REGISTER-DOWNTIME SECTION.
           PERFORM CREATE-FILE

           PERFORM DOWNTIME-ID

           OPEN I-O CALENDAR

           MOVE ZEROS TO  REG-END-DAY, REG-END-MONTH, REG-END-YEAR,
                          REG-START-DAY, REG-START-MONTH, REG-START-YEAR
           MOVE SPACES TO REG-DESCRIPTION

           MOVE FDKEYS TO WS-DOWNTIME-ID

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM DOWNTIME-START-DATE
           PERFORM DOWNTIME-END-DATE
           PERFORM DOWNTIME-DESCRIPTION

           PERFORM WITH TEST AFTER UNTIL SAVE-VALID
              ACCEPT SAVE-SCREEN
              IF NOT SAVE-VALID THEN
                 MOVE INVALID-OPTION TO INVALID-TEXT
                 ACCEPT INVALID-SCREEN
              END-IF
           END-PERFORM
           IF SAVE = "Y" OR "y"
              REWRITE FDKEYS
              END-REWRITE
              WRITE FD-CALENDAR FROM WS-CALENDAR
              END-WRITE
              CLOSE KEYS
              CLOSE CALENDAR
              MOVE MESSAGE-WRITE-YES TO INVALID-TEXT
              ACCEPT INVALID-SCREEN
           ELSE
              IF SAVE = "N" OR "n"
                 CLOSE KEYS
                 CLOSE CALENDAR
                 MOVE MESSAGE-WRITE-NO TO INVALID-TEXT
                 ACCEPT INVALID-SCREEN
              END-IF
           END-IF
           EXIT PROGRAM.

       DOWNTIME-ID SECTION.
           OPEN I-O KEYS
              READ KEYS
                 ADD 1 TO FDKEYS
           EXIT SECTION.

       DOWNTIME-START-DATE SECTION.
           MOVE SPACE TO DATE-VALID
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
              MOVE ZEROS TO REG-START-DAY, REG-START-MONTH,
              REG-START-YEAR
              DISPLAY REG-START-DATE
              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN
              ACCEPT REG-START-DAY
              MOVE   REG-START-DAY   TO WS-DAY
              ACCEPT REG-START-MONTH
              MOVE   REG-START-MONTH TO WS-MONTH
              ACCEPT REG-START-YEAR
              MOVE   REG-START-YEAR  TO WS-YEAR
              ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
              PERFORM CHECK-DATE
           END-PERFORM
           MOVE WS-DAY TO WS-START-DT-DAY
           MOVE WS-MONTH TO WS-START-DT-MONTH
           MOVE WS-YEAR TO WS-START-DT-YEAR
           EXIT SECTION.

       DOWNTIME-END-DATE SECTION.
           MOVE SPACE TO DATE-VALID
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
              MOVE ZEROS TO REG-END-DAY, REG-END-MONTH, REG-END-YEAR
              DISPLAY REG-END-DATE
              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN
              ACCEPT REG-END-DAY
              MOVE   REG-END-DAY   TO WS-DAY
              ACCEPT REG-END-MONTH
              MOVE   REG-END-MONTH TO WS-MONTH
              ACCEPT REG-END-YEAR
              MOVE   REG-END-YEAR  TO WS-YEAR
              ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
              PERFORM CHECK-DATE
           END-PERFORM
           MOVE WS-DAY TO WS-END-DT-DAY
           MOVE WS-MONTH TO WS-END-DT-MONTH
           MOVE WS-YEAR TO WS-END-DT-YEAR
           EXIT SECTION.

       DOWNTIME-DESCRIPTION SECTION.
           MOVE SPACES TO REG-DESCRIPTION
           MOVE INSTRUCTIONS-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-SCREEN
           ACCEPT REG-DESCRIPTION
           EXIT SECTION.

       CREATE-FILE SECTION.
           OPEN I-O CALENDAR
           IF CALENDAR-TEST = "35"
              OPEN OUTPUT CALENDAR
              CLOSE CALENDAR
           ELSE
              CLOSE CALENDAR
           END-IF

           OPEN I-O KEYS
           IF KEYS-TEST = "35"
              OPEN OUTPUT KEYS
                 MOVE 0 TO FDKEYS
                 WRITE FDKEYS
                 END-WRITE
              CLOSE KEYS
           ELSE
              CLOSE KEYS
           END-IF
           EXIT SECTION.

       CHECK-DATE SECTION.
           IF VALID-YEAR AND VALID-MONTH AND VALID-DAY THEN
              IF WS-DAY >= WS-CURRENT-DAY AND WS-MONTH >=
              WS-CURRENT-MONTH AND WS-YEAR >= WS-CURRENT-YEAR THEN
                 MOVE "Y" TO DATE-VALID
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

           IF DATE-VALID NOT = "Y" THEN
              MOVE INVALID-DATE TO INVALID-TEXT
              ACCEPT INVALID-SCREEN
           END-IF
           EXIT SECTION.

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

       END PROGRAM CAM-ADD.
