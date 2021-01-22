      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    REGISTER MODULE | V0.2 | IN UPDATE | 22.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAM-REG.

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
           FILE STATUS IS FILE-NOT-EXIST.

           SELECT KEYS ASSIGN TO "KEYSFILE.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS FILE-NOT-EXIST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
           COPY FDCALENDAR.

       FD  KEYS.
       01  FDKEYS                               PIC 9(003).

       WORKING-STORAGE SECTION.
           COPY WSCALENDAR.
           COPY ENLANGUAGE.

       01  OPTION                               PIC 9(001).
           88  OPTION-REGISTER                  VALUE 1.
           88  OPTION-VIEW                      VALUE 2.
           88  OPTION-EDIT                      VALUE 3.
           88  OPTION-DELETE                    VALUE 4.
           88  OPTION-EXIT                      VALUE 5.
           88  VALID-OPTION                     VALUE 1 THRU 5.
       77  FILE-NOT-EXIST                       PIC 9(002).
       77  PRESS-KEY                            PIC X.

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE MAIN-TEXT          LINE 03 COL 50.
           03 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 26 COL 01.
           03 VALUE MAIN-TEXT1 LINE 25 COL 99 FOREGROUND-COLOR 5.
           03 VALUE "  "       LINE 24 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  "       LINE 25 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  "       LINE 26 COL 96 BACKGROUND-COLOR 0.

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT             LINE 9 COL 25.
           05 VALUE REGISTER-TEXT-ID          LINE 12 COL 25.
           05 VALUE REGISTER-TEXT-DATE        LINE 14 COL 25.
           05 VALUE REGISTER-TEXT-DESCRIPTION LINE 16 COL 25.
           05 VALUE REGISTER-TEXT-DESCRIPTION LINE 17 COL 25.
           05 REG-REC.
              10 REG-ID PIC 9(003) LINE 12 COL 39
                 FROM WS-DOWNTIME-ID BLANK WHEN ZERO.
              10 REG-DATE.
                 15 REG-DAY PIC 9(002) LINE 14 COL 40 TO WS-DT-DAY
                 BLANK WHEN ZERO.
                 15 REG-MONTH PIC 9(002) LINE 14 COL 43 TO WS-DT-MONTH
                 BLANK WHEN ZERO.
                 15 REG-YEAR PIC 9(004) LINE 14 COL 46 TO WS-DT-YEAR
                 BLANK WHEN ZERO.
              10 REG-DESCRIPTION.
                 15 REG-DESCRIPTION1 PIC X(050) LINE 16 COL 47
                 TO WS-DOWNTIME-DESCRIPTION1.
                 15 REG-DESCRIPTION2 PIC X(050) LINE 17 COL 47
                 TO WS-DOWNTIME-DESCRIPTION2.
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

       PROCEDURE DIVISION.
       CREATE-FILE SECTION.
           OPEN I-O CALENDAR
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT CALENDAR
              CLOSE CALENDAR
           END-IF
           CLOSE CALENDAR

           MOVE ZEROS TO FILE-NOT-EXIST

           OPEN I-O KEYS
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT KEYS
              CLOSE KEYS
           END-IF
           CLOSE KEYS

           EXIT SECTION.

       REGISTER-DOWNTIME SECTION.
           PERFORM CREATE-FILE

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           OPEN I-O CALENDAR
              PERFORM DOWNTIME-ID
              PERFORM DOWNTIME-DATE
              PERFORM DOWNTIME-DESCRIPTION

              WRITE FD-CALENDAR FROM WS-CALENDAR
           CLOSE CALENDAR

           STOP RUN.

       DOWNTIME-ID SECTION.
           OPEN I-O KEYS
              READ KEYS
              ADD 1 TO FDKEYS
              REWRITE FDKEYS
              MOVE FDKEYS TO WS-DOWNTIME-ID
              DISPLAY REG-ID
           CLOSE KEYS

           EXIT SECTION.

       DOWNTIME-DATE SECTION.
           ACCEPT REG-DAY
           ACCEPT REG-MONTH
           ACCEPT REG-YEAR
           EXIT SECTION.

       DOWNTIME-DESCRIPTION SECTION.
           ACCEPT REG-DESCRIPTION1
           ACCEPT REG-DESCRIPTION2
           EXIT SECTION.

       END PROGRAM CAM-REG.
