      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    MAIN PROGRAM | V0.2 | IN UPDATE | 22.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAM-MAIN.

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

       01  MAIN-MENU
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           03 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           03 VALUE ALL " " PIC X(050) LINE 17 COL 35.
           03 VALUE OPTION-REGISTER1 LINE 11 COL 50.
           03 VALUE OPTION-VIEW2     LINE 12 COL 50.
           03 VALUE OPTION-EDIT3     LINE 13 COL 50.
           03 VALUE OPTION-DELETE4   LINE 14 COL 50.
           03 VALUE OPTION-EXIT5     LINE 15 COL 50.
           03 VALUE ACCEPT-OPTION    LINE 20 COL 45 REVERSE-VIDEO.
           03 SC-OPTION PIC 9(002) LINE 20 COL 69 TO OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

       01  LEAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 5.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
           05 VALUE EXIT-TEXT        LINE 10 COL 45.
           05 VALUE EXIT-TEXT1       LINE 11 COL 45.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL OPTION = 5

              MOVE ZERO TO SC-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU

              EVALUATE OPTION
                 WHEN 1     CALL "CAM-REG"
                 WHEN 2     PERFORM VIEW-DOWNTIME
                 WHEN 3     PERFORM EDIT-DOWNTIME
                 WHEN 4     PERFORM DELETE-DOWNTIME
              END-EVALUATE

           END-PERFORM

           DISPLAY LEAVE-SCREEN
           STOP RUN.




       VIEW-DOWNTIME SECTION.

       EDIT-DOWNTIME SECTION.

       DELETE-DOWNTIME SECTION.

       END PROGRAM CAM-MAIN.
