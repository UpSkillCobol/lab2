      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    MAIN PROGRAM | V0.5 | IN UPDATE | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAM-MAIN.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       COPY LANGUAGE.
       COPY WSCALENDAR.

       01  OPTION                               PIC 9(002).
           88  VALID-OPTION                     VALUE 1 THRU 5.
       77  PRESS-KEY                            PIC X.

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

       01  MAIN-MENU-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
           AUTO REQUIRED.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35.
           05 VALUE OPTION-REGISTER1 LINE 11 COL 50.
           05 VALUE OPTION-VIEW2     LINE 12 COL 50.
           05 VALUE OPTION-EDIT3     LINE 13 COL 50.
           05 VALUE OPTION-DELETE4   LINE 14 COL 50.
           05 VALUE OPTION-EXIT5     LINE 15 COL 50.
           05 VALUE ACCEPT-OPTION    LINE 20 COL 45 REVERSE-VIDEO.
           05 SC-OPTION PIC 9(002) LINE 20 COL 70 TO OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

       01  ERROR-MESSAGE-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ERROR-LINE LINE 25 COL 12 PIC X(080).
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

       01  LEAVE-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 BLANK SCREEN.
           05 VALUE ALL " " PIC X(050) LINE 08 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 18 COL 35 REVERSE-VIDEO.
           05 VALUE EXIT-TEXT          LINE 13 COL 47 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(048) LINE 09 COL 36.
           05 VALUE " "                LINE 10 COL 36.
           05 VALUE " "                LINE 11 COL 36.
           05 VALUE " "                LINE 12 COL 36.
           05 VALUE " "                LINE 13 COL 36.
           05 VALUE " "                LINE 14 COL 36.
           05 VALUE " "                LINE 15 COL 36.
           05 VALUE " "                LINE 16 COL 36.
           05 VALUE " "                LINE 10 COL 83.
           05 VALUE " "                LINE 11 COL 83.
           05 VALUE " "                LINE 12 COL 83.
           05 VALUE " "                LINE 13 COL 83.
           05 VALUE " "                LINE 14 COL 83.
           05 VALUE " "                LINE 15 COL 83.
           05 VALUE " "                LINE 16 COL 83.
           05 VALUE ALL " " PIC X(048) LINE 17 COL 36.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL OPTION = 5

              DISPLAY CLEAR-SCREEN
              MOVE ZERO TO SC-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU-SCREEN
              IF NOT VALID-OPTION
                   MOVE MAIN-MENU-ERROR TO ERROR-LINE
                   ACCEPT ERROR-MESSAGE-SCREEN
               END-IF

              EVALUATE OPTION
                 WHEN 1     CALL "CAM-ADD"
                 WHEN 2     CALL "CAM-VIEW"
                 WHEN 3     CALL "CAM-EDIT"
                 WHEN 4     CALL "CAM-DELETE"
              END-EVALUATE

           END-PERFORM

           DISPLAY LEAVE-SCREEN
           STOP RUN.

       END PROGRAM CAM-MAIN.
