      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    MAIN PROGRAM | V0.6 | IN UPDATE | 01.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMMAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS F3F4EXIT.

       DATA DIVISION.
       FILE SECTION.

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
           05 SS-OPTION PIC 9(002) LINE 20 COL 70 TO MAIN-OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

       01  ERROR-MESSAGE-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ERROR-LINE LINE 25 COL 12 PIC X(080).
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL MAIN-OPTION = 5

              DISPLAY CLEAR-SCREEN
              MOVE ZEROS TO SS-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU-SCREEN
              IF NOT VALID-MAIN-OPTION
                 MOVE OPTION-ERROR TO ERROR-LINE
                 ACCEPT ERROR-MESSAGE-SCREEN
               END-IF

              EVALUATE MAIN-OPTION
                 WHEN 1     CALL "CAMADD"
                            IF F3F4EXIT = 1004 THEN
                               EXIT PROGRAM
                            END-IF
                 WHEN 2     CALL "CAMVIEW"
                            IF F3F4EXIT = 1004 THEN
                               EXIT PROGRAM
                            END-IF
                 WHEN 3     CALL "CAMEDIT"
                            IF F3F4EXIT = 1004 THEN
                               EXIT PROGRAM
                            END-IF
                 WHEN 4     CALL "CAMDELETE"
                            IF F3F4EXIT = 1004 THEN
                               EXIT PROGRAM
                            END-IF
              END-EVALUATE

           END-PERFORM

           EXIT PROGRAM.

       END PROGRAM CAMMAIN.
