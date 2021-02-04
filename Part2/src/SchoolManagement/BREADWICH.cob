      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | MAIN PROGRAM | V0.1 | IN UPDATE | 03.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BREADWICH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       COPY BREADWICHCONSTANTS.
       COPY BREADWICHWS.

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 52.
           05 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 26 COL 01.

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
           05 VALUE ALL " " PIC X(050) LINE 18 COL 35.
           05 VALUE OPTION-1       LINE 11 COL 47.
           05 VALUE OPTION-2       LINE 12 COL 47.
           05 VALUE OPTION-3       LINE 13 COL 47.
           05 VALUE OPTION-4       LINE 14 COL 47.
           05 VALUE OPTION-5       LINE 15 COL 47.
           05 VALUE OPTION-6       LINE 16 COL 47.
           05 VALUE ACCEPT-OPTION  LINE 21 COL 46 REVERSE-VIDEO.
           05 SS-OPTION PIC 9(002) LINE 21 COL 71 TO MAIN-OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

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

       01  ERROR-MESSAGE-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ERROR-LINE LINE 25 COL 12 PIC X(080).
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL MAIN-OPTION = 6

              DISPLAY CLEAR-SCREEN
              MOVE ZEROS TO SS-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU-SCREEN
              IF NOT VALID-MAIN-OPTION
                 MOVE OPTION-ERROR TO ERROR-LINE
                 ACCEPT ERROR-MESSAGE-SCREEN
               END-IF

              EVALUATE MAIN-OPTION
                 WHEN 1     CALL "SCM-MAIN"
                 WHEN 2     CALL "CAMMAIN"
                 WHEN 3     CALL "MAININGREDS"
                 WHEN 4     CALL "suppliers"
                 WHEN 5     CALL "CATEGORIES"
              END-EVALUATE

           END-PERFORM

           ACCEPT LEAVE-SCREEN

           STOP RUN.

       END PROGRAM BREADWICH.
