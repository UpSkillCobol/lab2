      ******************************************************************
      *           BREADWICH | INGREDIENTS | MAIN
      ******************************************************************
      *           25-01-2021
      ******************************************************************
      *           V1
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-INGREDS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
           INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.
.
       WORKING-STORAGE SECTION.

       COPY CONSTANT-INGREDS.

       01  WS-OPTION              PIC 9(001).
           88 OPTION-VALID        VALUE
                                  "1","2","3","4","5".
       01  KEY-STATUS              PIC 9(04) VALUE ZEROS.

       SCREEN SECTION.

       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
           *>MAIN MENU SCREEN SECTION
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE MODULE-NAME LINE 03 COL 50.
           03 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           03 VALUE ALL " " PIC X(22) LINE 24 COL 98.
           03 VALUE ALL " " PIC X(22) LINE 25 COL 98.
           03 VALUE ALL " " PIC X(22) LINE 26 COL 98.
           03 VALUE BACK-EXIT
                   LINE 25 COL 99 FOREGROUND-COLOR 5.
           *>PRINCIPAL MENU OF SCREEN SECTION
       01  MAIN-MENU
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           03 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 16 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 17 COL 35.
           03 VALUE MAIN-MENU-OPTION1 LINE 11 COL 50.
           03 VALUE MAIN-MENU-OPTION2     LINE 12 COL 50.
           03 VALUE MAIN-MENU-OPTION3     LINE 13 COL 50.
           03 VALUE MAIN-MENU-OPTION4     LINE 14 COL 50.
           03 VALUE MAIN-MENU-OPTION5     LINE 15 COL 50.
           03 VALUE MAIN-MENU-CHOICE      LINE 20 COL 45
           REVERSE-VIDEO.
           03 SC-OPTION PIC 9(01) LINE 20 COL 69 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.


       PROCEDURE DIVISION.
       DISPLAY CLEAR-SCREEN.
       *>  OPTION CHOISE SECTION
       MENU-OPTION SECTION.
           DISPLAY MAIN-SCREEN
           DISPLAY MAIN-MENU

               MOVE ZEROS TO SC-OPTION
               ACCEPT SC-OPTION

           EVALUATE TRUE
                       WHEN WS-OPTION = 1 CALL "REGIST-INGREDS"
                       WHEN WS-OPTION = 2 CALL "VIEW-INGREDS"
                       WHEN WS-OPTION = 3 CALL "MODIFY-INGREDS"
                       WHEN WS-OPTION = 4 CALL "REMOVE-INGREDS"
                       *>CALL MAIN MODULE!
                       *>WHEN WS-OPTION = 5 CALL MAIN
           END-EVALUATE.

       END PROGRAM MAIN-INGREDS.
