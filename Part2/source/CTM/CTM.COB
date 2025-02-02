      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CATEGORIES MANAGEMENT
      ******************************************************************
      *    CATEGORIES MODULE - MAIN
      ******************************************************************
      *    EM ATUALIZAÇÃO | 23.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTMMENU.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       COPY CONSTANTSCTM.

       01  WS-OPTION                           PIC 9(002).
           88 VALID-OPTION                     VALUE 1 THRU 5.
       77  DUMMY                               PIC X(001).

       SCREEN SECTION.

       01  CLEAR-SCREEN.
           03 BLANK SCREEN.

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
           03 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.

           01  MAIN-MENU-SCREEN
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
           03 VALUE MAIN-MENU-OPTION2 LINE 12 COL 50.
           03 VALUE MAIN-MENU-OPTION3 LINE 13 COL 50.
           03 VALUE MAIN-MENU-OPTION4 LINE 14 COL 50.
           03 VALUE MAIN-MENU-OPTION5 LINE 15 COL 50.
           03 VALUE MAIN-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           03 MM-OPTION PIC 9(002) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

           01 ERROR-MESSAGE FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 ERROR-LINE LINE 25 COL 15 PIC X(80).
           03 SCREEN-DUMMY LINE 26 COL 95 PIC X TO DUMMY AUTO.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 5
               MOVE ZERO TO WS-OPTION MM-OPTION
               DISPLAY MAIN-SCREEN
               ACCEPT MAIN-MENU-SCREEN
               IF NOT VALID-OPTION
                   MOVE MAIN-MENU-ERROR TO ERROR-LINE
                   ACCEPT ERROR-MESSAGE
               END-IF
               PERFORM EVALUATE-MAIN-MENU
           END-PERFORM
           STOP RUN.

       EVALUATE-MAIN-MENU SECTION.
           EVALUATE WS-OPTION
               WHEN 1
                   CALL "CTMADD"
               WHEN 2
                   CALL "CTMVIEW"
               WHEN 3
                   CALL "CTMEDIT"
               WHEN 4
                   CALL "CTMDEL"
           END-EVALUATE
           EXIT SECTION.
