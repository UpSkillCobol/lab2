      ******************************************************************
      * Author: Bruno Lopes
      * Date: 20-01-2021
      * Purpose: Breadwich
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ingredients.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
           CLASS  VALIDNAME  IS "A" THRU "Z", "a" THRU "z", SPACES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INGREDS ASSIGN TO "INGREDS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY INGRED-ID
           ALTERNATE KEY IS INGRED-NAME WITH DUPLICATES
           FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD INGREDS.
       01  INGREDIENTS.
           05 INGRED-ID           PIC 9(004).
           05 INGRED-NAME         PIC X(040).
           05 INGRED-DESCRIP      PIC X(050).

       WORKING-STORAGE SECTION.

       01  WS-INGREDS.
           05 WS-INGRED-ID        PIC 9(004)  VALUE ZEROS.
           05 WS-INGRED-NAME      PIC A(040)  VALUE SPACES.
           05 WS-INGRED-DESCRIP   PIC X(050)  VALUE SPACES.
       01  WS-OPTION              PIC 9(001).
           88 OPTION-VALID        VALUE
                                  "1","2","3","4","5".
       77  FS-STATUS              PIC 9.
       77  KEY-STATUS             PIC 9(004).



       SCREEN SECTION.

       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE "INGREDIENTS MANAGEMENT" LINE 03 COL 50.
           03 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           03 VALUE ALL " " PIC X(22) LINE 24 COL 98.
           03 VALUE ALL " " PIC X(22) LINE 25 COL 98.
           03 VALUE ALL " " PIC X(22) LINE 26 COL 98.
           03 VALUE "F3 - BACK | F4 - EXIT"
                   LINE 25 COL 99 FOREGROUND-COLOR 5.

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
           03 VALUE "1 -Add Ingredient" LINE 11 COL 50.
           03 VALUE "2 -View Ingredient" LINE 12 COL 50.
           03 VALUE "3 -Modify Ingredient" LINE 13 COL 50.
           03 VALUE "4 -Delete Igredient" LINE 14 COL 50.
           03 VALUE "5 -Back to Main Menu" LINE 15 COL 50.
           03 VALUE "Please choose an option:" LINE 20 COL 45
           REVERSE-VIDEO.
           03 SC-OPTION PIC 9(01) LINE 20 COL 69 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

           01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE "Register Ingredient" LINE 9 COL 25.
           05 VALUE "Name       :" LINE 11 COL 26.
           05 VALUE "Description:" LINE 12 COL 26.

           05 SC-REG.
               10 REG-NAME-ING PIC X(040) LINE 11 COL 39
                   TO WS-INGRED-NAME.

               10 SC-ING-DESCRIP PIC X(040) LINE 12 COL 39
                   TO WS-INGRED-DESCRIP.


           05 VALUE ALL " " PIC X(80) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(80) LINE 21 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 18 BACKGROUND-COLOR 7.
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
           05 VALUE "  " LINE 8 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 96 BACKGROUND-COLOR 7.
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

       *>  OPTION CHOISE SECTION
       MENU-OPTION SECTION.
           DISPLAY MAIN-SCREEN
           DISPLAY MAIN-MENU

               MOVE ZEROS TO SC-OPTION
               ACCEPT SC-OPTION

           EVALUATE TRUE
                       WHEN WS-OPTION = 1 PERFORM REGIST
                       WHEN WS-OPTION = 2 PERFORM VIEW
                       WHEN WS-OPTION = 3 PERFORM MODIFY
                       WHEN WS-OPTION = 4 PERFORM REMOVE
                       *>CALL MAIN MODULE!
      *>               WHEN WS-OPTION = 5 CALL MAIN

           END-EVALUATE.

       FILE-TESTE SECTION.

           OPEN I-O INGREDS
           IF FS-STATUS = "35" THEN
               OPEN OUTPUT INGREDS
                   MOVE 0 TO INGRED-ID
                   WRITE INGRED-ID
                   END-WRITE
               CLOSE INGREDS
           ELSE
               CLOSE INGREDS
           END-IF
           EXIT SECTION.

       REGIST SECTION.

            DISPLAY CLEAR-SCREEN
            ACCEPT REGISTER-SCREEN.
      *>  OPEN I-O INGREDS
      *>          READ INGREDS
      *>              ADD 1 TO INGRED-ID
      *>              MOVE INGRED-ID TO WS-INGRED-ID
      *>              REWRITE INGRED-ID
      *>      CLOSE INGREDS

      *>      PERFORM WITH TEST AFTER UNTIL WS-INGRED-NAME IS VALIDNAME
      *>      *>ACCEPT SCREEN SECTION

      *>      END-PERFORM.

       VIEW SECTION.

       MODIFY SECTION.

       REMOVE SECTION.



            STOP RUN.
       END PROGRAM Ingredients.
