      ******************************************************************
      *           BREADWICH | INGREDIENTS | REGIST
      ******************************************************************
      *           25-01-2021
      ******************************************************************
      *           V1
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGIST-INGREDS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
           CLASS  VALIDNAME  IS "A" THRU "Z", "a" THRU "z", SPACES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INGREDS ASSIGN TO "INGREDIENTS"
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
           05 WS-INGRED-NAME      PIC X(040)  VALUE SPACES.
           05 WS-INGRED-DESCRIP   PIC X(050)  VALUE SPACES.
       01  WS-OPTION              PIC X(001) VALUE SPACES.
           88 OPTION-VALID        VALUE "S","N".

       77  FS-STATUS              PIC 9(001).
       77  KEY-STATUS             PIC 9(004).
       77  FLAG                   PIC 9(001).

       SCREEN SECTION.

       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE "Regist Ingredient" LINE 9 COL 22.
           05 VALUE "Ingredients Name:" LINE 11 COL 22.
           05 VALUE "Description:" LINE 13 COL 22.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 26 COL 98.
           05 VALUE "F3 - BACK | F4 - EXIT"
                   LINE 25 COL 99 FOREGROUND-COLOR 5.
           05 SC-REG.
               10 REG-NAME-ING PIC X(040) LINE 11 COL 39
                   TO WS-INGRED-NAME.

               10 SC-ING-DESCRIP PIC X(040) LINE 13 COL 34
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

        01 SC-SAVE-REGIST
           REQUIRED, BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(60) LINE 19 COL 30.
           05 VALUE ALL " " PIC X(60) LINE 20 COL 30.
           05 VALUE ALL " " PIC X(60) LINE 21 COL 30.
           05 VALUE "SAVE REGIST? (S/N)?"
               LINE 20 COL 35 FOREGROUND-COLOR 5.
           05 SC-OPTION    PIC X(01) LINE 20 COL 79
               TO WS-OPTION AUTO
                   FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.
        01 SC-SAVE-STATUS
           REQUIRED, BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(60) LINE 02 COL 30.
           05 VALUE ALL " " PIC X(60) LINE 03 COL 30.
           05 VALUE ALL " " PIC X(60) LINE 04 COL 30.
           05 VALUE "REGISTRATION SAVED"
           LINE 03 COL 50 FOREGROUND-COLOR 5.

        01 SC-SAVE-AGAIN
           REQUIRED, BACKGROUND-COLOR 7.
           05 VALUE "REGIST AGAIN? (S/N)"
           LINE 20 COL 35 FOREGROUND-COLOR 5.
           05 SC-OPTION-02    PIC X(01) LINE 20 COL 79
               TO WS-OPTION AUTO
                   FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           FILE-TESTE SECTION.

           OPEN I-O INGREDS
           IF FS-STATUS = 35 THEN
               OPEN OUTPUT INGREDS
                   MOVE 0 TO WS-INGRED-ID
                   MOVE WS-INGRED-ID TO INGRED-ID
                   WRITE INGRED-ID
                   END-WRITE
               CLOSE INGREDS
           ELSE
               CLOSE INGREDS
           END-IF
           EXIT SECTION.
       REGIST SECTION.
         *>AUTO ID
               OPEN I-O INGREDS
                 READ INGREDS
                   ADD 1 TO WS-INGRED-ID
                     MOVE WS-INGRED-ID TO INGRED-ID
                       WRITE INGRED-ID
            CLOSE INGREDS.

           *> REGIST INGREDIENT
           MOVE SPACES TO SC-REG
           MOVE ZEROS TO WS-INGRED-DESCRIP
           DISPLAY CLEAR-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL WS-INGRED-NAME IS VALIDNAME


           ACCEPT REG-NAME-ING
             MOVE FUNCTION UPPER-CASE (REG-NAME-ING) TO REG-NAME-ING
      *>        CALL "REMOVE-SPACES" USING BY REFERENCE REG-NAME-ING
            END-PERFORM

           *>DESCRIPTION REGIST


           PERFORM WITH TEST AFTER UNTIL SC-ING-DESCRIP IS VALIDNAME

            ACCEPT SC-ING-DESCRIP
            MOVE FUNCTION UPPER-CASE (SC-ING-DESCRIP) TO SC-ING-DESCRIP
      *>       CALL "REMOVE-SPACES" USING BY REFERENCE SC-ING-DESCRIP

           END-PERFORM.

           DISPLAY SC-SAVE-REGIST

           PERFORM WITH TEST AFTER UNTIL OPTION-VALID

           ACCEPT SC-OPTION
           MOVE FUNCTION UPPER-CASE (SC-OPTION) TO SC-OPTION

           END-PERFORM.

           EVALUATE TRUE

                   WHEN WS-OPTION = "S" PERFORM RECORD-REGIST
                   WHEN WS-OPTION = "N" PERFORM REGIST

           END-EVALUATE.

       RECORD-REGIST SECTION.

           OPEN I-O INGREDS
           WRITE INGREDIENTS FROM WS-INGREDS
           CLOSE INGREDS.
           DISPLAY SC-SAVE-STATUS.

           DISPLAY SC-SAVE-AGAIN
           PERFORM WITH TEST AFTER UNTIL OPTION-VALID

           ACCEPT SC-OPTION-02
           EVALUATE TRUE
                       WHEN WS-OPTION = "S" PERFORM REGIST
                       WHEN WS-OPTION = "N" CALL "MAIN-INGREDS"
                        STOP RUN
           END-PERFORM.

       END PROGRAM REGIST-INGREDS.
