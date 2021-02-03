      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS MANAGEMENT
      ******************************************************************
      *    INGREDIENTS MODULE - ADD INGREDIENTS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-INGREDS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FXINGREDLY ASSIGN TO "FXINGREDSS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS INGREDS-ID
                   ALTERNATE KEY IS INGREDS-NAME
                   FILE STATUS INGRED-STATUS.


               SELECT FXKEYS ASSIGN TO "INGREDKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXINGREDLY.

       COPY INGREDSFX.

       FD FXKEYS.
       01  FDINGREDKEYS                      PIC 9(003).

       WORKING-STORAGE SECTION.

       COPY CONSTANTS.

       COPY WS-INGREDSFX.

       01  ADD-OPTION1                     PIC X(002).
           88 ADD-VALID-OPTION1            VALUE "Y" "y" "N" "n".
           88 ADD-OPTION1-NO               VALUE "N" "n".
       77  DUMMY                           PIC X(001).
       77  INGRED-STATUS                     PIC 9(002).
       77  KEYSTATUS                       PIC 9(004).
       77  FXKEY-STATUS                    PIC 9(002).
       01  SAVE-IT                         PIC X(002).
           88 SAVE-IT-YES                  VALUE "Y" "y".
           88 SAVE-IT-VALID                VALUE "Y" "y" "N" "n".
       77 UNSTR                        PIC X(100).
       77 UNSTR1                       PIC X(015).
       77 UNSTR2                       PIC X(015).
       77 UNSTR3                       PIC X(015).
       77 UNSTR4                       PIC X(015).
       77 UNSTR5                       PIC X(015).
       77 UNSTR6                       PIC X(015).
       77 UNSTR7                       PIC X(015).
       77 UNSTR8                       PIC X(015).
       77 UNSTR9                       PIC X(015).
       77 UNSTR10                      PIC X(015).

      ******************************************************************

       SCREEN SECTION.
      ******************************************************************
       01  CLEAR-SCREEN.
           03 BLANK SCREEN.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************


        01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(082) LINE 10 COL 08.
           05 VALUE ALL " " PIC X(082) LINE 07 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 08
              BACKGROUND-COLOR 7.

           05 VALUE "  " LINE 08 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 88 BACKGROUND-COLOR 7.

           05 VALUE SCREEN-INGREDS-ID LINE 09 COL 11.
           05 VALUE MANUALLY-ADD-NAME LINE 13 COL 11.
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 16 COL 11.
           05 REG-INGRED-ID PIC 9(003) LINE 09 COL 25
               USING WSINGREDS-ID.
           05 REGISTER-RECORD.
               10 REG-INGRED-NAME PIC X(030) LINE 13 COL 27
                   TO WSINGREDS-NAME REQUIRED.
               10 REG-INGRED-DESCRIPTION.
                   15 REG-INGRED-DESCRIPTION1 PIC X(050) LINE 16 COL 23
                       TO WSINGREDS-DESCRIPTION1 REQUIRED AUTO.
      ******************************************************************
       01 ERROR-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 ERROR-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY1 LINE 26 COL 95 PIC X TO DUMMY AUTO.
      ******************************************************************
       01 INSTRUCTIONS-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01 WANT-TO-SAVE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE1 PIC X LINE 25 COL 67
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-CHECK-IF-INGRED-FILE-EXIST
           PERFORM 105-CHECK-IF-KEYS-FILE-EXIST
           PERFORM 110-GET-INGREDLY-ID
           OPEN I-O FXINGREDLY
           MOVE 1 TO WSINGREDS-IS-ACTIVE
           MOVE SPACES TO REG-INGRED-NAME REG-INGRED-DESCRIPTION
           MOVE FDINGREDKEYS TO WSINGREDS-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           PERFORM 115-GET-NAME
           PERFORM 120-GET-DESCRIPTION
                     PERFORM WITH TEST AFTER UNTIL SAVE-IT-VALID
                   MOVE "Y" TO WANT-TO-SAVE1
                   ACCEPT WANT-TO-SAVE
                   IF NOT SAVE-IT-VALID THEN
                       MOVE ERROR-SAVE TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
                   END-IF
           END-PERFORM
           IF SAVE-IT-YES THEN
               PERFORM 150-WRITE-RECORD
           END-IF
           EXIT PROGRAM.

       100-CHECK-IF-INGRED-FILE-EXIST SECTION.
           OPEN I-O FXINGREDLY
           IF INGRED-STATUS = "35" THEN
               OPEN OUTPUT FXINGREDLY
               CLOSE FXINGREDLY
           ELSE
               CLOSE FXINGREDLY
           END-IF
       EXIT SECTION.

       105-CHECK-IF-KEYS-FILE-EXIST SECTION.
           OPEN I-O FXKEYS
           IF FXKEY-STATUS = "35" THEN
               OPEN OUTPUT FXKEYS
                   MOVE 0 TO FDINGREDKEYS
                   WRITE FDINGREDKEYS
                   END-WRITE
               CLOSE FXKEYS
           ELSE
               CLOSE FXKEYS
           END-IF
       EXIT SECTION.

       110-GET-INGREDLY-ID SECTION.
           OPEN I-O FXKEYS
               READ FXKEYS
                   ADD 1 TO FDINGREDKEYS
       EXIT SECTION.

       115-GET-NAME SECTION.
           MOVE SPACE TO REG-INGRED-NAME
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE ACCEPT REG-INGRED-NAME
           CALL "LOWERUPPER" USING BY REFERENCE WSINGREDS-NAME
           MOVE TRIM(WSINGREDS-NAME) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSINGREDS-NAME
           IF WSINGREDS-NAME EQUAL SPACES THEN
               MOVE ERROR-NAME TO ERROR-TEXT ACCEPT ERROR-ZONE
           END-IF
       EXIT SECTION.

       120-GET-DESCRIPTION SECTION.
           MOVE SPACE TO REG-INGRED-DESCRIPTION
           MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-TEXT ACCEPT REG-INGRED-DESCRIPTION
           CALL "LOWERUPPER" USING BY REFERENCE WSINGREDS-DESCRIPTION1
           MOVE TRIM(WSINGREDS-DESCRIPTION1) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
              MOVE UNSTR TO WSINGREDS-DESCRIPTION1


       EXIT SECTION.

        150-WRITE-RECORD SECTION.
           REWRITE FDINGREDKEYS
           END-REWRITE
           CLOSE FXKEYS
           WRITE INGREDS-DETAILS FROM WSINGREDS-DETAILS
           END-WRITE
           CLOSE FXINGREDLY
           MOVE MESSAGE-WRITE-YES TO ERROR-TEXT ACCEPT ERROR-ZONE
       EXIT SECTION.

       155-REMOVE-EXTRA-SPACES SECTION.
           MOVE SPACE TO UNSTR1 UNSTR2 UNSTR3 UNSTR4 UNSTR5
           UNSTR6 UNSTR7 UNSTR8 UNSTR9 UNSTR10
           UNSTRING UNSTR DELIMITED BY ALL SPACES INTO UNSTR1
               UNSTR2 UNSTR3 UNSTR4 UNSTR5 UNSTR6 UNSTR7 UNSTR8 UNSTR9
               UNSTR10
           STRING UNSTR1 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR2 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR3 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR4 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR5 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR6 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR7 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR8 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR9 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR10 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
           INTO UNSTR
       EXIT SECTION.
