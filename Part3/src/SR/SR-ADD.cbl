      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SR-ADD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "CP-SELECTS-SR".
       DATA DIVISION.
       FILE SECTION.
           COPY "FD-FS-SR".
       WORKING-STORAGE SECTION.
       COPY "CB-WS-SR".
       COPY "SR-CONST".

       01  SR-TABLE OCCURS 1 TO MAX-SR TIMES
           DEPENDING ON NUMBER-SR
           INDEXED BY SR-INDEX.
           05 TABLE-SR-IID                     PIC 9(003).
           05 TABLE-SR-EID                     PIC X(005).
           05 TABLE-SR-S-DESC                  PIC X(025).
           05 TABLE-SR-L-DESC.
           10 TABLE-SR-L-DESC1                 PIC X(025).
           10 TABLE-SR-L-DESC2                 PIC X(025).
       01  ING-TABLE OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLE-ING-ID                      PIC 9(003).
           05 TABLE-ING-NAME                    PIC X(030).
           05 TABLE-ING-DESCRIPTION             PIC X(050).
           05 TABLE-ING-UNIT-SUPPLIER           PIC X(003).
           05 TABLE-ING-UNIT-SANDWICH           PIC X(003).
           05 TABLE-TRESHOLD                        PIC 9(003).
           05 TABLE-ING-IS-ACTIVE               PIC 9(001).
       01  CAT-TABLE OCCURS 1 TO MAX-CAT TIMES
           DEPENDING ON NUMBER-CAT
           INDEXED BY CAT-INDEX.
           05 TABLE-CAT-ID                          PIC 9(003).
           05 TABLE-CAT-NAME                        PIC X(030).
           05 TABLE-CAT-DESCRIPTION                 PIC X(050).
           05 TABLE-CAT-IS-ACTIVE                   PIC 9(001).
       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(023) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 26 COL 98.
           05 VALUE BACK-EXIT
               LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT LINE 9 COL 17.
           05 VALUE ADD-MENU-TEXT1 LINE 12 COL 13.
           05 VALUE ADD-MENU-TEXT2 LINE 13 COL 13.
           05 VALUE ADD-MENU-TEXT3 LINE 14 COL 13.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 62 BACKGROUND-COLOR 7.
           05 REG-REC.
               10 REG-EID PIC X(008) LINE 12 COL 32
                   TO WS-SR-EID.
               10 REG-S-DESCRIPTION PIC A(025) LINE 13 COL 32
                       TO WS-SR-S-DESCRIPTION.
               10 REG-L-DESCRIPTION.
                   15 REG-L-DESIGNATION1 PIC A(025) LINE 14 COL 32
                       TO WS-SR-L-DESCRIPTION1 AUTO.
                   15 REG-L-DESIGNATION2 PIC A(025) LINE 15 COL 32
                       TO WS-SR-L-DESCRIPTION2.
      ******************************************************************
       01  INSTRUCTIONS-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 INSTRUCTION-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01  ERROR-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 ERROR-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY LINE 27 COL 01 PIC X TO DUMMY AUTO.
      ******************************************************************
       01  CONFIRM-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 CONFIRM-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY LINE 27 COL 01 PIC X TO DUMMY AUTO.
      ******************************************************************
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08 COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 22 COL 110 BACKGROUND-COLOR 7.
      ******************************************************************
       01  GET-INGREDID
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
      *     05 VALUE MESSAGE-GET-INGREDID LINE 25 COL 15
      *         FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 NEW-INGREDID LINE 25 COL PLUS 1 PIC 9(003)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
           05 MESSAGE-LIST-PAGE LINE 25 COL 56 PIC X(030).
      ******************************************************************
       01  INGREDIENT-LIST1.
           05 LIST-INGRED-ID1 PIC 9(003) LINE ILIN COL ICOL
               FROM ING-TABLE (ING-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLE-ING-NAME (ING-INDEX).
      ******************************************************************
       01  CATEGORY-LIST1.
           05 LIST-CAT-ID1 PIC 9(003) LINE ILIN COL ICOL
               FROM CAT-TABLE (CAT-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-CAT-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLE-CAT-NAME (CAT-INDEX).
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 800-FILE-CHECK.
       050-OBTAIN-TABLES SECTION.
           DISPLAY "01-TABELA ING"
           SET ING-INDEX TO 1
           OPEN INPUT INGREDIENTS
           PERFORM UNTIL EOFINGREDS
               READ INGREDIENTS NEXT RECORD
                   AT END
                       SET EOFINGREDS TO TRUE
                       MOVE ING-INDEX TO NUMBER-ING
                   NOT AT END
                       PERFORM 060-LOAD-ING-TABLE
               END-READ
           END-PERFORM
           CLOSE INGREDIENTS.
           SET CAT-INDEX TO 1
           OPEN INPUT CATEGORIES
           PERFORM UNTIL EOFCATEGORY
               READ CATEGORIES NEXT RECORD
                   AT END
                       SET EOFCATEGORY TO TRUE
                       MOVE CAT-INDEX TO NUMBER-CAT
                   NOT AT END
                       PERFORM 070-LOAD-CAT-TABLE
               END-READ
           END-PERFORM
           CLOSE CATEGORIES
           EXIT SECTION.
       060-LOAD-ING-TABLE SECTION.
           MOVE INGREDS-DETAILS TO ING-TABLE (ING-INDEX)
           SET ING-INDEX UP BY 1
           EXIT SECTION.
       070-LOAD-CAT-TABLE SECTION.
           MOVE CATEGORY-DETAILS TO CAT-TABLE (CAT-INDEX)
           SET CAT-INDEX UP BY 1
           EXIT SECTION.
       100-MAIN SECTION.
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           PERFORM 900-CLEAR-VARIABLES
           PERFORM 110-REGISTER
               IF KEY-STATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
           EXIT PROGRAM.
       110-REGISTER SECTION.
           PERFORM 120-OBTAIN-IID
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 130-OBTAIN-EID
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 140-OBTAIN-SHORT-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 150-OBTAIN-LONG-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 160-OBTAIN-CATEGORIES
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 170-OBTAIN-INGREDIENTS
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           EXIT SECTION.
       120-OBTAIN-IID SECTION.
           MOVE ZERO TO REG-UNIQUE
           OPEN INPUT KEYS
               READ KEYS
                   ADD 1 TO REGKEY
                   MOVE REGKEY TO WS-SR-IID
           CLOSE KEYS
           PERFORM 180-IID-EXISTS
           EXIT SECTION.
       130-OBTAIN-EID SECTION.
           PERFORM WITH TEST AFTER UNTIL REG-UNIQUE = 1
           MOVE ZERO TO REG-UNIQUE
           MOVE EID-INSTR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
           ACCEPT REG-EID
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           PERFORM 190-EID-EXISTS
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           MOVE SPACES TO LINK-TEXT
           MOVE UPPER-CASE(WS-SR-EID) TO WS-SR-EID
           MOVE TRIM(WS-SR-EID) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-EID
           MOVE LINK-TEXT TO REG-EID
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       140-OBTAIN-SHORT-DESCRIPTION SECTION.
           MOVE S-DESCR-INSTR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
           ACCEPT REG-S-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           MOVE UPPER-CASE(WS-SR-S-DESCRIPTION) TO WS-SR-S-DESCRIPTION
           MOVE TRIM(WS-SR-S-DESCRIPTION) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-S-DESCRIPTION
           MOVE LINK-TEXT TO REG-S-DESCRIPTION
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       150-OBTAIN-LONG-DESCRIPTION SECTION.
           MOVE L-DESCR-INSTR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
           ACCEPT REG-L-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           MOVE UPPER-CASE(WS-SR-L-DESCRIPTION) TO WS-SR-L-DESCRIPTION
           MOVE TRIM(WS-SR-L-DESCRIPTION) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-L-DESCRIPTION
           MOVE LINK-TEXT TO REG-L-DESCRIPTION
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       160-OBTAIN-CATEGORIES SECTION.
       170-OBTAIN-INGREDIENTS SECTION.
       180-IID-EXISTS SECTION.
           PERFORM WITH TEST AFTER UNTIL REG-UNIQUE = 1
               MOVE WS-SR-IID TO SR-IID
               OPEN INPUT SANDWICHES
                   READ SANDWICHES
                       NOT INVALID KEY
                           MOVE ZERO TO REG-UNIQUE
                       INVALID KEY
                           MOVE 1 TO REG-UNIQUE
                   END-READ
               CLOSE SANDWICHES
               IF REG-UNIQUE = 0 THEN
                   ADD 1 TO WS-SR-IID
               END-IF
           END-PERFORM
           EXIT SECTION.
       190-EID-EXISTS SECTION.
           MOVE WS-SR-EID TO SR-EID
           OPEN INPUT SANDWICHES
               READ SANDWICHES
                   NOT INVALID KEY
                       MOVE ZERO TO REG-UNIQUE
                       MOVE ERROR-EID TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                           IF KEY-STATUS = F3 THEN
                               EXIT SECTION
                           END-IF
                   INVALID KEY
                       MOVE 1 TO REG-UNIQUE
               END-READ
           CLOSE SANDWICHES
           EXIT SECTION.
       200-LISTAGEM-ING SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           MOVE ZEROES TO NEW-INGREDID
           MOVE SPACES TO TRUE-YES
           SET ING-INDEX TO 1
           MOVE 09 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 12 TO PAGINA
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               DISPLAY INGREDIENT-LIST1
               ADD 1 TO ILIN
               ADD 1 TO PAGINA
               SET ING-INDEX UP BY 1
               IF ILIN = 21 THEN
                   ACCEPT GET-INGREDID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS =1001 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       SET ING-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 12 TO PAGINA
                   ELSE
                       IF KEY-STATUS = 1002 THEN
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           MOVE 09 TO ILIN
                           ADD 1 TO COUNTPAGE
                           MOVE 12 TO PAGINA
                       ELSE
                           EXIT SECTION
                       END-IF
                   END-IF
               END-IF
               IF ING-INDEX >= NUMBER-ING
                   ACCEPT GET-INGREDID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS =1001 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       SET ING-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 12 TO PAGINA
                   END-IF
               END-IF
           END-PERFORM
      *>     ACCEPT GET-INGREDID
      *>     IF KEYSTATUS = 1003 THEN
      *>         EXIT SECTION
      *>     END-IF
       EXIT SECTION.
       210-LISTAGEM-CAT SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           MOVE ZEROES TO NEW-INGREDID
           MOVE SPACES TO TRUE-YES
           SET ING-INDEX TO 1
           MOVE 09 TO ILIN
           MOVE 11 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 24 TO PAGINA
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               DISPLAY INGREDIENT-LIST1
               ADD 1 TO ILIN
               ADD 1 TO PAGINA
               SET ING-INDEX UP BY 1
               IF ILIN = 21 AND ICOL = 11 THEN
                   MOVE 09 TO ILIN
                   MOVE 51 TO ICOL
               ELSE
                   IF ILIN = 21 AND ICOL = 51 THEN
      *                 MOVE NEXT-PAGE TO MESSAGE-LIST-PAGE
                       ACCEPT GET-INGREDID
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS =1001 AND COUNTPAGE > 1
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           MOVE 09 TO ILIN
                           MOVE 11 TO ICOL
                           SET ING-INDEX DOWN BY PAGINA
                           SUBTRACT 1 FROM COUNTPAGE
                           MOVE 24 TO PAGINA
                       ELSE
                           IF KEY-STATUS = 1002 THEN
                               DISPLAY CLEAR-SCREEN
                               DISPLAY MAIN-SCREEN
                               DISPLAY LIST-FRAME
                               MOVE 09 TO ILIN
                               MOVE 11 TO ICOL
                               ADD 1 TO COUNTPAGE
                               MOVE 24 TO PAGINA
                           ELSE
                               EXIT SECTION
                           END-IF
                       END-IF
                   END-IF
               END-IF
               IF ING-INDEX >= NUMBER-ING
                   ACCEPT GET-INGREDID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS =1001 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       MOVE 11 TO ICOL
                       SET ING-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 24 TO PAGINA
                   END-IF
               END-IF
           END-PERFORM
      *>     ACCEPT GET-INGREDID
      *>     IF KEYSTATUS = 1003 THEN
      *>         EXIT SECTION
      *>     END-IF
       EXIT SECTION.
       700-SPACE-CHECK SECTION.
      *    SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO
           SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           UNSTRING LINK-TEXT DELIMITED BY ALL SPACES INTO
               SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           STRING
               SPACE-CHECK1 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK2 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK3 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK4 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK5 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK6 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK7 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK8 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK9 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK10 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK11 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK12 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK13 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK14 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK15 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               INTO LINK-TEXT
           EXIT SECTION.
       800-FILE-CHECK SECTION.
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SANDWICHES
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SANDWICHES
                   CLOSE SANDWICHES
               END-IF
           CLOSE SANDWICHES
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O CATEGORIES
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT CATEGORIES
                   CLOSE CATEGORIES
               END-IF
           CLOSE CATEGORIES
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O INGREDIENTS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT INGREDIENTS
                   CLOSE INGREDIENTS
               END-IF
           CLOSE INGREDIENTS
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SR-ING
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SR-ING
                   CLOSE SR-ING
               END-IF
           CLOSE SR-ING
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SR-CAT
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SR-CAT
                   CLOSE SR-CAT
               END-IF
           CLOSE SR-CAT
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O KEYS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT KEYS
                       MOVE ZEROS TO REGKEY
                       WRITE REGKEY
                   CLOSE KEYS
               END-IF
           CLOSE KEYS
           EXIT SECTION.
      ******************************************************************
       900-CLEAR-VARIABLES SECTION.
           MOVE ZEROS TO WS-SR-IID
           MOVE SPACES TO WS-SR-EID WS-SR-S-DESCRIPTION
           WS-SR-L-DESCRIPTION REG-EID REG-S-DESCRIPTION
           REG-L-DESCRIPTION
           EXIT SECTION.
       END PROGRAM SR-ADD.
