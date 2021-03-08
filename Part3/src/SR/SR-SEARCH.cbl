      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SR-SEARCH.
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
           05 TABLE-SR-IID                         PIC 9(003).
           05 TABLE-SR-EID                         PIC X(005).
           05 TABLE-SR-S-DESC                      PIC X(025).
           05 TABLE-SR-L-DESC.
           10 TABLE-SR-L-DESC1                     PIC X(025).
           10 TABLE-SR-L-DESC2                     PIC X(025).
       01  ING-TABLE OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLE-ING-ID                         PIC 9(003).
           05 TABLE-ING-NAME                       PIC X(030).
           05 TABLE-ING-DESCRIPTION                PIC X(050).
           05 TABLE-ING-UNIT-SUPPLIER              PIC X(003).
           05 TABLE-ING-UNIT-SANDWICH              PIC X(003).
           05 TABLE-TRESHOLD                       PIC 9(003).
           05 TABLE-ING-IS-ACTIVE                  PIC 9(001).
       01  CAT-TABLE OCCURS 1 TO MAX-CAT TIMES
           DEPENDING ON NUMBER-CAT
           INDEXED BY CAT-INDEX.
           05 TABLE-CAT-ID                         PIC 9(003).
           05 TABLE-CAT-NAME                       PIC X(030).
           05 TABLE-CAT-DESCRIPTION.
               10 TABLE-CAT-DESCRIPTION1           PIC X(050).
           05 TABLE-CAT-IS-ACTIVE                  PIC 9(001).
      *    SI (SANDWICH-INGREDIENT)
       01  SI-TABLE OCCURS 1 TO MAX-SI TIMES
           DEPENDING ON NUMBER-SI
           INDEXED BY SI-INDEX.
           05 TABLE-SI-ID.
               10 TABLE-SI-SAND-ID                 PIC 9(003).
               10 TABLE-SI-ING-ID                  PIC 9(003).
               10 TABLE-SI-ING-QTD                 PIC 9(003).
      *    SC (SANDWICH CATEGORIE)
       01  SC-TABLE OCCURS 1 TO MAX-SC TIMES
           DEPENDING ON NUMBER-SC
           INDEXED BY SC-INDEX.
           05 TABLE-SC-ID.
               10 TABLE-SC-SAND-ID                 PIC 9(003).
               10 TABLE-SC-CAT-ID                  PIC 9(003).
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
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01  CONFIRM-RECORD-SCREEN.
           05 VALUE ALL " " PIC X(107) LINE 7 col 05
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(107) LINE 22 col 05
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 col 110 BACKGROUND-COLOR 7.
           05 CONFIRM-REC.
               10 CONFIRM-SANDWICH-REC.
                   15 CONFIRM-EED PIC X(005) LINE 11 COL 29
                   FROM WS-SR-EID.
                   15 CONFIRM-S-DESCRIPTION PIC X(025) LINE 12 COL 29
                   FROM WS-SR-S-DESCRIPTION.
                   15 CONFIRM-L-DESCRIPTION.
                       20 CONFIRM-L-DESCRIPTION1 PIC X(025)
                       LINE 13 COL 29 FROM WS-SR-L-DESCRIPTION1.
                       20 CONFIRM-L-DESCRIPTION2 PIC X(025)
                       LINE 14 COL 29 FROM WS-SR-L-DESCRIPTION2.
               10 CONFIRM-CATEGORIES-REC.
                   15 CONFIRM-CATEGORIE1 PIC X(060) LINE 15 COL 29
                   FROM WS-CATEGORIES-STRING1.
                   15 CONFIRM-CATEGORIE2 PIC X(060) LINE 16 COL 29
                   FROM WS-CATEGORIES-STRING2.
               10 CONFIRM-INGREDIENTS-REC.
                   15 CONFIRM-INGREDIENT1 PIC X(060) LINE 18 COL 29
                   FROM WS-INGREDIENTS-STRING1.
                   15 CONFIRM-INGREDIENT2 PIC X(060) LINE 19 COL 29
                   FROM WS-INGREDIENTS-STRING2.
                   15 CONFIRM-INGREDIENT3 PIC X(060) LINE 20 COL 29
                   FROM WS-INGREDIENTS-STRING3.
           05 VALUE CONFIRM-TEXT LINE 09 COL 10.
           05 VALUE CONFIRM-TEXT1 LINE 11 COL 10.
           05 VALUE CONFIRM-TEXT2 LINE 12 COL 10.
           05 VALUE CONFIRM-TEXT3 LINE 13 COL 10.
           05 VALUE CONFIRM-TEXT4 LINE 15 COL 10.
           05 VALUE CONFIRM-TEXT5 LINE 18 COL 10.
      ******************************************************************
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08 COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 3 FOREGROUND-COLOR 5.
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
               FROM TABLE-CAT-ID (CAT-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-CAT-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLE-CAT-NAME (CAT-INDEX).
      ******************************************************************
       01  PREVIOUS-NEXT-TEXT.
           05 PREVIOUS-NEXT-MESSAGE PIC X(70) LINE 26 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
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
       PROCEDURE DIVISION.
           MOVE ZEROS TO INGREDIENT-EMPTY SANDWICH-EMPTY CATEGORY-EMPTY
           PERFORM 800-FILE-CHECK
           IF INGREDIENT-EMPTY = 1 THEN
               MOVE NO-INGREDIENTS TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-IF.
       010-OBTAIN-TABLES SECTION.
           SET SR-INDEX TO 1
           OPEN INPUT SANDWICHES
           MOVE 001 TO SR-IID
           START SANDWICHES KEY IS GREATER OR EQUAL SR-IID
               INVALID KEY
               MOVE 1 TO SANDWICH-EMPTY
               CLOSE SANDWICHES
               MOVE NO-SANDWICHES TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           PERFORM UNTIL SR-EOF
               READ SANDWICHES NEXT RECORD
                   AT END
                       SET SR-EOF TO TRUE
                       MOVE SR-INDEX TO NUMBER-SR
                   NOT AT END
                       PERFORM 020-LOAD-SR-TABLE
               END-READ
           END-PERFORM
           CLOSE SANDWICHES
           SET ING-INDEX TO 1
           OPEN INPUT INGREDIENTS
           MOVE 001 TO INGREDS-ID
           START INGREDIENTS KEY IS GREATER OR EQUAL INGREDS-ID
               INVALID KEY
               MOVE 1 TO INGREDIENT-EMPTY
               CLOSE INGREDIENTS
               MOVE NO-INGREDIENTS TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-START
           PERFORM UNTIL EOFINGREDS
               READ INGREDIENTS NEXT RECORD
                   AT END
                       SET EOFINGREDS TO TRUE
                       MOVE ING-INDEX TO NUMBER-ING
                   NOT AT END
                       PERFORM 030-LOAD-ING-TABLE
               END-READ
           END-PERFORM
           CLOSE INGREDIENTS.
           SET CAT-INDEX TO 1
           OPEN INPUT CATEGORIES
           MOVE ZEROS TO INGREDIENT-EMPTY
           MOVE 001 TO CATEGORY-ID
           START CATEGORIES KEY IS GREATER OR EQUAL CATEGORY-ID
               INVALID KEY
               MOVE 1 TO CATEGORY-EMPTY
               CLOSE CATEGORIES
               EXIT PROGRAM
           END-START
           PERFORM UNTIL EOFCATEGORY
               READ CATEGORIES NEXT RECORD
                   AT END
                       SET EOFCATEGORY TO TRUE
                       MOVE CAT-INDEX TO NUMBER-CAT
                   NOT AT END
                       PERFORM 040-LOAD-CAT-TABLE
               END-READ
           END-PERFORM
           CLOSE CATEGORIES
           SET SI-INDEX TO 1
           OPEN INPUT SR-ING
           PERFORM UNTIL SI-EOF
               READ SR-ING NEXT RECORD
                   AT END
                       SET SI-EOF TO TRUE
                       MOVE SI-INDEX TO NUMBER-SI
                   NOT AT END
                       PERFORM 050-LOAD-SI-TABLE
               END-READ
           END-PERFORM
           SET SC-INDEX TO 1
           OPEN INPUT SR-CAT
           PERFORM UNTIL SC-EOF
               READ SR-CAT NEXT RECORD
                   AT END
                       SET SC-EOF TO TRUE
                       MOVE SC-INDEX TO NUMBER-SC
                   NOT AT END
                       PERFORM 060-LOAD-SC-TABLE
                END-READ
           END-PERFORM
           EXIT SECTION.
       020-LOAD-SR-TABLE SECTION.
           MOVE SR-REC TO SR-TABLE (SR-INDEX)
           SET SR-INDEX UP BY 1
           EXIT SECTION.
       030-LOAD-ING-TABLE SECTION.
           MOVE INGREDS-DETAILS TO ING-TABLE (ING-INDEX)
           SET ING-INDEX UP BY 1
           EXIT SECTION.
       040-LOAD-CAT-TABLE SECTION.
           MOVE CATEGORY-DETAILS TO CAT-TABLE (CAT-INDEX)
           SET CAT-INDEX UP BY 1
           EXIT SECTION.
       050-LOAD-SI-TABLE SECTION.
           MOVE SR-ING-REC TO SI-TABLE (SI-INDEX)
           SET SI-INDEX UP BY 1
           EXIT SECTION.
       060-LOAD-SC-TABLE SECTION.
           MOVE SR-CAT-REC TO SC-TABLE (SC-INDEX)
           SET SC-INDEX UP BY 1
           EXIT SECTION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
      ******************************************************************
       800-FILE-CHECK SECTION.
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SANDWICHES
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SANDWICHES
                   CLOSE SANDWICHES
               END-IF
           CLOSE SANDWICHES
           MOVE ZEROS TO FILE-STATUS
           MOVE ZERO TO CATEGORY-EMPTY
           OPEN I-O CATEGORIES
               IF FILE-STATUS = 35 THEN
                   MOVE 1 TO CATEGORY-EMPTY
                   OPEN OUTPUT CATEGORIES
                   CLOSE CATEGORIES
               END-IF
           CLOSE CATEGORIES
           MOVE ZEROS TO FILE-STATUS
           MOVE ZERO TO INGREDIENT-EMPTY
           OPEN I-O INGREDIENTS
               IF FILE-STATUS = 35 THEN
                   MOVE 1 TO INGREDIENT-EMPTY
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
       END PROGRAM SR-SEARCH.
