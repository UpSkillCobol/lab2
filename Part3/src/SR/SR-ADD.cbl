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
           05 VALUE ADD-MENU-TEXT LINE 9 COL 40.
           05 VALUE ADD-MENU-TEXT1 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 14 COL 22.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 18 COL 18
               BACKGROUND-COLOR 7.
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
           05 REG-REC.
               10 REG-EID PIC X(008) LINE 12 COL 41
                   TO WS-SR-EID.
               10 REG-S-DESCRIPTION PIC A(030) LINE 13 COL 41
                       TO WS-SR-S-DESCRIPTION.
               10 REG-L-DESCRIPTION.
                   15 REG-L-DESIGNATION1 PIC A(050) LINE 14 COL 41
                       TO WS-SR-L-DESCRIPTION1 AUTO.
                   15 REG-L-DESIGNATION2 PIC A(050) LINE 15 COL 41
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
       PROCEDURE DIVISION.
       050-OBTAIN-TABLES SECTION.
           EXIT SECTION.
       100-MAIN SECTION.
           PERFORM 900-CLEAR-VARIABLES
           PERFORM 800-FILE-CHECK
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
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
