      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-ADD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCHOOLS ASSIGN TO "SCHOOLS"
           ORGANIZATION IS INDEXED
           RECORD KEY IS SCHOOL-INTERNAL-ID
           ALTERNATE KEY IS SCHOOL-EXTERNAL-ID
           ALTERNATE KEY IS SCHOOL-TOWN
           WITH DUPLICATES
           ALTERNATE KEY IS SCHOOL-POSTAL-CODE
           WITH DUPLICATES
           ACCESS IS DYNAMIC
           FILE STATUS IS FILE-STATUS.

           SELECT SCHOOLS1 ASSIGN TO "SCHOOLS1.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KEYS ASSIGN TO "KEYS-SCM.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SCHOOLS.
       01  SCHOOL-DETAILS.
           88 EOFSCHOOLS                           VALUE HIGH-VALUES.
           05 SCHOOL-INTERNAL-ID                   PIC 9(003).
           05 SCHOOL-EXTERNAL-ID                   PIC X(008).
           05 SCHOOL-DESIGNATION.
               10 SCHOOL-DESIGNATION1              PIC X(050).
               10 SCHOOL-DESIGNATION2              PIC X(050).
               10 SCHOOL-DESIGNATION3              PIC X(050).
           05 SCHOOL-ADRESS.
               10 SCHL-ADR-MAIN.
                   15 SCHL-ADR-MAIN1               PIC X(050).
                   15 SCHL-ADR-MAIN2               PIC X(050).
               10 SCHOOL-POSTAL-CODE.
                   15 SCHL-POSTAL-CODE1            PIC 9(004).
                   15 SCHL-POSTAL-CODE2            PIC 9(003).
               10 SCHOOL-TOWN                      PIC X(030).
           05 SCHOOL-IS-ACTIVE                     PIC 9(001).

       FD  SCHOOLS1.
           01 SCHOOL1                              PIC X(200).
       FD  KEYS.
           01 FD-KEYS.
               05 REGKEY                           PIC 9(003).
       WORKING-STORAGE SECTION.
       01  WS-SCHOOL-DETAILS.
           05 WS-SCHOOL-INTERNAL-ID                PIC 9(003).
           05 WS-SCHOOL-EXTERNAL-ID                PIC X(008).
               88 EXTERNAL-ID-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
           05 WS-SCHOOL-DESIGNATION.
               88 DESIGNATION-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
               10 WS-SCHOOL-DESIGNATION1           PIC X(050).
               10 WS-SCHOOL-DESIGNATION2           PIC X(050).
               10 WS-SCHOOL-DESIGNATION3           PIC X(050).
           05 WS-SCHOOL-ADRESS.
               10 WS-SCHL-ADR-MAIN.
                   88 ADDRESS-VLD                   VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES,
                                                   "0" THRU "9".
                   15 WS-SCHL-ADR-MAIN1            PIC X(050).
                   15 WS-SCHL-ADR-MAIN2            PIC X(050).
               10 WS-SCHOOL-POSTAL-CODE.
                   15 WS-SCHL-POSTAL-CODE1         PIC 9(004).
                       88 POSTAL-CODE1-VLD         VALUE "1000" THRU
                                                   "9999".
                   15 WS-SCHL-POSTAL-CODE2         PIC 9(003).
                       88 POSTAL-CODE2-VLD         VALUE "000" THRU
                                                   "999".
               10 WS-SCHOOL-TOWN                   PIC X(030).
                   88 TOWN-VLD                     VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
           05 WS-SCHOOL-IS-ACTIVE                  PIC 9(001).
       01  WS-OPTION                               PIC 9(002).
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
       01  WS-ADD                                  PIC X(001).
           88  ADD-VLD                             VALUE "Y", "S", "N".
       01  WS-EID-VLD                              PIC 9(001).
       COPY "CONSTANTSPT".
       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE BACK-EXIT
               LINE 25 COL 99 FOREGROUND-COLOR 5.

       01  MAIN-REGISTER-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           05 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 16 COL 35.
           05 VALUE ADD-MENU-OPTION1 LINE 11 COL 42.
           05 VALUE ADD-MENU-OPTION2 LINE 12 COL 42.
           05 VALUE ADD-MENU-OPTION3 LINE 13 COL 42.
           05 VALUE ADD-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           05 MP-OPTION PIC 9(02) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT LINE 9 COL 40.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 22.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 22.
           05 VALUE "-" LINE 18 COL 45.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 22.
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
           05 REG-IID PIC 9(003) LINE 11 COL 40
                   USING WS-SCHOOL-INTERNAL-ID
                   BLANK WHEN ZERO.
           05 REG-REC.
               10 REG-EED PIC X(008) LINE 12 COL 40
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 REG-DESIGNATION.
                   15 REG-DESIGNATION1 PIC X(050) LINE 13 COL 40
                       TO WS-SCHOOL-DESIGNATION1.
                   15 REG-DESIGNATION2 PIC X(050) LINE 14 COL 40
                       TO WS-SCHOOL-DESIGNATION2.
                   15 REG-DESIGNATION3 PIC X(050) LINE 15 COL 40
                       TO WS-SCHOOL-DESIGNATION3.
               10 REG-ADDRESS.
                   15 REG-ADDRESS1 PIC X(050) LINE 16 COL 40
                       TO WS-SCHL-ADR-MAIN1.
                   15 REG-ADDRESS2 PIC X(050) LINE 17 COL 40
                       TO WS-SCHL-ADR-MAIN2.
               10 REG-POSTAL-CODE.
                   15 REG-PC1 PIC 9(004) LINE 18 COL 40
                       TO WS-SCHL-POSTAL-CODE1
                       BLANK WHEN ZERO.
                   15 REG-PC2 PIC 9(003) LINE 18 COL 47
                       TO WS-SCHL-POSTAL-CODE2
                       BLANK WHEN ZERO.
               10 REG-TOWN PIC X(030) LINE 19 COL 40
                   TO WS-SCHOOL-TOWN.

       01  SAVE-RECORD-MENU1
           REQUIRED, BACKGROUND-COLOR 7.
           03 VALUE ADD-MENU-TEXT10
               LINE 25 COL 10 FOREGROUND-COLOR 4.
           03 SRM1-OPTION            PIC X(01) LINE 25 COL 54
               TO WS-ADD
                   FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  OPTION-INVALID-SCREEN.
           05 VALUE OPTION-INVALID-TEXT LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

      * 01  SAVE-RECORD-MENU2
      *     REQUIRED, BACKGROUND-COLOR 7.
      *     03 VALUE ADD-MENU-TEXT11
      *         LINE 25 COL 10 FOREGROUND-COLOR 5.
      *     03 SRM2-OPTION            PIC X(01) LINE 25 COL 61
      *         TO WS-ADD
      *             FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM CHECK-FILE
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 3
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY MAIN-REGISTER-SCREEN
               MOVE ZERO TO MP-OPTION
               ACCEPT MP-OPTION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM REGISTER-MANUAL
                   WHEN 2
                       PERFORM REGISTER-CSV
                   WHEN OTHER
                       DISPLAY OPTION-INVALID-SCREEN
           END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.

       REGISTER-MANUAL SECTION.
           MOVE SPACES TO WS-ADD
               MOVE SPACES TO  WS-SCHOOL-EXTERNAL-ID,
                               WS-SCHOOL-DESIGNATION, WS-SCHOOL-ADRESS,
                               WS-SCHOOL-TOWN
               MOVE SPACES TO REG-EED, REG-DESIGNATION, REG-ADDRESS,
                               REG-TOWN
               MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID,WS-SCHOOL-POSTAL-CODE
                             WS-SCHOOL-IS-ACTIVE
               MOVE ZEROS TO REG-IID, REG-POSTAL-CODE
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY REGISTER-SCREEN
               PERFORM REGISTER-INTERNAL-ID
               PERFORM REGISTER-EXTERNAL-ID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
               PERFORM REGISTER-DESIGNATION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
               PERFORM REGISTER-ADDRESS
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
               PERFORM CONFIRM-REGISTER
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
           EXIT SECTION.

       REGISTER-INTERNAL-ID SECTION.
           OPEN INPUT KEYS
               READ KEYS
                   ADD 1 TO REGKEY
               MOVE REGKEY TO WS-SCHOOL-INTERNAL-ID
           CLOSE KEYS
           MOVE WS-SCHOOL-INTERNAL-ID TO REG-IID
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.

       REGISTER-EXTERNAL-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL EXTERNAL-ID-VLD
               ACCEPT REG-EED
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTER-DESIGNATION SECTION.
           PERFORM WITH TEST AFTER UNTIL DESIGNATION-VLD
               ACCEPT REG-DESIGNATION
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTER-ADDRESS SECTION.
           PERFORM WITH TEST AFTER UNTIL ADDRESS-VLD
               ACCEPT REG-ADDRESS
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM


           PERFORM WITH TEST AFTER UNTIL POSTAL-CODE1-VLD AND
               POSTAL-CODE2-VLD
               ACCEPT REG-PC1
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
           END-IF
               ACCEPT REG-PC2
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM

           CALL "CPS" USING BY REFERENCE WS-SCHOOL-DETAILS
           MOVE WS-SCHOOL-TOWN TO REG-TOWN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL TOWN-VLD
                ACCEPT REG-TOWN
                IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM.
           EXIT SECTION.

       CONFIRM-REGISTER SECTION.

           MOVE "Y" TO SRM1-OPTION
           PERFORM WITH TEST AFTER UNTIL ADD-VLD
               ACCEPT SAVE-RECORD-MENU1
               MOVE SPACES TO SRM1-OPTION
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM.
               MOVE FUNCTION UPPER-CASE(WS-ADD) TO WS-ADD
           EVALUATE TRUE
               WHEN WS-ADD = "S"
                   OPEN I-O SCHOOLS
                       PERFORM LOWER-UPPER
                       MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
                       MOVE 1 TO SCHOOL-IS-ACTIVE
                       WRITE SCHOOL-DETAILS
                   CLOSE SCHOOLS
                   OPEN OUTPUT KEYS
                       MOVE WS-SCHOOL-INTERNAL-ID TO REGKEY
                       WRITE FD-KEYS
                   CLOSE KEYS
               WHEN WS-ADD = "Y"
                   OPEN I-O SCHOOLS
                       MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
                       WRITE SCHOOL-DETAILS
                   CLOSE SCHOOLS
                   OPEN OUTPUT KEYS
                       MOVE WS-SCHOOL-INTERNAL-ID TO REGKEY
                       WRITE FD-KEYS
                   CLOSE KEYS
           END-EVALUATE
           EXIT SECTION.

       REGISTER-CSV SECTION.
       CHECK-FILE SECTION.
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SCHOOLS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SCHOOLS
                   CLOSE SCHOOLS
               END-IF
           CLOSE SCHOOLS
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

       CHECK-KEY SECTION.
           IF KEY-STATUS = 1003 THEN
               EXIT SECTION
           END-IF
           IF KEY-STATUS = 1004 THEN
               STOP RUN
           END-IF
           EXIT SECTION.

       LOWER-UPPER SECTION.
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-EXTERNAL-ID) TO
           WS-SCHOOL-EXTERNAL-ID
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-DESIGNATION) TO
           WS-SCHOOL-DESIGNATION
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-ADRESS) TO
           WS-SCHOOL-ADRESS
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-TOWN) TO WS-SCHOOL-TOWN
           EXIT SECTION.
       END PROGRAM SCM-ADD.
