      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-ALT.
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
           WITH DUPLICATES
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
           88 WS-EOF                               VALUE HIGH-VALUES.
           05 WS-SCHOOL-INTERNAL-ID                PIC 9(003).
           05 WS-SCHOOL-EXTERNAL-ID                PIC X(008).
           05 WS-SCHOOL-DESIGNATION.
               10 WS-SCHOOL-DESIGNATION1           PIC X(050).
               10 WS-SCHOOL-DESIGNATION2           PIC X(050).
               10 WS-SCHOOL-DESIGNATION3           PIC X(050).
           05 WS-SCHOOL-ADRESS.
               10 WS-SCHL-ADR-MAIN.
                   15 WS-SCHL-ADR-MAIN1            PIC X(050).
                   15 WS-SCHL-ADR-MAIN2            PIC X(050).
               10 WS-SCHOOL-POSTAL-CODE.
                   15 WS-SCHL-POSTAL-CODE1         PIC 9(004).
                   15 WS-SCHL-POSTAL-CODE2         PIC 9(003).
               10 WS-SCHOOL-TOWN                   PIC X(030).
       01  WS-OPTION                                PIC 9(002).
           88 OPTION-VLD                            VALUE
                                                   "1","2","3","4".
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
       01  EDIT-WHAT                               PIC 9(002).
       01  PRESS-KEY                               PIC X(001).
       01  SC-LINE                                 PIC 9(004).
       01  FLAG                                    PIC X(001).
       01  WS-CONTROL                              PIC 9(001).
       COPY "CONSTANTSPT".

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

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

       01  VIEW-SCREEN
           REQUIRED.
           05 VALUE ALT-MENU-TEXT LINE 9 COL 40.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 22.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 22.
           05 VALUE "-" LINE 18 COL 45.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 25.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 18
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
           05 ALT-REC.
               10 ALT-IID PIC 9(003) LINE 11 COL 40 BLANK WHEN ZERO.
               10 ALT-EED PIC X(008) LINE 12 COL 40
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 ALT-DESIGNATION.
                   15 ALT-DESIGNATION1 PIC X(050) LINE 13 COL 40
                       TO WS-SCHOOL-DESIGNATION1.
                   15 ALT-DESIGNATION2 PIC X(050) LINE 14 COL 40
                       TO WS-SCHOOL-DESIGNATION2.
                   15 ALT-DESIGNATION3 PIC X(050) LINE 15 COL 40
                       TO WS-SCHOOL-DESIGNATION3.
               10 ALT-ADDRESS.
                   15 ALT-ADDRESS1 PIC X(050) LINE 16 COL 40
                       TO WS-SCHL-ADR-MAIN1.
                   15 ALT-ADDRESS2 PIC X(050) LINE 17 COL 40
                       TO WS-SCHL-ADR-MAIN2.
               10 ALT-POSTAL-CODE.
                   15 ALT-PC1 PIC 9(004) LINE 18 COL 40
                        TO WS-SCHL-POSTAL-CODE1 BLANK WHEN ZERO.
                   15 ALT-PC2 PIC 9(003) LINE 18 COL 47
                        TO WS-SCHL-POSTAL-CODE2 BLANK WHEN ZERO.
               10 ALT-TOWN PIC X(030) LINE 19 COL 40
                       TO WS-SCHOOL-TOWN.

        01 EDIT-WHAT-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(22) LINE 07 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 08 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 09 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 10 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 11 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 12 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 13 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 14 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 15 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 16 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 17 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 18 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 19 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 20 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 21 COL 98.
           05 VALUE ALL " " PIC X(22) LINE 22 COL 98.
           05 VALUE WHAT-TO-EDIT LINE 08 COL 103.
           05 VALUE EDIT1 LINE 10 COL 100.
           05 VALUE EDIT2 LINE 11 COL 100.
           05 VALUE EDIT3 LINE 12 COL 100.
           05 VALUE EDIT4 LINE 13 COL 100.
           05 VALUE EDIT5 LINE 14 COL 100.
           05 VALUE EDIT6 LINE 14 COL 100.
           05 VALUE CHOOSE LINE 20 COL 100.
           05 EDIT-CHOICE PIC 9(002) LINE 20 COL 117 BLANK WHEN ZERO
               REQUIRED TO EDIT-WHAT.

       01  LIST-SCREEN FOREGROUND-COLOUR 7 BACKGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(112) LINE 07 COL 05
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(112) LINE 22 COL 05
           BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 115 BACKGROUND-COLOR 7.
           05  SHOW LINE SC-LINE COL 10.
               10  SHOW-IID PIC 9(003)     FROM SCHOOL-INTERNAL-ID.
               10  VALUE "   ".
               10  SHOW-EED PIC X(008)     FROM SCHOOL-EXTERNAL-ID.
               10  VALUE "   ".
               10  SHOW-DESG PIC X(050)    FROM SCHOOL-DESIGNATION.
               10  VALUE "   ".
               10  SHOW-TOWN PIC X(030)    FROM SCHOOL-TOWN.
           05 VALUE ALT-MENU-OPTION LINE 25 COL 10
           FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           05  CONTINUE-LIST.
               10  CONTINUE-IID PIC 9(003) LINE 25 COL 46
               TO SCHOOL-INTERNAL-ID
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.

       01  END-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           03 VALUE END-OF-LIST-TEXT LINE 25 COL 70.

       01  EMPTY-LIST-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE EMPTY-LIST-TEXT LINE 25 COL 10.
           05  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

       01  NEXT-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE NEXT-LIST-TEXT LINE 25 COL 70.

       01  ID-ERROR-SCREEN
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 VALUE ID-ERROR-TEXT LINE 25 COL 10.

       PROCEDURE DIVISION.
       MAIN SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           PERFORM LIST
               IF FLAG = "Y" THEN
                 EXIT SECTION
              END-IF
              IF KEY-STATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              IF KEY-STATUS = 1004 THEN
                 EXIT PROGRAM
              END-IF
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           PERFORM WITH TEST AFTER UNTIL WS-CONTROL = 1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           OPEN INPUT SCHOOLS
               READ SCHOOLS
               INVALID KEY
                   DISPLAY ID-ERROR-SCREEN
                   MOVE SPACES TO CONTINUE-LIST
                   ACCEPT CONTINUE-LIST
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
               NOT INVALID KEY
                   MOVE SCHOOL-DETAILS TO ALT-REC
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY VIEW-SCREEN
                   ACCEPT OMITTED AT LINE 25 COL 10
                   MOVE 1 TO WS-CONTROL
               END-READ
           CLOSE SCHOOLS
           END-PERFORM
           PERFORM WHAT-TO-EDIT
           EXIT PROGRAM.

       WHAT-TO-EDIT SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 6
               DISPLAY EDIT-WHAT-SCREEN
               ACCEPT EDIT-CHOICE
               EVALUATE TRUE

                   WHEN EDIT-WHAT = 1
                       PERFORM EDIT-EED

                   WHEN EDIT-WHAT = 2
                       PERFORM EDIT-DESIGNATION

                   WHEN EDIT-WHAT = 3
                       PERFORM EDIT-ADDRESS

                   WHEN EDIT-WHAT = 4
                       PERFORM EDIT-POSTAL-CODE

                   WHEN EDIT-WHAT = 5
                       PERFORM EDIT-TOWN

           END-EVALUATE
           END-PERFORM
       EXIT SECTION.

       LIST SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-SCREEN
           MOVE SPACES TO FLAG
           MOVE SPACES TO CONTINUE-LIST
           MOVE ZERO TO SCHOOL-INTERNAL-ID
           OPEN INPUT SCHOOLS
           START SCHOOLS KEY IS GREATER OR EQUAL SCHOOL-INTERNAL-ID
              INVALID KEY
                 ACCEPT EMPTY-LIST-SCREEN
                 MOVE "Y" TO FLAG
                 IF FLAG = "Y" THEN
                    CLOSE SCHOOLS
                    EXIT SECTION
                 END-IF
           END-START
           MOVE 9 TO SC-LINE
           PERFORM UNTIL WS-EOF
              READ SCHOOLS NEXT RECORD
                 AT END SET WS-EOF TO TRUE
                    DISPLAY END-LIST-SCREEN
                    ACCEPT CONTINUE-LIST
                    MOVE "S" TO FLAG
                    IF FLAG = "S" THEN
                       CLOSE SCHOOLS
                       EXIT SECTION
                    END-IF
                    IF KEY-STATUS = 1003 THEN
                       CLOSE SCHOOLS
                       EXIT SECTION
                    END-IF
                    IF KEY-STATUS = 1004 THEN
                       CLOSE SCHOOLS
                       EXIT PROGRAM
                    END-IF
                 NOT AT END
                    DISPLAY LIST-SCREEN
                    ADD 01 TO SC-LINE
                    IF SC-LINE = 20 THEN
                       DISPLAY NEXT-LIST-SCREEN
                       ACCEPT CONTINUE-LIST
                       IF KEY-STATUS = 1002 THEN
                          DISPLAY CLEAR-SCREEN
                          DISPLAY MAIN-SCREEN
                          DISPLAY LIST-SCREEN
                          MOVE 9 TO SC-LINE
                       ELSE
                          MOVE "S" TO FLAG
                          IF FLAG = "S" THEN
                             CLOSE SCHOOLS
                             EXIT SECTION
                          END-IF
                       END-IF
                       IF KEY-STATUS = 1003 THEN
                          EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                          EXIT PROGRAM
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT SECTION.

       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN ALT-EED ALT-DESIGNATION
           ALT-ADDRESS ALT-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           ALT-IID ALT-POSTAL-CODE
           EXIT SECTION.

       EDIT-EED SECTION.

       EDIT-DESIGNATION SECTION.

       EDIT-ADDRESS SECTION.

       EDIT-POSTAL-CODE SECTION.

       EDIT-TOWN SECTION.

       END PROGRAM SCM-ALT.
