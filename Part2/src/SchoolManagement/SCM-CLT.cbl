      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-CLT.
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

       DATA DIVISION.
       FILE SECTION.
       FD SCHOOLS.
       COPY "CB-SCHOOLS".
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
       01  KEY-STATUS                              PIC 9(004).
       01  KEY-ADD                                 PIC 9(003).
       01  WS-VIEW                                 PIC X(001).
       01  WS-LINE                                 PIC 9(002).
       01  WS-CONTROL                              PIC 9(001).
       01  SC-LINE                                 PIC 9(004).
       01  SC-COL                                  PIC 9(004).
       01  FLAG                                    PIC X(001).
       01  PRESS-KEY                               PIC X(001).

       COPY "CONSTANTS".
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

       01  MAIN-VIEW-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           05 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           05 VALUE VIEW-MENU-OPTION1 LINE 11 COL 42.
           05 VALUE VIEW-MENU-OPTION2 LINE 12 COL 42.
           05 VALUE VIEW-MENU-OPTION3 LINE 13 COL 42.
           05 VALUE VIEW-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           05 MP-OPTION PIC 9(002) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  PRE-VIEW-IID-SCREEN
           REQUIRED.
           03 VALUE VIEW-MENU-OPTION5 LINE 25 COL 10
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.
           03 VW-OPTION PIC 9(003) LINE 25 COL 44 TO SCHOOL-INTERNAL-ID
           BLANK WHEN ZERO FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  PRE-VIEW-EED-SCREEN
           REQUIRED.
           03 VALUE VIEW-MENU-OPTION5 LINE 25 COL 10.
           03 VW-OPTION1 PIC X(008) LINE 25 COL 44 TO
               WS-SCHOOL-EXTERNAL-ID
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  VIEW-SCREEN
           REQUIRED.
           05 VALUE ADD-MENU-TEXT LINE 9 COL 40.
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
           05 VIEW-REC.
               10 VIEW-IID PIC 9(003) LINE 11 COL 40 BLANK WHEN ZERO.
               10 VIEW-EED PIC X(008) LINE 12 COL 40.
               10 VIEW-DESIGNATION.
                   15 VIEW-DESIGNATION1 PIC X(050) LINE 13 COL 40.
                   15 VIEW-DESIGNATION2 PIC X(050) LINE 14 COL 40.
                   15 VIEW-DESIGNATION3 PIC X(050) LINE 15 COL 40.
               10 VIEW-ADDRESS.
                   15 VIEW-ADDRESS1 PIC X(050) LINE 16 COL 40.
                   15 VIEW-ADDRESS2 PIC X(050) LINE 17 COL 40.
               10 VIEW-POSTAL-CODE.
                   15 VIEW-PC1 PIC 9(004) LINE 18 COL 40
                        BLANK WHEN ZERO.
                   15 VIEW-PC2 PIC 9(003) LINE 18 COL 47
                        BLANK WHEN ZERO.
               10 VIEW-TOWN PIC X(030) LINE 19 COL 40.

        01  VIEW-ALL-SCREEN
           REQUIRED.
           03 VALUE ALL " " PIC X(80) LINE 07 COL 20 BACKGROUND-COLOR 7.
           03 VALUE ALL " " PIC X(80) LINE 22 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 08 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 09 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 10 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 11 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 12 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 13 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 14 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 15 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 16 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 17 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 18 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 19 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 20 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 21 COL 20 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 08 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 09 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 10 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 11 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 12 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 13 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 14 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 15 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 16 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 17 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 18 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 19 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 20 COL 98 BACKGROUND-COLOR 7.
           03 VALUE "  " LINE 21 COL 98 BACKGROUND-COLOR 7.
           03 VALL-REC.
               10 VALL-IID PIC 9(003) LINE WS-LINE COL 23.
               10 VALL-EED PIC X(008) LINE WS-LINE COL 28.
               10 ALL-ADDRESS.
                   15 VALL-ADDRESS1 PIC X(050) LINE WS-LINE COL 38.

       01  VIEW-ALL-NEXT-SCREEN
           FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.
           03 VALUE VIEW-NEXT-TEXT LINE 25 COL 10.

       01  VIEW-ALL-END-SCREEN
           FOREGROUND-COLOR 5 BACKGROUND-COLOR 7.
           03 VALUE VIEW-END-TEXT LINE 25 COL 10.

       01  ID-ERROR-SCREEN
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 VALUE ID-ERROR-TEXT LINE 25 COL 10.

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
           05 VALUE VIEW-MENU-OPTION5 LINE 25 COL 10
           FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           05  CONTINUE-LIST.
               10  CONTINUE-IID PIC 9(003) LINE 25 COL 44
               TO SCHOOL-INTERNAL-ID
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.

       01  END-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
      *SCREEN DE MENSAGEGM PARA FIM DA LISTA.
           03 VALUE END-OF-LIST-TEXT LINE 25 COL 70.

       01  EMPTY-LIST-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE EMPTY-LIST-TEXT LINE 25 COL 10.
           05  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

       01  NEXT-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE NEXT-LIST-TEXT LINE 25 COL 70.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 3
           PERFORM CLEAR-VARIABLES
                MOVE ZERO TO MP-OPTION
                DISPLAY CLEAR-SCREEN
                DISPLAY MAIN-SCREEN
                DISPLAY MAIN-VIEW-SCREEN
                ACCEPT MP-OPTION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = 1004 THEN
                       STOP RUN
                   END-IF
                EVALUATE WS-OPTION
                   WHEN 1
                           PERFORM VIEW-ONE-IID
                               IF KEY-STATUS = 1003 THEN
                                   EXIT SECTION
                               END-IF
                               IF KEY-STATUS = 1004 THEN
                                   STOP RUN
                               END-IF
                   WHEN 2
                           PERFORM VIEW-ALL
                               IF KEY-STATUS = 1003 THEN
                                   EXIT SECTION
                               END-IF
                               IF KEY-STATUS = 1004 THEN
                                   STOP RUN
                               END-IF
               END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.

       VIEW-ONE-IID SECTION.
           MOVE SPACES TO VW-OPTION1
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
           MOVE ZEROS TO WS-CONTROL
           PERFORM WITH TEST AFTER UNTIL WS-CONTROL = 1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY VIEW-SCREEN
           OPEN INPUT SCHOOLS
               READ SCHOOLS
      *         RECORD KEY IS WS-SCHOOL-EXTERNAL-ID
               INVALID KEY
                   DISPLAY ID-ERROR-SCREEN
                   MOVE ZEROS TO VW-OPTION1
                   ACCEPT VW-OPTION
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
               NOT INVALID KEY
                   MOVE SCHOOL-DETAILS TO VIEW-REC
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY VIEW-SCREEN
                   ACCEPT OMITTED AT LINE 25 COL 10
                   MOVE 1 TO WS-CONTROL
               END-READ
           CLOSE SCHOOLS
           END-PERFORM
       EXIT SECTION.

       VIEW-ALL SECTION.
           MOVE ZEROS TO WS-CONTROL
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE 9 TO WS-LINE
           OPEN INPUT SCHOOLS
           DISPLAY VIEW-ALL-SCREEN
               MOVE ZEROS TO SCHOOL-INTERNAL-ID
               PERFORM WITH TEST AFTER UNTIL WS-CONTROL = 1
               READ SCHOOLS NEXT RECORD
                   AT END
                       MOVE 1 TO WS-CONTROL
                       DISPLAY VIEW-ALL-NEXT-SCREEN
                       ACCEPT OMITTED AT LINE 25 COL 10
                                  IF KEY-STATUS = 1003 THEN
                                       EXIT SECTION
                                   END-IF
                                   IF KEY-STATUS = 1004 THEN
                                       STOP RUN
                                   END-IF
                   NOT AT END
                       READ SCHOOLS RECORD
                       MOVE SCHOOL-DETAILS TO VALL-REC
                       DISPLAY VALL-IID
                       DISPLAY VALL-EED
                       DISPLAY VALL-ADDRESS1
                       ADD 1 TO WS-LINE
                           IF KEY-STATUS = 1003 THEN
                               EXIT SECTION
                           END-IF
                           IF KEY-STATUS = 1004 THEN
                               STOP RUN
                           END-IF
                       IF WS-LINE = 20 THEN
                           MOVE 9 TO WS-LINE
                           DISPLAY VIEW-ALL-NEXT-SCREEN
                               ACCEPT OMITTED AT LINE 25 COL 10
                                          IF KEY-STATUS = 1003 THEN
                                               EXIT SECTION
                                           END-IF
                                           IF KEY-STATUS = 1004 THEN
                                               STOP RUN
                                           END-IF
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY VIEW-ALL-SCREEN
                           IF KEY-STATUS = 1003 THEN
                               EXIT SECTION
                           END-IF
                           IF KEY-STATUS = 1004 THEN
                               STOP RUN
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE SCHOOLS
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

       CHECK-FILE SECTION.
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SCHOOLS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SCHOOLS
                   CLOSE SCHOOLS
               END-IF
           CLOSE SCHOOLS
           MOVE ZEROS TO FILE-STATUS
           EXIT SECTION.

       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN VIEW-EED VIEW-DESIGNATION
           VIEW-ADDRESS VIEW-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           VIEW-IID VIEW-POSTAL-CODE
           EXIT SECTION.

       END PROGRAM SCM-CLT.
