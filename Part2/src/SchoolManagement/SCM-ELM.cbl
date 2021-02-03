      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-ELM.
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
           05 WS-SCHOOL-IS-ACTIVE                  PIC 9(001).

       01  WS-OPTION                                PIC 9(002).
           88 OPTION-VLD                            VALUE
                                                   "1","2","3","4".
       01  WS-DLT                                  PIC X(01).
           88 DLT-VLD                              VALUE
                                                   "Y","S","N",
                                                   "y","s","n".
       01  WS-DLT-KEY                           PIC 9(001).
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
       01  PRESS-KEY                               PIC X(001).
       01  SC-LINE                                 PIC 9(004).
       01  FLAG                                    PIC X(001).
       01  WS-CONTROL                              PIC 9(001).
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

       01  PRE-DELETE-MENU
           REQUIRED.
      *    SCREEN PARA CONFIRMAR QUAL REGISTO O UTILIZADOR QUER ELIMINAR
           05 VALUE DLT-MENU-TEXT1 LINE 25 COL 10
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.
           05 DLT-OPTION PIC X(008) LINE 25 COL 47 TO
               WS-SCHOOL-EXTERNAL-ID
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.

       01  DELETE-SCREEN
           REQUIRED.
           05 VALUE DLT-MENU-TEXT LINE 9 COL 25.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 22.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 22.
           05 VALUE "-" LINE 18 COL 45.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 22.
           05 VALUE DLT-MENU-TEXT2 LINE 25 COL 10
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 4.
           05 DLT-OPTION1 PIC X(01) LINE 25 COL 46 TO WS-DLT
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 4.
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
           05 DLT-REC.
               10 DLT-IID PIC 9(003) LINE 11 COL 40 BLANK WHEN ZERO.
               10 DLT-EED PIC X(008) LINE 12 COL 40.
               10 DLT-DESIGNATION.
                   15 DLT-DESIGNATION1 PIC X(050) LINE 13 COL 40.
                   15 DLT-DESIGNATION2 PIC X(050) LINE 14 COL 40.
                   15 DLT-DESIGNATION3 PIC X(050) LINE 15 COL 40.
               10 DLT-ADDRESS.
                   15 DLT-ADDRESS1 PIC X(050) LINE 16 COL 40.
                   15 DLT-ADDRESS2 PIC X(050) LINE 17 COL 40.
               10 DLT-POSTAL-CODE.
                   15 DLT-PC1 PIC 9(004) LINE 18 COL 40
                        BLANK WHEN ZERO.
                   15 DLT-PC2 PIC 9(003) LINE 18 COL 47
                        BLANK WHEN ZERO.
               10 DLT-TOWN PIC X(030) LINE 19 COL 40.

       01  DELETED-SCREEN.
           05 VALUE DELETED-TEXT LINE 25 COL 10 BACKGROUND-COLOR 7
           FOREGROUND-COLOR 4.

       01  ID-ERROR.
           05 VALUE DLT-ID-ERROR LINE 26 COL 10 BACKGROUND-COLOR 7
           FOREGROUND-COLOR 4.

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
           05 VALUE LIST-SCREEN-TEXT4 LINE 8 COL 11 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT1 LINE 8 COL 17 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT2 LINE 8 COL 28 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT3 LINE 8 COL 81 FOREGROUND-COLOR 5.
           05 VALUE DLT-MENU-TEXT1 LINE 25 COL 10
           FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           05  CONTINUE-LIST.
               10  CONTINUE-IID PIC 9(003) LINE 25 COL 47
               TO SCHOOL-INTERNAL-ID
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.

       01  END-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE END-OF-LIST-TEXT LINE 25 COL 53.

       01  EMPTY-LIST-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE EMPTY-LIST-TEXT LINE 25 COL 53.
           05  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

       01  NEXT-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE NEXT-LIST-TEXT LINE 25 COL 53.

       PROCEDURE DIVISION.
       DELETE-REGISTER SECTION.
      *    DELETE REGISTERS SECTION
           PERFORM CLEAR-VARIABLES
           MOVE SPACES TO DLT-OPTION
           MOVE SPACES TO DLT-OPTION1
           MOVE SPACES TO WS-DLT
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
      *    SHOW A LIST OF ALL RECORDS AND ACCEPT THE ONE TO BE SHOW AND DELETED
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
      *    DISPLAY THE RECORD THE USER DID CHOOSE ON THE LIST SECTION
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY DELETE-SCREEN
           OPEN INPUT SCHOOLS
               READ SCHOOLS
               INVALID KEY
      *    IF THE RECORD DOESN'T EXIST A MESSAGE WILL BE SHOWN
                       DISPLAY EMPTY-LIST-SCREEN
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
               NOT INVALID KEY
      *    THE RECORD IS SHOWN IN THE SCREEN
                   MOVE SCHOOL-DETAILS TO DLT-REC
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY DELETE-SCREEN
                   MOVE 1 TO WS-CONTROL
               END-READ
           CLOSE SCHOOLS
           END-PERFORM
           PERFORM WITH TEST AFTER UNTIL DLT-VLD
      *    THE USER MUST DECIDE IF HE WANTS DO DELETE OR NOT THE FILE
           MOVE SPACES TO DLT-OPTION1
           ACCEPT DLT-OPTION1
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
           MOVE FUNCTION UPPER-CASE(WS-DLT) TO WS-DLT
           EVALUATE TRUE
      *    IF THE USER INTRODUCES "S" OR "Y" THE RECORD IS "DELETED"
               WHEN WS-DLT = "S" OR WS-DLT = "Y"
                   PERFORM DELETE-RECORD
                   DISPLAY DELETED-SCREEN
                   ACCEPT OMITTED AT LINE 25 COL 09
      *    IF THE USER INTRODUCES "N" THEN THE RECORD IS KEPT
               WHEN WS-DLT = "N"
                   PERFORM CLEAR-VARIABLES
           END-EVALUATE
           EXIT PROGRAM.
      ******************************************************************
       DELETE-RECORD SECTION.
      *    SECTION TO DELETE THE RECORD, IT ACTUALLY DOESNT DELETE THE RECORD
      *    JUST MOVES A 0 TO THE SCHOOL-IS-ACTIVE VARIABLE, MAKING THE SCHOOL
      *    INACTIVE
           OPEN I-O SCHOOLS
               MOVE 0 TO SCHOOL-IS-ACTIVE
           REWRITE SCHOOL-DETAILS FROM DLT-REC
       CLOSE SCHOOLS.
      ******************************************************************
       CLEAR-VARIABLES SECTION.
      *    SECTION TO CLEAR ALL VARIABLES THAT THE MODULE USES TO CHANGE
      *    THE RECORD
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN DLT-EED DLT-DESIGNATION
           DLT-ADDRESS DLT-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           WS-SCHOOL-IS-ACTIVE DLT-IID DLT-POSTAL-CODE
           EXIT SECTION.
      ******************************************************************
       LIST SECTION.
      *    LIST SECTION THAT CREATES A LIST OF ALL THE RECORDS TO BE SHOWN
      *    SO THE USER CAN CHOOSE THE ONE HE WANTS
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-SCREEN
           MOVE SPACES TO FLAG
           MOVE SPACES TO CONTINUE-LIST
           MOVE SPACES TO SCHOOL-EXTERNAL-ID
           MOVE ZEROS TO SCHOOL-INTERNAL-ID
           OPEN INPUT SCHOOLS
      *    POINT THE FILE IN THE START, IN THIS CASE ON ID "000" SO
      *    WE ARE SURE THAT THE PROGRAM WILL READ ALL RECORDS
           START SCHOOLS KEY IS GREATER OR EQUAL SCHOOL-INTERNAL-ID
              INVALID KEY
      *    IF THERE ARE NO RECORDS A MESSAGE WILL BE SHOWN
                 ACCEPT EMPTY-LIST-SCREEN
                 MOVE "Y" TO FLAG
                 IF FLAG = "Y" THEN
                    CLOSE SCHOOLS
                    EXIT SECTION
                 END-IF
           END-START
           MOVE 9 TO SC-LINE
           PERFORM UNTIL WS-EOF
      *    READ THE FILE GOING THROUGH EACH RECORD AND POSITIONING IT ON
      *    THE SCREEN
              READ SCHOOLS NEXT RECORD
      *             KEY IS SCHOOL-EXTERNAL-ID
                 AT END SET WS-EOF TO TRUE
      *    WHEN THE LAST RECORD IS REACHED, A MESSAGE IS SHOWN TO THE USER
                    DISPLAY END-LIST-SCREEN
      *    ACCEPT THE RECORD TO BE USED
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
                    IF SC-LINE = 21 THEN
      *    WHEN THE RECORDS REACH THE MAXIMUM AMMOUNT OF THE SPACE
      *    AVAILABLE ON THE SCREEN, THE PROGRAM ASKS THE USER
      *    TO EITHER INSERT A RECORD TO BE USED OR PRESS F2 TO GO
      *    TO THE NEXT PAGE AND SHOW MORE RECORDS
                       DISPLAY NEXT-LIST-SCREEN
      *    ACCEPT THE RECORD TO BE USED
                       ACCEPT CONTINUE-LIST
      *    PRESS F2 TO GO TO THE NEXT PAGE
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

       END PROGRAM SCM-ELM.
