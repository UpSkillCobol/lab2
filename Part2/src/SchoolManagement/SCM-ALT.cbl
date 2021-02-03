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

       DATA DIVISION.
       FILE SECTION.

       FD SCHOOLS.
       COPY "CB-SCHOOLS".

       WORKING-STORAGE SECTION.
       COPY "CB-WS-SCHOOLS".
       COPY "CONSTANTS".

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.
      ******************************************************************
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
      ******************************************************************
       01  ALT-SCREEN.
           05 VALUE ALT-MENU-TEXT LINE 9 COL 30.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 12.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 12.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 12.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 12.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 12.
           05 VALUE "-" LINE 18 COL 37.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 12.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 10
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 10
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 88 BACKGROUND-COLOR 7.
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
           05 ALT-REC.
               10 ALT-IID PIC 9(003) LINE 11 COL 32 BLANK WHEN ZERO.
               10 ALT-EED PIC X(008) LINE 12 COL 32
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 ALT-DESIGNATION.
                   15 ALT-DESIGNATION1 PIC X(050) LINE 13 COL 32
                       TO WS-SCHOOL-DESIGNATION1.
                   15 ALT-DESIGNATION2 PIC X(050) LINE 14 COL 32
                       TO WS-SCHOOL-DESIGNATION2.
                   15 ALT-DESIGNATION3 PIC X(050) LINE 15 COL 32
                       TO WS-SCHOOL-DESIGNATION3.
               10 ALT-ADDRESS.
                   15 ALT-ADDRESS1 PIC X(050) LINE 16 COL 32
                       TO WS-SCHL-ADR-MAIN1.
                   15 ALT-ADDRESS2 PIC X(050) LINE 17 COL 32
                       TO WS-SCHL-ADR-MAIN2.
               10 ALT-POSTAL-CODE.
                   15 ALT-PC1 PIC 9(004) LINE 18 COL 32
                        TO WS-SCHL-POSTAL-CODE1 BLANK WHEN ZERO.
                   15 ALT-PC2 PIC 9(003) LINE 18 COL 39
                        TO WS-SCHL-POSTAL-CODE2 BLANK WHEN ZERO.
               10 ALT-TOWN PIC X(030) LINE 19 COL 32
                       TO WS-SCHOOL-TOWN.
      ******************************************************************
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
      ******************************************************************
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
           05 VALUE ALT-MENU-OPTION LINE 25 COL 10
           FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           05  CONTINUE-LIST.
               10  CONTINUE-IID PIC 9(003) LINE 25 COL 44
               TO SCHOOL-INTERNAL-ID
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
      ******************************************************************
       01  END-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE END-OF-LIST-TEXT LINE 25 COL 53.
      ******************************************************************
       01  EMPTY-LIST-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE EMPTY-LIST-TEXT LINE 25 COL 53.
           05  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.
      ******************************************************************
       01  NEXT-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE NEXT-LIST-TEXT LINE 25 COL 53.
      ******************************************************************
       01  ID-ERROR-SCREEN
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 VALUE ID-ERROR-TEXT LINE 25 COL 10.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
      *    CALL THE LIST SECTION TO SHOW A LIST OF ALL RECORDS ALREADY
      *    SAVED ON THE FILE SO THE USER CAN CHOOSE ONE TO USE
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
      *    READ THE FILE TO CHECK IF THE RECORD THE USER DID CHOOSE IS
      *    VALID OR NOT, IF IT IS, THE RECORD IS SHOWN TO THE USER AND
      *    THEN GOES TO THE CHOOSE-EDIT SECTION.
           OPEN INPUT SCHOOLS
               READ SCHOOLS
               INVALID KEY
                   DISPLAY ID-ERROR-SCREEN
                   MOVE ZEROS TO CONTINUE-LIST
                   ACCEPT CONTINUE-LIST
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
               NOT INVALID KEY
                   PERFORM CLEAR-VARIABLES
                   MOVE SCHOOL-DETAILS TO ALT-REC
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY ALT-SCREEN
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
                   MOVE 1 TO WS-CONTROL
               END-READ
           CLOSE SCHOOLS
           END-PERFORM
           PERFORM CHOOSE-EDIT
           EXIT PROGRAM.
      ******************************************************************
       CHOOSE-EDIT SECTION.
      *    SECTION WHERE THE USER CHOOSES WHAT HE WANTS TO EDIT ON THE RECORD
      *    THAT HE CHOSE PREVIOUSLY
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 6
               MOVE ZEROS TO EDIT-CHOICE
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY ALT-SCREEN
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
      ******************************************************************
       LIST SECTION.
      *    LIST SECTION THAT CREATES A LIST OF ALL THE RECORDS TO BE SHOWN
      *    SO THE USER CAN CHOOSE THE ONE HE WANTS
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-SCREEN
           MOVE SPACES TO FLAG
           MOVE SPACES TO CONTINUE-LIST
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
      *    READ THE FILE GOING THROUGH EACH RECORD AND DISPLAYING THEM ON
      *    THE SCREEN
              READ SCHOOLS NEXT RECORD
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
      ******************************************************************
      *    SECTION TO CLEAR ALL VARIABLES THAT THE MODULE USES TO CHANGE
      *    THE RECORD
       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN ALT-EED ALT-DESIGNATION
           ALT-ADDRESS ALT-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           ALT-IID ALT-POSTAL-CODE
           EXIT SECTION.
************************************************************************
       EDIT-EED SECTION.
      *    SECTION TO CHANGE EXTERNAL ID
           PERFORM WITH TEST AFTER UNTIL EXTERNAL-ID-VLD
           MOVE SPACES TO ALT-EED
               ACCEPT ALT-EED
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE WS-SCHOOL-EXTERNAL-ID TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-EXTERNAL-ID
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-DESIGNATION SECTION.
      *    SECTION TO CHANGE DESIGNATION
           PERFORM WITH TEST AFTER UNTIL DESIGNATION-VLD
           MOVE SPACES TO ALT-DESIGNATION
               ACCEPT ALT-DESIGNATION
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE WS-SCHOOL-DESIGNATION TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-DESIGNATION
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-ADDRESS SECTION.
      *    SECTION TO CHANGE ADDRESS
           PERFORM WITH TEST AFTER UNTIL ADDRESS-VLD
               ACCEPT ALT-ADDRESS
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE WS-SCHL-ADR-MAIN TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHL-ADR-MAIN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-POSTAL-CODE SECTION.
      *    SECTION TO CHANGE THE POSTAL CODE
       PERFORM WITH TEST AFTER UNTIL POSTAL-CODE1-VLD AND
               POSTAL-CODE2-VLD
               ACCEPT ALT-PC1
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
           END-IF
               ACCEPT ALT-PC2
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
      *    HERE PROGRAM CALLS TO CHANGE THE TOWN,
      *    USUALLY IF YOU CHANGE POSTAL CODE YOU CHANGE THE TOWN ASWELL
           PERFORM EDIT-TOWN
           EXIT SECTION.
      ******************************************************************
       EDIT-TOWN SECTION.
      *    SECTION TO CHANGE THE TOWN
      *    CALL CPS MODULE TO USE THE POSTAL CODE AND AUTOMATICALLY
      *    FILL THE TOWN CAMP
           CALL "CPS" USING BY REFERENCE ALT-REC
           MOVE WS-SCHOOL-TOWN TO ALT-TOWN
           DISPLAY ALT-SCREEN
           PERFORM WITH TEST AFTER UNTIL TOWN-VLD
                ACCEPT ALT-TOWN
                IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF
           END-PERFORM
      *    PERFORM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE WS-SCHOOL-TOWN TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-TOWN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       LOWER-UPPER SECTION.
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-EXTERNAL-ID) TO
           WS-SCHOOL-EXTERNAL-ID
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-DESIGNATION) TO
           WS-SCHOOL-DESIGNATION
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-ADRESS) TO
           WS-SCHOOL-ADRESS
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-TOWN) TO WS-SCHOOL-TOWN
           MOVE FUNCTION UPPER-CASE (ALT-EED) TO ALT-EED
           MOVE FUNCTION UPPER-CASE (ALT-DESIGNATION) TO ALT-DESIGNATION
           MOVE FUNCTION UPPER-CASE (ALT-ADDRESS) TO ALT-ADDRESS
           MOVE FUNCTION UPPER-CASE (ALT-TOWN) TO ALT-TOWN
           EXIT SECTION.
      ******************************************************************
       SPACE-CHECK SECTION.
      *    SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO
           SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           MOVE TRIM(LINK-TEXT) TO LINK-TEXT
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
       END PROGRAM SCM-ALT.
