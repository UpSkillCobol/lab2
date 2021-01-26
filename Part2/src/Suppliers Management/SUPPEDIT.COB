      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - EDIT SUPPLIER DLL
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 23.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPEDIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FXSUPPLY ASSIGN TO "FXSUPPLIERS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SUPPLIER-ID
                   ALTERNATE KEY IS SUPPLIER-TOWN
                   FILE STATUS SUPP-STATUS.

               SELECT CODPOST ASSIGN TO "CPTODOS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CODTODOS
                   FILE STATUS CP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXSUPPLY.

       COPY SUPPLIERFX.

       FD CODPOST.
       01  CODPOST1.
           03 CODTODOS.
               05 CODTODOS1            PIC 9(004).
               05 CODTODOS2            PIC 9(003).
           03 CODTODOSLOCALIDADE       PIC X(030).

       WORKING-STORAGE SECTION.

       COPY CONSTANTS.

       COPY WSSUPPLIERFX.

       01  EDIT-OPTION                         PIC X(002).
           88 EDIT-VALID-OPTION                VALUE "Y" "y" "N" "n".
           88 EDIT-OPTION-NO                   VALUE "N" "n".
       77  DUMMY                               PIC X(001).
       77  SUPP-STATUS                          PIC 9(002).
       77  KEYSTATUS                          PIC 9(004).
       77  CP-STATUS                           PIC 9(002).
       77  FXKEY-STATUS                           PIC 9(002).
       01  SAVE-IT1                                 PIC X(002).
           88 SAVE-IT1-YES                              VALUE "Y" "y".
           88 SAVE-IT1-VALID                    VALUE "Y" "y" "N" "n".
       01  GET-VALID-ID                     PIC 9(003).
           88 VALID-ID                      VALUE 1 THRU 999.
       01  SUPPEXIST                           PIC X(002).
           88 SUPPEXIST-YES                    VALUE "Y".
       01  EDIT-WHAT                           PIC 9(002).
           88 VALID-EDIT                       VALUE 1 2 3 4 5 6 7 8.
           88 EDIT-WHAT-EXIT                   VALUE 8.


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
       01 EDIT-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(73) LINE 07 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 08 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 09 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 10 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 11 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 12 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 13 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 14 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 15 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 16 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 17 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 18 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 19 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 20 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 21 COL 20.
           05 VALUE ALL " " PIC X(73) LINE 22 COL 20.
           05 VALUE SCREEN-SUPPLIER-ID LINE 08 COL 25.
           05 VALUE MANUALLY-ADD-NAME LINE 09 COL 25.
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 10 COL 25.
           05 VALUE MANUALLY-ADD-ADDRESS LINE 13 COL 25.
           05 VALUE MANUALLY-ADD-POSTAL-CODE LINE 15 COL 25.
           05 VALUE MANUALLY-ADD-TOWN LINE 15 COL 48.
           05 VALUE MANUALLY-ADD-EMAIL1 LINE 16 COL 25.
           05 VALUE MANUALLY-ADD-EMAIL2 LINE 17 COL 25.
           05 VALUE MANUALLY-ADD-EMAIL3 LINE 18 COL 25.
           05 VALUE MANUALLY-ADD-PHONE1 LINE 19 COL 25.
           05 VALUE MANUALLY-ADD-PHONE2 LINE 20 COL 25.
           05 VALUE MANUALLY-ADD-PHONE3 LINE 21 COL 25.
           05 VALUE "-" LINE 13 COL 43.
           05 REGISTER-RECORD.
               10 REG-SUPP-ID PIC 9(003) LINE 08 COL 39
                   FROM WSSUPPLIER-ID.
               10 REG-SUPP-NAME PIC X(030) LINE 09 COL 39
                   FROM WSSUPPLIER-NAME REQUIRED.
               10 REG-SUPP-DESCRIPTION.
                   15 REG-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 39
                       FROM WSSUPPLIER-DESCRIPTION1 REQUIRED AUTO.
                   15 REG-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 39
                       FROM WSSUPPLIER-DESCRIPTION2 AUTO.
                   15 REG-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 39
                       FROM WSSUPPLIER-DESCRIPTION3.
               10 REG-SUPP-ADDRESS.
                   15 REG-SUPP-ADRESS1 PIC X(050) LINE 13 COL 39
                       FROM WSSUPP-ADR-MAIN1 AUTO.
                   15 REG-SUPP-ADRESS2 PIC X(050) LINE 14 COL 39
                       FROM WSSUPP-ADR-MAIN2.
               10 REG-SUPP-POSTAL-CODE.
                   15 REG-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 39
                       FROM WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO AUTO
                       REQUIRED.
                   15 REG-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 44
                       FROM WSSUPPLIER-POSTAL-CODE2
                       BLANK WHEN ZERO AUTO.
               10 REG-SUPP-TOWN PIC X(030) LINE 15 COL 59
                   FROM WSSUPPLIER-TOWN AUTO REQUIRED.
               10 REG-SUPP-EMAIL.
                   15 REG-SUPP-EMAIL1 PIC X(040) LINE 16 COL 39
                       FROM WSSUPPLIER-EMAIL1 REQUIRED.
                   15 REG-SUPP-EMAIL2 PIC X(040) LINE 17 COL 39
                       FROM WSSUPPLIER-EMAIL2.
                   15 REG-SUPP-EMAIL3 PIC X(040) LINE 18 COL 39
                       FROM WSSUPPLIER-EMAIL3.
               10 REG-SUPP-PHONE.
                   15 REG-SUPP-PHONE1 PIC 9(009) LINE 19 COL 39
                       FROM WSSUPPLIER-TELEPHONE1 AUTO REQUIRED
                       BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE2 PIC 9(009) LINE 20 COL 39
                       FROM WSSUPPLIER-TELEPHONE2 AUTO BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE3 PIC 9(009) LINE 21 COL 39
                       FROM WSSUPPLIER-TELEPHONE3 AUTO BLANK WHEN ZERO.
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
           05 VALUE WHAT-TO-EDIT LINE 08 COL 102.
           05 VALUE EDIT1 LINE 10 COL 100.
           05 VALUE EDIT2 LINE 11 COL 100.
           05 VALUE EDIT3 LINE 12 COL 100.
           05 VALUE EDIT4 LINE 13 COL 100.
           05 VALUE EDIT5 LINE 14 COL 100.
           05 VALUE EDIT6 LINE 15 COL 100.
           05 VALUE EDIT7 LINE 16 COL 100.
           05 VALUE EDIT8 LINE 17 COL 100.
           05 VALUE CHOOSE LINE 20 COL 100.
           05 EDIT-CHOICE PIC 9(002) LINE 20 COL 117 BLANK WHEN ZERO
               REQUIRED TO EDIT-WHAT.
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
       01 GET-SUPPID-MESSAGE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-GET-SUPPID LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 NEW-SUPPID LINE 25 COL 43 PIC 9(3)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACE TO EDIT-OPTION
           PERFORM WITH TEST AFTER UNTIL EDIT-OPTION-NO
               OPEN I-O FXSUPPLY
               MOVE SPACE TO SUPPEXIST
               PERFORM UNTIL SUPPEXIST-YES
                   PERFORM GET-SUPPLIER-ID
                   PERFORM CHECK-IF-SUPPID-EXISTS
               END-PERFORM
               MOVE ZERO TO EDIT-WHAT
               PERFORM EDIT-SUPPLIER UNTIL EDIT-WHAT-EXIT

           END-PERFORM
           EXIT PROGRAM.

       GET-SUPPLIER-ID SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           PERFORM WITH TEST AFTER UNTIL VALID-ID
               MOVE ZERO TO NEW-SUPPID
               ACCEPT GET-SUPPID-MESSAGE
               IF NOT VALID-ID THEN
                   MOVE ERROR-GET-SUPPID TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       CHECK-IF-SUPPID-EXISTS SECTION.
           MOVE GET-VALID-ID TO SUPPLIER-ID
               READ FXSUPPLY INTO WSSUPPLIER-DETAILS
                   NOT INVALID KEY
                       MOVE "Y" TO SUPPEXIST
                   INVALID KEY
                       MOVE ERROR-SUPPID-NO TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
               END-READ
       EXIT SECTION.

       EDIT-SUPPLIER SECTION.
           DISPLAY CLEAR-SCREEN DISPLAY MAIN-SCREEN DISPLAY EDIT-SCREEN
           ACCEPT EDIT-WHAT-SCREEN
       EXIT SECTION.

       GET-NAME SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-NAME
           NOT EQUAL SPACES
               MOVE SPACES TO WSSUPPLIER-NAME
               MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-NAME
               IF WSSUPPLIER-NAME NOT EQUAL SPACES THEN
                  CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-NAME
                  CALL "RMVEXTSPACES" USING BY REFERENCE WSSUPPLIER-NAME
               ELSE
                   MOVE ERROR-NAME TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       GET-DESCRIPTION SECTION.
           MOVE SPACES TO WSSUPPLIER-DESCRIPTION
           MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-SUPP-DESCRIPTION
           CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-DESCRIPTION1
           CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-DESCRIPTION2
           CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-DESCRIPTION3
           CALL "RMVEXTSPACES" USING BY REFERENCE
           WSSUPPLIER-DESCRIPTION1
           CALL "RMVEXTSPACES" USING BY REFERENCE
           WSSUPPLIER-DESCRIPTION2
           CALL "RMVEXTSPACES" USING BY REFERENCE
           WSSUPPLIER-DESCRIPTION3
       EXIT SECTION.

       GET-ADDRESS SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPP-ADR-MAIN1
           NOT EQUAL SPACES
               MOVE SPACES TO WSSUPP-ADR-MAIN
               MOVE MESSAGE-ADDRESS TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-ADDRESS
               IF WSSUPP-ADR-MAIN1 NOT EQUAL SPACES THEN
                   CALL "LOWERUPPER" USING BY REFERENCE WSSUPP-ADR-MAIN1
                   CALL "RMVEXTSPACES"
                   USING BY REFERENCE WSSUPP-ADR-MAIN1
                   CALL "LOWERUPPER" USING BY REFERENCE WSSUPP-ADR-MAIN2
                   CALL "RMVEXTSPACES"
                   USING BY REFERENCE WSSUPP-ADR-MAIN2
               ELSE
                   MOVE ERROR-ADDRESS TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       GET-POSTAL-CODE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-POSTAL-CODE
               MOVE ZERO TO REG-SUPP-POSTAL-CODE
               MOVE MESSAGE-POSTAL-CODE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-POSTAL-CODE
               IF NOT VALID-POSTAL-CODE THEN
                   MOVE ERROR-POSTAL-CODE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       GET-TOWN SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-TOWN IS ALPHABETIC
               MOVE WSSUPPLIER-POSTAL-CODE TO CODTODOS
               OPEN INPUT CODPOST
               READ CODPOST
                   NOT INVALID KEY
                       MOVE CODTODOSLOCALIDADE TO REG-SUPP-TOWN
               END-READ
               CLOSE CODPOST
               MOVE MESSAGE-TOWN TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-TOWN
               IF WSSUPPLIER-TOWN IS ALPHABETIC
                  CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-TOWN
                  CALL "RMVEXTSPACES" USING BY REFERENCE WSSUPPLIER-TOWN
               ELSE
                  MOVE ERROR-TOWN TO ERROR-TEXT
                  ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       GET-EMAIL SECTION.
           MOVE SPACES TO REG-SUPP-EMAIL
           MOVE MESSAGE-EMAIL TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-SUPP-EMAIL
       EXIT SECTION.

       GET-PHONE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-PHONE1 AND VALID-PHONE2
           AND VALID-PHONE3
               MOVE ZERO TO REG-SUPP-PHONE
               MOVE MESSAGE-PHONE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-PHONE
               IF NOT VALID-PHONE1 OR NOT VALID-PHONE2 OR NOT
               VALID-PHONE3 THEN
                   MOVE ERROR-PHONE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
           END-PERFORM
       EXIT SECTION.
