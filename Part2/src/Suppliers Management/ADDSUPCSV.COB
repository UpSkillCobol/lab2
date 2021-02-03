      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - ADD SUPPLIER MANUALLY
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDSUPCSV.
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
                   ALTERNATE KEY IS SUPPLIER-TOWN WITH DUPLICATES
                   ALTERNATE KEY IS SUPPLIER-NAME WITH DUPLICATES
                   FILE STATUS SUPP-STATUS.

               SELECT CSVSUPPLY ASSIGN TO "SUPPLIERS.CSV"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS CSVSUPP-STATUS.

               SELECT CSVERRORLOG ASSIGN TO "CSVERLOG.TXT"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS CSVERRLOG-STATUS.

               SELECT CODPOST ASSIGN TO "CPTODOS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CODTODOS
                   FILE STATUS CP-STATUS.

               SELECT FXKEYS ASSIGN TO "SUPPKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXSUPPLY.
       COPY SUPPLIERFX.

       FD FXKEYS.
       01  FDSUPPKEYS                      PIC 9(003).

       FD CODPOST.
       01  CODPOST1.
           03 CODTODOS.
               05 CODTODOS1                PIC 9(004).
               05 CODTODOS2                PIC 9(003).
           03 CODTODOSLOCALIDADE           PIC X(030).

       FD CSVSUPPLY.
       01  SUPCSV.
       88  EOFCSV                          VALUE HIGH-VALUES.
       03  SUPCSVDATA                       PIC X(480).

       FD CSVERRORLOG.
       01  ERRORLOG.
           03 ERRORSUPP                   PIC X(500).

       WORKING-STORAGE SECTION.

       COPY CONSTANTS.

       COPY WSSUPPLIERFX.

       01  ADD-OPTION1                     PIC X(002).
           88 ADD-VALID-OPTION1            VALUE "Y" "y" "N" "n".
           88 ADD-OPTION1-NO               VALUE "N" "n".
       77  DUMMY                           PIC X(001).
       77  SUPP-STATUS                     PIC 9(002).
       77  KEYSTATUS                       PIC 9(004).
       77  CP-STATUS                       PIC 9(002).
       77  FXKEY-STATUS                    PIC 9(002).
       77  CSVSUPP-STATUS                  PIC 9(002).
       77  CSVERRLOG-STATUS                PIC 9(002).
       01  SAVE-IT                         PIC X(002).
           88 SAVE-IT-YES                  VALUE "Y" "y".
           88 SAVE-IT-VALID                VALUE "Y" "y" "N" "n".
       77 UNSTR                        PIC X(150).
       77 UNSTR1                       PIC X(050).
       77 UNSTR2                       PIC X(050).
       77 UNSTR3                       PIC X(050).
       77 UNSTR4                       PIC X(050).
       77 UNSTR5                       PIC X(050).
       77 UNSTR6                       PIC X(050).
       77 UNSTR7                       PIC X(050).
       77 UNSTR8                       PIC X(050).
       77 UNSTR9                       PIC X(050).
       77 UNSTR10                      PIC X(050).
       01 CSV-VALID                    PIC X(001).
           88 CSV-NOT-VALID            VALUE "N".

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
       01 CSV-ADD-SUPPLIER-SCREEN.
      *     05 VALUE ALL "_" PIC X(080) LINE 10 COL 18.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 22 COL 08
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
           05 VALUE "  " LINE 08 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 86 BACKGROUND-COLOR 7.
           05 VALUE SCREEN-SUPPLIER-ID LINE 08 COL 15.
           05 VALUE MANUALLY-ADD-NAME LINE 09 COL 15.
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 10 COL 15.
           05 VALUE MANUALLY-ADD-ADDRESS LINE 13 COL 15.
           05 VALUE MANUALLY-ADD-POSTAL-CODE LINE 15 COL 15.
           05 VALUE MANUALLY-ADD-TOWN LINE 15 COL 37.
           05 VALUE MANUALLY-ADD-EMAIL1 LINE 16 COL 15.
           05 VALUE MANUALLY-ADD-EMAIL2 LINE 17 COL 15.
           05 VALUE MANUALLY-ADD-EMAIL3 LINE 18 COL 15.
           05 VALUE MANUALLY-ADD-PHONE1 LINE 19 COL 15.
           05 VALUE MANUALLY-ADD-PHONE2 LINE 20 COL 15.
           05 VALUE MANUALLY-ADD-PHONE3 LINE 21 COL 15.
           05 VALUE "-" LINE 15 COL 33.
           05 REG-SUPP-ID PIC 9(003) LINE 08 COL 29 FROM WSSUPPLIER-ID.
           05 REGISTER-RECORD.
               10 REG-SUPP-NAME PIC X(030) LINE 09 COL 29
                   FROM WSSUPPLIER-NAME.
               10 REG-SUPP-DESCRIPTION.
                   15 REG-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 29
                       FROM WSSUPPLIER-DESCRIPTION1.
                   15 REG-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 29
                       FROM WSSUPPLIER-DESCRIPTION2.
                   15 REG-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 29
                       FROM WSSUPPLIER-DESCRIPTION3.
               10 REG-SUPP-ADDRESS.
                   15 REG-SUPP-ADRESS1 PIC X(050) LINE 13 COL 29
                       FROM WSSUPP-ADR-MAIN1.
                   15 REG-SUPP-ADRESS2 PIC X(050) LINE 14 COL 29
                       FROM WSSUPP-ADR-MAIN2.
               10 REG-SUPP-POSTAL-CODE.
                   15 REG-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 29
                       FROM WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO.
                   15 REG-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 34
                       FROM WSSUPPLIER-POSTAL-CODE2
                       BLANK WHEN ZERO AUTO.
               10 REG-SUPP-TOWN PIC X(030) LINE 15 COL 49
                   FROM WSSUPPLIER-TOWN.
               10 REG-SUPP-EMAIL.
                   15 REG-SUPP-EMAIL1 PIC X(040) LINE 16 COL 29
                       FROM WSSUPPLIER-EMAIL1.
                   15 REG-SUPP-EMAIL2 PIC X(040) LINE 17 COL 29
                       FROM WSSUPPLIER-EMAIL2.
                   15 REG-SUPP-EMAIL3 PIC X(040) LINE 18 COL 29
                       FROM WSSUPPLIER-EMAIL3.
               10 REG-SUPP-PHONE.
                   15 REG-SUPP-PHONE1 PIC 9(009) LINE 19 COL 29
                       FROM WSSUPPLIER-TELEPHONE1.
                   15 REG-SUPP-PHONE2 PIC 9(009) LINE 20 COL 29
                       FROM WSSUPPLIER-TELEPHONE2 BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE3 PIC 9(009) LINE 21 COL 29
                       FROM WSSUPPLIER-TELEPHONE3 BLANK WHEN ZERO.
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
           05 WANT-TO-SAVE1 LINE 25 COL 67
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.
      ******************************************************************
       01 INSTRUCTIONS-test.
           05 VALUE ALL " " PIC X(095) LINE 19 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 20 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 21 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 18 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 22 COL 01.
           05 INSTRUCTIONS-teste LINE 18 COL 03 PIC X(480)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE2 LINE 25 COL 67
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 102-CHECK-IF-CSV-FILE-EXISTS
           PERFORM 100-CHECK-IF-SUPP-FILE-EXIST
           PERFORM 105-CHECK-IF-KEYS-FILE-EXIST
           PERFORM 103-CHECK-IF-LOG-FILE-EXISTS
           PERFORM 112-GET-CSV-DATA
           EXIT PROGRAM.

       100-CHECK-IF-SUPP-FILE-EXIST SECTION.
           OPEN I-O FXSUPPLY
           IF SUPP-STATUS = "35" THEN
               OPEN OUTPUT FXSUPPLY
               CLOSE FXSUPPLY
           ELSE
               CLOSE FXSUPPLY
           END-IF
       EXIT SECTION.

       102-CHECK-IF-CSV-FILE-EXISTS SECTION.
       OPEN I-O CSVSUPPLY
           IF CSVSUPP-STATUS = "35" THEN
               MOVE CSV-NO-EXIST TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               EXIT PROGRAM
           ELSE
               CLOSE CSVSUPPLY
           END-IF
       EXIT SECTION.

       103-CHECK-IF-LOG-FILE-EXISTS SECTION.
       OPEN I-O CSVERRORLOG
           IF CSVERRLOG-STATUS = "35" THEN
               OPEN OUTPUT CSVERRORLOG
               CLOSE CSVERRORLOG
           ELSE
               CLOSE CSVERRORLOG
           END-IF
       EXIT SECTION.

       105-CHECK-IF-KEYS-FILE-EXIST SECTION.
           OPEN I-O FXKEYS
           IF FXKEY-STATUS = "35" THEN
               OPEN OUTPUT FXKEYS
                   MOVE 0 TO FDSUPPKEYS
                   WRITE FDSUPPKEYS
                   END-WRITE
               CLOSE FXKEYS
           ELSE
               CLOSE FXKEYS
           END-IF
       EXIT SECTION.

       110-GET-SUPPLY-ID SECTION.
           OPEN I-O FXKEYS
               READ FXKEYS
                   ADD 1 TO FDSUPPKEYS
       EXIT SECTION.

       112-GET-CSV-DATA SECTION.
           OPEN INPUT CSVSUPPLY
           MOVE SPACE TO CSV-VALID
           PERFORM UNTIL EOFCSV
               READ CSVSUPPLY
                   AT END SET EOFCSV TO TRUE
                       MOVE END-CSV-READ TO ERROR-TEXT
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT ERROR-ZONE
      *         END-READ
                   NOT AT END
                       PERFORM 110-GET-SUPPLY-ID
                       MOVE 1 TO WSSUPPLIER-IS-ACTIVE
                       MOVE ZERO TO WSSUPPLIER-POSTAL-CODE1
                       WSSUPPLIER-POSTAL-CODE2 WSSUPPLIER-TELEPHONE
                       MOVE SPACES TO WSSUPPLIER-NAME
                       WSSUPPLIER-DESCRIPTION WSSUPP-ADR-MAIN
                       WSSUPPLIER-TOWN WSSUPPLIER-EMAIL
                       MOVE FDSUPPKEYS TO WSSUPPLIER-ID
                       UNSTRING SUPCSVDATA DELIMITED BY ";" INTO
                       WSSUPPLIER-NAME WSSUPPLIER-DESCRIPTION
                       WSSUPP-ADR-MAIN WSSUPPLIER-POSTAL-CODE
                       WSSUPPLIER-TOWN WSSUPPLIER-EMAIL1
                       WSSUPPLIER-EMAIL2 WSSUPPLIER-EMAIL3
                       WSSUPPLIER-TELEPHONE1 WSSUPPLIER-TELEPHONE2
                       WSSUPPLIER-TELEPHONE3
      *                 PERFORM 115-CHECK-NAME-VALIDITY
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 120-CHECK-DESCRIPTION-VALIDITY
      *                 END-IF
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 125-CHECK-ADDRESS-VALIDITY
      *                 END-IF
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 130-CHECK-POSTALCODE-VALIDITY
      *                 END-IF
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 135-CHECK-TOWN-VALIDITY
      *                 END-IF
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 140-CHECK-EMAIL-VALIDITY
      *                 END-IF
      *                 IF NOT CSV-NOT-VALID
      *                     PERFORM 140-CHECK-PHONE-VALIDITY
      *                 END-IF
                       PERFORM 150-WRITE-RECORD
               END-READ
           END-PERFORM
           CLOSE CSVSUPPLY
       EXIT SECTION.

       115-CHECK-NAME-VALIDITY SECTION.
           IF WSSUPPLIER-NAME EQUAL SPACES THEN
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
                MOVE "name" TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           ELSE
               CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-NAME
      *         MOVE TRIM(WSSUPPLIER-NAME) TO UNSTR
      *         PERFORM 155-REMOVE-EXTRA-SPACES
      *         MOVE UNSTR TO WSSUPPLIER-NAME
           END-IF
       EXIT SECTION.

       120-CHECK-DESCRIPTION-VALIDITY SECTION.
           IF WSSUPPLIER-DESCRIPTION1 NOT EQUAL SPACES
               CALL "LOWERUPPER"
               USING BY REFERENCE WSSUPPLIER-DESCRIPTION1
      *         MOVE TRIM(WSSUPPLIER-DESCRIPTION1) TO UNSTR
      *         PERFORM 155-REMOVE-EXTRA-SPACES
      *         MOVE UNSTR TO WSSUPPLIER-DESCRIPTION1
           END-IF
           IF WSSUPPLIER-DESCRIPTION2 NOT EQUAL SPACES
               CALL "LOWERUPPER"
               USING BY REFERENCE WSSUPPLIER-DESCRIPTION2
      *         MOVE TRIM(WSSUPPLIER-DESCRIPTION2) TO UNSTR
      *         PERFORM 155-REMOVE-EXTRA-SPACES
      *         MOVE UNSTR TO WSSUPPLIER-DESCRIPTION2
           END-IF
           IF WSSUPPLIER-DESCRIPTION3 NOT EQUAL SPACES
               CALL "LOWERUPPER"
               USING BY REFERENCE WSSUPPLIER-DESCRIPTION3
      *         MOVE TRIM(WSSUPPLIER-DESCRIPTION3) TO UNSTR
      *         PERFORM 155-REMOVE-EXTRA-SPACES
      *         MOVE UNSTR TO WSSUPPLIER-DESCRIPTION3
           END-IF
       EXIT SECTION.

       125-CHECK-ADDRESS-VALIDITY SECTION.
           IF WSSUPP-ADR-MAIN1 EQUAL SPACES THEN
               MOVE "address" TO ERROR-TEXT
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           ELSE
               CALL "LOWERUPPER" USING BY REFERENCE WSSUPP-ADR-MAIN1
      *         MOVE TRIM(WSSUPP-ADR-MAIN1) TO UNSTR
      *         PERFORM 155-REMOVE-EXTRA-SPACES
      *         MOVE UNSTR TO WSSUPP-ADR-MAIN1
               IF WSSUPP-ADR-MAIN2 NOT EQUAL SPACES
                   CALL "LOWERUPPER" USING BY REFERENCE WSSUPP-ADR-MAIN2
      *             MOVE TRIM(WSSUPP-ADR-MAIN2) TO UNSTR
      *             PERFORM 155-REMOVE-EXTRA-SPACES
      *             MOVE UNSTR TO WSSUPP-ADR-MAIN2
               END-IF
           END-IF
       EXIT SECTION.

       130-CHECK-POSTALCODE-VALIDITY SECTION.
           IF NOT VALID-POSTAL-CODE
           OR WSSUPPLIER-POSTAL-CODE2 IS ALPHABETIC THEN
               MOVE "postal code" TO ERROR-TEXT
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           END-IF
       EXIT SECTION.

       135-CHECK-TOWN-VALIDITY SECTION.
           IF WSSUPPLIER-TOWN IS NOT ALPHABETIC
           AND WSSUPPLIER-TOWN NOT EQUAL SPACES
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
               MOVE "town" TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           ELSE
               IF WSSUPPLIER-TOWN NOT EQUAL SPACES
                   CALL "LOWERUPPER" USING BY REFERENCE WSSUPPLIER-TOWN
      *             MOVE TRIM(WSSUPPLIER-TOWN) TO UNSTR
      *             PERFORM 155-REMOVE-EXTRA-SPACES
      *             MOVE UNSTR TO WSSUPPLIER-TOWN
           END-IF
       EXIT SECTION.

       140-CHECK-EMAIL-VALIDITY SECTION.
           IF WSSUPPLIER-EMAIL1 EQUAL SPACES
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
               MOVE "email1" TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           END-IF
       EXIT SECTION.

       140-CHECK-PHONE-VALIDITY SECTION.
           IF NOT VALID-PHONE1
      *         MOVE CSV-READ-ERROR TO ERROR-TEXT
               MOVE "phone1" TO ERROR-TEXT
               MOVE "N" TO CSV-VALID
           ELSE
               IF NOT VALID-PHONE2
                   MOVE ZERO TO WSSUPPLIER-TELEPHONE2
               END-IF
               IF NOT VALID-PHONE3
                   MOVE ZERO TO WSSUPPLIER-TELEPHONE3
               END-IF
           END-IF
       EXIT SECTION.

       150-WRITE-RECORD SECTION.
      *     IF CSV-VALID = "N"
      *         MOVE SUPCSVDATA TO ERRORSUPP
      *         OPEN I-O CSVERRORLOG
      *             WRITE ERRORLOG
      *         CLOSE CSVERRORLOG
      *         CLOSE FXKEYS
      *         DISPLAY CLEAR-SCREEN
      *         DISPLAY MAIN-SCREEN
      *         ACCEPT ERROR-ZONE
      *     ELSE
               PERFORM WITH TEST AFTER UNTIL SAVE-IT-VALID
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY CSV-ADD-SUPPLIER-SCREEN
                   MOVE "Y" TO WANT-TO-SAVE1
                   ACCEPT WANT-TO-SAVE
                   IF NOT SAVE-IT-VALID THEN
                       MOVE ERROR-SAVE TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
                   END-IF
               END-PERFORM
               IF SAVE-IT-YES THEN
                   REWRITE FDSUPPKEYS
                   END-REWRITE
                   CLOSE FXKEYS
                   OPEN I-O FXSUPPLY
                   WRITE SUPPLIER-DETAILS FROM WSSUPPLIER-DETAILS
                   END-WRITE
                   CLOSE FXSUPPLY
                   MOVE MESSAGE-WRITE-YES TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               ELSE
                   CLOSE FXKEYS
               END-IF
      *     END-IF
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
