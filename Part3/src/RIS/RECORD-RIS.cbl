      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS SUPPLIERS MANAGEMENT
      ******************************************************************
      *    RIS MODULE - REGISTRATION INGREDIENTS SUPPLIERS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 04.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-RIS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          *> INGREDIENTS SUPPLIERS FILE
               SELECT FXRISUPPLY ASSIGN TO "FXRISSUPLY"
                   ORGANISATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS RIS-ID
                   FILE STATUS RIS-STATUS.

          *> INGREDIENTS FILE
               SELECT FXINGRED ASSIGN TO "FXINGREDS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS INGREDS-ID
                   FILE STATUS INGRED-STATUS.

          *> SUPPLIER FILE
               SELECT FXSUPPLY ASSIGN TO "FXSUPPLIERS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SUPPLIER-ID
                   ALTERNATE KEY IS SUPPLIER-TOWN WITH DUPLICATES
                   ALTERNATE KEY IS SUPPLIER-NAME WITH DUPLICATES
                   FILE STATUS SUPP-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *> FD RESGISTRATION SUPPLY INGREDIENT MANAGEMENT
       FD FXRISUPPLY.
              COPY FD-RIS.
      *> FD INGREDEINT MANAGEMNET
       FD FXINGRED.
               COPY FD-INGREDSFX.

      *> FD SUPPLY MANAGEMENT
       FD FXSUPPLY.
               COPY SUPPLIERFX.

       WORKING-STORAGE SECTION.
      *> CONSTANTS SCREEN SECTION
       COPY CONSTANTS-RIS.

      *> WS VARIABLES INGREDIENTS
       COPY WS-INGREDSFX.

      *> WS VARIABLE RECORD INGREDIENTS SUPLIERS
       COPY WS-RIS.

       COPY WSSupplierFX.

      *> UTILITY VARIBLES
       01  EDIT-OPTION                         PIC X(002).
           88 EDIT-VALID-OPTION                VALUE "Y" "y" "N" "n".
           88 EDIT-OPTION-NO                   VALUE "N" "n".
       77  DUMMY                               PIC X(001).
       77  INGRED-STATUS                       PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  FXKEY-STATUS                        PIC 9(002).
       77  SUPP-STATUS                         PIC 9(002).

       78  NOT-FILE                            VALUE "35".
       78  F1                                  VALUE "1001".
       78  F2                                  VALUE "1002".
       78  F3                                  VALUE "1003".
       77 RIS-STATUS                           PIC 9(002).
       01  SAVE-IT1                            PIC X(002).
           88 SAVE-IT1-YES                     VALUE "Y" "y".
           88 SAVE-IT1-VALID                   VALUE "Y" "y" "N" "n".
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  INGREDEXIST                         PIC X(001).
           88 INGREDEXIST-YES                  VALUE "Y".
       01  SUPP-EXIST                          PIC X(001).
           88 SUPP-YES                         VALUE "Y".
       77  VIEW-NAME-SUPP                      PIC X(030).
       77  VIEW-NAME-ING                       PIC X(030).

       77 ILIN                         PIC 9(002).
       77 ICOL                         PIC 9(002).
       77 EOF                          PIC X(001).
       77 TRUE-YES                     PIC X(001).
       77 COUNTPAGE                    PIC 9(002).

       01 PAGINA                       PIC 9(003).
       78 MAX-ING                      VALUE 999.
       01 TABLE-INGREDS OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLEINGREDS-ID                  PIC 9(003).
           05 TABLEINGREDS-NAME                PIC X(030).
      *>      05 TABLEINGREDS-DESCRIPTION         PIC X(050).
      *>      05 TABLEINGREDS-UNIT-SUPPLIER       PIC X(003).
      *>      05 TABLEINGREDS-UNIT-SANDWICH       PIC X(003).
      *>      05 TABLETRESHOLD                    PIC 9(003).
      *>      05 TABLEINGREDS-IS-ACTIVE           PIC 9(001).
       77 NUMBER-ING                           PIC 9(003) VALUE 999.
       01 FLAGTABLE                PIC 9(001).

       78 MAX-SUPP                  VALUE 999.
       01 TABLE-SUPP OCCURS 1 TO MAX-SUPP TIMES
           DEPENDING ON NUMBER-SUPP
           INDEXED BY SUPP-INDEX.
           05 TABLESUPPLIER-ID                          PIC 9(003).
           05 TABLESUPPLIER-NAME                        PIC X(030).
      *>       05 TABLESUPPLIER-DESCRIPTION.
      *>           10 TABLESUPPLIER-DESCRIPTION1            PIC X(050).
      *>           10 TABLESUPPLIER-DESCRIPTION2            PIC X(050).
      *>           10 TABLESUPPLIER-DESCRIPTION3            PIC X(050).
      *>       05 TABLESUPPLIER-ADRESS.
      *>           10 TABLESUPP-ADR-MAIN.
      *>               15 TABLESUPP-ADR-MAIN1               PIC X(050).
      *>               15 TABLESUPP-ADR-MAIN2               PIC X(050).
      *>           10 TABLESUPPLIER-POSTAL-CODE.
      *>               15 TABLESUPPLIER-POSTAL-CODE1        PIC 9(004).
      *>               15 TABLESUPPLIER-POSTAL-CODE2        PIC 9(003).
      *>           10 TABLESUPPLIER-TOWN                    PIC X(030).
      *>       05 TABLESUPPLIER-EMAIL.
      *>           10 TABLESUPPLIER-EMAIL1                  PIC X(040).
      *>           10 TABLESUPPLIER-EMAIL2                  PIC X(040).
      *>           10 TABLESUPPLIER-EMAIL3                  PIC X(040).
      *>       05 TABLESUPPLIER-TELEPHONE.
      *>           10 TABLESUPPLIER-TELEPHONE1              PIC 9(009).
      *>           10 TABLESUPPLIER-TELEPHONE2              PIC 9(009).
      *>           10 TABLESUPPLIER-TELEPHONE3              PIC 9(009).
      *>       05 TABLESUPPLIER-IS-ACTIVE                   PIC 9(001).
       01 NUMBER-SUPP               PIC 9(003) VALUE 999.


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
           05 VALUE MODULE-NAME-MAIN   LINE 03 COL 43.
           05 VALUE ALL " " PIC X(95)  LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22)  LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25  COL 100 FOREGROUND-COLOR 5.
      ******************************************************************

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT  LINE 09 COL 13.
           05 VALUE ADD-SUPP-NAME  LINE 11 COL 13.
           05 VALUE ADD-MENU-TEXT1 LINE 13 COL 13.
           05 VALUE ADD-INGRED-NAME LINE 15 COL 13.
           05 VALUE ADD-MENU-TEXT2 LINE 17 COL 13.
           05 VALUE ADD-MENU-TEXT3 LINE 17 COL 31.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 GET-SUPPLIER-ID PIC 9(003) LINE 09 COL 27
               TO WS-RIS-ID-SUPP.
            05 SUPP-NAME-VIEW PIC X(30) LINE 11 COL 14
                   FROM VIEW-NAME-SUPP.
           05 GET-INGREDIENT-ID PIC 9(003) LINE 13 COL 28
               TO WS-RIS-ID-ING.
            05 INGRED-NAME-VIEW PIC X(30) LINE 15 COL 13
                   FROM VIEW-NAME-ING.
           05 GET-PRICE PIC 9(003) LINE 17 COL 19
               TO WS-RIS-PRICE.
           05 GET-EXPIRATION-DATE.
               10 GET-DAY PIC 9(002) LINE 17 COL 41
                   TO WS-RIS-DAY.
               10 GET-MONTH PIC 9(002) LINE 17 COL 44
                   TO WS-RIS-MONTH.
               10 GET-YEAR PIC 9(004) LINE 17 COL 47
                   TO WS-RIS-YEAR.
           05 VALUE PRICE-EURO LINE 17 COL 23.
           05 VALUE "/"  LINE 17 COL 43.
           05 VALUE "/"  LINE 17 COL 46.
           05 VALUE "|"  LINE 17 COL 29.
           05 VALUE "  " LINE 8 COL 09  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09  BACKGROUND-COLOR 7.
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
           05 VALUE "  " LINE 8  COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9  COL 62 BACKGROUND-COLOR 7.
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

      ******************************************************************
      *> VIEW NAME SUPPLIER
      *>  01 VIEW-SUPP.
      *>      05 SUPP-NAME-VIEW PIC X(30) LINE 11 COL 14
      *>          FROM VIEW-NAME-SUPP.


      *> VIEW NAME INGREDIENT
      *>  01 VIEW-INGRED.
      *>      05 INGRED-NAME-VIEW PIC X(30) LINE 15 COL 13
      *>          FROM WSINGREDS-NAME.

      ******************************************************************
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08  COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08  COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 68  BACKGROUND-COLOR 7.
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
       01  INSTRUCTIONS-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 INSTRUCTION-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  PREVIOUS-NEXT-TEXT.
           05 PREVIOUS-NEXT-MESSAGE PIC X(90) LINE 26 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01 SUPP-LIST.
           05 LIST-SUPP-ID PIC 9(003) LINE ILIN COL ICOL
               FROM TABLESUPPLIER-ID (SUPP-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLESUPPLIER-NAME (SUPP-INDEX).
      ******************************************************************
       01 INGRED-LIST.
           05 LIST-INGRED-ID PIC 9(003) LINE ILIN COL ICOL
               FROM TABLEINGREDS-ID (ING-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLEINGREDS-NAME (ING-INDEX).
      ******************************************************************
       01  EMPTY-LIST-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 18 COL 35.
           05 VALUE EMPTY-RECORDS      LINE 12 COL 38.
           05 VALUE EMPTY-RECORDS2     LINE 15 COL 47.
           05 LINE 01 COL 01 PIC X TO DUMMY AUTO.
      ******************************************************************

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      *>      PERFORM UNTIL TRUE-YES = "Y"
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           PERFORM CHECK-FILES-OK
           PERFORM FILL-TABLES


           PERFORM GET-SUPPLIER
           PERFORM GET-INGREDIENT


      *>      END-PERFORM

           EXIT PROGRAM.
      *     PERFORM SHOW-TABLE
               PERFORM SUPPLIER-LIST
                   IF TRUE-YES = "Y" OR KEYSTATUS = 1003 THEN
                       MOVE SPACE TO INGREDEXIST
                       EXIT PROGRAM
                   END-IF
      *>          PERFORM 106-CHECK-IF-RIS-ID-EXISTS
               IF KEYSTATUS = 1003 THEN
                   MOVE SPACE TO INGREDEXIST
                   EXIT PROGRAM
               END-IF.

      *> PRECISO DE COLOCAR VERIFICACAO DE FICHEIROS VAZIOS!
       FILL-TABLES SECTION.

           SET SUPP-INDEX TO 0

           OPEN INPUT FXSUPPLY
           PERFORM UNTIL EOFSUPPLIER
           READ FXSUPPLY
               AT END SET EOFSUPPLIER TO TRUE
               MOVE SUPP-INDEX TO NUMBER-SUPP
               NOT AT END
                   SET SUPP-INDEX UP BY 1
                   PERFORM LOAD-SUPP-TABLE
               END-READ
           END-PERFORM
           CLOSE FXSUPPLY

                    SET ING-INDEX TO 0
            OPEN INPUT FXINGRED
            PERFORM UNTIL EOFINGREDS
                READ FXINGRED NEXT RECORD
                    AT END
                        SET EOFINGREDS TO TRUE
                        MOVE ING-INDEX TO NUMBER-ING
                    NOT AT END
                        SET ING-INDEX UP BY 1
                        PERFORM LOAD-INGRED-TABLE
                END-READ
            END-PERFORM
            CLOSE FXINGRED
       EXIT SECTION.

       LOAD-INGRED-TABLE SECTION.
           MOVE INGREDS-DETAILS TO TABLE-INGREDS (ING-INDEX)

       EXIT SECTION.
       LOAD-SUPP-TABLE SECTION.
           MOVE SUPPLIER-DETAILS TO TABLE-SUPP (SUPP-INDEX)

       EXIT SECTION.


       SHOW-TABLE SECTION.
           SET ING-INDEX TO 1
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               DISPLAY TABLE-INGREDS (ING-INDEX) ACCEPT OMITTED
               SET ING-INDEX UP BY 1
           END-PERFORM

           EXIT SECTION.

       GET-INGREDIENT SECTION.
           DISPLAY LIST-FRAME
           DISPLAY INGRED-LIST
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL INGREDEXIST-YES
               PERFORM INGREDIENT-LIST
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM CHECK-INGRED
                IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF


           END-PERFORM

           EXIT SECTION.

       GET-SUPPLIER SECTION.

           DISPLAY LIST-FRAME
           DISPLAY SUPP-LIST
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL SUPP-YES
               PERFORM SUPPLIER-LIST
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM CHECK-SUPP
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               DISPLAY SUPP-NAME-VIEW
           END-PERFORM

           EXIT SECTION.

       SUPPLIER-LIST SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-SCREEN
           MOVE ZEROES TO GET-SUPPLIER-ID
           MOVE SPACES TO TRUE-YES
           SET SUPP-INDEX TO 1
           MOVE 09 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 12 TO PAGINA
           PERFORM UNTIL SUPP-INDEX >= NUMBER-SUPP
               DISPLAY SUPP-LIST
               ADD 1 TO ILIN
               ADD 1 TO PAGINA
               SET SUPP-INDEX UP BY 1
               IF ILIN = 21 THEN
                    MOVE "ING-INST" TO INSTRUCTION-MESSAGE
                    DISPLAY INSTRUCTION-MESSAGE
                    MOVE F1-F2 TO PREVIOUS-NEXT-MESSAGE
                    DISPLAY PREVIOUS-NEXT-TEXT
                   ACCEPT GET-SUPPLIER-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       SET SUPP-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 12 TO PAGINA
                   ELSE
                       IF KEYSTATUS = F2 THEN
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

               IF SUPP-INDEX >= NUMBER-SUPP
                   MOVE "ING-INSTR" TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-SUPPLIER-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       SET SUPP-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 12 TO PAGINA
                   END-IF

               END-IF
           END-PERFORM

           EXIT SECTION.

          *> INGREDEINT SCREEN VIEW
           INGREDIENT-LIST SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-SCREEN
      *>      MOVE ZEROES TO NEW-INGREDID
      *>      MOVE SPACES TO TRUE-YES
           SET ING-INDEX TO 1
           MOVE 09 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 12 TO PAGINA
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               DISPLAY INGRED-LIST
               ADD 1 TO ILIN
               ADD 1 TO PAGINA
               SET ING-INDEX UP BY 1
               IF ILIN = 21 THEN
                    MOVE "ING-INST" TO INSTRUCTION-MESSAGE
                    DISPLAY INSTRUCTION-MESSAGE
                    MOVE F1-F2 TO PREVIOUS-NEXT-MESSAGE
                    DISPLAY PREVIOUS-NEXT-TEXT
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 09 TO ILIN
                       SET ING-INDEX DOWN BY PAGINA
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 12 TO PAGINA
                   ELSE
                       IF KEYSTATUS = F2 THEN
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
                   MOVE "ING-INSTR" TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
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
           EXIT SECTION.
      *> CHECK FILE STATUS INGREDIENTS SUPPLIER AND CREATE, IF OTHERS
      *> FILES DONT EXIST, DISPLAY ERROR MESSAGE
      *> NOT-FILE IS A CONSTANT WITH VALUE 35
       CHECK-FILES-OK SECTION.
          *> CHECK FILE INGREDIENTS SUPPLY
           OPEN INPUT FXRISUPPLY
               IF RIS-STATUS = NOT-FILE THEN
                   OPEN OUTPUT FXRISUPPLY
                   CLOSE FXRISUPPLY
                ELSE
                   CLOSE FXRISUPPLY
                END-IF
          *> CHECK INGREDIENTS FILE EXIST
           OPEN INPUT FXINGRED
               IF INGRED-STATUS = NOT-FILE THEN
                   MOVE FILE-STATUS-INGREDIENTS TO ERROR-TEXT
                   DISPLAY MAIN-SCREEN
                   ACCEPT ERROR-ZONE
                   EXIT PROGRAM
                ELSE
                   CLOSE FXINGRED
                END-IF
          *> CHECK SUPPLIERS FILE EXIST
           OPEN INPUT FXSUPPLY
               IF SUPP-STATUS = NOT-FILE THEN
                   MOVE FILE-STATUS-SUPPLIER TO ERROR-TEXT
                   DISPLAY MAIN-SCREEN
                   ACCEPT ERROR-ZONE
                   EXIT PROGRAM
                ELSE
                   CLOSE FXSUPPLY
                END-IF
           EXIT SECTION.

       CHECK-SUPP SECTION.
           MOVE SPACES TO SUPP-EXIST
              SET SUPP-INDEX TO 1
           PERFORM UNTIL SUPP-INDEX > NUMBER-SUPP
               IF WS-RIS-ID-SUPP = TABLESUPPLIER-ID (SUPP-INDEX)
                   MOVE "Y" TO SUPP-EXIST
                   MOVE TABLESUPPLIER-NAME (SUPP-INDEX)
                       TO VIEW-NAME-SUPP
                   MOVE NUMBER-SUPP TO SUPP-INDEX

               END-IF
               SET SUPP-INDEX UP BY 1
           END-PERFORM
           IF SUPP-EXIST <> "Y" THEN
               MOVE ERROR-SUPPID-NO TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       CHECK-INGRED SECTION.
           MOVE SPACES TO INGREDEXIST
           SET ING-INDEX TO 1
           PERFORM UNTIL ING-INDEX > NUMBER-ING
               IF WS-RIS-ID-ING = TABLEINGREDS-ID (ING-INDEX)
                   MOVE "Y" TO INGREDEXIST
                   MOVE TABLEINGREDS-NAME (ING-INDEX)
                       TO VIEW-NAME-ING
                   MOVE NUMBER-ING TO ING-INDEX
               END-IF
               SET ING-INDEX UP BY 1
           END-PERFORM
           IF INGREDEXIST <> "Y" THEN
               MOVE ERROR-INGREDID-NO TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF




       EXIT SECTION.
