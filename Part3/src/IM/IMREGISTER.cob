
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMREGISTER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

              SELECT INVENTORY ASSIGN TO "INVENTORYFILE"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS IM-ID
                   FILE STATUS IS INVENTORY-FS.

               SELECT FXINGRED ASSIGN TO "FXINGREDS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS INGREDS-ID
                   FILE STATUS INGRED-STATUS.

               SELECT FXKEYS ASSIGN TO "INGREDKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY.
       COPY FD-IM.

       FD FXINGRED.
       COPY FD-INGREDSFX.

       FD FXKEYS.
       01  FDINGREDKEYS                   PIC 9(003).

       WORKING-STORAGE SECTION.
       COPY WS-IM.

       COPY RSOWSVAR.
       COPY IMTABLES.
       COPY VAR-VALIDDATE.
       COPY RSOCONSTANTS.
       COPY WS-INGREDSFX.

       77  INGRED-STATUS                       PIC 9(002).
       77  INVENTORY-FS                        PIC 9(002).
       77  FXKEY-STATUS                        PIC 9(002).

       77  DUMMY                               PIC X(001).
       77  SUPP-STATUS                         PIC 9(002).

       78  NOT-FILE                            VALUE "35".
       78  F1                                  VALUE "1001".
       78  F2                                  VALUE "1002".
       78  F3                                  VALUE "1003".
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

       01 MAXPERPAGE                   PIC 9(003).
       78 MAX-ING                      VALUE 999.


      ******************************************************************

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

      ******************************************************************

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 45.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(022) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 26 COL 98.
           05 VALUE MAIN-TEXT1 LINE 25 COL 100 FOREGROUND-COLOR 5.

      ******************************************************************

       01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(046) LINE 10 COL 03.
           05 VALUE ALL " " PIC X(048) LINE 07 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(048) LINE 22 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 49 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 12.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 05.
           05 VALUE REGISTER-TEXT-IN-DATE       LINE 15 COL 05.
           05 VALUE REGISTER-TEXT-OUT-DATE      LINE 16 COL 05.
           05 VALUE REGISTER-TEXT-IN-QUANTITY   LINE 17 COL 05.
           05 VALUE REGISTER-TEXT-OUT-QUANTITY  LINE 18 COL 05.
           05 VALUE REGISTER-TEXT-ACTZ-DATE     LINE 19 COL 05.
           05 REG-ID PIC 9(003) LINE 13 COL 26 USING WS-INGRED-ID.
           05 REG-REC.
              10 REG-IN-DATE.
                 15 REG-IN-DAY PIC X(002) LINE 15 COL 26 TO
                    WS-TIME-MOVE-IN-DAY AUTO REQUIRED.
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-IN-MONTH PIC X(002) LINE 15 COL 29 TO
                    WS-TIME-MOVE-IN-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-IN-YEAR PIC X(004) LINE 15 COL 32 TO
                    WS-TIME-MOVE-IN-YEAR AUTO REQUIRED.
              10 REG-IN-TIME.
                 15 LINE 15 COL 37 VALUE "|".
                 15 REG-IN-HOUR PIC X(002) LINE 15 COL 39 TO
                    WS-TIME-MOVE-IN-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 41 VALUE ":".
                 15 REG-IN-MINUTE PIC X(002) LINE 15 COL 42 TO
                    WS-TIME-MOVE-IN-MINUTE AUTO REQUIRED.


              10 REG-OUT-DATE.
                 15 REG-OUT-DAY PIC X(002) LINE 16 COL 26 TO
                    WS-TIME-MOVE-OUT-DAY AUTO REQUIRED.
                 15 LINE 16 COL 28 VALUE "/".
                 15 REG-OUT-MONTH PIC X(002) LINE 16 COL 29 TO
                    WS-TIME-MOVE-OUT-MONTH AUTO REQUIRED.
                 15 LINE 16 COL 31 VALUE "/".
                 15 REG-OUT-YEAR PIC X(004) LINE 16 COL 32 TO
                    WS-TIME-MOVE-OUT-YEAR AUTO REQUIRED.
              10 REG-OUT-TIME.
                 15 LINE 16 COL 37 VALUE "|".
                 15 REG-OUT-HOUR PIC X(002) LINE 16 COL 39 TO
                    WS-TIME-MOVE-OUT-HOUR AUTO REQUIRED.
                 15 LINE 16 COL 41 VALUE ":".
                 15 REG-OUT-MINUTE PIC X(002) LINE 16 COL 42 TO
                    WS-TIME-MOVE-OUT-MINUTE AUTO REQUIRED.


              10 REG-QUANTITY-IN PIC 9(003) LINE 17 COL 26
                 TO WS-MOVE-IN-QTD AUTO REQUIRED.
              10 REG-QUANTITY-OUT PIC 9(003) LINE 18 COL 26
                 TO WS-MOVE-OUT-QTD AUTO REQUIRED.

              10 REG-ACTZ-DATE.
                 15 REG-ACTZ-DAY PIC X(002) LINE 19 COL 26 TO
                    WS-TIME-ACTZ-DAY AUTO REQUIRED.
                 15 LINE 19 COL 28 VALUE "/".
                 15 REG-ACTZ-MONTH PIC X(002) LINE 19 COL 29 TO
                    WS-TIME-ACTZ-MONTH AUTO REQUIRED.
                 15 LINE 19 COL 31 VALUE "/".
                 15 REG-ACTZ-YEAR PIC X(004) LINE 19 COL 32 TO
                    WS-TIME-ACTZ-YEAR AUTO REQUIRED.
              10 REG-ACTZ-TIME.
                 15 LINE 19 COL 37 VALUE "|".
                 15 REG-ACTZ-HOUR PIC X(002) LINE 19 COL 39 TO
                    WS-TIME-ACTZ-HOUR AUTO REQUIRED.
                 15 LINE 19 COL 41 VALUE ":".
                 15 REG-ACTZ-MINUTE PIC X(002) LINE 19 COL 42 TO
                    WS-TIME-ACTZ-MINUTE AUTO REQUIRED.

      ******************************************************************

       01  SHOW-REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(088) LINE 10 COL 03.
           05 VALUE ALL " " PIC X(090) LINE 07 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(090) LINE 22 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 91 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 33.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 05.
           05 VALUE REGISTER-TEXT-IN-DATE       LINE 15 COL 05.
           05 VALUE REGISTER-TEXT-OUT-DATE      LINE 16 COL 05.
           05 VALUE REGISTER-TEXT-IN-QUANTITY   LINE 17 COL 05.
           05 VALUE REGISTER-TEXT-OUT-QUANTITY  LINE 18 COL 05.
           05 VALUE REGISTER-TEXT-ACTZ-DATE     LINE 19 COL 05.

           05 REG-ID2 PIC 9(003) LINE 13 COL 26 USING WS-INGRED-ID.
           05 REG-REC2.
              10 REG-IN-DATE2.
                 15 REG-IN-DAY2 PIC X(002) LINE 15 COL 26 TO
                    WS-TIME-MOVE-IN-DAY AUTO REQUIRED.
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-IN-MONTH2 PIC X(002) LINE 15 COL 29 TO
                    WS-TIME-MOVE-IN-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-IN-YEAR2 PIC X(004) LINE 15 COL 32 TO
                    WS-TIME-MOVE-IN-YEAR AUTO REQUIRED.
              10 REG-IN-TIME2.
                 15 LINE 15 COL 37 VALUE "|".
                 15 REG-IN-HOUR2 PIC X(002) LINE 15 COL 39 TO
                    WS-TIME-MOVE-IN-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 41 VALUE ":".
                 15 REG-IN-MINUTE2 PIC X(002) LINE 15 COL 42 TO
                    WS-TIME-MOVE-IN-MINUTE AUTO REQUIRED.


              10 REG-OUT-DATE2.
                 15 REG-OUT-DAY2 PIC X(002) LINE 16 COL 26 TO
                    WS-TIME-MOVE-OUT-DAY AUTO REQUIRED.
                 15 LINE 16 COL 28 VALUE "/".
                 15 REG-OUT-MONTH2 PIC X(002) LINE 16 COL 29 TO
                    WS-TIME-MOVE-OUT-MONTH AUTO REQUIRED.
                 15 LINE 16 COL 31 VALUE "/".
                 15 REG-OUT-YEAR2 PIC X(004) LINE 16 COL 32 TO
                    WS-TIME-MOVE-OUT-YEAR AUTO REQUIRED.
              10 REG-OUT-TIME2.
                 15 LINE 16 COL 37 VALUE "|".
                 15 REG-OUT-HOUR2 PIC X(002) LINE 16 COL 39 TO
                    WS-TIME-MOVE-OUT-HOUR AUTO REQUIRED.
                 15 LINE 16 COL 41 VALUE ":".
                 15 REG-OUT-MINUTE2 PIC X(002) LINE 16 COL 42 TO
                    WS-TIME-MOVE-OUT-MINUTE AUTO REQUIRED.


              10 REG-QUANTITY-IN2 PIC 9(003) LINE 17 COL 26
                 TO WS-MOVE-IN-QTD AUTO REQUIRED.
              10 REG-QUANTITY-OUT2 PIC 9(003) LINE 18 COL 26
                 TO WS-MOVE-OUT-QTD AUTO REQUIRED.

              10 REG-ACTZ-DATE2.
                 15 REG-ACTZ-DAY2 PIC X(002) LINE 19 COL 26 TO
                    WS-TIME-ACTZ-DAY AUTO REQUIRED.
                 15 LINE 19 COL 28 VALUE "/".
                 15 REG-ACTZ-MONTH2 PIC X(002) LINE 19 COL 29 TO
                    WS-TIME-ACTZ-MONTH AUTO REQUIRED.
                 15 LINE 19 COL 31 VALUE "/".
                 15 REG-ACTZ-YEAR2 PIC X(004) LINE 19 COL 32 TO
                    WS-TIME-ACTZ-YEAR AUTO REQUIRED.
              10 REG-ACTZ-TIME2.
                 15 LINE 19 COL 37 VALUE "|".
                 15 REG-ACTZ-HOUR2 PIC X(002) LINE 19 COL 39 TO
                    WS-TIME-ACTZ-HOUR AUTO REQUIRED.
                 15 LINE 19 COL 41 VALUE ":".
                 15 REG-ACTZ-MINUTE2 PIC X(002) LINE 19 COL 42 TO
                    WS-TIME-ACTZ-MINUTE AUTO REQUIRED.

      ******************************************************************

       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(064) LINE 07 COL 54
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(064) LINE 22 COL 54
              BACKGROUND-COLOR 7.
           05 TEXT0 PIC X(057)    LINE 08 COL 58 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(064) LINE 09 COL 54.
           05 VALUE ALL "Ä" PIC X(064) LINE 20 COL 54.
           05 TEXT1 PIC X(020)    LINE 21 COL 58 FOREGROUND-COLOR 5.
           05 TEXT2 PIC X(019)    LINE 21 COL 97 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 22 COL 118 BACKGROUND-COLOR 7.

      ******************************************************************

       01  MOVES-LIST LINE ILIN COL ICOL.
           05 LIST-IN-DAY   PIC X(002) FROM MOVES-IN-DAY (IND-MOVES).
           05 VALUE "/".
           05 LIST-IN-MONTH PIC X(002) FROM MOVES-IN-MONTH (IND-MOVES).
           05 VALUE "/".
           05 LIST-IN-YEAR PIC X(004)  FROM MOVES-IN-YEAR (IND-MOVES).
           05 VALUE " - ".
           05 LIST-IN-HOUR PIC X(002)  FROM MOVES-IN-HOUR (IND-MOVES).
           05 VALUE ":".
           05 LIST-IN-MIN PIC X(002)   FROM MOVES-IN-MIN (IND-MOVES).
           05 VALUE UNTIL-LIST.

           05 LIST-OUT-DAY   PIC X(002) FROM MOVES-OUT-DAY (IND-MOVES).
           05 VALUE "/".
           05 LIST-OUT-MONTH PIC X(002)
               FROM MOVES-OUT-MONTH (IND-MOVES).
           05 VALUE "/".
           05 LIST-OUT-YEAR PIC X(004)  FROM MOVES-OUT-YEAR (IND-MOVES).
           05 VALUE " - ".
           05 LIST-OUT-HOUR PIC X(002)  FROM MOVES-OUT-HOUR (IND-MOVES).
           05 VALUE ":".
           05 LIST-OUT-MIN PIC X(002)   FROM MOVES-OUT-MIN (IND-MOVES).


           05 LIST-ACTZ-DAY PIC X(002) FROM MOVES-ACTZ-DAY (IND-MOVES).
           05 VALUE "/".
           05 LIST-ACTZ-MONTH PIC X(002)
               FROM MOVES-ACTZ-MONTH (IND-MOVES).
           05 VALUE "/".
           05 LIST-ACTZ-YEAR PIC X(004)
               FROM MOVES-ACTZ-YEAR (IND-MOVES).
           05 VALUE " - ".
           05 LIST-ACTZ-HOUR PIC X(002)
               FROM MOVES-ACTZ-HOUR (IND-MOVES).
           05 VALUE ":".
           05 LIST-ACTZ-MIN PIC X(002) FROM MOVES-ACTZ-MIN (IND-MOVES).

      ******************************************************************

       01  TOTALQUANT-LIST.
           05 LIST-INGREDS-ID PIC 9(003)   LINE ILIN COL ICOL
              FROM TAB-INGREDS-ID (IND-TOTALQUANT).
           05 VALUE "|"                   LINE ILIN COL PLUS 2.
           05 LIST-QTD-TOTAL PIC 9(003) LINE ILIN COL PLUS 2
              FROM TAB-QTD-TOTAL (IND-TOTALQUANT).


      ******************************************************************

       01  CLEAR-LIST.
           05 VALUE ALL " " PIC X(060) LINE 10 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 11 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 12 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 13 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 14 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 15 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 16 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 17 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 18 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 19 COL 56.

      ******************************************************************

       01  COMMENTS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  INSTRUCTIONS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

      ******************************************************************

       01  SAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SS-SAVE PIC X(002) LINE 25 COL 61
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
