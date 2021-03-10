      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS SUPPLIERS MANAGEMENT
      ******************************************************************
      *    REPORT MODULE - REPORT INGREDIENTS SUPPLIERS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 04.03.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT-RIS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RIS-FILE
              ASSIGN TO "FXRISSUPLY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RIS-FILE-REPORT ASSIGN TO "RIS.rpt".

       DATA DIVISION.
       FILE SECTION.
       FD  RIS-FILE.
       01  RIS-RECORD.
           88 EndOfFile VALUE HIGH-VALUES.
           05  RIS-ID                     PIC 9(06).
      *>          10 RIS-ID-ING              PIC 9(03).
      *>          10 RIS-ID-SUPP             PIC 9(03).
           05 RIS-PRICE                   PIC 9(03).
           05  DATE-VALIDITY               PIC 9(08).
      *>        10  DATE-YEAR                 PIC 9(004).
      *>        10  DATE-MONTH                PIC 9(002).
      *>        10  DATE-DAY                  PIC 9(02).
      *>      05 DATE-REPORT                     PIC 9(008).
        FD RIS-FILE-REPORT
           REPORT IS RIS-REPORT.

       WORKING-STORAGE SECTION.

      *> CONSTANTS SCREEN SECTION
       COPY CONSTANTS-RIS.

       *> OPTION VARIABLES

       01  DUMMY           PIC X(001).

       78 MAX-PAGE     VALUE 999.

       01 TABELA OCCURS 1 TO MAX-PAGE TIMES
           DEPENDING ON MAX-PAGE1
           INDEXED BY IND.
           05  TABRIS-ID                    PIC 9(06).
      *>          10 TABRIS-ID-ING              PIC 9(03).
      *>          10 TABRIS-ID-SUPP             PIC 9(03).
           05 TABRIS-PRICE                   PIC 9(03).
           05 TABDATE-VALIDITY               PIC 9(08).
      *>        10 TABDATE-YEAR                 PIC 9(004).
      *>        10 TABDATE-MONTH                PIC 9(002).
      *>        10 TABDATE-DAY                  PIC 9(02).


       01 MAX-PAGE1 PIC 999 VALUE 999.

       78 NUMBERCONST VALUE "  ID".
       78 PRICE       VALUE "PRICE".
       78 DATECONST   VALUE "PRICE VALIDITY".
       78 FATE-REPORT VALUE "DATE-REPORT".
       78 PAGECONST   VALUE "PAGE".
       78 REPORTTITLECONST
           VALUE "B R E A D W I C H  I N G R E D I E N T  S U P P L I E
      -    "R S   R E P O R T".



       REPORT SECTION.
       RD RIS-REPORT
      *>      CONTROLS ARE TBSUPPLIER-NUMBER (IND)

      * PAGE LIMIT IS THE MAX NUMBER OF LINES PER PAGE.
           PAGE LIMIT IS 54
      * FIRST DETAIL IS THE FIRST LINE WHERE THE TYPE IS DETAIL STARTS
           FIRST DETAIL 5
      * LAST DETAIL IS THE LAST LINE OF THE TYPE IS DETAIL
           LAST DETAIL 46
      * FOOTING IS THE LINE WHERE THE FOOTER STARTS
           FOOTING 48.
      * TYPE IS REPORT IS THE REPORTS NAME STRUCTURE. IN THE BREADWICH PROJECT
      *EACH WORD IS SEPARATED BY 3 SPACES AND EACH LETTER BY 1.
       01 TYPE IS REPORT HEADING.
           02 LINE 1.
           03 COLUMN 02 VALUE REPORTTITLECONST.
            02 LINE PLUS 1.
           03 COL 02 VALUE "TESTE DA LINHA".

      *TYPE IS PAGE DEFINES THE HEADER STRUCTURE OF EVERY PAGE
       01 TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
           03 COLUMN 02 VALUE NUMBERCONST.
           03 COLUMN 17 VALUE PRICE.
           03 COLUMN 32 VALUE DATECONST.
      * TYPE IS DETAIL IS THE REPORTS STRUCTURE, THE DATA WE READ FROM THE DATABASE
      * WETHER IT'S A FILE OR AN ARRAY.
       01 LINE-DETAIL TYPE IS DETAIL.
           02 LINE IS PLUS 1.
           03 COLUMN 02 PIC 9(6)
              SOURCE TABRIS-ID  (IND).
           03 COLUMN 18 PIC X(25)
              SOURCE TABRIS-PRICE (IND).
      *>      03 COLUMN 32 PIC 9(03) SOURCE TABRIS-PRICE  (IND).
           03 COLUMN  35 PIC 9(08) SOURCE TABDATE-VALIDITY (IND).
      * TYPE IS FOOTING IT'S THE FOOTER STRUCTURE OF EACH PAGE
       01 TYPE IS PAGE FOOTING.
           02 LINE IS 49.
             03 COLUMN 60 PIC X(6) VALUE PAGECONST.
             03 COLUMN PLUS 1 PIC Z9 SOURCE PAGE-COUNTER.

       SCREEN SECTION.

        01  CLEAR-SCREEN.
           03 BLANK SCREEN.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME-REPORT LINE 03 COL 43.
           05 VALUE ALL " " PIC X(95)  LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22)  LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25  COL 100 FOREGROUND-COLOR 5.

            01 ERROR-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 ERROR-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY1 LINE 26 COL 95 PIC X TO DUMMY AUTO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           PERFORM REPORT-RIS
           MOVE REPORT-DONE TO ERROR-TEXT
           ACCEPT ERROR-ZONE
           EXIT PROGRAM.
       REPORT-RIS SECTION.

           OPEN INPUT RIS-FILE.
           SET IND TO 1
           PERFORM UNTIL EndOfFile
               READ RIS-FILE
                   AT END
                       SET ENDOFFILE TO TRUE
                       MOVE IND TO MAX-PAGE1
                   NOT AT END PERFORM LOAD-TAB
               END-READ
           END-PERFORM
           CLOSE RIS-FILE

           OPEN OUTPUT RIS-FILE-REPORT.
           INITIATE RIS-REPORT.
           SET IND TO 1
           PERFORM UNTIL IND >= MAX-PAGE1
               PERFORM PRINT-REPORT
           END-PERFORM
           TERMINATE RIS-REPORT.
           CLOSE RIS-FILE-REPORT.
           EXIT SECTION.
           PRINT-REPORT SECTION.
           GENERATE LINE-DETAIL
           SET IND UP BY 1.

           LOAD-TAB SECTION.
           MOVE RIS-RECORD TO TABELA (IND)
           SET IND UP BY 1
           EXIT SECTION.
