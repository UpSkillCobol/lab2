      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 13.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALENDAR-MANAGEMENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALENDARFILE ASSIGN TO "CALENDARFILE.txt"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FD-DOWNTIME-ID.
           SELECT FDKEYS ASSIGN TO "KEYSFILE.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS FILE-NOT-EXIST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDARFILE.
           COPY FDCALENDAR.

       FD  FDKEYS.
       01  WRITEKEYS                            PIC 9(003).

       WORKING-STORAGE SECTION.
           COPY WSCALENDAR.

       77  FILE-NOT-EXIST                       PIC X(002).
       01  OPTION                               PIC 9(001).
           88  OPTION-ADD                       VALUE 1.
           88  OPTION-UPDATE                    VALUE 2.
           88  OPTION-REMOVE                    VALUE 3.
           88  OPTION-VIEW                      VALUE 4.
           88  OPTION-EXIT                      VALUE 5.
           88  VALID-ALTERAR                    VALUE 1 THRU 5.
       77  PRESS-KEY                            PIC X.

       SCREEN SECTION.
       01  MAIN-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 6.
           03  BLANK SCREEN.
           03  LINE 11 COL 47 VALUE "C A L E N D A R     M A N A G E M E
      -        "N T".
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 19 COL 03 VALUE "CHOOSE AN OPTION:".
           03  OPTION-SCREEN PIC 9(001) LINE 19 COL 21 TO OPTION AUTO.
           03  LINE 20 COL 01 PIC X(20) VALUE ALL "_".

       PROCEDURE DIVISION.
       CREATE-FILE SECTION.
           OPEN I-O CALENDARFILE
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT CALENDARFILE
              CLOSE CALENDARFILE
           END-IF
           CLOSE CALENDARFILE
           EXIT SECTION.

           OPEN I-O FDKEYS
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT FDKEYS
              CLOSE FDKEYS
           END-IF
           CLOSE FDKEYS
           EXIT SECTION.

       MAIN-MENU SECTION.
           PERFORM UNTIL OPTION = 5

              ACCEPT MAIN-SCREEN

              EVALUATE OPTION
                 WHEN 1
                    PERFORM ID-DOWNTOWN
                 WHEN 2
                    PERFORM ID-DOWNTOWN
                 WHEN 3
                    PERFORM ID-DOWNTOWN
                 WHEN 4
                    PERFORM ID-DOWNTOWN
              END-EVALUATE

           END-PERFORM
           STOP RUN
           EXIT SECTION.

       ID-DOWNTOWN SECTION.
           OPEN I-O FDKEYS
           READ FDKEYS
           ADD 1 TO WRITEKEYS
           MOVE WRITEKEYS TO WS-DOWNTOWN-ID
           REWRITE FDKEYS
           EXIT SECTION.
       END PROGRAM CALENDAR-MANAGEMENT.
