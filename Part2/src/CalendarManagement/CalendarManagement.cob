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
           SELECT CALENDAR ASSIGN TO "CALENDARFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FD-DOWNTIME-ID
           FILE STATUS FILE-NOT-EXIST.

           SELECT KEYS ASSIGN TO "KEYSFILE"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS FILE-NOT-EXIST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
           COPY FDCALENDAR.

       FD  KEYS.
       01  FDKEYS                            PIC 9(003).

       WORKING-STORAGE SECTION.
           COPY WSCALENDAR.
           COPY PTLANGUAGE.

       77  FILE-NOT-EXIST                       PIC X(002).
       01  OPTION                               PIC 9(001).
           88  OPTION-REGISTER                  VALUE 1.
           88  OPTION-VIEW                      VALUE 2.
           88  OPTION-EDIT                      VALUE 3.
           88  OPTION-DELETE                    VALUE 4.
           88  VALID-OPTION                     VALUE 1 THRU 4.
       77  PRESS-KEY                            PIC X.

       SCREEN SECTION.
       01  MAIN-MENU-TEXT
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           03 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 16 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 17 COL 35.
           03 VALUE OPTION-REGISTER1 LINE 11 COL 50.
           03 VALUE OPTION-VIEW2     LINE 12 COL 50.
           03 VALUE OPTION-EDIT3     LINE 13 COL 50.
           03 VALUE OPTION-DELETE4   LINE 14 COL 50.
           03 VALUE OPTION-EXIT5     LINE 15 COL 50.
           03 VALUE "Please choose an option:" LINE 20 COL 45
           REVERSE-VIDEO.
           03 SC-OPTION PIC 9(02) LINE 20 COL 69 TO OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

       01  LIMPAR-ECRAN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120)    LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120)    LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120)    LINE 04 COL 01.
           03 VALUE "CALENDAR MANAGEMENT" LINE 03 COL 50.
           03 VALUE ALL " " PIC X(120)    LINE 24 COL 01.
           03 VALUE ALL " " PIC X(120)    LINE 25 COL 01.
           03 VALUE ALL " " PIC X(120)    LINE 26 COL 01.
           03 VALUE "F3 - BACK | F4 - EXIT"
           LINE 25 COL 99 FOREGROUND-COLOR 5.
           03 VALUE "  " LINE 24 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 96 BACKGROUND-COLOR 0.

       01  MAIN-REGISTER-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           05 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(50) LINE 16 COL 35.

       PROCEDURE DIVISION.
       CREATE-FILE SECTION.
           OPEN I-O CALENDAR
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT CALENDAR
              CLOSE CALENDAR
           END-IF
           CLOSE CALENDAR

           OPEN I-O KEYS
           IF FILE-NOT-EXIST = "35"
              OPEN OUTPUT KEYS
              CLOSE KEYS
           END-IF
           CLOSE KEYS

           EXIT SECTION.

       MAIN-MENU SECTION.
           PERFORM CREATE-FILE
           PERFORM UNTIL VALID-OPTION

              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU-TEXT

              EVALUATE OPTION
                 WHEN 1     PERFORM REGISTER-DOWNTIME
                 WHEN 2     PERFORM VIEW-DOWNTIME
                 WHEN 3     PERFORM EDIT-DOWNTIME
                 WHEN 4     PERFORM DELETE-DOWNTIME
              END-EVALUATE

           END-PERFORM
           STOP RUN
           EXIT SECTION.

       REGISTER-DOWNTIME SECTION.
           OPEN I-O CALENDAR

           PERFORM DOWNTIME-ID
           PERFORM DOWNTIME-DATE
           PERFORM DOWNTIME-DESCRIPTION

           WRITE FD-CALENDAR FROM WS-CALENDAR
           CLOSE CALENDAR

           EXIT SECTION.

       DOWNTIME-ID SECTION.
           OPEN I-O KEYS
           READ KEYS
           ADD 1 TO FDKEYS
           REWRITE FDKEYS
           MOVE FDKEYS TO WS-DOWNTIME-ID
           CLOSE KEYS

           EXIT SECTION.

       DOWNTIME-DATE SECTION.


       DOWNTIME-DESCRIPTION SECTION.


       VIEW-DOWNTIME SECTION.

       EDIT-DOWNTIME SECTION.

       DELETE-DOWNTIME SECTION.

       END PROGRAM CALENDAR-MANAGEMENT.
