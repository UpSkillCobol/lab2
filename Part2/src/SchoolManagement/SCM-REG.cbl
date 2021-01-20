      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-REG.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCHOOLS ASSIGN TO "SCHOOLS"
           ORGANIZATION IS INDEXED
           RECORD KEY IS SCHOOL-INTERNAL-ID
           ACCESS IS DYNAMIC
           ALTERNATE KEY IS SCHOOL-EXTERNAL-ID
           FILE STATUS IS FILE-STATUS.

           SELECT SCHOOLS1 ASSIGN TO "SCHOOLS1.csv"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT KEYS ASSIGN TO "KEYS-SCM.txt"
           ORGANIZATION IS SEQUENTIAL
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
           05 SCHOOL-IS-ACTIVE                     PIC 9(009).

       FD  SCHOOLS1.
           01 SCHOOL1                              PIC X(200).

       FD  KEYS.
           01 FD-KEYS.
               05 REGKEY                           PIC 9(003).
       WORKING-STORAGE SECTION.
       01  WS-SCHOOL-DETAILS.
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
       01  KEY-ADD                                 PIC 9(003).

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE "SCHOOL MANAGEMENT" LINE 03 COL 50.
           03 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 26 COL 01.
           03 VALUE "F3 - BACK | F4 - EXIT"
           LINE 25 COL 99 FOREGROUND-COLOR 5.
           03 VALUE "  " LINE 24 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 96 BACKGROUND-COLOR 0.

       01  REGISTER-MENU
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           03 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 16 COL 35.
           03 VALUE "1 - Register School Manually" LINE 11 COL 42.
           03 VALUE "2 - Register School Through CSV File"
               LINE 12 COL 42.
           03 VALUE "3 - Back to Main Menu" LINE 13 COL 42.
           03 VALUE "What do you wish to do:" LINE 20 COL 45
           REVERSE-VIDEO.
           03 MP-OPTION PIC 9(02) LINE 20 COL 68 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM CHECK-FILE

           PERFORM WITH TEST AFTER UNTIL OPTION-VLD
               MOVE ZERO TO MP-OPTION
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY REGISTER-MENU
               ACCEPT MP-OPTION

               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM REGISTER-MANUAL

                   WHEN 2
                       PERFORM REGISTER-CSV

                   WHEN 3
                       EXIT PROGRAM

           END-PERFORM.

       REGISTER-MANUAL SECTION.

       REGISTER-INTERNAL-ID.
           OPEN INPUT KEYS
               READ KEYS
               ADD 1 TO REGKEY
               MOVE REGKEY TO WS-SCHOOL-INTERNAL-ID
           CLOSE KEYS
           DISPLAY WS-SCHOOL-INTERNAL-ID

           OPEN I-O SCHOOLS
           WRITE SCHOOL-DETAILS FROM WS-SCHOOL-DETAILS
           CLOSE SCHOOLS

           EXIT SECTION.

       REGISTER-EXTERNAL-ID.

       REGISTER-DESIGNATION.

       REGISTER-ADRESS.

       REGISTER-CSV SECTION.

       CHECK-FILE SECTION.
           MOVE ZEROS TO FILE-STATUS

           OPEN I-O SCHOOLS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SCHOOLS
                   CLOSE SCHOOLS
               END-IF
           CLOSE SCHOOLS

           MOVE ZEROS TO FILE-STATUS

           OPEN I-O KEYS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT KEYS
                   CLOSE KEYS
               END-IF
           CLOSE KEYS

           EXIT SECTION.

       CHECK-KEY SECTION.

       END PROGRAM SCM-REG.
