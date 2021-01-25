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
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE "SCHOOL MANAGEMENT" LINE 03 COL 50.
           05 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 26 COL 01.
           05 VALUE "F3 - BACK | F4 - EXIT"
               LINE 25 COL 99 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 24 COL 96 BACKGROUND-COLOR 0.
           05 VALUE "  " LINE 25 COL 96 BACKGROUND-COLOR 0.
           05 VALUE "  " LINE 26 COL 96 BACKGROUND-COLOR 0.

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
           05 VALUE "1 - Register School Manually" LINE 11 COL 42.
           05 VALUE "2 - Register School Through CSV File"
               LINE 12 COL 42.
           05 VALUE "3 - Back to Main Menu" LINE 13 COL 42.
           05 VALUE "What do you wish to do:" LINE 20 COL 45
           REVERSE-VIDEO.
           05 MP-OPTION PIC 9(02) LINE 20 COL 68 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE "Register New School" LINE 9 COL 25.
           05 VALUE "Internal ID:" LINE 11 COL 26.
           05 VALUE "External ID:" LINE 12 COL 26.
           05 VALUE "Designation1:" LINE 13 COL 25.
           05 VALUE "Designation2:" LINE 14 COL 25.
           05 VALUE "Designation3:" LINE 15 COL 25.
           05 VALUE "Address1:" LINE 16 COL 29.
           05 VALUE "Address2:" LINE 17 COL 29.
           05 VALUE "Postal Code:" LINE 18 COL 25.
           05 VALUE "-" LINE 18 COL 44.
           05 VALUE "Town:" LINE 19 COL 25.
           05 REG-REC.
               10 REG-IID PIC 9(003) LINE 11 COL 39
                   TO WS-SCHOOL-INTERNAL-ID
                   BLANK WHEN ZERO.
               10 REG-EED PIC X(008) LINE 12 COL 39
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 REG-DESIGNATION.
                   15 REG-DESIGNATION1 PIC X(050) LINE 13 COL 39
                       TO WS-SCHOOL-DESIGNATION1.
                   15 REG-DESIGNATION2 PIC X(050) LINE 14 COL 39
                       TO WS-SCHOOL-DESIGNATION2.
                   15 REG-DESIGNATION3 PIC X(050) LINE 15 COL 39
                       TO WS-SCHOOL-DESIGNATION3.
               10 REG-ADDRESS.
                   15 REG-ADDRESS1 PIC X(050) LINE 16 COL 39
                       TO WS-SCHL-ADR-MAIN1.
                   15 REG-ADDRESS2 PIC X(050) LINE 17 COL 39
                       TO WS-SCHL-ADR-MAIN2.
               10 REG-POSTAL-CODE.
                   15 REG-PC1 PIC 9(004) LINE 18 COL 39
                       TO WS-SCHL-POSTAL-CODE1
                       BLANK WHEN ZERO.
                   15 REG-PC2 PIC 9(003) LINE 18 COL 46
                       TO WS-SCHL-POSTAL-CODE2
                       BLANK WHEN ZERO.
               10 REG-TOWN PIC X(030) LINE 19 COL 39
                   TO WS-SCHOOL-TOWN.
           05 VALUE ALL " " PIC X(80) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(80) LINE 21 COL 18
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



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM CHECK-FILE

           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 3
               MOVE ZERO TO MP-OPTION
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY MAIN-REGISTER-SCREEN
               ACCEPT MP-OPTION

               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM REGISTER-MANUAL

                   WHEN 2
                       PERFORM REGISTER-CSV

           END-EVALUATE

           END-PERFORM.

           EXIT PROGRAM.

       REGISTER-MANUAL SECTION.

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           ACCEPT REGISTER-SCREEN.

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
