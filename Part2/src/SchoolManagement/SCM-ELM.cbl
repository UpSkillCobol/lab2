      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-ELM.
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
           ACCESS IS DYNAMIC
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
       01  KEY-STATUS                              PIC 9(004).

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE "SCHOOL MANAGEMENT" LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE "F3 - BACK | F4 - EXIT"
           LINE 25 COL 99 FOREGROUND-COLOR 5.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Hello world"
           STOP RUN.
       END PROGRAM SCM-ELM.
