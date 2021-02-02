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
           ALTERNATE KEY IS SCHOOL-EXTERNAL-ID
           ALTERNATE KEY IS SCHOOL-TOWN
           WITH DUPLICATES
           ALTERNATE KEY IS SCHOOL-POSTAL-CODE
           WITH DUPLICATES
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
           05 SCHOOL-IS-ACTIVE                     PIC 9(001).


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
           05 WS-SCHOOL-IS-ACTIVE                  PIC 9(001).

       01  WS-OPTION                                PIC 9(002).
           88 OPTION-VLD                            VALUE
                                                   "1","2","3","4".
       01  WS-DLT                                  PIC X(01).
           88 DLT-VLD                              VALUE
                                                   "Y","S","N",
                                                   "y","s","n".
       01  WS-DLT-KEY                           PIC 9(001).
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
       COPY "CONSTANTS".

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE BACK-EXIT
               LINE 25 COL 99 FOREGROUND-COLOR 5.

       01  PRE-DELETE-MENU
           REQUIRED.
      *    SCREEN PARA CONFIRMAR QUAL REGISTO O UTILIZADOR QUER ELIMINAR
           05 VALUE DLT-MENU-TEXT1 LINE 25 COL 10
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.
           05 DLT-OPTION PIC X(008) LINE 25 COL 47 TO
               WS-SCHOOL-EXTERNAL-ID
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 4.

       01  DELETE-SCREEN
           REQUIRED.
           05 VALUE DLT-MENU-TEXT LINE 9 COL 25.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 22.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 22.
           05 VALUE "-" LINE 18 COL 45.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 22.
           05 VALUE DLT-MENU-TEXT2 LINE 25 COL 10
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 4.
           05 DLT-OPTION1 PIC X(01) LINE 25 COL 46 TO WS-DLT
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 4.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 18
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
           05 DLT-REC.
               10 DLT-IID PIC 9(003) LINE 11 COL 40 BLANK WHEN ZERO.
               10 DLT-EED PIC X(008) LINE 12 COL 40.
               10 DLT-DESIGNATION.
                   15 DLT-DESIGNATION1 PIC X(050) LINE 13 COL 40.
                   15 DLT-DESIGNATION2 PIC X(050) LINE 14 COL 40.
                   15 DLT-DESIGNATION3 PIC X(050) LINE 15 COL 40.
               10 DLT-ADDRESS.
                   15 DLT-ADDRESS1 PIC X(050) LINE 16 COL 40.
                   15 DLT-ADDRESS2 PIC X(050) LINE 17 COL 40.
               10 DLT-POSTAL-CODE.
                   15 DLT-PC1 PIC 9(004) LINE 18 COL 40
                        BLANK WHEN ZERO.
                   15 DLT-PC2 PIC 9(003) LINE 18 COL 47
                        BLANK WHEN ZERO.
               10 DLT-TOWN PIC X(030) LINE 19 COL 40.

       01  DELETED-SCREEN.
           05 VALUE DELETED-TEXT LINE 25 COL 10 BACKGROUND-COLOR 7
           FOREGROUND-COLOR 4.

       01  ID-ERROR.
           05 VALUE DLT-ID-ERROR LINE 26 COL 10 BACKGROUND-COLOR 7
           FOREGROUND-COLOR 4.

       PROCEDURE DIVISION.
       DELETE-REGISTER SECTION.
           PERFORM CLEAR-VARIABLES
           MOVE SPACES TO DLT-OPTION
           MOVE SPACES TO DLT-OPTION1
           MOVE SPACES TO WS-DLT
           PERFORM WITH TEST AFTER UNTIL WS-DLT-KEY = 1
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY PRE-DELETE-MENU
                   ACCEPT DLT-OPTION
                       IF KEY-STATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                       IF KEY-STATUS = 1004 THEN
                           STOP RUN
                       END-IF
               MOVE ZEROS TO WS-DLT-KEY
               MOVE FUNCTION UPPER-CASE (WS-SCHOOL-EXTERNAL-ID) TO
               WS-SCHOOL-EXTERNAL-ID
               MOVE WS-SCHOOL-EXTERNAL-ID TO SCHOOL-EXTERNAL-ID
               PERFORM READ-RECORD
           END-PERFORM
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY DELETE-SCREEN
           ACCEPT DLT-OPTION1.
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF KEY-STATUS = 1004 THEN
                   STOP RUN
               END-IF.
           EVALUATE TRUE
               WHEN WS-DLT = "S" OR WS-DLT = "Y"
                   PERFORM DELETE-RECORD
                   DISPLAY DELETED-SCREEN
                   ACCEPT OMITTED AT LINE 25 COL 09
               WHEN WS-DLT = "N"
                   PERFORM CLEAR-VARIABLES
           END-EVALUATE
           EXIT PROGRAM.


       READ-RECORD SECTION.
           OPEN INPUT SCHOOLS
               READ SCHOOLS RECORD
                   KEY SCHOOL-EXTERNAL-ID
                   INVALID KEY
                       DISPLAY ID-ERROR
                       MOVE 0 TO WS-DLT-KEY
                   NOT INVALID KEY
                       MOVE SCHOOL-DETAILS TO DLT-REC
                       MOVE 1 TO WS-DLT-KEY
               END-READ
           CLOSE SCHOOLS.

       DELETE-RECORD SECTION.
           OPEN I-O SCHOOLS
               MOVE 0 TO SCHOOL-IS-ACTIVE
           REWRITE SCHOOL-DETAILS FROM DLT-REC
       CLOSE SCHOOLS.

       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN DLT-EED DLT-DESIGNATION
           DLT-ADDRESS DLT-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           WS-SCHOOL-IS-ACTIVE DLT-IID DLT-POSTAL-CODE
           EXIT SECTION.

       END PROGRAM SCM-ELM.
