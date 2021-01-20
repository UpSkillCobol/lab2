      ******************************************************************
      * Author: Cesar de Sousa Costa
      * Date: 13/01/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHOOL-MNG.
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

           SELECT KEYS ASSIGN TO "KEYS.txt"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD SCHOOLS.
           COPY CB-SCHOOLS.

       FD  SCHOOLS1.
           01 SCHOOL1                              PIC X(200).

       FD  KEYS.
           01 FD-KEYS.
               05 REGKEY                           PIC 9(003).


       WORKING-STORAGE SECTION.
       COPY CB-WS-SCHOOLS.
       01  WS-OPCAO                                PIC 9(002).
           88 OPCAO-VLD                            VALUE
                                                   "1","2","3","4".
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).

       SCREEN SECTION.
       01  LIMPAR-ECRAN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.

       01  MAIN-MENU
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE "SCHOOL MANAGEMENT" LINE 03 COL 50.
           03 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 26 COL 01.
           03 VALUE "F3 - Go Back | F4 - Leave"
           LINE 25 COL 95 FOREGROUND-COLOR 5.
           03 VALUE "  " LINE 24 COL 92 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 92 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 92 BACKGROUND-COLOR 0.

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
           03 VALUE "1 - Register School" LINE 11 COL 50.
           03 VALUE "2 - View School" LINE 12 COL 50.
           03 VALUE "3 - Remove School" LINE 13 COL 50.
           03 VALUE "4 - Change School" LINE 14 COL 50.
           03 VALUE "What do you wish to do:" LINE 20 COL 45
           REVERSE-VIDEO.
           03 MP-OPCAO PIC 9(02) LINE 20 COL 68 TO WS-OPCAO
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  ID-ERROR.
           03 VALUE "ID ALREADY EXISTS!" LINE 25 COL 10
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           03 VALUE "  " LINE 24 COL 92 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 92 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 92 BACKGROUND-COLOR 0.

       PROCEDURE DIVISION.
       MAIN SECTION.
           DISPLAY LIMPAR-ECRAN
           DISPLAY MAIN-MENU
           DISPLAY MAIN-MENU-TEXT
           DISPLAY ID-ERROR
           ACCEPT MP-OPCAO

           EVALUATE WS-OPCAO

               WHEN 1
                       PERFORM REGISTER
               WHEN 2
                       PERFORM CHECK
               WHEN 3
                       PERFORM CHANGE
               WHEN 4
                       PERFORM LEAVE
               WHEN OTHER
                       ACCEPT WS-OPCAO

           END-EVALUATE.

       REGISTER SECTION.

           MOVE ZEROS TO WS-OPCAO

           DISPLAY "HOW DO YOU WANT TO REGISTER A SCHOOL"
           DISPLAY " 1 - MANUALLY"
           DISPLAY " 2 - THROUGH A .CSV FILE"
           DISPLAY " 3 - EXIT"

           PERFORM WITH TEST AFTER UNTIL
                       WS-OPCAO = 1 OR WS-OPCAO = 2 OR WS-OPCAO = 3
           ACCEPT WS-OPCAO

           EVALUATE WS-OPCAO

               WHEN 1
                       PERFORM REGISTER-MANUAL
               WHEN 2
                       PERFORM REGISTER-CSV
               WHEN 3
                       EXIT SECTION
               WHEN OTHER
                       ACCEPT WS-OPCAO

           END-EVALUATE

           END-PERFORM

           EXIT SECTION.

       REGISTER-MANUAL SECTION.

       REGISTER-INTERNAL-ID.
           OPEN I-O KEYS
               READ KEYS
               ADD 1 TO REGKEY
               MOVE REGKEY TO WS-SCHOOL-INTERNAL-ID
               REWRITE FD-KEYS
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

       CHECK SECTION.

       CHANGE SECTION.

       LEAVE SECTION.
           EXIT PROGRAM.
