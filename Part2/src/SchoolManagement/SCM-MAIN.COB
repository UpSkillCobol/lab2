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
       01  WS-OPTION                               PIC 9(002).
           88 OPTION-VLD                           VALUE
                                                   "1","2","3","4","5".
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

       01  MAIN-MENU
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
           03 VALUE "1 - Register School" LINE 11 COL 50.
           03 VALUE "2 - View School" LINE 12 COL 50.
           03 VALUE "3 - Remove School" LINE 13 COL 50.
           03 VALUE "4 - Change School" LINE 14 COL 50.
           03 VALUE "5 - Exit Program" LINE 15 COL 50.
           03 VALUE "What do you wish to do:" LINE 20 COL 45
           REVERSE-VIDEO.
           03 MP-OPTION PIC 9(02) LINE 20 COL 68 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  ID-ERROR.
           03 VALUE "ID ALREADY EXISTS!" LINE 25 COL 10
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           03 VALUE "  " LINE 24 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 96 BACKGROUND-COLOR 0.

       01  LEAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 5.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
           05 VALUE "Leaving Program!" LINE 10 COL 45.
           05 VALUE "THANK YOU!" LINE 11 COL 45.

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL WS-OPTION = 5
           MOVE ZERO TO MP-OPTION
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY MAIN-MENU

           ACCEPT MP-OPTION

           EVALUATE WS-OPTION

               WHEN 1
                       CALL "SCM-REG"
               WHEN 2
                       CALL "SCM-CLT"
               WHEN 3
                       CALL "SCM-ELM"
               WHEN 4
                       CALL "SCM-ALT"

           END-EVALUATE

           END-PERFORM

           DISPLAY LEAVE-SCREEN
           STOP RUN.
