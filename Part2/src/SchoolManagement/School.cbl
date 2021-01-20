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


       PROCEDURE DIVISION.
       MAIN SECTION.

           OPEN INPUT SCHOOLS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SCHOOLS
                   CLOSE SCHOOLS
               ELSE
                   CLOSE SCHOOLS
               END-IF


           DISPLAY "CHOOSE WHAT TO DO"

           MOVE ZEROS TO WS-OPCAO

           PERFORM WITH TEST AFTER UNTIL OPCAO-VLD

           ACCEPT WS-OPCAO

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

           END-EVALUATE

           END-PERFORM.

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
