      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CPS ASSIGN TO "cps.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS FD-CP
               FILE STATUS IS FILE-CHECK.
       DATA DIVISION.
       FILE SECTION.

       FD  CPS.
           01 FD-CPS.
              05 FD-CP.
                  10 FD-CP-Q               PIC 9(004).
                  10 FD-CP-T               PIC 9(003).
              05 FD-LOC                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  FILE-CHECK                      PIC 9(002).
       LINKAGE SECTION.
       01  WS-SCHOOL-DETAILS.
           05 WS-SCHOOL-INTERNAL-ID                PIC 9(003).
           05 WS-SCHOOL-EXTERNAL-ID                PIC X(008).
               88 ID-VLD                           VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
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
       PROCEDURE DIVISION USING WS-SCHOOL-DETAILS.
       MAIN-PROCEDURE.

           OPEN INPUT CPS
               MOVE WS-SCHOOL-POSTAL-CODE TO FD-CP
               READ CPS RECORD
                   KEY IS FD-CP
                   INVALID KEY
                       MOVE SPACES TO WS-SCHOOL-TOWN
                   NOT INVALID KEY
                       MOVE FD-LOC TO WS-SCHOOL-TOWN
                   END-READ
           CLOSE CPS
           EXIT.

       END PROGRAM CPS.
