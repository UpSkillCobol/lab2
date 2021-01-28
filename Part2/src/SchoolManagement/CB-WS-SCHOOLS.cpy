      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SCHOOL MANAGEMENT
      ******************************************************************
      *    WORKING-STORAGE
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 25.01.2020
      ******************************************************************
       01  WS-SCHOOL-DETAILS.
           05 WS-SCHOOL-INTERNAL-ID                PIC 9(003).
           05 WS-SCHOOL-EXTERNAL-ID                PIC X(008).
               88 EXTERNAL-ID-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
           05 WS-SCHOOL-DESIGNATION.
               88 DESIGNATION-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
               10 WS-SCHOOL-DESIGNATION1           PIC X(050).
               10 WS-SCHOOL-DESIGNATION2           PIC X(050).
               10 WS-SCHOOL-DESIGNATION3           PIC X(050).
           05 WS-SCHOOL-ADRESS.
               10 WS-SCHL-ADR-MAIN.
                   88 ADDRESS-VLD                   VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES,
                                                   "0" THRU "9".
                   15 WS-SCHL-ADR-MAIN1            PIC X(050).
                   15 WS-SCHL-ADR-MAIN2            PIC X(050).
               10 WS-SCHOOL-POSTAL-CODE.
                   15 WS-SCHL-POSTAL-CODE1         PIC 9(004).
                       88 POSTAL-CODE1-VLD         VALUE "1000" THRU
                                                   "9999".
                   15 WS-SCHL-POSTAL-CODE2         PIC 9(003).
                       88 POSTAL-CODE2-VLD         VALUE "000" THRU
                                                   "999".
               10 WS-SCHOOL-TOWN                   PIC X(030).
                   88 TOWN-VLD                     VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
       01  WS-OPTION                               PIC 9(002).
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
