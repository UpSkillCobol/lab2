      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    REGULAR VARIABLES | V0.6 | IN UPDATE | 02.02.2021
      ******************************************************************

       01  MAIN-OPTION                          PIC 9(002).
           88  VALID-MAIN-OPTION                VALUE 1 THRU 5.
       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".
       77  CALENDAR-TEST                        PIC 9(002).
       77  KEYS-TEST                            PIC 9(002).
       77  PRESS-KEY                            PIC X(001).
       01  F3F4EXIT                             PIC 9(004).
       01  FLAG-TRUE                            PIC X(001).
       01  VIEW-OPTION                          PIC 9(002).
           88  VALID-VIEW-OPTION                VALUE 1 THRU 3.
       01  EOF                                  PIC X(001).
       01  FLAG                                 PIC X(001).
       01  VIEW-ID                              PIC 9(003).
