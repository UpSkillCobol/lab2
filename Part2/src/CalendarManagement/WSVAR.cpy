       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".
       77  CALENDAR-TEST                        PIC 9(002).
       77  KEYS-TEST                            PIC 9(002).
       77  PRESS-KEY                            PIC X.

       01  WS-VALID-DATE.
           05  WS-DAY                          PIC 9(002).
               88  VALID-DAY                   VALUE 01 THRU 31.
               88  DAY-30                      VALUE 01 THRU 30.
               88  DAY-FEBRUARY                VALUE 01 THRU 28.
               88  FEB-LEAP-YEAR               VALUE 01 THRU 29.
               88  DAY-31                      VALUE 01 THRU 31.
           05  WS-MONTH                        PIC 9(002).
               88  VALID-MONTH                 VALUE 01 THRU 12.
               88  MONTH-30                    VALUE 04, 06, 09, 11.
               88  MONTH-FEB                   VALUE 02.
           05  WS-YEAR                         PIC 9(004).
               88  VALID-YEAR                  VALUE 2021 THRU 2021.
       01  DATE-VALID                          PIC X.
       01  LEAP-YEAR                           PIC X.
           88  LEAP-YEAR-YES                   VALUE "Y".
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR                 PIC 9(004).
           05  WS-CURRENT-MONTH                PIC 9(002).
           05  WS-CURRENT-DAY                  PIC 9(002).
