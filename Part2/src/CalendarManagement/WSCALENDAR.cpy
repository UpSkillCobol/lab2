       01  WS-CALENDAR.
           05  WS-DOWNTIME-ID                      PIC 9(003).
           05  WS-START-DOWNTIME.
               10  WS-START-DT-DAY                 PIC 9(002).
                   88  VALID-DAY                   VALUE 1 THRU 31.
               10  WS-START-DT-MONTH               PIC 9(002).
                   88  VALID-MONTH                 VALUE 1 THRU 12.
               10  WS-START-DT-YEAR                PIC 9(004).
                   88 VALID-YEAR                   VALUE 2021 THRU 2022.
           05  WS-END-DOWNTIME.
               10  WS-END-DT-DAY                   PIC 9(002).
                   88  VALID-DAY1                  VALUE 1 THRU 31.
               10  WS-END-DT-MONTH                 PIC 9(002).
                   88  VALID-MONTH1                VALUE 1 THRU 12.
               10  WS-END-DT-YEAR                  PIC 9(004).
                   88 VALID-YEAR1                  VALUE 2021 THRU 2022.
           05  WS-DOWNTIME-DESCRIPTION.
               10  WS-DOWNTIME-DESCRIPTION1        PIC X(050).
               10  WS-DOWNTIME-DESCRIPTION2        PIC X(050).
