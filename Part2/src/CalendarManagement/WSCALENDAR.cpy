       01  WS-CALENDAR.
           05  WS-DOWNTIME-ID                      PIC 9(003).
           05  WS-START-DOWNTIME.
               10  WS-START-DT-DAY                 PIC 9(002).
               10  WS-START-DT-MONTH               PIC 9(002).
               10  WS-START-DT-YEAR                PIC 9(004).
           05  WS-END-DOWNTIME.
               10  WS-END-DT-DAY                   PIC 9(002).
               10  WS-END-DT-MONTH                 PIC 9(002).
               10  WS-END-DT-YEAR                  PIC 9(004).
           05  WS-DOWNTIME-DESCRIPTION.
               10  WS-DOWNTIME-DESCRIPTION1        PIC X(050).
               10  WS-DOWNTIME-DESCRIPTION2        PIC X(050).
