       01  WS-CALENDAR.
           88  WS-EOF-DOWNTIME                        VALUE HIGH-VALUES.
           05  WS-DOWNTIME-ID                         PIC 9(003).
           05  WS-DOWNTIME.
               10  WS-DT-DAY                          PIC 9(002).
               10  WS-DT-MONTH                        PIC 9(002).
               10  WS-DT-YEAR                         PIC 9(004).
           05  WS-DOWNTIME-DESCRIPTION.
               10  WS-DOWNTIME-DESCRIPTION1           PIC X(050).
               10  WS-DOWNTIME-DESCRIPTION2           PIC X(050).
