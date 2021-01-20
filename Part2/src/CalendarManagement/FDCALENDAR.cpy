       01  FD-CALENDAR.
           88  FD-EOF-DOWNTIME                        VALUE HIGH-VALUES.
           05  FD-DOWNTIME-ID                         PIC 9(003).
           05  FD-DOWNTIME.
               10  FD-DT-DAY                          PIC 9(002).
               10  FD-DT-MONTH                        PIC 9(002).
               10  FD-DT-YEAR                         PIC 9(004).
           05  FD-DOWNTIME-DESCRIPTION.
               10  FD-DOWNTIME-DESCRIPTION1           PIC X(050).
               10  FD-DOWNTIME-DESCRIPTION2           PIC X(050).
