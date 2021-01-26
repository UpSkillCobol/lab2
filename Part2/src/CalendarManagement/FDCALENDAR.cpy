       01  FD-CALENDAR.
           05  FD-DOWNTIME-ID                      PIC 9(003).
           05  FD-START-DOWNTIME.
               10  FD-START-DT-DAY                 PIC 9(002).
               10  FD-START-DT-MONTH               PIC 9(002).
               10  FD-START-DT-YEAR                PIC 9(004).
           05  FD-END-DOWNTIME.
               10  FD-END-DT-DAY                   PIC 9(002).
               10  FD-END-DT-MONTH                 PIC 9(002).
               10  FD-END-DT-YEAR                  PIC 9(004).
           05  FD-DOWNTIME-DESCRIPTION.
               10  FD-DOWNTIME-DESCRIPTION1        PIC X(050).
               10  FD-DOWNTIME-DESCRIPTION2        PIC X(050).
