       01  FD-INVENTORY.
           88 EOFINVENTORY                         VALUE HIGH-VALUES.
           05 MOVE-DETAILS.
               10 MOVE-IN-ID                       PIC X(002).
               10 MOVE-OUT-ID                      PIC X(002).
               10 MOVE-IN-QTD                      PIC 9(003).
               10 MOVE-OUT-QTD                     PIC 9(003).

           05  INGRED-DETAILS.
               10 INGRED-ID                        PIC 9(003).
               10 INGRED-UNIT                      PIC X(003).
               10 THRESHOLD                        PIC 9(003).
               10 INGRED-IS-ACTIVE                 PIC 9(001).

           05  TIME-DETAILS.
               10  TIME-MOVE-IN.
                   15  TIME-MOVE-IN-YEAR           PIC 9(004).
                   15  TIME-MOVE-IN-MONTH          PIC 9(002).
                   15  TIME-MOVE-IN-DAY            PIC 9(002).
                   15  TIME-MOVE-IN-HOUR           PIC 9(002).
                   15  TIME-MOVE-IN-MINUTE         PIC 9(002).
               10  TIME-MOVE-OUT.
                   15  TIME-MOVE-OUT-YEAR          PIC 9(004).
                   15  TIME-MOVE-OUT-MONTH         PIC 9(002).
                   15  TIME-MOVE-OUT-DAY           PIC 9(002).
                   15  TIME-MOVE-OUT-HOUR          PIC 9(002).
                   15  TIME-MOVE-OUT-MINUTE        PIC 9(002).
               10  TIME-ACTZ.
                   15  TIME-ACTZ-YEAR              PIC 9(004).
                   15  TIME-ACTZ-MONTH             PIC 9(002).
                   15  TIME-ACTZ-DAY               PIC 9(002).
                   15  TIME-ACTZ-HOUR              PIC 9(002).
                   15  TIME-ACTZ-MINUTE            PIC 9(002).
