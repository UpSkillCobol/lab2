       01  WS-INVENTORY.

           05 WS-QTD-TOTAL                            PIC 9(003).

           05 WS-MOVE-DETAILS.
               10 WS-MOVE-IN-ID                       PIC X(002).
               10 WS-MOVE-OUT-ID                      PIC X(002).
               10 WS-MOVE-IN-QTD                      PIC 9(003).
               10 WS-MOVE-OUT-QTD                     PIC 9(003).

           05  INGRED-DETAILS.
               10 WS-INGRED-ID                        PIC 9(003).
               10 WS-INGRED-UNIT                      PIC X(003).
               10 WS-THRESHOLD                        PIC 9(003).
               10 WS-INGRED-IS-ACTIVE                 PIC 9(001).

           05  WS-TIME-DETAILS.
               10  WS-TIME-MOVE-IN.
                   15  WS-TIME-MOVE-IN-YEAR           PIC 9(004).
                   15  WS-TIME-MOVE-IN-MONTH          PIC 9(002).
                   15  WS-TIME-MOVE-IN-DAY            PIC 9(002).
                   15  WS-TIME-MOVE-IN-HOUR           PIC 9(002).
                   15  WS-TIME-MOVE-IN-MINUTE         PIC 9(002).
               10  WS-TIME-MOVE-OUT.
                   15  WS-TIME-MOVE-OUT-YEAR          PIC 9(004).
                   15  WS-TIME-MOVE-OUT-MONTH         PIC 9(002).
                   15  WS-TIME-MOVE-OUT-DAY           PIC 9(002).
                   15  WS-TIME-MOVE-OUT-HOUR          PIC 9(002).
                   15  WS-TIME-MOVE-OUT-MINUTE        PIC 9(002).
               10  WS-TIME-ACTZ.
                   15  WS-TIME-ACTZ-YEAR              PIC 9(004).
                   15  WS-TIME-ACTZ-MONTH             PIC 9(002).
                   15  WS-TIME-ACTZ-DAY               PIC 9(002).
                   15  WS-TIME-ACTZ-HOUR              PIC 9(002).
                   15  WS-TIME-ACTZ-MINUTE            PIC 9(002).
