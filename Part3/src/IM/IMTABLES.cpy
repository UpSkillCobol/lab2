
       78  MAX-TABLES                     VALUE 999.



       77  MAX-TOTALQUANT                 PIC 999 VALUE 999.

       01  TAB-TOTALQUANT OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-TOTALQUANT INDEXED BY IND-TOTALQUANT.
           05 TAB-INGREDS-ID              PIC 9(003).
           05 TAB-INGREDS-NAME            PIC X(030).
           05 TAB-QTD-TOTAL               PIC 9(003).




       77  MAX-MOVES                      PIC 999 VALUE 999.

       01  TAB-MOVES OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-MOVES INDEXED BY IND-MOVES.
           05 MOVES-IN.
              10 MOVES-IN-YEAR              PIC X(004).
              10 MOVES-IN-MONTH             PIC X(002).
              10 MOVES-IN-DAY               PIC X(002).
              10 MOVES-IN-HOUR              PIC X(002).
              10 MOVES-IN-MIN               PIC X(002).
           05 MOVES-OUT.
              10 MOVES-OUT-YEAR             PIC X(004).
              10 MOVES-OUT-MONTH            PIC X(002).
              10 MOVES-OUT-DAY              PIC X(002).
              10 MOVES-OUT-HOUR             PIC X(002).
              10 MOVES-OUT-MIN              PIC X(002).
           05 MOVES-ACTZ.
              10 MOVES-ACTZ-YEAR            PIC 9(004).
              10 MOVES-ACTZ-MONTH           PIC 9(002).
              10 MOVES-ACTZ-DAY             PIC 9(002).
              10 MOVES-ACTZ-HOUR            PIC 9(002).
              10 MOVES-ACTZ-MINUTE          PIC 9(002).
