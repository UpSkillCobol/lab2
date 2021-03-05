      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    TABLES | V0.1 | IN UPDATE | 05.03.2021
      ******************************************************************

       78  MAX-CAL                VALUE 999.
       77  MAX-CAL1               PIC 999 VALUE 999.
       77  MAX-AGG                PIC 999 VALUE 999.

       01  TAB-CAL OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-CAL1 INDEXED BY IND-CAL.
           05 TAB-BEGIN           PIC X(12).
           05 TAB-END             PIC X(12).

       01  TAB-AGG OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-AGG INDEXED BY IND-AGG.
           05 AGG-BEGIN.
              10 AGG-BEGIN-YEAR   PIC X(004).
              10 AGG-BEGIN-MONTH  PIC X(002).
              10 AGG-BEGIN-DAY    PIC X(002).
              10 AGG-BEGIN-HOUR   PIC X(002).
              10 AGG-BEGIN-MIN    PIC X(002).
           05 AGG-END.
              10 AGG-END-YEAR     PIC X(004).
              10 AGG-END-MONTH    PIC X(002).
              10 AGG-END-DAY      PIC X(002).
              10 AGG-END-HOUR     PIC X(002).
              10 AGG-END-MIN      PIC X(002).
