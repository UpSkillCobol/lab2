      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGULAR VARIABLES | V0.3 | IN UPDATE | 07.03.2021
      ******************************************************************

       01  MAIN-OPTION                          PIC 9(002).
           88  VALID-MAIN-OPTION                VALUE 1 THRU 4.
      *>  01  SAVE                                 PIC X(002).
      *>      88 SAVE-YES                          VALUE "Y" "y" "S" "s".
      *>      88 SAVE-NO                           VALUE "N" "n".
      *>      88 SAVE-VALID                        VALUE "Y" "y" "S" "s"
      *>                                                 "N" "n".
       77  KEYSTATUS                            PIC 9(004).
       78  F1                                   VALUE 1001.
       78  F2                                   VALUE 1002.
       78  F3                                   VALUE 1003.
       77  ORDERS-FS                            PIC 9(002).
       77  ORDERSKEYS-FS                        PIC 9(002).
       77  CALENDAR-FS                          PIC 9(002).
       77  SCHOOL-FS                            PIC 9(002).
       77  SANDWICH-FS                          PIC 9(002).
       77  PRESS-KEY                            PIC X(001).
       77  TEST1                                PIC 9(008).
       77  TEST2                                PIC 9(008).
       77  TEST3                                PIC 9(008).
       77  FLAG-TRUE                            PIC X(001).
       77  CALENDAR-EXISTS                      PIC X(001).
       77  FLAG-CALENDAR                        PIC X(001).
       77  SCHOOL-EXISTS                        PIC X(001).
       77  SANDWICH-EXISTS                      PIC X(001).
       77  ILIN                                 PIC 9(002).
       77  ICOL                                 PIC 9(002).
       77  COUNTPAGE                            PIC 9(003).
       77  MAXPERPAGE                           PIC 9(003).
       77  PRICEQUANTITY                        PIC 9(005).
       77  COUNTER                              PIC 9(003).
