      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGULAR VARIABLES | V0.1 | IN UPDATE | 04.03.2021
      ******************************************************************

       01  MAIN-OPTION                          PIC 9(002).
           88  VALID-MAIN-OPTION                VALUE 1 THRU 4.
       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".
       77  KEYSTATUS                            PIC 9(004).
       77  ORDERS-FS                            PIC 9(002).
       77  ORDERSKEYS-FS                        PIC 9(002).
       77  PRESS-KEY                            PIC X(001).
       77  TEST1                                PIC 9(001).
       77  TEST2                                PIC 9(008).
       77  TEST3                                PIC 9(008).

       77  FLAG-TRUE                            PIC X(001).
       01  VIEW-OPTION                          PIC 9(002).
           88  VALID-VIEW-OPTION                VALUE 1 THRU 3.
       77  EOF                                  PIC X(001).
       01  REQUEST-ID                           PIC 9(003).
           88 VALID-ID                          VALUE 1 THRU 999.
       77  ILIN                                 PIC 9(002).
       77  ICOL                                 PIC 9(002).
