      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    FILE DESCRIPTOR VARIABLES | V0.1 | IN UPDATE | 04.03.2021
      ******************************************************************

       01  FD-ORDER.
           05  FD-ORDERS-ID                         PIC 9(005).
           05  FD-DELIVERY-DATE-TIME.
               10  FD-DELIVERY-DATE.
                   15  FD-DELIVERY-YEAR             PIC 9(004).
                   15  FD-DELIVERY-MONTH            PIC 9(002).
                   15  FD-DELIVERY-DAY              PIC 9(002).
               10  FD-DELIVERY-HOUR.
                   15  FD-DELIVERY-HOUR             PIC 9(002).
                   15  FD-DELIVERY-MINUTE           PIC 9(002).
           05  FD-ORDERS-SCHOOL-INTERNAL-ID         PIC 9(003).
           05  FD-ORDERS-SANDWICH-INTERNAL-ID       PIC 9(003).
           05  FD-ORDERS-QUANTITY                   PIC 9(003).
           05  FD-ORDERS-DATE.
               10  FD-ORDERS-YEAR                   PIC 9(004).
               10  FD-ORDERS-MONTH                  PIC 9(002).
               10  FD-ORDERS-DAY                    PIC 9(002).
