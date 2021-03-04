       01  WS-SR-REC.
           05  WS-SR-IID                   PIC 9(003).
           05  WS-SR-EED                   PIC X(005).
           05  WS-SR-S-DESCRIPTION         PIC X(030).
           05  WS-SR-L-DESCRIPTION.
               10  WS-SR-L-DESCRIPTION1    PIC X(050).
               10  WS-SR-L-DESCRIPTION2    PIC X(050).
       01  WS-SR-ING-REC.
           05  WS-SR-SAND-ING-ID.
               10  WS-SR-SANDWICH-ID       PIC 9(003).
               10  WS-SR-INGREDIENT-ID     PIC 9(003).
       01  WS-SR-CAT-REC.
           05  WS-SR-SAND-CAT-ID.
               10  WS-SR-SANDWICH-ID       PIC 9(003).
               10  WS-SR-CATEGORY-ID       PIC 9(003).
       77  WS-OPTION                       PIC 9(001).
       77  KEY-STATUS                      PIC 9(004).
