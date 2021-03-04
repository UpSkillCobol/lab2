       01  WS-SR-REC.
           05  WS-SR-IID                   PIC 9(003).
           05  WS-SR-EID                   PIC X(005).
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
       01  WSINGREDS-DETAILS.
           05 WSINGREDS-ID                 PIC 9(003).
           05 WSINGREDS-NAME               PIC X(030).
           05 WSINGREDS-DESCRIPTION        PIC X(050).
           05 WSINGREDS-UNIT-SUPPLIER      PIC X(003).
           05 WSINGREDS-UNIT-SANDWICH      PIC X(003).
           05 WSTRESHOLD                   PIC 9(003).
           05 WSINGREDS-IS-ACTIVE          PIC 9(001).
       01 WSCATEGORY-DETAILS.
           05 WSCATEGORY-ID                PIC 9(003).
           05 WSCATEGORY-NAME              PIC X(030).
           05 WSCATEGORY-DESCRIPTION.
               10 WSCATEGORY-DESCRIPTION1  PIC X(050).
           05 WSCATEGORY-IS-ACTIVE         PIC 9(001).
       77  WS-OPTION                       PIC 9(001).
       77  KEY-STATUS                      PIC 9(004).
       77  FILE-STATUS                     PIC 9(002).
       77  REG-UNIQUE                      PIC 9(001).
       77  DUMMY                           PIC X(001).
       77  LINK-TEXT                       PIC X(150).
       77  SPACE-CHECK1                    PIC X(050).
       77  SPACE-CHECK2                    PIC X(050).
       77  SPACE-CHECK3                    PIC X(050).
       77  SPACE-CHECK4                    PIC X(050).
       77  SPACE-CHECK5                    PIC X(050).
       77  SPACE-CHECK6                    PIC X(050).
       77  SPACE-CHECK7                    PIC X(050).
       77  SPACE-CHECK8                    PIC X(050).
       77  SPACE-CHECK9                    PIC X(050).
       77  SPACE-CHECK10                   PIC X(050).
       77  SPACE-CHECK11                   PIC X(050).
       77  SPACE-CHECK12                   PIC X(050).
       77  SPACE-CHECK13                   PIC X(050).
       77  SPACE-CHECK14                   PIC X(050).
       77  SPACE-CHECK15                   PIC X(050).
       78  MAX-ING                         VALUE 999.
       77  NUMBER-ING                      PIC 9(003) VALUE 999.
       78  MAX-CAT                         VALUE 999.
       77  NUMBER-CAT                      PIC 9(003) VALUE 999.
