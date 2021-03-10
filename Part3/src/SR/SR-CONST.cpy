       01  WS-REG                          PIC X(001).
           88 REG-YES                      VALUE "Y","y".
           88 REG-NO                       VALUE "N","n".
           88 REG-OPTION-VLD               VALUE "Y","y","N","n".
       78  MODULE-NAME         VALUE "SANDWICH REGISTRATION".
       78  MAIN-MENU-OPTION1   VALUE "1 - REGISTER SANDWICHES".
       78  MAIN-MENU-OPTION2   VALUE "2 - SEARCH SANDWICHES".
       78  MAIN-MENU-OPTION4   VALUE "3 - EXIT".
       78  MAIN-MENU-CHOICE    VALUE "PLEASE CHOOSE AN OPTION".
       78  MAIN-MENU-ERROR     VALUE "INVALID OPTION, PLEASE SELECT A VA
      -    "LID OPTION. PRESS ANY KEY TO CONTINUE".
       78  LEAVE-MESSAGE       VALUE "LEAVING PROGRAM!".
       78  LEAVE-THANKS        VALUE "THANK YOU!".
       78  BACK-EXIT           VALUE " F3 - MAIN MENU SR".
       78  F3                  VALUE "1003".
       78  F2                  VALUE "1002".
       78  F1                  VALUE "1001".
       78  ADD-MENU-TEXT       VALUE "REGISTER NEW SANDWICH".
       78  ADD-MENU-TEXT1      VALUE "      EXTERNAL ID:".
       78  ADD-MENU-TEXT2      VALUE "SHORT DESCRIPTION:".
       78  ADD-MENU-TEXT3      VALUE " LONG DESCRIPTION:".
       78  ERROR-EID           VALUE "EXTERNAL ID ALREADY EXISTS".
       78  EID-INSTR           VALUE "ALPHABETIC AND NUMERIC | 5 CHAR MA
      -    "X | MUST BE UNIQUE".
       78  S-DESCR-INSTR       VALUE "MUST BE ALPHABETIC | 30 CHAR MAX".
       78  L-DESCR-INSTR       VALUE"MUST BE ALPHABETIC | 100 CHAR MAX".
       78  LIST-FRAME1         VALUE "ID".
       78  LIST-FRAME2         VALUE "NAME".
       78  ADD-CAT-MENU-TEXT   VALUE "ADD CATEGORIES".
       78  ADD-CAT-MENU-TEXT1  VALUE "        CATEGORIE:".
       78  ADD-ING-MENU-TEXT   VALUE "ADD INGREDIENTS".
       78  ADD-ING-MENU-TEXT1  VALUE "       INGREDIENT:".
       78  ING-INSTR           VALUE "VALID ID | CANT REPEAT THE SAME IN
      -    "GREDIENT | WRITE '999' WHEN NO INGREDIENT".
       78  CAT-INSTR           VALUE "VALID ID | CANT REPEAT THE SAME CA
      -    "TEGORIE | WRITE '999' WHEN NO CATEGORIE".
       78  ING-ERROR           VALUE "INGREDIENT DOESNT EXIST".
       78  CAT-ERROR           VALUE "CATEGORIE DOESNT EXIST".
       78  ING-ZERO            VALUE "AT LEAST ONE INGREDIENT IS REQUIRE
      -    "D".
       78  FILE-NOT-EXISTENT   VALUE "35".
       78  ING-DUPLICATE-ERROR VALUE "THIS INGREDIENT HAS ALREADY BEEN A
      -    "SSIGNED TO THIS SANDWICH".
       78  CAT-DUPLICATE-ERROR VALUE "THIS CATEGORIE HAS ALREADY BEEN AS
      -    "SIGNED TO THIS SANDWICH".
       78  CATEGORIES-FILLED   VALUE "ALL CATEGORIES HAVE BEEN FILLED".
       78  NO-CATEGORIES       VALUE "NO CATEGORIES RECORDS FOUND | IMPO
      -    "SSIBLE TO ASSIGN CATEGORIES".
       78  NO-INGREDIENTS      VALUE "NO INGREDIENTS RECORDS FOUND | RET
      -    "URNING TO MAIN MENU".
       78  CONFIRM-TEXT        VALUE "          SANDWICH".
       78  CONFIRM-TEXT1       VALUE "      EXTERNAL-ID:".
       78  CONFIRM-TEXT2       VALUE "SHORT DESCRIPTION:".
       78  CONFIRM-TEXT3       VALUE " LONG DESCRIPTION:".
       78  CONFIRM-TEXT4       VALUE "       CATEGORIES:".
       78  CONFIRM-TEXT5       VALUE "      INGREDIENTS:".
       78  CONFIRM-TEXT6       VALUE "PRICE:".
       78  CONFIRM-TEXT7       VALUE "EUROS".
       78  WANT-TO-SAVE        VALUE "DO YOU WANT TO SAVE THIS RECORD? (
      -    "Y)ES/(N)O".
       78  RECORD-SAVED        VALUE "RECORD SAVED SUCCESSFULLY!".
       78  RECORD-NOT-SAVED    VALUE "RECORD NOT SAVED!".
       78  NO-SANDWICHES       VALUE "THERE ARE NO SANDWICHES REGISTERED
      -    "! PRESS ANY KEY TO EXIT!".
       78  PREVIOUS-PAGE       VALUE " F1 - PREVIOUS PAGE".
       78  NEXT-PAGE           VALUE "     F2 - NEXT PAGE".
       78  LAST-PAGE           VALUE "          LAST PAGE".
       78  MAIN-SEARCH-OPTION1 VALUE "1 - SEARCH BY INGREDIENTS".
       78  MAIN-SEARCH-OPTION2 VALUE "2 - SEARCH BY CATEGORIES".
       78  MAIN-SEARCH-OPTION3 VALUE "3 - SEARCH BY SANDWICH".
       78  MAIN-SEARCH-OPTION4 VALUE "4 - SEARCH BETWEEN TWO PRICES".
       78  MAIN-SEARCH-OPTION5 VALUE "5 - CREATE A REPORT".
       78  MAIN-SEARCH-OPTION6 VALUE "6 - GO BACK TO MAIN MENU".
       78  MAIN-SEARCH-CHOICE  VALUE "WHAT DO YOU WANT TO DO:".
       78  ALPHA-ERROR         VALUE "FIRST FIELD MUST BE ALPHABETIC".
       78  NO-MATCH            VALUE "NO MORE MATCHES HAVE BEEN FOUND!".
       78  SRCH-ING-MENU-TEXT  VALUE "SEARCH BY INGREDIENT".
       78  SEARCH-NAME         VALUE "SEARCH SANDWICHES".
       78  WRONG-ING           VALUE "INGREDIENT DOESNT EXIST".
       78  WRONG-CAT           VALUE "CATEGORIE DOESNT EXIST".
       78  SRCH-CAT-MENU-TEXT  VALUE "SEARCH BY CATEGORIE".
       78  SRCH-ING-INSTR      VALUE "VALID ID | CANT REPEAT THE SAME IN
      -    "GREDIENT | WRITE '999' TO PROCEED TO SEARCH".
       78  SRCH-CAT-INSTR      VALUE "VALID ID | CANT REPEAT THE SAME CA
      -    "TEGORIE | WRITE '999' TO PROCEED TO SEARCH".
       78  SR-INSTR            VALUE "ENTER A VALID ID".
       78  SRCH-SR-MENU-TEXT   VALUE "SEARCH BY SANDWICH".
       78  SRCH-SR-MENU-TEXT1  VALUE "         SANDWICH:".
       78  SRCH-PRC-MENU-TEXT  VALUE "SEARCH BETWEEN TWO PRICES".
       78  SRCH-PRC-MENU-TEXT1 VALUE "      LOWER PRICE:".
       78  SRCH-PRC-MENU-TEXT2 VALUE "     HIGHER PRICE:".
       78  PRICE-INSTR         VALUE "VALUES MUST BE WITHIN AN INTERVAL
      -    " | IF ZEROS THEN WONT SEARCH".
       78  PRICE-ERROR         VALUE "VALUES ARENT WITHIN A VALID INTERV
      -    "AL".
       78  NO-RECORDS          VALUE "NO RESULTS FOUND | PRESS ANY KEY T
      -    "O CONTINUE".
       78  RPT-DONE            VALUE "REPORT HAS BEEN CREATED".
       78  REPORT-TITLE        VALUE "SANDWICH REPORT".
       78  REPORT-ID           VALUE "ID".
       78  REPORT-S-DESCRIPTION    VALUE "SHORT DESCRIPTION".
       78  REPORT-L-DESCRIPTION    VALUE "LONG DESCRIPTION".
       78  REPORT-PRICE        VALUE "PRICE".
       78  REPORT-CATEGORIES   VALUE "CATEGORIE".
       78  REPORT-INGREDIENTS  VALUE "INGREDIENT".
       78  REPORT-QUANTITY     VALUE "QUANTITY".
       78  REPORT-UNIT         VALUE "UNIT OF MEASURE".
       78  PAGECONST           VALUE "PAGE".
       78  REPORT-DATE-TEXT    VALUE "DATE:".
       78  REPORT-HOUR-TEXT    VALUE "TIME:".
