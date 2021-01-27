      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    ALL CONSTANTS
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 27.01.2020
      ******************************************************************
      *MODULE NAME
       78 MODULE-NAME          VALUE "SUPPLIERS MANAGEMENT".
      *BACK
       78 BACK-EXIT            VALUE "F3 - BACK | F4 - EXIT".
      * MAIN MENU OPTION 1
       78 MAIN-MENU-OPTION1    VALUE "1 - REGISTER SUPPLIER".
      * MAIN MENU OPTION 2
       78 MAIN-MENU-OPTION2    VALUE "2 - VIEW SUPPLIER".
      * MAIN MENU OPTION 3
       78 MAIN-MENU-OPTION3    VALUE "3 - EDIT SUPPLIER".
      * MAIN MENU OPTION 4
       78 MAIN-MENU-OPTION4    VALUE "4 - DELETE SUPPLIER".
      * MAIN MENU OPTION 5
       78 MAIN-MENU-OPTION5    VALUE "5 - EXIT".
      *MAIN MENU OPTION CHOICE
       78 MAIN-MENU-CHOICE     VALUE "PLEASE CHOOSE AN OPTION".
      *MAIN MENU ERROR
       78 MAIN-MENU-ERROR
           VALUE "INVALID OPTION, PLEASE SELECT A VALID OPTION. PRESS AN
      -    "Y KEY TO CONTINUE".
      *REGISTER SUPPLIER MENU OPTION 1
       78 SUPPLIER-MENU-OPTION1 VALUE "1 - REGISTER SUPPLIER MANUALLY".
      *REGISTER SUPPLIER MENU OPTION 2
       78 SUPPLIER-MENU-OPTION2 VALUE "2 - REGISTER SUPPLIER USING A CSV
      -    " FILE".
      *REGISTER SUPPLIER MENU OPTION 3
       78 SUPPLIER-MENU-OPTION3   VALUE "3 - RETURN TO MAIN MENU".
      *REGISTER SUPPLIER MENU CHOICE
       78 SUPPLIER-MENU-CHOICE   VALUE "PLEASE CHOOSE AN OPTION".
      *ADD SUPPLIER MENU ERROR
       78 ADD-SUPPLIER-MENU-ERROR
           VALUE "INVALID OPTION, PLEASE SELECT A VALID OPTION. PRESS AN
      -    "Y KEY TO CONTINUE".
      *SUPPLIER ID
       78  SCREEN-SUPPLIER-ID VALUE "SUPPLIER ID:".
      *>  78  SCREEN-SUPPLIER-ID VALUE "ID FORNECEDOR:".
      *MANUALLY ADD SUPPLIER NAME
       78 MANUALLY-ADD-NAME VALUE "       NAME:".

       78 MANUALLY-ADD-DESCRIPTION VALUE "DESCRIPTION:".

       78 MANUALLY-ADD-ADDRESS VALUE "    ADDRESS:".

       78 MANUALLY-ADD-POSTAL-CODE VALUE "POSTAL CODE:".

       78 MANUALLY-ADD-TOWN VALUE "       TOWN:".

       78 MANUALLY-ADD-EMAIL1 VALUE "    EMAIL 1:".

       78 MANUALLY-ADD-EMAIL2 VALUE "    EMAIL 2:".

       78 MANUALLY-ADD-EMAIL3 VALUE "    EMAIL 3:".

       78 MANUALLY-ADD-PHONE1 VALUE "TELEPHONE 1:".

       78 MANUALLY-ADD-PHONE2 VALUE "TELEPHONE 2:".

       78 MANUALLY-ADD-PHONE3 VALUE "TELEPHONE 3:".

       78 MESSAGE-NAME VALUE "30 CHARS MAX AUTO UPPER-CASE CONVERSION, E
      -    "XTRA SPACES WILL BE REMOVED".

       78 ERROR-NAME VALUE "THE NAME MUST BE FILLED".

       78 MESSAGE-DESCRIPTION VALUE "150 CHARS MAX|AUTO UPPER-CASE|AUTO
      -    "LINE CHANGE AT LINE END|TAB TO MOVE BETWEEN LINES".

       78 MESSAGE-ADDRESS VALUE "100 CHARS MAX|AUTO UPPER-CASE|AUTO LINE
      -    "CHANGE AT LINE END|TAB TO MOVE BETWEEN LINES".

       78 ERROR-ADDRESS VALUE "THE ADDRESS MUST BE FILLED".

       78 MESSAGE-POSTAL-CODE VALUE "BETWEEN 1000000 AND 9999999|AUTO MO
      -    "VE AT END|TAB TO MOVE BETWEEN FIELDS".

       78 ERROR-POSTAL-CODE VALUE "BETWEEN 1000000 AND 9999999".

       78 MESSAGE-TOWN VALUE "PROPOSED TOWN IS USUALLY CORRECT|VALUE IF
      -    "CHANGED MUST BE ALPHABETIC".

       78 ERROR-TOWN VALUE "NO NUMBERS ALLOWED. MUST BE ALPHABETIC".

       78 MESSAGE-EMAIL VALUE "3 EMAILS CAN BE INSERTED|CYCLE BETWEEN TH
      -    "EM USING TAB|ONLY E-MAIL 1 IS MANDATORY".

       78 ERROR-EMAIL VALUE "ENTER VALID E-MAIL".

       78 MESSAGE-PHONE VALUE "ONLY VALID PORTUGUESE PHONES|JUST PHONE 1
      -    " IS MANDATORY|TAB TO MOVE BETWEEN PHONES".

       78 ERROR-PHONE VALUE "INVALID. ENTER A VALID TELEPHONE NUMBER".

       78 MESSAGE-SAVE VALUE "DO YOU WANT TO SAVE THIS NEW SUPPLIER?(Y)E
      -    "S (N)O:".

       78 ERROR-SAVE VALUE "INVALID. ENTER A VALID CHOICE (Y) FOR YES, (
      -    "N) FOR NO".

       78 MESSAGE-WRITE-YES VALUE "SUPPLIER RECORD SAVED IN THE DATABASE
      -    "".

       78 MESSAGE-WRITE-NO VALUE "SUPPLIER NOT SAVED IN THE DATABASE".

       78 ERROR-GET-SUPPID VALUE "INVALID. SUPPLIER ID MUST BE BETWEEN
      -    "001 AND 999".

       78 MESSAGE-GET-SUPPID VALUE "PLEASE INDICATE SUPPLIER ID:".

       78 ERROR-SUPPID-NO VALUE "THAT SUPPLIER ID DOESN'T EXIST. PLEASE
      -    "INSERT A VALID SUPPLIER ID".

       78 MESSAGE-EDIT-ALL VALUE "MOVE BETWEEN THE FIELDS WITH TAB|PRESS
      -    " ENTER WHEN DONE EDITING".

       78 WHAT-TO-EDIT VALUE "WHAT TO EDIT".

       78 EDIT1 VALUE "1 - NAME".

       78 EDIT2 VALUE "2 - DESCRIPTION".

       78 EDIT3 VALUE "3 - ADDRESS".

       78 EDIT4 VALUE "4 - POSTAL CODE".

       78 EDIT5 VALUE "5 - TOWN".

       78 EDIT6 VALUE "6 - EMAIL".

       78 EDIT7 VALUE "7 - TELEPHONE".

       78 EDIT8 VALUE "8 - PREVIOUS MENU".

       78 CHOOSE VALUE "CHOOSE AN OPTION:".

       78 LIST-NEXT-PAGE VALUE "PRESS ANY KEY TO SHOW THE NEXT RECORDS".

       78 VIEW-NEXT-RECORD VALUE "PRESS ANY KEY TO SHOW THE NEXT SUPPLIE
      -    "R".
      *REGISTER SUPPLIER MENU OPTION 1
       78 VIEW-MENU-OPTION1 VALUE "1 - VIEW ALL SUPPLIERS".
      *REGISTER SUPPLIER MENU OPTION 2
       78 VIEW-MENU-OPTION2 VALUE "2 - VIEW A SPECIFIC SUPPLIER".
      *REGISTER SUPPLIER MENU OPTION 3
       78 VIEW-MENU-OPTION3   VALUE "3 - MAIN MENU".
      *REGISTER SUPPLIER MENU CHOICE
       78 VIEW-MENU-CHOICE   VALUE "PLEASE CHOOSE AN OPTION".
      *ADD SUPPLIER MENU ERROR
       78 VIEW-SUPPLIER-MENU-ERROR
           VALUE "INVALID OPTION, PLEASE SELECT A VALID OPTION. PRESS AN
      -    "Y KEY TO CONTINUE".

       78 VIEW-SPECIFIC VALUE "PRESS ANY KEY TO CONTINUE".

       78 VIEW-ALL-SUPP-NEXT-ONE VALUE "PRESS ANY KEY TO SHOW THE NEXT S
      -    "UPPLIER".

       78 DELETE-SUPPLIER VALUE "ARE YOU SURE YOU WANT TO DELETE THIS SU
      -    "PPLIER? (Y)ES (N)O:".

       78 DELETE-ERROR
           VALUE "INVALID OPTION, PLEASE SELECT A VALID OPTION. PRESS AN
      -    "Y KEY TO CONTINUE".

       78 DELETE-YES VALUE "SUPPLIER REMOVED FROM THE DATABASE".

       78 DELETE-NO VALUE "SUPPLIER NOT REMOVED FROM THE DATABASE".
