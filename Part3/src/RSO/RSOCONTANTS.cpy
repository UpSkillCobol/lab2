      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    ENGLISH LANGUAGE | V0.3 | IN UPDATE | 07.03.2021
      ******************************************************************

       78  MAIN-TEXT        VALUE "REGISTRATION OF SANDWICH ORDERS".
       78  MAIN-TEXT1       VALUE "F3 - MAIN MENU RSO".

       78  OPTION-REGISTER1 VALUE "1 - REGISTER ORDERS".
       78  OPTION-SEARCH2   VALUE "2 - SEARCH ORDERS".
       78  OPTION-REPORT3   VALUE "3 - CREATE REPORT".
       78  OPTION-EXIT4     VALUE "4 - EXIT PROGRAM".
       78  ACCEPT-OPTION    VALUE "PLEASE CHOOSE AN OPTION:".

       78  OPTION-ERROR
           VALUE "INVALID OPTION. PLEASE SELECT A VALID OPTION | PRESS A
      -    "NY KEY TO CONTINUE".

       78  REGISTER-TEXT               VALUE "          NEW ORDER".
       78  REGISTER-TEXT-ID            VALUE "          ORDER ID:".
       78  REGISTER-TEXT-DELIVERY-DATE VALUE "     DELIVERY DATE:".
       78  REGISTER-TEXT-SCHOOL        VALUE "         SCHOOL ID:".
       78  REGISTER-TEXT-SANDWICH      VALUE "       SANDWICH ID:".
       78  REGISTER-TEXT-QUANTITY      VALUE "          QUANTITY:".
       78  REGISTER-TEXT-ORDER-DATE    VALUE "        ORDER DATE:".

       78  SCHOOLS-INEXISTENT
           VALUE "TO DO AN ORDER YOU NEED AT LEAST ONE SCHOOL REGISTRED
      -    "| PRESS ANY KEY TO CONTINUE".

       78  SANDWICH-INEXISTENT
           VALUE "TO DO AN ORDER YOU NEED AT LEAST ONE SANDWICH REGISTRE
      -    "D | PRESS ANY KEY TO CONTINUE".

       78  INSTRUCTIONS-DATE
           VALUE "ONLY VALID DATES AFTER 3 DAYS OF ORDER WILL BE ACCEPTE
      -    "D".

       78  INSTRUCTIONS-TIME
           VALUE "TIME MUST BE INSERTED BETWEEN 09:00 TO 17:59".

       78  INSTRUCTIONS-SCHOOL
           VALUE "SEARCH THROUGH THE LIST TO FIND YOUR SCHOOL ID".

       78  INSTRUCTIONS-SANDWICH
           VALUE "SEARCH THROUGH THE LIST TO FIND THE SANDWICH ID THAT Y
      -    "OU WOULD LIKE TO ORDER".

       78  INSTRUCTIONS-QUANTITY
           VALUE "TYPE THE AMOUNT OF SANDWICH YOU WOULD LIKE TO ORDER".

       78  INVALID-DATE1
           VALUE "INVALID DATE. PLEASE ENTER A VALID DATE | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  INVALID-DATE2
           VALUE "INSERTED DELIVERY DATE IS LESS THAN 3 DAYS IN ADVANCE
      -    "| PRESS ANY KEY TO CONTINUE".

       78  INVALID-DATE3
           VALUE "INSERTED DELIVERY DATE COINCIDES WITH A PERIOD OF UNAV
      -    "AILABILITY | PRESS ANY KEY TO CONTINUE".

       78  INVALID-TIME
           VALUE "INVALID TIME. PLEASE ENTER A VALID TIME | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  INVALID-SCHOOL
           VALUE "INVALID SCHOOL. PLEASE ENTER A VALID SCHOOL | PRESS AN
      -    "Y KEY TO CONTINUE".

       78  INVALID-SANDWICH
           VALUE "INVALID SANDWICH. PLEASE ENTER A VALID SANDWICH | PRES
      -    "S ANY KEY TO CONTINUE".

       78  INVALID-QUANTITY
           VALUE "INVALID QUANTITY. PLEASE ENTER A VALID QUANTITY | PRES
      -    "S ANY KEY TO CONTINUE".

       78  MESSAGE-SAVE
           VALUE "WOULD YOU LIKE TO SAVE THIS SANDWICH ORDER? (Y)ES | (N
      -    ")O:".

       78  INVALID-OPTION
           VALUE "INVALID. ENTER A VALID OPTION: (Y) FOR YES, (N) FOR NO
      -    "".

       78  MESSAGE-WRITE-YES
           VALUE "SANDWICH ORDER SAVED SUCCESSFULLY | PRESS ANY KEY TO C
      -    "ONTINUE".

       78  MESSAGE-WRITE-NO
           VALUE "SANDWICH ORDER NOT SAVED | PRESS ANY KEY TO CONTINUE".

       78  LIST-FRAME1 VALUE "ID  |  NAME".

       78  LIST-FRAME2 VALUE "LIST OF PERIODS OF UNAVAILABILITY".

       78  PREVIOUS-PAGE VALUE "F1 - PREVIOUS PAGE".

       78  NEXT-PAGE VALUE "     F2 - NEXT PAGE".

       78  LAST-PAGE VALUE "          LAST PAGE".



      ******************************************************************

       78  VIEW-MENU-OPTION1      VALUE "1 - VIEW DOWNTIME ONE BY ONE".
       78  VIEW-MENU-OPTION2      VALUE "2 - VIEW A SPECIFIC DOWNTIME".
       78  VIEW-MENU-OPTION3      VALUE "3 - MAIN MENU".
       78  VIEW-MENU-ACCEPT       VALUE "PLEASE CHOOSE AN OPTION:".

       78  EMPTY-FIELD-TEXT VALUE "(EMPTY)".

       78  REQUEST-ID-TEXT VALUE "PLEASE TYPE DOWN DOWNTIME ID:".

       78  INVALID-ID-TEXT
           VALUE "INVALID ID. PLEASE ENTER A VALID ID | PRESS ANY KEY TO
      -    " CONTINUE".

       78  VIEW-RECORDS-ONEBYONE
           VALUE "PRESS ANY KEY TO NAVIGATE THROUGH DOWNTIME RECORDS".

       78  END-RECORDS-VIEW
           VALUE "YOU HAVE REACHED THE END OF THE RECORDS | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  VIEW-SPECIFIC VALUE "PRESS ANY KEY TO LEAVE SCREEN".

       78  ID-NONEXISTENT
           VALUE "DOWNTIME ID RECEIVED DOESN'T EXIST | PRESS ANY KEY TO
      -    "CONTINUE".

       78  EMPTY-RECORDS
           VALUE "THERE IS NO REGISTERS RECORDED IN THIS FILE".

       78  EMPTY-RECORDS2 VALUE "PRESS ANY KEY TO CONTINUE".

       78  WHAT-TO-EDIT VALUE "WHAT TO EDIT".

       78  EDIT1 VALUE "1 - START DATE".

       78  EDIT2 VALUE "2 - START TIME".

       78  EDIT3 VALUE "3 - END DATE".

       78  EDIT4 VALUE "4 - END TIME".

       78  EDIT5 VALUE "5 - DESCRIPTION".

       78  EDIT6 VALUE "6 - PREVIOUS MENU".

       78  CHOOSE VALUE "CHOOSE AN OPTION:".

       78  MESSAGE-EDITED
           VALUE "DOWNTIME RECORD EDITED SUCCESSFULLY | PRESS ANY KEY TO
      -    " CONTINUE".

       78  MESSAGE-DELETE
           VALUE "ARE YOU SURE DO YOU WANT TO DELETE THIS DOWNTIME RECOR
      -    "D? (Y)ES | (N)O:".

       78  MESSAGE-DELETE-YES
           VALUE "DOWNTIME RECORD DELETED SUCCESSFULLY | PRESS ANY KEY T
      -    "O CONTINUE".

       78  MESSAGE-DELETE-NO
           VALUE "DOWNTIME RECORD NOT DELETED | PRESS ANY KEY TO CONTINU
      -    "E".

      ******************************************************************



       78  REPORTNUMBERCONST   VALUE "NUMBER".
       78  REPORTNAMECONST     VALUE "NAME".
       78  REPORRTCODECONST    VALUE "COUNTRY".
       78  REPORTPAGECONST     VALUE "PAGE".
       78  REPORTTITLECONST1
           VALUE "B R E A D W I C H   R E P O R T".
       78  REPORTTITLECONST2
           VALUE "R E G I S T R A T I O N   O F   S A N D W I C H   O R
      -    "D E R S".
