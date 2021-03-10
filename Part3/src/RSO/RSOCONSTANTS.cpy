      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    ENGLISH LANGUAGE | V0.5 | IN UPDATE | 09.03.2021
      ******************************************************************

       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".

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
           VALUE "SEARCH THROUGH THE LIST TO FIND THE SCHOOL ID".

       78  INSTRUCTIONS-SANDWICH
           VALUE "SEARCH THROUGH THE LIST TO FIND THE SANDWICH ID".

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

       78  LIST-FRAME3
           VALUE "ID  |  NAME                                         PR
      -    "ICE".

       78  PREVIOUS-PAGE VALUE "F1 - PREVIOUS PAGE".

       78  NEXT-PAGE VALUE "     F2 - NEXT PAGE".

       78  LAST-PAGE VALUE "          LAST PAGE".

       78  UNTIL-LIST VALUE "        UNTIL        ".

       78  EUROS VALUE "EUROS".

       78  PRICE VALUE "    TOTAL ORDER PRICE:".

       78  SEARCH-MENU-OPTION1
           VALUE "1 - VIEW ORDERS BY A SPECIFIC SCHOOL".
       78  SEARCH-MENU-OPTION2
           VALUE "2 - VIEW ORDERS BY A SPECIFIC SANDWICH".
       78  SEARCH-MENU-OPTION3
           VALUE "3 - VIEW ORDERS BY A SPECIFIC PERIOD OF TIME".
       78  SEARCH-MENU-OPTION4
           VALUE "4 - VIEW ORDERS BY A SPECIFIC SCHOOL AND SANDWICH".
       78  SEARCH-MENU-OPTION5
           VALUE "5 - VIEW ORDERS BY A SPECIFIC PERIOD OF TIME AND SPECI
      -    "FIC SCHOOL".
       78  SEARCH-MENU-OPTION6
           VALUE "6 - BACK MAIN MENU".
       78  SEARCH-MENU-ACCEPT
           VALUE "PLEASE CHOOSE AN OPTION:".

       78  ORDERS-INEXISTENT
           VALUE "NO ORDERS REGISTRED AT THE MOMENT | PRESS ANY KEY TO C
      -    "ONTINUE".

       78  ERROR-SEARCH
           VALUE "NO RESULTS FOUND | PRESS ANY KEY TO CONTINUE".

       78  PERIOD-SEARCH
           VALUE "      PERIOD OF TIME TO SEARCH:".

       78  SCHOOL-SEARCH
           VALUE "              SCHOOL TO SEARCH:".

       78  SANDWICH-SEARCH
           VALUE "            SANDWICH TO SEARCH:".

       78  VIEW-ORDERS-ONEBYONE
           VALUE "PRESS ANY KEY TO NAVIGATE THROUGH THE ORDERS".

       78  THROUGH-TEXT VALUE "THROUGH".

       78  NO-MORE-MATCHES VALUE "NO MORE RESULTS HAVE BEEN FOUND | PRES
      -    "S ANY KEY TO CONTINUE".

       78  REPORTTITLECONST1
           VALUE "B R E A D W I C H   R E P O R T".
       78  REPORTTITLECONST2
           VALUE "R E G I S T R A T I O N   O F   S A N D W I C H   O R
      -    "D E R S".

       78  REPORTORDERNUMBER   VALUE "ORDER".
       78  REPORTDELIVERYDATE  VALUE "DELIVERY".
       78  REPORTORDERSCHOOL   VALUE "SCHOOL".
       78  REPORTORDERSANDWICH VALUE "SANDWICH".
       78  REPORTORDERQUANTITY VALUE "QUANTITY".
       78  REPORTORDERDATE     VALUE "DATE".
       78  REPORTPAGE          VALUE "  PAGE:".

       78  REPORT-DONE VALUE "REPORT CREATED SUCESSFULLY | PRESS ANY KEY
      -    " TO CONTINUE".
