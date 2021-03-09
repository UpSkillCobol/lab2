
       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".


       78  MAIN-TEXT        VALUE "INVENTORY MANAGEMENT".
       78  MAIN-TEXT1       VALUE "F3 - MAIN MENU INVENTORY".

       78  OPTION-REGISTER1 VALUE "1 - REGISTER INVENTORY".
       78  OPTION-SEARCH2   VALUE "2 - SEARCH INVENTORY".
       78  OPTION-REPORT3   VALUE "3 - CREATE REPORT".
       78  OPTION-EXIT4     VALUE "4 - EXIT PROGRAM".
       78  ACCEPT-OPTION    VALUE "PLEASE CHOOSE AN OPTION:".


       78  OPTION-ERROR
           VALUE "INVALID OPTION. PLEASE SELECT A VALID OPTION | PRESS A
      -    "NY KEY TO CONTINUE".

       78  REGISTER-TEXT               VALUE "          REGISTER:".
       78  REGISTER-TEXT-ID            VALUE "     INGREDIENT ID:".
       78  REGISTER-TEXT-IN-DATE       VALUE "           DATE IN:".
       78  REGISTER-TEXT-OUT-DATE      VALUE "          DATE OUT:".
       78  REGISTER-TEXT-IN-QUANTITY   VALUE "       QUANTITY IN:".
       78  REGISTER-TEXT-OUT-QUANTITY  VALUE "      QUANTITY OUT:".
       78  REGISTER-TEXT-ACTZ-DATE     VALUE "       UPDATE DATE:".

       78  INVALID-DATE
           VALUE "INVALID DATE. PLEASE ENTER A VALID DATE | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  INSTRUCTIONS-ID
           VALUE "SEARCH THROUGH THE INGREDIENT ID LIST.".

       78  INVALID-ID
           VALUE "INVALID ID. ENTER A VALID ID.".


       78  INVALID-QUANTITY
           VALUE "INVALID QUANTITY. PLEASE ENTER A VALID QUANTITY | PRES
      -    "S ANY KEY TO CONTINUE".

       78  MESSAGE-SAVE
           VALUE "WOULD YOU LIKE TO SAVE ? (Y)ES | (N)O:".

       78  INVALID-OPTION
           VALUE "INVALID. ENTER A VALID OPTION: (Y) FOR YES, (N) FOR NO
      -    "".

       78  MESSAGE-WRITE-YES
           VALUE "SAVED SUCCESSFULLY | PRESS ANY KEY TO CONTINUE".

       78  MESSAGE-WRITE-NO
           VALUE "NOT SAVED | PRESS ANY KEY TO CONTINUE".

       78  LIST-FRAME1 VALUE "ID  |  INGREDIENT".

       78  LIST-FRAME2 VALUE "LIST OF QUANTITIES".

       78  LIST-FRAME3
           VALUE "ID  |  NAME                                         UN
      -    "ITS".

       78  PREVIOUS-PAGE VALUE "F1 - PREVIOUS PAGE".

       78  NEXT-PAGE VALUE "     F2 - NEXT PAGE".

       78  LAST-PAGE VALUE "          LAST PAGE".

       78  UNTIL-LIST VALUE "        UNTIL        ".


       78  ERROR-SEARCH
           VALUE "NO RESULTS FOUND | PRESS ANY KEY TO CONTINUE".



      ******************************************************************

       78  VIEW-MENU-OPTION1     VALUE "1 - VIEW QUANTITIES ONE BY ONE".
       78  VIEW-MENU-OPTION2     VALUE "2 - VIEW A SPECIFIC QUANTITY".
       78  VIEW-MENU-OPTION3     VALUE "3 - MAIN MENU".
       78  VIEW-MENU-ACCEPT      VALUE "PLEASE CHOOSE AN OPTION:".

       78  EMPTY-FIELD-TEXT VALUE "(EMPTY)".

       78  REQUEST-ID-TEXT VALUE "PLEASE TYPE DOWN INGREDIENT ID:".

       78  INVALID-ID-TEXT
           VALUE "INVALID ID. PLEASE ENTER A VALID ID | PRESS ANY KEY TO
      -    " CONTINUE".

       78  VIEW-RECORDS-ONEBYONE
           VALUE "PRESS A KEY TO SEE INGREDIENTS.".

       78  END-RECORDS-VIEW
           VALUE "YOU HAVE REACHED THE END OF THE RECORDS | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  VIEW-SPECIFIC VALUE "PRESS ANY KEY TO LEAVE SCREEN".

       78  ID-NONEXISTENT
           VALUE "ID DOESN'T EXIST | PRESS ANY KEY TO CONTINUE".

       78  EMPTY-RECORDS
           VALUE "THERE IS NO REGISTERS RECORDED IN THIS FILE".

       78  EMPTY-RECORDS2 VALUE "PRESS ANY KEY TO CONTINUE".

       78  WHAT-TO-EDIT VALUE "WHAT TO EDIT".

       78  EDIT1 VALUE "1 - DATE IN".

       78  EDIT2 VALUE "2 - DATE OUT".

       78  EDIT3 VALUE "3 - QUANTITY IN".

       78  EDIT4 VALUE "4 - QUANTITY OUT".

       78  EDIT5 VALUE "5 - UPDATE DATE".

       78  EDIT6 VALUE "6 - PREVIOUS MENU".

       78  CHOOSE VALUE "CHOOSE AN OPTION:".

       78  MESSAGE-EDITED
           VALUE "EDITED SUCCESSFULLY | PRESS ANY KEY TO CONTINUE".


      ******************************************************************



       78  REPORTNUMBERCONST   VALUE "NUMBER".
       78  REPORTNAMECONST     VALUE "NAME".
       78  REPORTPAGECONST     VALUE "PAGE".
       78  REPORTTITLECONST1
           VALUE "B R E A D W I C H   R E P O R T".
       78  REPORTTITLECONST2
           VALUE "I N V E N T O R Y    M A N A G E M E N T".
