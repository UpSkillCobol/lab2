      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    ENGLISH LANGUAGE | V0.6 | IN UPDATE | 02.02.2021
      ******************************************************************

       78  MAIN-TEXT        VALUE "CALENDAR MANAGEMENT".
       78  MAIN-TEXT1       VALUE "F3 - BACK | F4 - EXIT".

       78  OPTION-REGISTER1 VALUE "1 - ADD DOWNTIME".
       78  OPTION-VIEW2     VALUE "2 - VIEW DOWNTIME".
       78  OPTION-EDIT3     VALUE "3 - EDIT DOWNTIME".
       78  OPTION-DELETE4   VALUE "4 - DELETE DOWNTIME".
       78  OPTION-EXIT5     VALUE "5 - EXIT PROGRAM".
       78  ACCEPT-OPTION    VALUE "PLEASE CHOOSE AN OPTION:".

       78  OPTION-ERROR
           VALUE "INVALID OPTION. PLEASE SELECT A VALID OPTION | PRESS A
      -    "NY KEY TO CONTINUE".

       78  REGISTER-TEXT             VALUE " REGISTER NEW DOWNTIME".
       78  REGISTER-TEXT-ID          VALUE "          DOWNTIME ID:".
       78  REGISTER-TEXT-DATE        VALUE "  DOWNTIME START DATE:".
       78  REGISTER-TEXT-DATE1       VALUE "    DOWNTIME END DATE:".
       78  REGISTER-TEXT-DESCRIPTION VALUE " DOWNTIME DESCRIPTION:".

       78  INVALID-DATE
           VALUE "INVALID DATE. PLEASE ENTER A VALID DATE".

       78  EXISTENT-DATE
           VALUE "INSERTED DATE IS ALREADY REGISTERED! TRY ANOTHER ONE".

       78  INSTRUCTIONS-DATE
           VALUE "DATES MUST BE WRITTEN ACCORDING TO THE CURRENT DAY AND
      -    " A VALID DAY ACCORDING TO THE MONTH".

       78  INSTRUCTIONS2-DATE
           VALUE "PRESS ENTER TO BLANK IF YOU DON'T KNOW THE END OF DOWN
      -    "TIME DATE".

       78  INSTRUCTIONS-DESCRIPTION
           VALUE "OPTIONAL FIELD: TYPE ANY DESCRIPTION OR NOTES OR PRESS
      -    " ENTER TO BLANK".

       78  MESSAGE-SAVE
           VALUE "WOULD YOU LIKE TO SAVE THIS DOWNTIME RECORD? (Y)ES | (
      -    "N)O:".

       78  INVALID-OPTION
           VALUE "INVALID. ENTER A VALID OPTION: (Y) FOR YES, (N) FOR NO
      -    "".

       78  MESSAGE-WRITE-YES
           VALUE "DOWNTIME RECORD SAVED SUCCESSFULLY".

       78  MESSAGE-WRITE-NO
           VALUE "DOWNTIME RECORD NOT SAVED".

       78  VIEW-MENU-OPTION1      VALUE "1 - VIEW DOWNTIME ONE BY ONE".
       78  VIEW-MENU-OPTION2      VALUE "2 - VIEW A SPECIFIC DOWNTIME".
       78  VIEW-MENU-OPTION3      VALUE "3 - MAIN MENU".
       78  VIEW-MENU-ACCEPT       VALUE "PLEASE CHOOSE AN OPTION:".

       78  EMPTY-FIELD-TEXT VALUE "(EMPTY)".

       78  VIEW-RECORDS-ONEBYONE
           VALUE "PRESS ANY KEY TO NAVIGATE THROUGH DOWNTIME RECORDS".

       78  END-RECORDS-VIEW
           VALUE "YOU HAVE REACHED THE END OF THE RECORDS | PRESS ANY KE
      -    "Y TO CONTINUE".

       78  VIEW-SPECIFIC VALUE "PRESS ANY KEY TO LEAVE SCREEN".

       78  ID-NONEXISTENT
           VALUE "DOWNTIME ID RECEIVED DOESN'T EXIST | PRESS ANY KEY TO
      -    "CONTINUE".
