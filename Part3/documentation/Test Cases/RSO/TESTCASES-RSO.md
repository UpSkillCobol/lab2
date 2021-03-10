# Test Cases
## Registration of Sandwich Orders

| UC | MODULE | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| RSO01 | REGISTER | (30/02/2022); (04/03/2021) | "Invalid date" | DL | BL & CC & DL & FB | 05/03/2021 | PASSED - "Invalid date" | X |
| RSO02 | REGISTER | (05/03/2021); (06/03/2021); (07/03/2021) | "Less than 3 days in advance" | DL | BL & CC & DL & FB | 05/03/2021 | PASSED - "Less than 3 days in advance" | X |
| RSO03 | REGISTER | SCHOOL ID / SANDWICH ID / QUANTITY -> (000) | "Invalid" | DL | BL & CC & DL & FB | 05/03/2021 | PASSED - "Invalid" | X |
| RSO04 | REGISTER | (08:00); (18:00) | "Invalid hour" | DL | DL | 05/03/2021 | PASSED - "Invalid hour" | X |
| RSO05 | REGISTER | (15/03/2021) | "Coincides with a period of unavailability" | DL | FB | 10/03/2021 | PASSED - "Coincides with a period of unavailability" | X |
| RSO06 | REGISTER | Test F2 and F1 to navigate through the lists | Search through next and previous page | DL | BL | 10/03/2021 | PASSED | X |
| RSO07 | REGISTER | Test F3 anytime | Back to main menu | DL | CC | 10/03/2021 | PASSED | X |
| RSO08 | REPORT | Create Report | Creation of report file | DL | FB | 10/03/2021 | PASSED | X |
| RSO09 | SEARCH | 1 - Search orders by specific school | All orders corresponding to that particular school | DL | CC | 10/03/2021 | PASSED | X |
| RSO10 | SEARCH | 2 - Search orders by specific sandwich | All orders corresponding to that particular sandwich | DL | BL | 10/03/2021 | PASSED | X |
| RSO11 | SEARCH | 3 - Search orders by specific period of time | All orders corresponding to that particular period of time | DL | FB | 10/03/2021 | PASSED | X |
| RSO12 | SEARCH | 4 - Search orders by specific school and sandwich | All orders corresponding to that particular school and sandwich | DL | BL | 10/03/2021 | PASSED | X |
| RSO13 | SEARCH | 5 - Search orders by specific period of time and school | All orders corresponding to that particular period of time and school | DL | FB | 10/03/2021 | PASSED | X |