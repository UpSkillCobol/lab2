# Test Cases
## Registration of Ingredients Suppliers

| UC | MODULE | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| RIS01 | REGISTER | Supplier - (000) | "Invalid supplier." | BL | DL & BL | 08/03/2021 | PASSED - "Invalid supplier." | X |
| RIS02 | REGISTER | Supplier - (002) | Move on to the next accept | BL | FB | 10/03/2021 | PASSED | X |
| RIS03 | REGISTER | Ingredient - (002) | Move on to the next accept | BL | DL | 10/03/2021 | PASSED | X |
| RIS04 | REGISTER | Price - (000) | Doesn't accept | BL | CC | 10/03/2021 | PASSED - Doesn't accept | Doesn't show unit measure of ingredient  |
| RIS05 | REGISTER | Date - Day before or same day | "Invalid date." | BL | DL | 10/03/2021 | FAILED | X |
| RIS06 | REGISTER | Date - Day before or same day | "Invalid date." | BL | DL | 10/03/2021 | PASSED - "Invalid date." | RIS05 Bug fixed |
| RIS07 | REGISTER | Test F3 anytime | Back to main menu | BL | FB | 10/03/2021 | PASSED | X |
RIS08 | REGISTER | Same supplier and ingredient already registred | "Already exist" | BL | CC | 10/03/2021 | PASSED | X |
| RIS09 | SEARCH | 1 - Search prices with invalid dates | Supposed to show one record (saved in file) with invalid date | BL | FB | 10/03/2021 | PASSED | X |
| RIS10 | SEARCH | 2 - Search supplier by ingredient | Supposed to show supplier that sells that specific ingredient | BL | DL | 10/03/2021 | PASSED | X |
| RIS11 | SEARCH | Create Report | Creation of report file | BL | CC | 10/03/2021 | PASSED | X |
| RIS12 | REGISTER | Price | Check if unit measure of ingredient it's shown while register price | BL | FB | 10/03/2021 | PASSED | RIS04 Bug fixed |