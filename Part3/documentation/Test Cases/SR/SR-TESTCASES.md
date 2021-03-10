# Test Cases
## Sandwich Registration

| UC | MODULE | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| SR01 | REGISTER | Creation of Sandwich (Test of Ingredients List) | "001 - Atum / 002 - Queijo" | CC | BL | 05/03/2021 | PASSED - "001 - Atum / 002 Queijo" | X |
| SR02  | REGISTER | Creation of Sandwich (Test of Categories List) | "001 - Saudavel / 003 - Picante" | CC | DL | 05/03/2021 | PASSED - "001 - Saudavel / 003 - Picante" | X |
| SR03 | REGISTER | Creation of Sandwich (Test of Confirmation Screen) | Show all the details from the sandwich | CC | DL | 05/03/2021 | PASSED - All details shown successfully | X |
| SR04  | REGISTER | Creation of Sandwich (Test Saving Record) | Records saved on all 3 files | CC | CC | 05/03/2021 | PASSED - All records saved on all 3 files | X |
| SR05 | REGISTER | Creation of Sandwich (Test of Ingredients List) | Next page when pressing F2 | CC | DL | 05/03/2021 | PASSED - Page changed when F2 was pressed | X |
| SR06 |  REGISTER | Creation of Sandwich (Test of Ingredients List) | Previous page when pressing F1 | CC | DL | 05/03/2021 | PASSED - Page changed when F1 was pressed | X |
| SR07 | REGISTER | Creation of SandWich (Test of Categories List) | Next page when pressing F2 | CC | BL | 05/03/2021 | PASSED - Page changed when F2 was presse | X |
| SR08 | REGISTER | Creation of SandWich (Test of Categories List) | Previous page when pressing F1 | CC | BL | 05/03/2021 | PASSED - Page changed when F1 was pressed | X |
| SR09 | REGISTER | Delete Ingredients File | Show error and exit program | CC | FB | 05/03/2021 | PASSED - Error appeared and program did exit | X |
| SR10 | REGISTER | Delete Categories File | Show error and skip adding categories to the sandwich | CC | FB | 05/03/2021 | PASSED - Error appeared and the program didn't ask to assign caregories to the sandwich | X |
| SR11 | SEARCH | Search by Ingredient | Search with 2 ingredients (001/002) - Expected 2 records shown | CC | BL | 09/03/2021 | PASSED with bug - 2 records shown | Bug found: message of "no more records found" not appearing |
| SR12 | SEARCH | Search by Ingredient | Search with 2 ingredients (001/002) - Expected 2 records shown | CC | BL | 09/03/2021 | PASSED - 2 records shown | SR11 Bug fixed |
| SR13 | SEARCH | Search by Categorie | Search with 2 categories (001/006) - Expected 2 records shown | CC | DL | 09/03/2021 | PASSED - 2 records shown | X |
| SR14 | SEARCH | Search by Price | Search between two prices (01/02) - Expected 3 records shown | CC | FB | 09/03/2021 | FAILED | Program doesn't allow to introduce the higher value of price to search |
| SR15 | SEARCH | Search by Price | Search between two prices (01/02) - Expected 3 records shown | CC | FB | 09/03/2021 | PASSED - 3 records shown | SR13 Bug fixed |
| SR16 | SEARCH | Search by Sandwich | Search a non existent sandwich - Expected error | CC | DL | 10/03/2021 | PASSED - No records shown, message of no records to show obtained | X |
| SR17 | SEARCH | Search by Sandwich | Search an existent sandwich | CC | BL | 10/03/2021 | PASSED - Record shown | X |
| SR18 | MAIN | Compile program through CMD | Fully compile | CC | FB | 10/03/2021 | FAILED - Errors compiling | X |
| SR19 | MAIN | Compile program through CMD | Fully compile | CC | FB | 10/03/2021 | PASSED - Fully compiled | X |
| SR20 | REGISTER | Creation of Sandwich (Test of Categories List) | Previous page when pressing F1 and next page when pressing F2 | CC | FB | 10/03/2021 | FAILED | When no mores pages to change, doens't show error message but accept 000 as empty categorie |
| SR21 | REGISTER | Creation of Sandwich (Test of Categories List) | Previous page when pressing F1 and next page when pressing F2 | CC | FB | 10/03/2021 | PASSED - When no mores pages to change, shown error message | SR19 Bug fixed |
| SR22 | SEARCH | Create Report | Creation of report file | CC | DL | 10/03/2021 | PASSED - Creation of report file | X |