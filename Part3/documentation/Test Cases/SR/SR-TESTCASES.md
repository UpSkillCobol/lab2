#Test Cases

| UC | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| CAM01 | List of unavailabilities (202102120000 - 202102162359) (202103181100 - 202103262359) (202102141200 - 202103191200) | Aggregated unavailabilities (202102120000 - 202103262359) | FB | CC & DL | 02/03/2021 | FAILED - (202102120000 - 202103191200) | X |
| SR01  | Creation of SandWich (Test of Ingredients List) | "001 - Atum - 002 - Queijo" | CC | CC & DL & FB & BL | 05/03/2021 | PASSED ("001 - Atum / 002 Queijo") | X |
| SR02  | Creation of SandWich (Test of Categories List) | "001 | Saudavel | 003 | Picante"  | CC | CC & DL & FB & BL | 05/03/2021 | PASSED ("001 | Saudavel | 003 | Picante") | X |
| SR03  | Creation of SandWich (Test of Confirmation Screen) | Show all the details from the sandwich | CC | CC | 05/03/2021 | PASSED (All details shown successfully) | X |
| SR04  | Creation of SandWich (Test Saving Record) | Records saved on all 3 files | CC | CC | 05/03/2021 | PASSED (All records saved on all 3 files) | X |
| SR05  | Creation of SandWich (Test of Ingredients List) | Next page when pressing F2 | CC | CC | 05/03/2021 | PASSED (Page changed when F2 was pressed) | X |
| SR06  | Creation of SandWich (Test of Ingredients List) | Previous page when pressing F1 | CC | CC | 05/03/2021 | PASSED (Page changed when F1 was pressed) | X |
| SR07  | Creation of SandWich (Test of Categories List) | Next page when pressing F2 | CC | CC | 05/03/2021 | PASSED (Page changed when F2 was presse) | X |
| SR08  | Creation of SandWich (Test of Categories List) | Previous page when pressing F1 | CC | CC | 05/03/2021 | PASSED (Page changed when F1 was pressed) | X |
| SR09  | Delete Ingredients File | Show error and exit program | CC | CC | 05/03/2021 | PASSED (Error appeared and program did exit) | X |
| SR10  | Delete Categories File | Show error and skip adding categories to the sandwich | CC | CC | 05/03/2021 | PASSED (Error appeared and the program didn't ask to assign caregories to the sandwich) | X |
| SR11  | Search by Ingredient | Search with 2 ingredients (001/002) Expected 2 records shown | CC | CC | 09/03/2021 | PASSED w/bug (2 records shown) | Bug found, message of "no more records found" not appearing |
| SR12  | Search by Ingredient | Search with 2 ingredients (001/002) Expected 2 records shown | CC | CC | 09/03/2021 | PASSED (2 records shown) | SR11 bug fixed |
| SR13  | Search by Categorie | Search with 2 categories (001/006) Expected 2 records shown | CC | CC | 09/03/2021 | PASSED (2 records shown) | X |
| SR13  | Search by Price | Search between two prices (01/02) Expected 3 records shown | CC | CC | 09/03/2021 | FAILED | Program doesnt allow to introduce the higher value of price to search |
| SR14  | Search by Price | Search between two prices (01/02) Expected 3 records shown | CC | CC | 09/03/2021 | PASSED (3 records shown) | SR13 Bug fixed |
| SR15  | Search by Sandwich | Search a non existent sandwich, expected error | CC | CC | 09/03/2021 | PASSED (no records shown, message of no records to show obtained) | X |
| SR16  | Search by Sandwich | Search an existent sandwich | CC | CC | 09/03/2021 | PASSED (record shown) | X |
| SR17  | Compile program through CMD | Fully compile | CC | FB | 10/03/2021 | FAILED (Errors compiling) | X |
| SR18  | Compile program through CMD | Fully compile | CC | FB | 10/03/2021 | PASSED(Fully Compiled) | X |