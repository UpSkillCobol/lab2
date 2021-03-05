#Test Cases

| UC | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| CAM01 | List of unavailabilities (202102120000 - 202102162359) (202103181100 - 202103262359) (202102141200 - 202103191200) | Aggregated unavailabilities (202102120000 - 202103262359) | FB | CC & DL | 02/03/2021 | FAILED - (202102120000 - 202103191200) | X |
| SR01  | Creation of SandWich (Test of Ingredients List) | "001 - Atum / 002 Queijo" | CC | CC & DL & FB & BL | 05/03/2021 | PASSED ("001 - Atum / 002 Queijo") | X |
| RSO01 | (30/02/2022); (04/03/2021) | "Invalid date." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Invalid date.") | X |
| RSO02 | (05/03/2021); (06/03/2021); (07/03/2021) | "Less than 3 days in advance." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Less than 3 days in advance.") | X |
| RSO03 | SCHOOL ID / SANDWICH ID / QUANTITY -> 000 | "Invalid." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Invalid.") | X |
