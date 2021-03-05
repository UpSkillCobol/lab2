# Test Cases
## Registration of Sandwich Orders

| UC | INPUT | OUTPUT/ EXPECTED RESULT | DEVELOPER | TESTER | TEST DATE | TEST OUTPUT (PASSED OR FAILED) | COMMENTS
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| RSO01 | (30/02/2022); (04/03/2021) | "Invalid date." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Invalid date.") | X |
| RSO02 | (05/03/2021); (06/03/2021); (07/03/2021) | "Less than 3 days in advance." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Less than 3 days in advance.") | X |
| RSO03 | SCHOOL ID / SANDWICH ID / QUANTITY -> (000) | "Invalid." | DL | BL & CC & DL & FB | 05/03/2021 | PASSED ("Invalid.") | X |
| RSO04 | (08:00); (18:00) | "Invalid hour." | DL | DL | 05/03/2021 | PASSED ("Invalid hour.") | X |