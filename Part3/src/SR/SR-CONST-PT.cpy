       01  WS-REG                          PIC X(001).
           88 REG-YES                      VALUE "S","s".
           88 REG-NO                       VALUE "N","n".
           88 REG-OPTION-VLD               VALUE "N","n","s","S".
       78  MODULE-NAME     VALUE "REGISTO DE SANDUICHES".
       78  MAIN-MENU-OPTION1   VALUE "1 - REGISTAR SANDUICHES".
       78  MAIN-MENU-OPTION2   VALUE "2 - PESQUISAR SANDUICHES".
       78  MAIN-MENU-OPTION4   VALUE "3 - SAIR".
       78  MAIN-MENU-CHOICE    VALUE "POR FAVOR ESCOLHA UMA OPCAO".
       78  MAIN-MENU-ERROR     VALUE "OPCAO INVALIDA. POR FAVOR ESCOLHA
      -    " UMA OPCAO VALIDA. PRESSIONE QUALQUER TECLA PARA CONTINUAR".
       78  LEAVE-MESSAGE       VALUE "A SAIR DO PROGRAMA!".
       78  LEAVE-THANKS        VALUE "OBRIGADO!".
       78  BACK-EXIT           VALUE "F3-MENU PRINCIPAL SR".
       78  F3                  VALUE "1003".
       78  F2                  VALUE "1002".
       78  F1                  VALUE "1001".
       78  ADD-MENU-TEXT       VALUE "REGISTAR NOVA SANDUICHE".
       78  ADD-MENU-TEXT1      VALUE "       ID EXTERNO:".
       78  ADD-MENU-TEXT2      VALUE "  DESCRICAO CURTA:".
       78  ADD-MENU-TEXT3      VALUE "  DESCRICAO LONGA:".
       78  ERROR-EID           VALUE "ID EXTERNO JA EXISTE".
       78  EID-INSTR           VALUE "PODE CONTER NUMEROS E LETRAS | 5 C
      -    "ARACTERES NO MAXIMO | TEM QUE SER UNICO".
       78  S-DESCR-INSTR       VALUE "APENAS CARACTERES ALFABETICOS | 30
      -    " CARACTERES NO MAXIMO".
       78  L-DESCR-INSTR       VALUE "APENAS CARACTERES ALFABETICOS | 15
      -    "0 CARACTERES NO MAXIMO".
       78  LIST-FRAME1         VALUE "ID".
       78  LIST-FRAME2         VALUE "NOME".
       78  ADD-CAT-MENU-TEXT   VALUE "ADICIONAR CATEGORIAS".
       78  ADD-CAT-MENU-TEXT1  VALUE "        CATEGORIA:".
       78  ADD-ING-MENU-TEXT   VALUE "ADICIONAR INGREDIENTES".
       78  ADD-ING-MENU-TEXT1  VALUE "      INGREDIENTE:".
       78  ING-INSTR           VALUE "ID VALIDO | NAO REPETIR INGREDIENT
      -    "ES | DEIXAR COMO '000' SE NAO QUISER COLOCAR INGREDIENTE".
       78  CAT-INSTR           VALUE "ID VALIDO | NAO REPETIR CATEGORIAS
      -    " | DEIXAR COMO '000' SE NAO QUISER COLOCAR CATEGORIA".
       78  ING-ERROR           VALUE "INGREDIENTE NAO EXISTENTE".
       78  CAT-ERROR           VALUE "CATEGORIA NAO EXISTENTE".
       78  ING-ZERO            VALUE "E NECESSARIO PELO MENOS UM INGREDI
      -    "ENTE".
       78  FILE-NOT-EXISTENT   VALUE "35".
       78  ING-DUPLICATE-ERROR VALUE "ESTE INGREDIENTE JA FOI ATRIBUIDO
      -    " A ESTA SANDWICH".
       78  CAT-DUPLICATE-ERROR VALUE "ESTA CATEGORIA JA FOI ATRIBUIDA A
      -    " ESTA SANDWICH".
       78  CATEGORIES-FILLED   VALUE "CATEGORIAS PREENCHIDAS".
       78  NO-CATEGORIES       VALUE "NAO EXISTEM CATEGORIAS REGISTADAS
      -    " | IMPOSSIVEL ATRIBUIR CATEGORIAS".
       78  NO-INGREDIENTS      VALUE "NAO EXISTEM INGREDIENTES REGISTADO
      -    "S | O PROGRAMA IRA VOLTAR AO MENU PRINCIPAL".
       78  CONFIRM-TEXT        VALUE "         SANDUICHE".
       78  CONFIRM-TEXT1       VALUE "       ID EXTERNO:".
       78  CONFIRM-TEXT2       VALUE "  DESCRICAO CURTA:".
       78  CONFIRM-TEXT3       VALUE "  DESCRICAO LONGA:".
       78  CONFIRM-TEXT4       VALUE "       CATEGORIAS:".
       78  CONFIRM-TEXT5       VALUE "     INGREDIENTES:".
       78  CONFIRM-TEXT6       VALUE "PRECO:".
       78  CONFIRM-TEXT7       VALUE "EUROS".
       78  WANT-TO-SAVE        VALUE "PRETENDE GUARDAR ESTE REGISTO? (S)
      -    "IM/(N)AO".
       78  RECORD-SAVED        VALUE "REGISTO GUARDADO COM SUCESSO!".
       78  RECORD-NOT-SAVED    VALUE "REGISTO NAO GUARDADO!".
       78  NO-SANDWICHES       VALUE "THERE ARE NO SANDWICHES REGISTERED
      -    "! PRESS ANY KEY TO EXIT THE PROGRAM!".
       78  PREVIOUS-PAGE       VALUE " F1 - PAG. ANTERIOR".
       78  NEXT-PAGE           VALUE "  F2 - PROXIMA PAG.".
       78  LAST-PAGE           VALUE "      ULTIMA PAGINA".
       78  MAIN-SEARCH-OPTION1 VALUE "1 - PESQUISAR POR INGREDIENTES".
       78  MAIN-SEARCH-OPTION2 VALUE "2 - PESQUISAR POR CATEGORIAS".
       78  MAIN-SEARCH-OPTION3 VALUE "3 - PESQUISAR POR SANDUICHE".
       78  MAIN-SEARCH-OPTION4 VALUE "4 - PESQUISAR ENTRE DOIS PRECOS".
       78  MAIN-SEARCH-OPTION5 VALUE "5 - CRIAR UM RELATORIO".
       78  MAIN-SEARCH-OPTION6 VALUE "6 - VOLTAR AO MENU PRINCIPAL".
       78  MAIN-SEARCH-CHOICE  VALUE "O QUE PRETENDE FAZER:".
       78  ALPHA-ERROR         VALUE "PRIMEIRO ESPACO DEVE SER ALFABETIC
      -    "O".
       78  NO-MATCH            VALUE "NAO FORAM ENCONTRADOS MAIS RESULTA
      -    "DOS".
       78  SRCH-ING-MENU-TEXT  VALUE "PESQUISA POR INGREDIENTE".
       78  SEARCH-NAME         VALUE "PESQUISA DE SANDUICHES".
       78  WRONG-ING           VALUE "INGREDIENTE NAO EXISTENTE".
       78  SRCH-CAT-MENU-TEXT  VALUE "PESQUISA POR CATEGORIA".
       78  SRCH-ING-INSTR      VALUE "ID VALIDO | NAO REPETIR INGREDIENT
      -    "ES | DEIXAR COMO '000' PARA PROSSEGIR A PESQUISA".
       78  SRCH-CAT-INSTR      VALUE "ID VALIDO | NAO REPETIR CATEGORIAS
      -    " | DEIXAR COMO '000' PARA PROSSEGIR A PESQUISA".
       78  SRCH-SR-MENU-TEXT   VALUE "PESQUISA POR SANDUICHE".
       78  SRCH-SR-MENU-TEXT1  VALUE "         SANDUICHE:".
       78  SRCH-PRC-MENU-TEXT1 VALUE "    PRECO INFERIOR:".
       78  SRCH-PRC-MENU-TEXT2 VALUE "    PRECO SUPERIOR:".
       78  PRICE-INSTR         VALUE "VALORES DEVEM ESTAR DENTRO DE UM I
      -    "NTERVALO | SE ZERO NAO IRA PESQUISAR".
       78  PRICE-ERROR         VALUE "VALORES NAO SE ENCONTRAM DENTRO DE
      -    "UM INTERVALO VALIDO".
       78  NO-RECORDS          VALUE "NAO FORAM ENCONTRADOS RESULTADOS |
      -    " PRESSIONE UMA TECLA PARA CONTINUAR".
       78  RPT-DONE            VALUE "RELATORIO FOI CRIADO COM SUCESSO".
