       78  MODULE-NAME     VALUE "REGISTO DE SANDUICHES".
       78  MAIN-MENU-OPTION1   VALUE "1 - REGISTAR SANDUICHES".
       78  MAIN-MENU-OPTION2   VALUE "2 - CONSULTAR SANDUICHES".
       78  MAIN-MENU-OPTION3   VALUE "3 - CRIAR RELATORIO".
       78  MAIN-MENU-OPTION4   VALUE "4 - SAIR".
       78  MAIN-MENU-CHOICE    VALUE "POR FAVOR ESCOLHA UMA OPCAO".
       78  MAIN-MENU-ERROR     VALUE "OPCAO INVALIDA. POR FAVOR ESCOLHA
      -    " UMA OPCAO VALIDA. PRESSIONE QUALQUER TECLA PARA CONTINUAR".
       78  LEAVE-MESSAGE       VALUE "A SAIR DO PROGRAMA!".
       78  LEAVE-THANKS        VALUE "OBRIGADO!".
       78  BACK-EXIT           VALUE "F3-MENU PRINCIPAL SR".
       78  F3                  VALUE "KEY-STATUS = 1003".
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
       78  ADD-CAT-MENU-TEXT1  VALUE "CATEGORIA:".
       78  ADD-ING-MENU-TEXT   VALUE "ADICIONAR INGREDIENTES".
       78  ADD-ING-MENU-TEXT1  VALUE "INGREDIENTE:".
       78  ING-INSTR           VALUE "ID VALIDO | NAO REPETIR INGREDIENT
      -    "ES | DEIXAR COMO '000' SE NAO QUISER COLOCAR INGREDIENTE".
       78  CAT-INSTR           VALUE "ID VALIDO | NAO REPETIR CATEGORIAS
      -    " | DEIXAR COMO '000' SE NAO QUISER COLOCAR CATEGORIA".
       78  ING-ERROR           VALUE "INGREDIENTE NAO EXISTENTE".
       78  CAT-ERROR           VALUE "CATEGORIA NAO EXISTENTE".
       78  F1                  VALUE "F1 - PAGINA ANTERIOR".
       78  F2                  VALUE "F2 - PROXIMA PAGINA".
       78  F1-F2               VALUE " F1 - PAGINA ANTERIOR | F2 - PROXI
      -    "MA PAGINA".
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
       78  CONFIRM-TEXT        VALUE "        SANDUICHE:".
       78  CONFIRM-TEXT1       VALUE "       ID EXTERNO:".
       78  CONFIRM-TEXT2       VALUE "  DESCRICAO CURTA:".
       78  CONFIRM-TEXT3       VALUE "  DESCRICAO LONGA:".
       78  CONFIRM-TEXT4       VALUE "       CATEGORIAS:".
       78  CONFIRM-TEXT5       VALUE "     INGREDIENTES:".
       78  WANT-TO-SAVE        VALUE "PRETENDE GUARDAR ESTE REGISTO? (S)
      -    "IM/(N)AO".
       78  RECORD-SAVED        VALUE "REGISTO GUARDADO COM SUCESSO!".
       78  RECORD-NOT-SAVED    VALUE "REGISTO NAO GUARDADO!".
