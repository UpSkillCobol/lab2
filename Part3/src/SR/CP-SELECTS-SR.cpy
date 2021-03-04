           SELECT SANDWICHES ASSIGN TO "FX-SR"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS SR-IID
           FILE STATUS IS FILE-STATUS.

           SELECT CATEGORIES ASSIGN TO "FXCATEGORIES"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CATEGORY-ID
           FILE STATUS IS FILE-STATUS.

           SELECT INGREDIENTS ASSIGN TO "FXINGREDS"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INGREDS-ID
           FILE STATUS IS FILE-STATUS.

           SELECT SR-ING ASSIGN TO "FX-SR-ING"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS SR-SAND-ING-ID
           FILE STATUS IS FILE-STATUS.

           SELECT SR-CAT ASSIGN TO "FX-SR-CAT"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC
           RECORD KEY IS SR-SAND-CAT-ID
           FILE STATUS IS FILE-STATUS.

           SELECT KEYS ASSIGN TO "KEYS-SR"
           ORGANISATION IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
