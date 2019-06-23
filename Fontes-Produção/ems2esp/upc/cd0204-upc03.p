DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table    AS ROWID           NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR wh-cd0204-img-pdf    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-text-img-pdf      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-bt-pdf            AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE c-objeto AS CHARACTER   NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA,"~/"), p-wgh-object:PRIVATE-DATA,"~/").

/* MESSAGE "evento:     " p-ind-event              SKIP */
/*         "objeto:     " p-ind-object             SKIP */
/*         "tabela:     " p-cod-table              SKIP */
/*         "frame:      " p-wgh-frame:NAME         SKIP */
/*         "wgh objeto: " p-wgh-object:FILE-NAME   SKIP */
/*         "Rowid:      " string(p-row-table)      SKIP */
/*         "C-Objeto:   " c-objeto                      */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */

IF c-objeto MATCHES "*v36in172.w*" THEN
DO:
    FIND FIRST ITEM WHERE ROWID(item) = p-row-table NO-ERROR.
    IF NOT VALID-HANDLE(wh-cd0204-img-pdf) THEN
    DO:    
        CREATE FILL-IN wh-cd0204-img-pdf
        ASSIGN FRAME        =   p-wgh-frame
            WIDTH               =   30
            HEIGHT              =   0.88
            ROW                 =   5.25
            COL                 =   52.8
            VISIBLE             =   YES
            SENSITIVE           =   NO
/*             LABEL               =   "Uf Origem" */
            FORMAT              =   "x(200)"
            NAME                =   "image-pdf"
            HELP                =   ""            
            TRIGGERS:
                ON F5,mouse-select-dblclick PERSISTENT RUN upc/cd0204-upc04.p.
            END.
            wh-cd0204-img-pdf:LOAD-MOUSE-POINTER("image\lupa.cur").
    END.


    IF NOT VALID-HANDLE(wh-text-img-pdf) THEN
        CREATE TEXT wh-text-img-pdf
        ASSIGN FRAME        =   p-wgh-frame
        WIDTH-CHARS         =   7
/*         HEIGHT              =   1 */
        ROW                 =   5.30
        COL                 =   45.5
        VISIBLE             =   YES
        FORMAT              =   "x(7)".
        wh-text-img-pdf:SCREEN-VALUE  =   "Arq Img:".


    IF NOT VALID-HANDLE(wh-bt-pdf) THEN
        CREATE BUTTON wh-bt-pdf
           ASSIGN ROW  = 7.4
           COLUMN    = 60
           WIDTH     = 8
           HEIGHT    = 0.8
           ROW       = 6.25
           FRAME     = p-wgh-frame
           SENSITIVE = NO
           VISIBLE   = yes
/*            IMAGE     = "image\toolbar\im-show.bmp" */
           LABEL     = "Visualizar"
           TOOLTIP   = "PDF".

    ON CHOOSE OF wh-bt-pdf PERSISTENT RUN upc\cd0204-upc05.p(INPUT p-row-table).



    IF p-ind-event = 'Display'  AND p-ind-object = 'VIEWER'
                                AND VALID-HANDLE(wh-cd0204-img-pdf) THEN
    DO:
        ASSIGN wh-cd0204-img-pdf:SCREEN-VALUE       =   ""
               wh-cd0204-img-pdf:SENSITIVE   =   NO
               wh-bt-pdf:SENSITIVE = YES.
        IF AVAIL item THEN
            FIND FIRST mgesp.ext-item-arq WHERE mgesp.ext-item-arq.it-codigo  =   item.it-codigo NO-ERROR.

        IF AVAIL mgesp.ext-item-arq THEN
            ASSIGN wh-cd0204-img-pdf:SCREEN-VALUE     =       mgesp.ext-item-arq.image-pdf.
    END.

    IF p-ind-event = 'ENABLE' AND p-ind-object  =   'VIEWER'
                              AND VALID-HANDLE(wh-cd0204-img-pdf) THEN
        ASSIGN wh-cd0204-img-pdf:SENSITIVE = YES
               wh-bt-pdf:SENSITIVE = NO.

    IF p-ind-event = 'ASSIGN' AND p-ind-object  =   'VIEWER'
                              AND VALID-HANDLE(wh-cd0204-img-pdf) THEN
    DO:
        IF AVAIL item THEN
        DO:
            FIND FIRST mgesp.ext-item-arq WHERE mgesp.ext-item-arq.it-codigo  =   item.it-codigo NO-ERROR.
            IF NOT AVAIL mgesp.ext-item-arq THEN
            DO:               
                CREATE mgesp.ext-item-arq.
                ASSIGN mgesp.ext-item-arq.it-codigo    =  item.it-codigo.
            END.
                ASSIGN mgesp.ext-item-arq.image-pdf        =  wh-cd0204-img-pdf:SCREEN-VALUE.                

        END.

    END.

    IF p-ind-event = 'DISABLE' AND p-ind-object = 'VIEWER'
                               AND VALID-HANDLE(wh-cd0204-img-pdf) THEN
        ASSIGN wh-cd0204-img-pdf:SENSITIVE = NO
               wh-bt-pdf:SENSITIVE = YES.

END.

RETURN "OK".

