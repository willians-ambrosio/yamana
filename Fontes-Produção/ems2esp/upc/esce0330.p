/* ======================================================
    Autor   : Felipe Silvestre - Kraft Consulting
    Data    : 01/07/2010
    Objetivo: Criar toggle box Raw Material no CE0330

   ====================================================== */
   
def input param p-ind-event        as char          no-undo.
def input param p-ind-object       as char          no-undo.
def input param p-wgh-object       as handle        no-undo.
def input param p-wgh-frame        as widget-handle no-undo.
def input param p-cod-table        as char          no-undo.
def input param p-row-table        as rowid         no-undo.
def var c-objeto as char no-undo.
def var c-objeto2 as char no-undo.
DEFINE NEW GLOBAL SHARED Variable wh-ce0330-fpage1                  as handle         no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE whCodRota     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-ce0330-toggle AS WIDGET-HANDLE NO-UNDO.

MESSAGE 1
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*
message "EVENTO" p-ind-event skip
        "OBJETO" p-ind-object skip
        "NOME OBJ" c-objeto skip
        "objeto 2 " c-objeto2 SKIP
        "p-wgh-object " p-wgh-object:FILE-NAME SKIP
        "FRAME" p-wgh-frame:NAME skip
        "TABELA" p-cod-table skip
        "ROWID" string(p-row-table) SKIP 
        VALID-HANDLE(wh-ce0330-fpage1) view-as alert-box. */
        

IF NOT VALID-HANDLE(wh-ce0330-fpage1) THEN 
DO:                                        
    run pi-vld-widget (input p-wgh-frame,
                       input "fpage1",
                       output wh-ce0330-fpage1).
  
END.



IF VALID-HANDLE(wh-ce0330-fpage1) THEN
DO:

    IF NOT VALID-HANDLE(wh-ce0330-toggle)  THEN
    DO:
        CREATE TOGGLE-BOX wh-ce0330-toggle
        ASSIGN FRAME     = wh-ce0330-fpage1
               WIDTH     = 22
               HEIGHT    = 0.88
               ROW       = 7.3
               COL       = 68.7
               VISIBLE   = YES
               SENSITIVE = NO
               LABEL     = "(XTIVITY): sss"
               NAME      = "raw-material"
               HELP      = "".
    END.
END.

FIND FIRST item-uni-estab
         WHERE ROWID(item-uni-estab) = p-row-table NO-ERROR.


/* =========================== EVENTOS =========================== */

/* =========== TRATAR EVENTO DISPLAY DO CONTAINER ========= */

IF  p-ind-event = 'AFTER-Display' AND p-ind-object = "CONTAINER"
        AND valid-handle(wh-ce0330-toggle) THEN
    DO:
        FIND FIRST item-uni-estab
          WHERE ROWID(item-uni-estab) = p-row-table NO-ERROR.

        IF AVAIL ITEM-UNI-ESTAB THEN
            FIND FIRST ext-ITEM-UNI-ESTAB NO-LOCK
                   WHERE ext-ITEM-UNI-ESTAB.IT-CODIGO   = Item-uni-estab.it-codigo
                    AND  ext-ITEM-UNI-ESTAB.cod-estabel = Item-uni-estab.cod-estabel NO-ERROR.
       
        IF AVAIL ext-item-uni-estab THEN 
        DO:
            ASSIGN wh-ce0330-toggle:CHECKED     = ext-item-uni-estab.l-considera-xt.                    
        END.
        ELSE ASSIGN wh-ce0330-toggle:CHECKED     = NO.                    
            
       
    END.


    /* =========== TRATAR EVENTO ENABLE DO CONTAINER ========= */

    IF  p-ind-event = "after-update" AND p-ind-object = "CONTAINER"
        AND valid-handle(wh-ce0330-toggle) THEN
    DO:        
        ASSIGN wh-ce0330-toggle:SENSITIVE = YES.
    END.
        
    /* =========== TRATAR EVENTO ASSIGN DO CONTAINER ========= */

    IF  p-ind-event = "after-ASSIGN" 
        AND p-ind-object = "CONTAINER"
        AND valid-handle(wh-ce0330-toggle)
        AND P-ROW-TABLE                                <> ? THEN
    DO:
        FIND FIRST item-uni-estab
          WHERE ROWID(item-uni-estab) = p-row-table NO-ERROR.

        IF AVAIL item-uni-estab THEN
        DO:            
           FIND FIRST ext-ITEM-UNI-ESTAB
                   WHERE ext-ITEM-UNI-ESTAB.IT-CODIGO   = Item-uni-estab.it-codigo
                    AND  ext-ITEM-UNI-ESTAB.cod-estabel = Item-uni-estab.cod-estabel NO-ERROR.
            IF NOT AVAIL ext-item-uni-estab THEN
            DO:
               CREATE ext-item-uni-estab.
               ASSIGN ext-item-uni-estab.it-codigo     = item-uni-estab.it-codigo
                      ext-item-uni-estab.cod-estabel   = item-uni-estab.cod-estabel.
                                           
            END.
            ASSIGN ext-item-uni-estab.l-considera-xt = wh-ce0330-toggle:CHECKED. 
        END.
    END.

    /* =========== TRATAR EVENTO DISABLE DO CONTAINER ========= */

    IF  p-ind-event = "AFTER-DISABLE" AND p-ind-object = "CONTAINER" 
        AND VALID-HANDLE(wh-ce0330-toggle) THEN
    DO:
        ASSIGN wh-ce0330-toggle:SENSITIVE = NO.
    END.



procedure pi-vld-widget:

    Def Input  Param f-pagexx      As Handle.
    def input  param p-nome-objeto as char.
    def output param p-wh-objeto   as widget-handle.

    def var v-wh-group as widget-handle.
    def var v-wh-child as widget-handle.
    def var h_frame as widget-handle.

    assign v-wh-group = f-pagexx:first-child.

    bloco-encontra-objeto:

    do while v-wh-group <> ?:
       assign v-wh-child = v-wh-group:first-child.
       do while v-wh-child <> ?:
           if  v-wh-child:name = p-nome-objeto
           then do:
               assign p-wh-objeto = v-wh-child.
               leave bloco-encontra-objeto.
           end.
           assign v-wh-child = v-wh-child:next-sibling.
       end.
       assign v-wh-group = v-wh-group:next-sibling.
    end.

end procedure.





