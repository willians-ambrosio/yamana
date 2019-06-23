/*
*---------------------------------------------------------*
* Programa: upc-pe3130
* objetivo: N∆o permitir inclus∆o de batida para usu†rios
*           sem permiss∆o do grupo de seguranáa BAT       
* Autor   : Edson - Amgra 
* Data    : 09/12/2007
*---------------------------------------------------------*
*/
DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR NO-UNDO.
DEF INPUT PARAM p-wgh-object AS handle NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS widget-handle NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR NO-UNDO.
DEF INPUT PARAM p-row-table  AS rowid NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-btinc   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btmod   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bttop   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btmod-a AS WIDGET-HANDLE NO-UNDO.

DEF VAR wh-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/*pega o nome do programa sem o diretorio e as barras*/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA,"~\"), p-wgh-object:PRIVATE-DATA, "~\").
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(c-objeto,"~/"), c-objeto, "~/").
/*get-link-handle  retorna todos os handles de um determinado tipo de link*/
/* primeiro parametro Ç o nome da container, o segundo Ç o tipo e o terceiro
Ç a variavel de retorno dos tipo links)*/

/* para decobrir nome das viewers no programa
depois abra uma tela nova instacie a viewer suspeita para ter certeza*/

IF  p-ind-event = "initialize" THEN DO:
    FIND FIRST usuar_grp_usuar NO-LOCK
         WHERE usuar_grp_usuar.cod_usuario   = c-seg-usuario
           AND usuar_grp_usuar.cod_grp_usuar = "BAT" NO-ERROR.
    IF AVAIL  usuar_grp_usuar THEN RETURN "OK".
     /* MESSAGE 
    p-ind-event  
    p-ind-object 
    p-wgh-object 
    p-wgh-frame:NAME  
    p-cod-table  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

     ASSIGN wh-frame = p-wgh-frame:FIRST-CHILD
             wh-frame = wh-frame:FIRST-CHILD.

    DO  WHILE wh-frame <> ?:
        IF  wh-frame:TYPE <> "field-group" THEN DO:
            IF  wh-frame:NAME="bt-inc" THEN
                ASSIGN wh-btinc = wh-frame.

            IF  wh-frame:NAME="bt-mod" THEN DO:
                ASSIGN wh-btmod = wh-frame.
                LEAVE.
            END.
            ASSIGN wh-frame=wh-frame:NEXT-SIBLING.
        END.
        ELSE 
            ASSIGN wh-frame = wh-frame:FIRST-CHILD.
    END.

    IF  valid-handle(wh-btinc) THEN DO:
        CREATE BUTTON wh-bttop
        ASSIGN FRAME    = p-wgh-frame
              WIDTH     = wh-btinc:WIDTH
              HEIGHT    = wh-btinc:HEIGHT
              LABEL     = ""  
              ROW       = wh-btinc:ROW 
              COLUMN    = wh-btinc:column
              TOOLTIP   = "Vocà n∆o tem permiss∆o para inclus∆o" 
              NAME      = "bt-cptop" 
              SENSITIVE = no
              VISIBLE   = YES.

        wh-bttop:LOAD-IMAGE("image\im-add").

        ASSIGN wh-btinc:TAB-STOP = FALSE
               wh-btinc:VISIBLE = NO.
    END.

    IF  valid-handle(wh-btmod) THEN DO:
        CREATE BUTTON wh-btmod-a
        ASSIGN FRAME     = p-wgh-frame
               WIDTH     = wh-btmod:WIDTH
               HEIGHT    = wh-btmod:HEIGHT
               LABEL     = ""  
               ROW       = wh-btmod:ROW 
               COLUMN    = wh-btmod:column
               TOOLTIP   = "Vocà n∆o tem permiss∆o para alteraá∆o" 
               NAME      = "bt-btmod" 
               SENSITIVE = no
               VISIBLE   = YES.

        wh-btmod-a:LOAD-IMAGE("image\im-mod").

        ASSIGN wh-btmod:TAB-STOP = FALSE
               wh-btmod:VISIBLE = NO.
    END.
END.
