/* =========================================================================== 
   PROGRAMA    : cd0134-U01.p
   DATA        : 24/05/2010
   DESENVOLVIDO: Marcio Sacramoni - Kraft
   VERSAO      : 000
   OBJETIVO    : UPC no programa Itens x Materiais. 
   =========================================================================== */

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i  cd0134-u01.P 2.06.00.000}
{tools/fc-handle-obj.i}
{utp\ut-glob.i}

/* ==========> Parametros obrigatorios para UPC. <========== */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.

/* ==========> Definicao das Variavel Globais.   <========== */
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-res-for-comp     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-res-for-comp-ant         AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-deposito-pad     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-deposito-pad-ant         AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-cod-localiz      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cod-localiz-ant          AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-res-int-comp     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-res-int-comp-ant         AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-cod-unid-negoc   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cod-unid-negoc-ant       AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-prioridade-aprov AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-prioridade-aprov-ant     AS INTEGER       NO-UNDO.


DEFINE VARIABLE c-handle-obj   AS CHAR NO-UNDO.
DEFINE VARIABLE c-return-value AS CHAR NO-UNDO.

DEFINE BUFFER b-familia     FOR familia.
DEFINE BUFFER b-familia-mat FOR familia-mat.

/*
MESSAGE "p-ind-event..:" p-ind-event                  SKIP 
        "p-ind-object.:" p-ind-object                 SKIP 
        "p-cod-table..:" STRING(p-cod-table)          SKIP 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP 
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP 
        "p-row-table..:" STRING(p-row-table) SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/* ==========> Mapeador de  Enderecos de Objeos  <========== */
IF NOT VALID-HANDLE(wh-cd0134-res-for-comp)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("res-for-comp",p-wgh-frame).
   ASSIGN wh-cd0134-res-for-comp = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-res-for-comp-ant     = 0.
END.

IF NOT VALID-HANDLE(wh-cd0134-deposito-pad)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("deposito-pad",p-wgh-frame).
   ASSIGN wh-cd0134-deposito-pad = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-deposito-pad-ant     = ''.
END.

IF NOT VALID-HANDLE(wh-cd0134-cod-localiz)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("cod-localiz",p-wgh-frame).
   ASSIGN wh-cd0134-cod-localiz = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-cod-localiz-ant     = ''.
END.

IF NOT VALID-HANDLE(wh-cd0134-res-int-comp)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("res-int-comp",p-wgh-frame).
   ASSIGN wh-cd0134-res-int-comp = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-res-int-comp-ant     = 0.
END.

IF NOT VALID-HANDLE(wh-cd0134-cod-unid-negoc)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("c-cod-unid-negoc",p-wgh-frame).
   ASSIGN wh-cd0134-cod-unid-negoc = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-cod-unid-negoc-ant     = ''.
END.

IF NOT VALID-HANDLE(wh-cd0134-prioridade-aprov)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("cb-prioridade-aprova",p-wgh-frame).
   ASSIGN wh-cd0134-prioridade-aprov = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-prioridade-aprov-ant     = 0.

END.

IF p-ind-event  = "BEFORE-ASSIGN" AND
   p-ind-object = "CONTAINER"     THEN
DO:
    FIND b-familia NO-LOCK WHERE
         ROWID(b-familia) = p-row-table NO-ERROR.
    IF AVAIL b-familia THEN DO:
       ASSIGN i-res-for-comp-ant   = b-familia.res-for-comp
              i-deposito-pad-ant   = b-familia.deposito-pad
              i-cod-localiz-ant    = b-familia.cod-localiz
              i-res-int-comp-ant   = b-familia.res-int-comp.

       FIND b-familia-mat NO-LOCK 
            WHERE b-familia-mat.fm-codigo = b-familia.fm-codigo
            NO-ERROR.
       IF AVAIL b-familia-mat THEN
           ASSIGN i-cod-unid-negoc-ant   = b-familia-mat.cod-unid-negoc
                  i-prioridade-aprov-ant = b-familia-mat.prioridade-aprov.
    END.
END. /* p-ind-event  = "BEFORE-ASSIGN" */

IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    THEN
DO:
    FIND b-familia NO-LOCK WHERE
         ROWID(b-familia) = p-row-table NO-ERROR.
    IF AVAIL b-familia THEN DO:

        FIND FIRST b-familia-mat NO-LOCK OF b-familia NO-ERROR.

        IF b-familia.res-for-comp <> i-res-for-comp-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = ""
                   ext-audit-item-fam.campo     = "res-for-comp"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                   ext-audit-item-fam.programa  = "cd0134"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-res-for-comp-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-familia.res-for-comp).

        END.

        IF b-familia.deposito-pad <> i-deposito-pad-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = ""
                   ext-audit-item-fam.campo     = "deposito-pad"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                   ext-audit-item-fam.programa  = "cd0134"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-deposito-pad-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-familia.deposito-pad).

        END.

        IF b-familia.cod-localiz <> i-cod-localiz-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = ""
                   ext-audit-item-fam.campo     = "cod-localiz"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                   ext-audit-item-fam.programa  = "cd0134"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-cod-localiz-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-familia.cod-localiz).

        END.

        IF b-familia.res-int-comp <> i-res-int-comp-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = ""
                   ext-audit-item-fam.campo     = "res-int-comp"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                   ext-audit-item-fam.programa  = "cd0134"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-res-int-comp-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-familia.res-int-comp).

        END.

        IF AVAIL b-familia-mat THEN DO:

            IF b-familia-mat.cod-unid-negoc <> i-cod-unid-negoc-ant THEN DO:
    
                CREATE ext-audit-item-fam.
                ASSIGN ext-audit-item-fam.it-codigo = ""
                       ext-audit-item-fam.campo     = "cod-unid-negoc"
                       ext-audit-item-fam.data      = TODAY
                       ext-audit-item-fam.hora      = TIME
                       ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                       ext-audit-item-fam.programa  = "cd0134"
                       ext-audit-item-fam.usuario   = c-seg-usuario
                       ext-audit-item-fam.valor-ant = STRING(i-cod-unid-negoc-ant)
                       ext-audit-item-fam.valor-atu = STRING(b-familia-mat.cod-unid-negoc).
    
            END.

            IF b-familia-mat.prioridade-aprov <> i-prioridade-aprov-ant THEN DO:
    
                CREATE ext-audit-item-fam.
                ASSIGN ext-audit-item-fam.it-codigo = ""
                       ext-audit-item-fam.campo     = "prioridade-aprov"
                       ext-audit-item-fam.data      = TODAY
                       ext-audit-item-fam.hora      = TIME
                       ext-audit-item-fam.fm-codigo = b-familia.fm-codigo
                       ext-audit-item-fam.programa  = "cd0134"
                       ext-audit-item-fam.usuario   = c-seg-usuario.


                CASE i-prioridade-aprov-ant:
                     WHEN 0 THEN ASSIGN ext-audit-item-fam.valor-ant = "".
                     WHEN 1 THEN ASSIGN ext-audit-item-fam.valor-ant = "Muito Alta".
                     WHEN 2 THEN ASSIGN ext-audit-item-fam.valor-ant = "Alta".
                     WHEN 3 THEN ASSIGN ext-audit-item-fam.valor-ant = "M‚dia".
                     WHEN 4 THEN ASSIGN ext-audit-item-fam.valor-ant = "Baixa".
                END CASE.
                
                CASE b-familia-mat.prioridade-aprov:
                     WHEN 0 THEN ASSIGN ext-audit-item-fam.valor-atu = "".
                     WHEN 1 THEN ASSIGN ext-audit-item-fam.valor-atu = "Muito Alta".
                     WHEN 2 THEN ASSIGN ext-audit-item-fam.valor-atu = "Alta".
                     WHEN 3 THEN ASSIGN ext-audit-item-fam.valor-atu = "M‚dia".
                     WHEN 4 THEN ASSIGN ext-audit-item-fam.valor-atu = "Baixa".
                END CASE.
    
            END.

        END.

    END. /* AVAIL b-familia */
END. /* p-ind-event  = "AFTER-ASSIGN"  */
