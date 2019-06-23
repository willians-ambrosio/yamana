/*******************************************************************************
** Programa: cn0201c-upc
** Autor...: Log¡stica (log339640)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa cn0201c.w
** Objetivo: 
*******************************************************************************/

DEF INPUT PARAM p-ind-event  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS   ROWID         NO-UNDO.



DEFINE VARIABLE c-beneficio AS CHARACTER   NO-UNDO.

{cdp/cdcfgmat.i}

/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardrÆo para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri‡Æo",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
 
DEFINE VARIABLE wh-objeto   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wgh-child   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wh-grupo    AS   WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE wh-child    AS   WIDGET-HANDLE NO-UNDO.
define variable wh-pesquisa as   widget-handle no-undo.
DEFINE VARIABLE c-char      AS   CHARACTER     NO-UNDO.
DEFINE VARIABLE h-esapi001  AS   HANDLE        NO-UNDO.

def var cod-empresa like param-global.empresa-prin no-undo.

DEFINE NEW GLOBAL SHARED VAR wgh-bt-ok-cn0201c            AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-bt-okUPC-cn0201c         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-val-total-cn0201c        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-nr-contrato-cn0201c      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-it-codigo-cn0201c        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-num-seq-item-cn0201c     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-lbl-ordem-invest-cn0201c AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-i-ordem-invest-cn0201c   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-log-libera-cn0201c       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-cn0201c-upc                AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR l-aditivo                    AS LOGICAL       NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR l-add                        AS LOGICAL       NO-UNDO.

DEF SHARED VAR c-seg-usuario AS CHAR FORMAT "x(16)" NO-UNDO.
DEF SHARED VAR i-ep-codigo-usuario AS CHAR NO-UNDO  .
DEFINE VARIABLE c-hora AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-data AS CHARACTER FORMAT "x(10)"  NO-UNDO.

DEF TEMP-TABLE tt-beneficio
    FIELD cod-beneficio AS INT
    FIELD rid-ben       AS ROWID
    FIELD TP-TAX        AS CHAR
    FIELD PRIOR         AS INT.






DEFINE NEW GLOBAL SHARED VAR de-preco-orig-cn0201c    like item-contrat.val-total NO-UNDO.

ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                  p-wgh-object:FILE-NAME,'/').



                                    



IF  p-ind-event = "before-initialize":U and c-char = "cnp\cn0201c.w":U THEN DO:
    RUN upc\cn0201c-upc.p PERSISTENT SET h-cn0201c-upc (INPUT "", INPUT "", INPUT p-wgh-object, INPUT p-wgh-frame, INPUT "", INPUT p-row-table).
END.

IF  p-ind-event = "before-initialize":U and c-char = "cn0201c-v04.w":U THEN DO:
    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                when "fill-in" then do:
                    IF wh-child:NAME = "val-total":U THEN 
                        ASSIGN wgh-val-total-cn0201c = wh-child.
                end.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END.
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    if valid-handle(wgh-val-total-cn0201c) then
        assign de-preco-orig-cn0201c = wgh-val-total-cn0201c:input-value.
END.

IF  p-ind-event = "before-initialize":U and c-char = "cn0201c-v01.w":U THEN DO:
    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                when "fill-in" then do:
                    IF wh-child:NAME = "i-nr-contrato":U THEN 
                        ASSIGN wgh-nr-contrato-cn0201c  = wh-child.
                    IF wh-child:NAME = "it-codigo":U THEN 
                        ASSIGN wgh-it-codigo-cn0201c    = wh-child.
                    IF wh-child:NAME = "num-seq-item":U THEN 
                        ASSIGN wgh-num-seq-item-cn0201c = wh-child.
                end.
                WHEN "toggle-box" THEN DO:
                    IF  wh-child:NAME = "log-libera":U THEN 
                        ASSIGN wgh-log-libera-cn0201c = wh-child.
                END.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    if valid-handle(wgh-nr-contrato-cn0201c) then do:
        create text wgh-lbl-ordem-invest-cn0201c
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(15)"
               WIDTH        = 14
               SCREEN-VALUE = "Num Ordem Inv:"
               ROW          = 13.40
               COL          = 59
               VISIBLE      = YES
               font         = 1.

        CREATE fill-in wgh-i-ordem-invest-cn0201c
        ASSIGN FRAME             = p-wgh-frame
               data-type         = "integer"
               FORMAT            = ">>>,>>>,>>9"
               NAME              = "num-ord-inv"
               side-label-handle = wgh-lbl-ordem-invest-cn0201c:handle
               WIDTH             = 12
               HEIGHT            = 0.88
               ROW               = 13.38
               COL               = 70
               VISIBLE           = YES
               SENSITIVE         = yes
               FONT              = 1.

        wgh-i-ordem-invest-cn0201c:LOAD-MOUSE-POINTER("image\lupa.cur").
        on "F5"                     of wgh-i-ordem-invest-cn0201c persistent run upc/OrdemInvZoom.p (input wgh-i-ordem-invest-cn0201c,
                                                                                                     input p-wgh-object).
        on "MOUSE-SELECT-DBLCLICK"  of wgh-i-ordem-invest-cn0201c persistent run upc/OrdemInvZoom.p (input wgh-i-ordem-invest-cn0201c,
                                                                                                     input p-wgh-object).
    end.

    IF VALID-HANDLE(wgh-num-seq-item-cn0201c) THEN
        ON "LEAVE" OF wgh-num-seq-item-cn0201c PERSISTENT
            RUN pi-leave-seq IN h-cn0201c-upc.
END.

IF  c-char = "cn0201c-v01.w":U THEN DO:
    if valid-handle(wgh-lbl-ordem-invest-cn0201c) then
        assign wgh-lbl-ordem-invest-cn0201c:screen-value = "Num Ordem Inv:".
end.

IF  p-ind-event = "VALIDATE" AND
    c-char = "cn0201c-v04.w"  THEN DO: 
    run esp\esapi001.p persistent set h-esapi001.

    
    if valid-handle(wgh-nr-contrato-cn0201c   ) and
       valid-handle(wgh-num-seq-item-cn0201c  ) and
       valid-handle(wgh-i-ordem-invest-cn0201c) and 
       valid-handle(wgh-val-total-cn0201c     ) AND
       VALID-HANDLE(wgh-log-libera-cn0201c) then do:
        if wgh-val-total-cn0201c:input-value <> 0 then do:
            find first contrato-for no-lock
                 where contrato-for.nr-contrato = wgh-nr-contrato-cn0201c:input-value no-error.
            if avail contrato-for then do:
                find first estabelec no-lock
                     where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
                if avail estabelec then
                    assign cod-empresa = estabelec.ep-codigo.
            end.
            else do:
                find first param-global no-lock no-error.
                assign cod-empresa = param-global.empresa-prin.

            END.



                          
            ASSIGN l-aditivo = NO.

            FIND FIRST item-contrat
                 WHERE item-contrat.nr-contrato     = wgh-nr-contrato-cn0201c:input-value
                   AND item-contrat.num-seq-item    = wgh-num-seq-item-cn0201c:input-value NO-LOCK NO-ERROR.

            FIND FIRST contrato-for  
                 WHERE contrato-for.nr-contrato     = item-contrat.nr-contrato 
                   AND contrato-for.ind-sit-contrat = 2 NO-LOCK NO-ERROR .
            IF AVAIL contrato-for AND
               AVAIL item-contrat 
                 AND item-contrat.log-libera = NO THEN
                ASSIGN l-aditivo = YES. 

            find first param-inv no-lock 
                 where param-inv.ep-codigo = cod-empresa no-error.

            IF  wgh-i-ordem-invest-cn0201c:input-value <> 0
            AND wgh-log-libera-cn0201c:CHECKED THEN DO:
                if  can-find(first contrato-for-ext no-lock
                             where contrato-for-ext.nr-contrato = wgh-nr-contrato-cn0201c:input-value
                             and   contrato-for-ext.num-ord-inv <> 0 ) then do:
                    create tt-erro.
                    assign tt-erro.cd-erro  = 17006
                           tt-erro.mensagem = "Ordem de Investimento j  cadastrada no Contrato!".
                    run cdp/cd0669.w (input table tt-erro).
                    return "NOK":U.
                end.

                find first item-contrat-ext exclusive-lock
                     where item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                       and item-contrat-ext.num-seq-item = wgh-num-seq-item-cn0201c:input-value no-error.
                if avail item-contrat-ext then do:
                     assign item-contrat-ext.num-ord-inv = wgh-i-ordem-invest-cn0201c:input-value
                            item-contrat-ext.ep-codigo   = cod-empresa.
                end.
                else do:
                    create item-contrat-ext.
                    assign item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value 
                           item-contrat-ext.num-seq-item = wgh-num-seq-item-cn0201c:input-value
                           item-contrat-ext.num-ord-inv  = wgh-i-ordem-invest-cn0201c:input-value
                           item-contrat-ext.ep-codigo    = cod-empresa.
                end.

                ASSIGN de-preco-orig-cn0201c = 0.
                FIND FIRST item-contrat
                     WHERE item-contrat.nr-contrato = wgh-nr-contrato-cn0201c:input-value
                       AND item-contrat.num-seq-item  = wgh-num-seq-item-cn0201c:input-value NO-LOCK NO-ERROR.
                IF AVAIL item-contrat THEN
                    ASSIGN de-preco-orig-cn0201c = item-contrat.val-total.

                FIND FIRST contrato-for
                    WHERE contrato-for.nr-contrato = wgh-nr-contrato-cn0201c:input-value NO-LOCK NO-ERROR.

                run pi-atualiza-verba in h-esapi001 (input  1, /* Valida Verba */
                                                     input  cod-empresa,
                                                     input  item-contrat-ext.num-ord-inv,
                                                     input  contrato-for.dt-contrato,
                                                     input  param-inv.moeda-inv,
                                                     input  (wgh-val-total-cn0201c:input-value - de-preco-orig-cn0201c),
                                                     input  0,
                                                     output table tt-erro).
                if  can-find(first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    return "NOK":U.
                end.

                find first item-contrat exclusive-lock
                     where item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                       and item-contrat.num-seq-item = wgh-num-seq-item-cn0201c:input-value no-error.
                if avail item-contrat-ext then do:
                    FIND FIRST contrato-for NO-LOCK
                         WHERE contrato-for.nr-contrato     = item-contrat.nr-contrato 
                           AND contrato-for.ind-sit-contrat = 2 NO-ERROR.
                    IF  AVAIL contrato-for AND
                        wgh-val-total-cn0201c:input-value <> item-contrat.val-total THEN
                        ASSIGN l-aditivo = YES. 
                END.
            end.
        end.
    end.
    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.
end.
IF  p-ind-event = "ASSIGN" AND
    c-char = "cn0201c-v04.w" THEN DO: 

        run esp\esapi001.p persistent set h-esapi001.

    if valid-handle(wgh-nr-contrato-cn0201c   ) and
       valid-handle(wgh-num-seq-item-cn0201c  ) and
       valid-handle(wgh-i-ordem-invest-cn0201c) and 
       valid-handle(wgh-val-total-cn0201c     ) AND
       VALID-HANDLE(wgh-log-libera-cn0201c) THEN DO:

        if wgh-val-total-cn0201c:input-value <> 0 then do:
            find first contrato-for no-lock
                 where contrato-for.nr-contrato = wgh-nr-contrato-cn0201c:input-value no-error.
            if  avail contrato-for then do:
                find first estabelec no-lock
                     where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
                if avail estabelec then
                    assign cod-empresa = estabelec.ep-codigo.
            end.
            else do:
                find first param-global no-lock no-error.
                assign cod-empresa = param-global.empresa-prin.
            end.

            find first param-inv no-lock 
                 where param-inv.ep-codigo = cod-empresa no-error.

            IF   wgh-num-seq-item-cn0201c:SENSITIVE = NO /*Modificacao*/
            AND  l-aditivo                          = NO
            AND (wgh-i-ordem-invest-cn0201c:input-value = 0 OR
                 wgh-log-libera-cn0201c:CHECKED = NO) THEN DO:
                FIND FIRST item-contrat EXCLUSIVE-LOCK
                     WHERE item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                       AND item-contrat.num-seq-item = wgh-num-seq-item-cn0201c:input-value NO-ERROR.
                IF AVAIL item-contrat THEN DO:
                    &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                        ASSIGN item-contrat.num-ord-inv = wgh-i-ordem-invest-cn0201c:input-value.
                    &ELSE
                        ASSIGN OVERLAY(item-contrat.char-1,1,9) = STRING(wgh-i-ordem-invest-cn0201c:input-value).
                    &ENDIF

                    IF wgh-log-libera-cn0201c:CHECKED = NO THEN 
                    &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                        ASSIGN item-contrat.num-ord-inv = 0.
                    &ELSE
                        ASSIGN OVERLAY(item-contrat.char-1,1,9) = STRING(0).
                    &ENDIF
                END.
                
                find first item-contrat-ext exclusive-lock
                     where item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                       and item-contrat-ext.num-seq-item = wgh-num-seq-item-cn0201c:input-value no-error.
                if avail item-contrat-ext then do:
                    for each  controle-inv-esp exclusive-lock
                        where controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
                        and   controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                        and   controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
                        and   controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:
                        run pi-atualiza-verba in h-esapi001 (input 2,
                                                             input controle-inv-esp.ep-codigo,
                                                             input controle-inv-esp.num-ord-inv,
                                                             input controle-inv-esp.dt-trans,
                                                             input param-inv.moeda-inv,
                                                             input controle-inv-esp.ent-comp * -1,
                                                             input controle-inv-esp.ent-real * -1,
                                                             output table tt-erro).
                        if  can-find(first tt-erro) then do:
                            run cdp/cd0669.w (input table tt-erro).
                            if valid-handle(h-esapi001) then do:
                                delete procedure h-esapi001.
                                assign h-esapi001 = ?.
                            end.
                            return "NOK":U.
                        end.
                        delete controle-inv-esp.
                    end.
                    delete item-contrat-ext.
                end.
            end.

            find first item-contrat no-lock
                 where item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                 and   item-contrat.num-seq-item = wgh-num-seq-item-cn0201c:input-value no-error.
            if avail item-contrat then do:
                if wgh-num-seq-item-cn0201c:SENSITIVE = YES then /*Inclusao*/
                    run pi-principal in h-esapi001 (input 2,
                                                    input rowid(item-contrat),
                                                    input 1,
                                                    output table tt-erro).
                else if wgh-num-seq-item-cn0201c:SENSITIVE = no AND
                        l-aditivo                          = NO THEN /*Modificacao*/
                    
                    run pi-principal in h-esapi001 (input 2,
                                                    input rowid(item-contrat),
                                                    input 2,
                                                    output table tt-erro).

                if  can-find(first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    return "NOK":U.
                end.
            end.
        end.
    end.
    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.
    RUN pi-sugere-conta.
end.

IF  p-ind-event = "ADD" AND
    c-char = "cn0201c-v01.w" THEN DO: 
    if valid-handle(wgh-i-ordem-invest-cn0201c) then
        assign wgh-i-ordem-invest-cn0201c:screen-value = "0".
end.

IF  p-ind-event = "DISPLAY" AND
    c-char = "cn0201c-v01.w" THEN DO: 
    

    IF VALID-HANDLE(wgh-i-ordem-invest-cn0201c)  AND 
       wgh-nr-contrato-cn0201c:input-value <> 0 THEN DO:
        ASSIGN wgh-i-ordem-invest-cn0201c:SENSITIVE    = YES.
        /*FIND FIRST item-contrat
             WHERE ROWID(item-contrat) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL item-contrat AND item-contrat.ind-sit-item = 2 THEN DO:
            FIND FIRST contrato-for 
                 WHERE contrato-for.nr-contrato = item-contrat.nr-contrato NO-LOCK NO-ERROR.
            IF AVAIL contrato-for
                 AND contrato-for.ind-sit-contrat = 2 THEN
                ASSIGN wgh-i-ordem-invest-cn0201c:SENSITIVE = NO.
        END.*/

        FIND FIRST contrato-for-ext 
             WHERE contrato-for-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
               AND contrato-for-ext.num-ord-inv  <> 0 NO-LOCK NO-ERROR.
        IF AVAIL contrato-for-ext THEN
            ASSIGN wgh-i-ordem-invest-cn0201c:SENSITIVE = NO.

        FIND FIRST medicao-contrat
             WHERE medicao-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
               AND medicao-contrat.num-seq-item = wgh-num-seq-item-cn0201c:input-value  NO-LOCK NO-ERROR.
        IF AVAIL medicao-contrat THEN
            ASSIGN wgh-i-ordem-invest-cn0201c:SENSITIVE = NO.
    END.

    find first item-contrat-ext no-lock
         where item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
         and   item-contrat-ext.num-seq-item = wgh-num-seq-item-cn0201c:input-value no-error.
    if avail item-contrat-ext then
         assign wgh-i-ordem-invest-cn0201c:screen-value = string(item-contrat-ext.num-ord-inv).
    ELSE
        assign wgh-i-ordem-invest-cn0201c:screen-value = "0".
end.

PROCEDURE pi-leave-seq:
    
    IF VALID-HANDLE(wgh-i-ordem-invest-cn0201c)  THEN DO:
        FIND FIRST contrato-for-ext 
             WHERE contrato-for-ext.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
               AND contrato-for-ext.num-ord-inv  <> 0 NO-LOCK NO-ERROR.

        FIND FIRST contrato-for
             WHERE contrato-for.nr-contrato     = wgh-nr-contrato-cn0201c:input-value
               AND contrato-for.ind-sit-contrat = 2 NO-LOCK NO-ERROR.
        IF AVAIL contrato-for           AND 
           NOT AVAIL contrato-for-ext   THEN
            ASSIGN wgh-i-ordem-invest-cn0201c:SENSITIVE = YES.
    END.
END PROCEDURE.

PROCEDURE pi-sugere-conta:
    
    find first contrato-for no-lock
         where contrato-for.nr-contrato = wgh-nr-contrato-cn0201c:input-value no-error.
    if avail contrato-for then do:
        find first estabelec no-lock
             where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
        if avail estabelec then
            assign cod-empresa = estabelec.ep-codigo.
    end.
    else do:
        find first param-global no-lock no-error.
        assign cod-empresa = param-global.empresa-prin.
    end.

    find first sub-div-ordem no-lock
         where sub-div-ordem.ep-codigo      = cod-empresa 
           and sub-div-ordem.num-ord-magnus = wgh-i-ordem-invest-cn0201c:input-value no-error.
    if avail sub-div-ordem then do:
        find first ordem-inv no-lock
             where ordem-inv.ep-codigo    = sub-div-ordem.ep-codigo
             and   ordem-inv.cod-est-exec = sub-div-ordem.cod-est-exec
             and   ordem-inv.num-projeto  = sub-div-ordem.num-projeto
             and   ordem-inv.num-ordem    = sub-div-ordem.num-ordem no-error.
        if avail ordem-inv then do:
            for each  matriz-rat-item exclusive-lock
                where matriz-rat-item.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                and   matriz-rat-item.num-seq-item = wgh-num-seq-item-cn0201c:input-value:
                delete matriz-rat-item.
            end.

            find first item-uni-estab no-lock
                 where item-uni-estab.it-codigo   = wgh-it-codigo-cn0201c:input-value
                   and item-uni-estab.cod-estabel = sub-div-ordem.cod-est-exec no-error.

            CREATE matriz-rat-item.
            ASSIGN matriz-rat-item.nr-contrato    = wgh-nr-contrato-cn0201c:input-value
                   matriz-rat-item.it-codigo      = wgh-it-codigo-cn0201c:input-value
                   matriz-rat-item.num-seq-item   = wgh-num-seq-item-cn0201c:input-value
                   matriz-rat-item.ct-codigo      = ordem-inv.ct-codigo
                   matriz-rat-item.sc-codigo      = ordem-inv.sc-codigo
                   matriz-rat-item.cod-unid-negoc = if  avail item-uni-estab then item-uni-estab.cod-unid-negoc else "00"
                   matriz-rat-item.conta-contabil = ordem-inv.ct-codigo + ordem-inv.sc-codigo
                   matriz-rat-item.perc-rateio    = 100.

            FIND FIRST item-contrat EXCLUSIVE-LOCK
                 WHERE item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value
                   AND item-contrat.num-seq-item = wgh-num-seq-item-cn0201c:input-value NO-ERROR.
            IF AVAIL item-contrat THEN DO:
                if  wgh-log-libera-cn0201c:CHECKED then do:
                    &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                        ASSIGN item-contrat.num-ord-inv = sub-div-ordem.num-ord-magnus.
                    &ELSE
                        ASSIGN OVERLAY(item-contrat.char-1,1,9) = STRING(sub-div-ordem.num-ord-magnus).
                    &ENDIF
                end.
            end.
        end.
    end.
end procedure.



IF  p-ind-event = "VALIDATE" AND c-char = "cn0201c-v04.w" THEN DO: 

    FIND contrato-for WHERE contrato-for.nr-contrato = wgh-nr-contrato-cn0201c:INPUT-VALUE NO-LOCK NO-ERROR.
    IF AVAIL contrato-for THEN DO:

         RUN esp/procura-beneficio.p (INPUT contrato-for.cod-estabel,   
                                      INPUT wgh-it-codigo-cn0201c:SCREEN-VALUE,   
                                      INPUT contrato-for.cod-emitente,                                     
                                      OUTPUT TABLE tt-beneficio ). 



         IF RETURN-VALUE = "CFA" THEN DO:

             create tt-erro.
             assign tt-erro.cd-erro  = 15835
                    tt-erro.mensagem = "CFA NÆo cadastrado para o ITEM: " + wgh-it-codigo-cn0201c:SCREEN-VALUE + ", informe a area de TAX.".
             run cdp/cd0669.w (input table tt-erro).
        END.

    END.

    FOR EACH TT-BENEFICIO  BREAK BY  TT-BENEFICIO.tp-tax BY TT-BENEFICIO.PRIOR:

        IF FIRST-OF(TT-BENEFICIO.tp-tax) THEN DO:
            
            FIND FIRST es-ben-estab NO-LOCK
                 WHERE ROWID(es-ben-estab) = TT-BENEFICIO.rid-ben NO-ERROR.
            IF AVAIL es-ben-estab THEN DO:

/*                 IF es-ben-estab.compras-comprador-icms <> 0 AND TT-BENEFICIO.tp-tax = "ICMS" THEN DO:                                      */
/*                     FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-icms NO-LOCK NO-ERROR.        */
/*                     IF AVAIL es-mensagem-ben THEN DO:                                                                                      */
/*                         run utp/ut-msgs.p (input "show",                                                                                   */
/*                                            input 27979,                                                                                    */
/*                                            input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).                      */
/*                     END.                                                                                                                   */
/*                 END.                                                                                                                       */
/*                 IF  es-ben-estab.compras-comprador-pis-confis <> 0  AND TT-BENEFICIO.tp-tax = "PIS" THEN DO:                               */
/*                     FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-pis-confis NO-LOCK NO-ERROR.  */
/*                     IF AVAIL es-mensagem-ben THEN DO:                                                                                      */
/*                         run utp/ut-msgs.p (input "show",                                                                                   */
/*                                              input 27979,                                                                                  */
/*                                              input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).                    */
/*                     END.                                                                                                                   */
/*                 END.                                                                                                                       */
/*                 ASSIGN c-hora = STRING(TIME,"hh:mm").                                                                                      */
/*                        c-data = STRING(TODAY,"99/99/9999").                                                                                */
/*                                                                                                                                            */
/*                 CREATE ext-item-contrat.                                                                                                   */
/*                        ASSIGN ext-item-contrat.usuario      = c-seg-usuario.                                                               */
/*                               ext-item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value.                                         */
/*                               ext-item-contrat.mensagem     = es-mensagem-ben.narrativa.                                                   */
/*                               ext-item-contrat.cod-mensagem = es-mensagem-ben.cod-mensagem.                                                */
/*                               ext-item-contrat.it-codigo    = wgh-it-codigo-cn0201c:input-value.                                           */
/*                               ext-item-contrat.hora         = c-hora.                                                                      */
/*                               ext-item-contrat.data         = date(c-data).                                                                */

                /* Inicio - Validacao do Beneficio ao encontrar a classe */
                FIND FIRST ext-item-cfa NO-LOCK
                     WHERE ext-item-cfa.it-codigo = wgh-it-codigo-cn0201c:SCREEN-VALUE
                       AND ext-item-cfa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
                IF AVAIL ext-item-cfa THEN DO:

                    FIND FIRST es-beneficio-cfa NO-LOCK
                         WHERE es-beneficio-cfa.cod-beneficio = TT-BENEFICIO.cod-beneficio
                           AND es-beneficio-cfa.classe        = ext-item-cfa.classe NO-ERROR.
                    IF AVAIL es-beneficio-cfa THEN DO:

                        IF es-ben-estab.compras-comprador-icms <> 0 AND TT-BENEFICIO.tp-tax = "ICMS" THEN DO:

                            FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-icms NO-LOCK NO-ERROR.
                            IF AVAIL es-mensagem-ben THEN DO:
                                run utp/ut-msgs.p (input "show",
                                                   input 27979,
                                                   input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).
                            END.

                        END.

                        IF es-ben-estab.compras-comprador-pis-confis <> 0 AND TT-BENEFICIO.tp-tax = "PIS" THEN DO:

                            FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-pis-confis NO-LOCK NO-ERROR.
                            IF AVAIL es-mensagem-ben THEN DO:
                                run utp/ut-msgs.p (input "show",
                                                     input 27979,
                                                     input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).
                            END.

                        END.

                        ASSIGN c-hora = STRING(TIME,"hh:mm").                                                         
                               c-data = STRING(TODAY,"99/99/9999").                                                    

                        CREATE ext-item-contrat.                                                                        
                        ASSIGN ext-item-contrat.usuario      = c-seg-usuario.                                   
                               ext-item-contrat.nr-contrato  = wgh-nr-contrato-cn0201c:input-value.                
                               ext-item-contrat.mensagem     = IF AVAIL es-mensagem-ben THEN es-mensagem-ben.narrativa ELSE "".
                               ext-item-contrat.cod-mensagem = IF AVAIL es-mensagem-ben THEN es-mensagem-ben.cod-mensagem ELSE 0.
                               ext-item-contrat.it-codigo    = wgh-it-codigo-cn0201c:input-value.               
                               ext-item-contrat.hora         = c-hora.
                               ext-item-contrat.data         = date(c-data).

                    END.

                END.
                /* Fim - Validacao do Beneficio ao encontrar a classe */

            END.

        END.

    END.

    /*   ASSIGN c-hora = STRING(TIME,"hh:mm").                                                                        */
    /*          c-data = STRING(TODAY,"99/99/9999").                                                                  */
    /*                                                                                                                */
    /*   CREATE ext-item-contrat.                                                                                     */
    /*          ASSIGN ext-item-contrat.usuario     = c-seg-usuario.                                                  */
    /*                 ext-item-contrat.nr-contrato = wgh-nr-contrato-cn0201c:input-value.                            */
    /*                 ext-item-contrat.mensagem    = c-beneficio.                                                    */
    /*                 ext-item-contrat.it-codigo   = wgh-it-codigo-cn0201c:input-value.                              */
    /*                 ext-item-contrat.hora        = c-hora.                                                         */
    /*                 ext-item-contrat.data        = date(c-data).                                                   */
    /*                                                                                                                */

END.
