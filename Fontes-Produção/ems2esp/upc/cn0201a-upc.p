/*******************************************************************************
** Programa: cn0201a-upc
** Autor...: Log°stica (log339640)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa cn0201a.w
** Objetivo: 
*******************************************************************************/

DEF INPUT PARAM p-ind-event  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS   ROWID         NO-UNDO.

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
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descriá∆o",
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

DEFINE NEW GLOBAL SHARED VAR wgh-cod-projeto-cn0201a      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-vl-limite-contr-cn0201a  AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-nr-contrato-cn0201a      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-lbl-ordem-invest-cn0201a AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-i-ordem-invest-cn0201a   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-log-libera-cn0201a       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-cn0201a-upc                AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR l-aditivo-contrato           AS LOGICAL       NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR l-add                        AS LOGICAL       NO-UNDO.

ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                  p-wgh-object:FILE-NAME,'/').

IF  p-ind-event = "initialize":U and c-char = "cnp\cn0201a.w":U THEN DO:
    RUN upc\cn0201a-upc.p PERSISTENT SET h-cn0201a-upc (INPUT "", INPUT "", INPUT p-wgh-object, INPUT p-wgh-frame, INPUT "", INPUT p-row-table).

    RUN select-page IN p-wgh-object (INPUT 2).
    RUN select-page IN p-wgh-object (INPUT 1).
    ASSIGN l-add = NO.
END.

IF p-ind-event = "change-page" THEN DO:
    IF VALID-HANDLE (wgh-nr-contrato-cn0201a)     AND
       VALID-HANDLE (wgh-i-ordem-invest-cn0201a)  THEN DO:
        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value NO-ERROR.
        IF NOT AVAIL contrato-for THEN
            ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = YES
                   l-add = YES.
        ELSE DO:
            FIND FIRST medicao-contrat
                 WHERE medicao-contrat.nr-contrato = wgh-nr-contrato-cn0201a:input-value NO-LOCK NO-ERROR.
            IF AVAIL medicao-contrat THEN
                ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.

            FIND FIRST item-contrat-ext 
                 WHERE item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201a:input-value
                   AND item-contrat-ext.num-ord-inv  <> 0 NO-LOCK NO-ERROR.
            IF AVAIL item-contrat-ext THEN
                ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.

            blk_ordem:
            FOR EACH item-contrat
               WHERE item-contrat.nr-contrato      = wgh-nr-contrato-cn0201a:input-value
                 AND item-contrat.ind-tipo-control = 2 NO-LOCK:
                FIND FIRST ordem-compra
                     WHERE ordem-compra.nr-contrato  = item-contrat.nr-contrato
                       AND ordem-compra.num-seq-item = item-contrat.num-seq-item NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN DO:
                    ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.
                    LEAVE blk_ordem.
                end.
            END.
        END.
    END.
END.

IF p-ind-event = "ADD" THEN
    ASSIGN l-add = YES.

IF  p-ind-event = "before-initialize":U and c-char = "cn0201a-v02.w":U THEN DO:
    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                when "fill-in" then do:
                    IF wh-child:NAME = "cod-projeto":U THEN 
                        ASSIGN wgh-cod-projeto-cn0201a = wh-child.
                    IF wh-child:NAME = "dec-2":U THEN 
                        ASSIGN wgh-vl-limite-contr-cn0201a = wh-child.
                end.
                WHEN "toggle-box" THEN DO:
                    IF  wh-child:NAME = "log-libera":U THEN 
                        ASSIGN wgh-log-libera-cn0201a = wh-child.
                END.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    if valid-handle(wgh-cod-projeto-cn0201a) then do:
        create text wgh-lbl-ordem-invest-cn0201a
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(15)"
               WIDTH        = 14
               SCREEN-VALUE = "Num Ordem Inv:"
               ROW          = wgh-cod-projeto-cn0201a:row + 0.2
               COL          = wgh-cod-projeto-cn0201a:col + 31
               VISIBLE      = YES
               font         = 1.

        CREATE fill-in wgh-i-ordem-invest-cn0201a
        ASSIGN FRAME             = p-wgh-frame
               data-type         = "integer"
               FORMAT            = ">>>,>>>,>>9"
               NAME              = "num-ord-inv"
               side-label-handle = wgh-lbl-ordem-invest-cn0201a:handle
               WIDTH             = 12
               HEIGHT            = 0.88
               ROW               = wgh-cod-projeto-cn0201a:row
               COL               = wgh-cod-projeto-cn0201a:col + 43
               VISIBLE           = YES
               SENSITIVE         = yes
               FONT              = 1.

        wgh-i-ordem-invest-cn0201a:LOAD-MOUSE-POINTER("image\lupa.cur").
        on "F5"                     of wgh-i-ordem-invest-cn0201a persistent run upc/OrdemInvZoom.p (input wgh-i-ordem-invest-cn0201a,
                                                                                                     input p-wgh-object).
        on "MOUSE-SELECT-DBLCLICK"  of wgh-i-ordem-invest-cn0201a persistent run upc/OrdemInvZoom.p (input wgh-i-ordem-invest-cn0201a,
                                                                                                     input p-wgh-object).
        assign wgh-i-ordem-invest-cn0201a:screen-value = "0".
    end.
END.

IF  p-ind-event = "before-initialize":U and c-char = "cn0201a-v01.w":U THEN DO:
    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                when "fill-in" then do:
                    IF wh-child:NAME = "i-nr-contrato":U THEN 
                        ASSIGN wgh-nr-contrato-cn0201a  = wh-child.
                end.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.
END.

IF  c-char = "cn0201a-v02.w":U THEN DO:
    if valid-handle(wgh-lbl-ordem-invest-cn0201a) then
        assign wgh-lbl-ordem-invest-cn0201a:screen-value = "Num Ordem Inv:".
end.

IF  p-ind-event = "Validate" AND
    c-char = "cn0201a-v02.w" THEN DO: 
    run esp\esapi001.p persistent set h-esapi001.

    if valid-handle(wgh-nr-contrato-cn0201a    ) and
       valid-handle(wgh-vl-limite-contr-cn0201a) and
       valid-handle(wgh-i-ordem-invest-cn0201a ) AND
       VALID-HANDLE(wgh-log-libera-cn0201a) then do:
        find first contrato-for no-lock
             where contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value no-error.
        if  avail contrato-for then do:
            find first estabelec no-lock
                 where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
            if  avail estabelec then
                assign cod-empresa = estabelec.ep-codigo.
        end.
        else do:
            find first param-global no-lock no-error.
            assign cod-empresa = param-global.empresa-prin.
        end.

        ASSIGN l-aditivo-contrato = NO.

        IF wgh-vl-limite-contr-cn0201a:input-value <> 0
        OR contrato-for.dec-2 <> 0 THEN DO:
            find first param-inv no-lock 
                 where param-inv.ep-codigo = cod-empresa no-error.

            IF  wgh-i-ordem-invest-cn0201a:input-value <> 0 
            AND wgh-log-libera-cn0201a:CHECKED THEN DO:
                if  can-find(first item-contrat-ext no-lock
                             where item-contrat-ext.nr-contrato = wgh-nr-contrato-cn0201a:input-value
                             and   item-contrat-ext.num-ord-inv <> 0) then do:
                    create tt-erro.
                    assign tt-erro.cd-erro  = 17006
                           tt-erro.mensagem = "Ordem de Investimento j† cadastrada no Item do Contrato!".
                    run cdp/cd0669.w (input table tt-erro).
                    return "NOK":U.
                end.

                find first contrato-for-ext exclusive-lock
                     where contrato-for-ext.nr-contrato  = wgh-nr-contrato-cn0201a:input-value no-error.
                if avail contrato-for-ext then do:
                     assign contrato-for-ext.num-ord-inv = wgh-i-ordem-invest-cn0201a:input-value
                            contrato-for-ext.ep-codigo   = cod-empresa.
                end.
                else do:
                    create contrato-for-ext.
                    assign contrato-for-ext.nr-contrato   = wgh-nr-contrato-cn0201a:input-value 
                           contrato-for-ext.num-ord-inv   = wgh-i-ordem-invest-cn0201a:input-value
                           contrato-for-ext.ep-codigo     = cod-empresa.
                end.

                run pi-atualiza-verba in h-esapi001 (input 1, /* Valida Verba */
                                                     input cod-empresa,
                                                     input wgh-i-ordem-invest-cn0201a:input-value,
                                                     input contrato-for.dt-contrato,
                                                     input param-inv.moeda-inv,
                                                     input (contrato-for.dec-2
                                                          - dec(wgh-vl-limite-contr-cn0201a:screen-value)),
                                                     input 0,
                                                     output table tt-erro).
                if  can-find(first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    return "NOK":U.
                end.

                FIND FIRST contrato-for NO-LOCK
                     WHERE contrato-for.nr-contrato     = wgh-nr-contrato-cn0201a:input-value 
                       AND contrato-for.ind-sit-contrat = 2 NO-ERROR.
                IF  AVAIL contrato-for AND
                    wgh-vl-limite-contr-cn0201a:input-value <> contrato-for.dec-2 THEN
                    ASSIGN l-aditivo-contrato = YES. 
            END.
        end.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato     = wgh-nr-contrato-cn0201a:input-value
               AND contrato-for.ind-sit-contrat = 2 NO-ERROR.
        IF AVAIL contrato-for 
             AND wgh-log-libera-cn0201a:CHECKED <> contrato-for.log-libera 
             AND contrato-for.log-libera = YES THEN
            ASSIGN l-aditivo-contrato = YES. 
    end.
    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.
end.
IF  p-ind-event = "ASSIGN" AND
    c-char = "cn0201a-v02.w" THEN DO: 
    run esp\esapi001.p persistent set h-esapi001.

    if valid-handle(wgh-nr-contrato-cn0201a    ) and
       valid-handle(wgh-vl-limite-contr-cn0201a) and
       valid-handle(wgh-i-ordem-invest-cn0201a ) AND
       VALID-HANDLE(wgh-log-libera-cn0201a) then do:

       /* if  wgh-vl-limite-contr-cn0201a:input-value <> 0 then do: */
            find first contrato-for no-lock
                 where contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value no-error.
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

            IF  wgh-i-ordem-invest-cn0201a:input-value = 0 
            OR  wgh-log-libera-cn0201a:CHECKED = NO
            OR (INT(wgh-vl-limite-contr-cn0201a:SCREEN-VALUE) = 0
            AND contrato-for.dec-2 <> 0) then do:
                FIND CURRENT contrato-for EXCLUSIVE-LOCK NO-ERROR.

                &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                    ASSIGN contrato-for.num-ord-inv = wgh-i-ordem-invest-cn0201a:input-value.
                &ELSE
                    ASSIGN OVERLAY(contrato-for.char-1,1,9) = STRING(wgh-i-ordem-invest-cn0201a:input-value).
                &ENDIF

                IF wgh-log-libera-cn0201a:CHECKED = NO THEN
                    &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                        ASSIGN contrato-for.num-ord-inv = 0.
                    &ELSE
                        ASSIGN OVERLAY(contrato-for.char-1,1,9) = "0".
                    &ENDIF

                FIND CURRENT contrato-for NO-LOCK NO-ERROR.
                
                if wgh-nr-contrato-cn0201a:SENSITIVE = no 
                AND l-aditivo-contrato = NO then do: /*Modificacao*/
                    find first contrato-for-ext exclusive-lock
                         where contrato-for-ext.nr-contrato  = wgh-nr-contrato-cn0201a:input-value no-error.
                    if avail contrato-for-ext then do:
                        for each  controle-inv-esp exclusive-lock
                            where controle-inv-esp.ep-codigo    = contrato-for-ext.ep-codigo
                            and   controle-inv-esp.num-ord-inv  = contrato-for-ext.num-ord-inv
                            and   controle-inv-esp.nr-contrato  = contrato-for-ext.nr-contrato:
                            if valid-handle(h-esapi001) then do: 
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
                            end.
                            delete controle-inv-esp.
                        end.
                        delete contrato-for-ext.
                    END.
                end.
            end.

        if wgh-nr-contrato-cn0201a:SENSITIVE = YES then /*Inclusao*/
            run pi-principal in h-esapi001 (input 1,
                                            input rowid(contrato-for),
                                            input 1,
                                            output table tt-erro).
        else if wgh-nr-contrato-cn0201a:SENSITIVE = no AND 
             l-aditivo-contrato                   = NO then /*Modificacao*/
            run pi-principal in h-esapi001 (input 1,
                                            input rowid(contrato-for),
                                            input 2,
                                            output table tt-erro).

        if  can-find(first tt-erro) then do:
            run cdp/cd0669.w(input table tt-erro).
            return "NOK":U.
        end.
    end.
    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.
    RUN pi-sugere-conta.
end.

IF  p-ind-event = "ADD" AND
    c-char = "cn0201a-v02.w" THEN DO: 
    if valid-handle(wgh-i-ordem-invest-cn0201a) then
        assign wgh-i-ordem-invest-cn0201a:screen-value = "0".
end.

IF  p-ind-event = "DISPLAY" AND
    c-char = "cn0201a-v02.w" THEN DO: 
    assign cod-empresa = "0".
    find first contrato-for no-lock
         where contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value no-error.
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

    find first contrato-for-ext no-lock
         where contrato-for-ext.nr-contrato  = wgh-nr-contrato-cn0201a:input-value no-error.
    if avail contrato-for-ext then
         assign wgh-i-ordem-invest-cn0201a:screen-value = string(contrato-for-ext.num-ord-inv).
    else 
        assign wgh-i-ordem-invest-cn0201a:screen-value = "0".
end.

IF p-ind-event = "DISPLAY" AND
   c-char = "cn0201a-v02.w" THEN DO:
    IF VALID-HANDLE(wgh-i-ordem-invest-cn0201a) AND
       VALID-HANDLE(wgh-nr-contrato-cn0201a) THEN DO:
        ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = YES.
        /*FIND FIRST contrato-for
             WHERE contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value NO-LOCK NO-ERROR.
        IF AVAIL contrato-for 
             AND contrato-for.ind-sit-contrat = 2 THEN
            ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.*/

        FIND FIRST medicao-contrat
             WHERE medicao-contrat.nr-contrato = wgh-nr-contrato-cn0201a:input-value NO-LOCK NO-ERROR.

        IF AVAIL medicao-contrat THEN
            ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.

        blk_ordem:
        FOR EACH item-contrat
           WHERE item-contrat.nr-contrato      = wgh-nr-contrato-cn0201a:input-value
             AND item-contrat.ind-tipo-control = 2 NO-LOCK:

            FIND FIRST ordem-compra
                 WHERE ordem-compra.nr-contrato  = item-contrat.nr-contrato
                   AND ordem-compra.num-seq-item = item-contrat.num-seq-item NO-LOCK NO-ERROR.
            IF AVAIL ordem-compra THEN DO:
                ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.
                LEAVE blk_ordem.
            END.
        END.

        FIND FIRST item-contrat-ext NO-LOCK
             WHERE item-contrat-ext.nr-contrato  = wgh-nr-contrato-cn0201a:input-value
               AND item-contrat-ext.num-ord-inv  <> 0 NO-ERROR.
        IF AVAIL item-contrat-ext THEN
            ASSIGN wgh-i-ordem-invest-cn0201a:SENSITIVE = NO.
    END.
END.

PROCEDURE pi-sugere-conta:
    find first contrato-for no-lock
         where contrato-for.nr-contrato = wgh-nr-contrato-cn0201a:input-value no-error.
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
           and sub-div-ordem.num-ord-magnus = wgh-i-ordem-invest-cn0201a:input-value no-error.
    if avail sub-div-ordem then do:
        find first ordem-inv no-lock
             where ordem-inv.ep-codigo    = sub-div-ordem.ep-codigo
             and   ordem-inv.cod-est-exec = sub-div-ordem.cod-est-exec
             and   ordem-inv.num-projeto  = sub-div-ordem.num-projeto
             and   ordem-inv.num-ordem    = sub-div-ordem.num-ordem no-error.
        if avail ordem-inv then do:
                for each  matriz-rat-contr exclusive-lock
                    where matriz-rat-contr.nr-contrato  = wgh-nr-contrato-cn0201a:input-value:
                    delete matriz-rat-contr.
                end.

                CREATE matriz-rat-contr.
                ASSIGN matriz-rat-contr.nr-contrato    = wgh-nr-contrato-cn0201a:input-value
                       matriz-rat-contr.ct-codigo      = ordem-inv.ct-codigo
                       matriz-rat-contr.sc-codigo      = ordem-inv.sc-codigo
                       matriz-rat-contr.conta-contabil = ordem-inv.ct-codigo + ordem-inv.sc-codigo
                       matriz-rat-contr.perc-rateio    = 100.

                IF AVAIL contrato-for THEN DO:
                    FIND CURRENT contrato-for EXCLUSIVE-LOCK NO-ERROR.
                    IF wgh-log-libera-cn0201a:CHECKED = YES THEN
                        &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                            ASSIGN contrato-for.num-ord-inv = sub-div-ordem.num-ord-magnus.
                        &ELSE
                            ASSIGN OVERLAY(contrato-for.char-1,1,9) = string(sub-div-ordem.num-ord-magnus).
                        &ENDIF
                end.
        end.
    end.
end procedure.
