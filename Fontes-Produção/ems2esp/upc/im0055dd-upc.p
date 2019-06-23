/*******************************************************************************
** Programa: im0055dd-upc.p
** Autor...: Log­stica (log339640)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa im0055dd
** Objetivo: Inclus’o do campo "Ordem Invest" na inclus’o de despesas do ponto 
             de controle
*******************************************************************************/

DEF INPUT PARAM p-ind-event  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS   ROWID         NO-UNDO.
DEFINE VARIABLE l-eadi  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE clocal  AS CHARACTER   NO-UNDO.

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
**  UT-LITER.I - Chamada pardr’o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr’o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri»’o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
 

DEFINE VARIABLE wh-objeto   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wgh-child   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wh-grupo    AS   WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE wh-child    AS   WIDGET-HANDLE NO-UNDO.
define variable wh-pesquisa as   widget-handle no-undo.
DEFINE VARIABLE h-esapi001   AS HANDLE NO-UNDO.
DEFINE VARIABLE i-acao      AS INTEGER    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-c-incoterm-rateio-im0055dd AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-c-ordem-invest-im0055dd    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-lbl-ordem-invest-im0055dd  AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btOk-im0055dd              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btOkUPC-im0055dd           AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btCancel-im0055dd          AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btCancelUPC-im0055dd       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-im0055dd-upc                 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-estabel-im0055dd       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-embarque-im0055dd          AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-itiner-im0055dd        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-pto-contr-im0055dd     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-desp-im0055dd          AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-emitente-desp-im0055dd AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-val-desp-im0055dd           AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR l-inclui-im0055dc   as logical no-undo.
DEFINE NEW GLOBAL SHARED VAR l-modifica-im0055dc as logical no-undo.
DEFINE NEW GLOBAL SHARED VAR h-bocx120 AS HANDLE NO-UNDO.


IF  p-ind-event = "after-initialize":U THEN DO:
    RUN upc\im0055dd-upc.p PERSISTENT SET h-im0055dd-upc (INPUT "", INPUT "", INPUT p-wgh-object, INPUT p-wgh-frame, INPUT "", INPUT p-row-table).

    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                WHEN "fill-in":U THEN DO:

                    IF wh-child:NAME = "c-incoterm-rateio":U THEN 
                        ASSIGN wgh-c-incoterm-rateio-im0055dd = wh-child.
                    IF wh-child:NAME = "cod-estabel":U THEN 
                        ASSIGN wgh-cod-estabel-im0055dd       = wh-child.
                    IF wh-child:NAME = "embarque":U THEN 
                        ASSIGN wgh-embarque-im0055dd          = wh-child.
                    IF wh-child:NAME = "cod-itiner":U THEN 
                        ASSIGN wgh-cod-itiner-im0055dd        = wh-child.
                    IF wh-child:NAME = "cod-pto-contr":U THEN 
                        ASSIGN wgh-cod-pto-contr-im0055dd     = wh-child.
                    IF wh-child:NAME = "cod-desp":U THEN 
                        ASSIGN wgh-cod-desp-im0055dd          = wh-child.
                    IF wh-child:NAME = "cod-emitente-desp":U THEN 
                        ASSIGN wgh-cod-emitente-desp-im0055dd = wh-child.
                    IF wh-child:NAME = "val-desp":U THEN 
                        ASSIGN wgh-val-desp-im0055dd = wh-child.
                END.
                WHEN "button":U THEN DO:
                    IF wh-child:NAME = "btOk":U THEN 
                        ASSIGN wgh-btOk-im0055dd = wh-child.
                    IF wh-child:NAME = "btCancel":U THEN 
                        ASSIGN wgh-btCancel-im0055dd = wh-child.
                END.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    if valid-handle(wgh-c-incoterm-rateio-im0055dd) then do:

        create text wgh-lbl-ordem-invest-im0055dd
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(15)"
               WIDTH        = 13
               SCREEN-VALUE = "Ordem Invest:"
               ROW          = wgh-c-incoterm-rateio-im0055dd:row + 1.1
               COL          = wgh-c-incoterm-rateio-im0055dd:col - 10
               VISIBLE      = YES
               font         = 1.

        CREATE fill-in wgh-c-ordem-invest-im0055dd
        ASSIGN FRAME             = p-wgh-frame
               data-type         = "integer"
               FORMAT            = ">>>,>>>,>>9"
               NAME              = "ordem-invest"
               side-label-handle = wgh-lbl-ordem-invest-im0055dd:handle
               WIDTH             = 12
               HEIGHT            = 0.88
               ROW               = wgh-c-incoterm-rateio-im0055dd:row + 1
               COL               = wgh-c-incoterm-rateio-im0055dd:col
               VISIBLE           = YES
               SENSITIVE         = yes
               FONT              = 1.

        wgh-c-ordem-invest-im0055dd:LOAD-MOUSE-POINTER("image\lupa.cur").

        create button wgh-btOkUPC-im0055dd
        ASSIGN FRAME        = p-wgh-frame
               WIDTH        = wgh-btOk-im0055dd:width
               HEIGHT       = wgh-btOk-im0055dd:HEIGHT
               LABEL        = wgh-btOk-im0055dd:label
               ROW          = wgh-btOk-im0055dd:ROW
               COL          = wgh-btOk-im0055dd:col
               FONT         = wgh-btOk-im0055dd:FONT
               VISIBLE      = yes
               SENSITIVE    = yes.

        wgh-btOkUPC-im0055dd:MOVE-TO-TOP().
        on "CHOOSE"                 of wgh-btOkUPC-im0055dd        persistent run pi-btOkUPC in h-im0055dd-upc.

        create button wgh-btCancelUPC-im0055dd
        ASSIGN FRAME        = p-wgh-frame
               WIDTH        = wgh-btCancel-im0055dd:width
               HEIGHT       = wgh-btCancel-im0055dd:HEIGHT
               LABEL        = wgh-btCancel-im0055dd:label
               ROW          = wgh-btCancel-im0055dd:ROW
               COL          = wgh-btCancel-im0055dd:col
               FONT         = wgh-btCancel-im0055dd:FONT
               VISIBLE      = yes
               SENSITIVE    = yes.


        wgh-btCancelUPC-im0055dd:MOVE-TO-TOP().
        on "CHOOSE"                 of wgh-btCancelUPC-im0055dd        persistent run pi-btCancelUPC in h-im0055dd-upc.
       /* ON "ENTRY"                  OF wgh-cod-emitente-desp-im0055dd  persistent run pi-entry-upc in h-im0055dd-upc.*/
        on "F5"                     of wgh-c-ordem-invest-im0055dd persistent run upc/OrdemInvZoom.p (input wgh-c-ordem-invest-im0055dd,
                                                                                                      input p-wgh-object).
        on "MOUSE-SELECT-DBLCLICK"  of wgh-c-ordem-invest-im0055dd persistent run upc/OrdemInvZoom.p (input wgh-c-ordem-invest-im0055dd,
                                                                                                      input p-wgh-object).
    end.

    if valid-handle(wgh-c-ordem-invest-im0055dd) then do:

        if valid-handle(wgh-cod-estabel-im0055dd      ) and
           valid-handle(wgh-embarque-im0055dd         ) and
           valid-handle(wgh-cod-itiner-im0055dd       ) and
           valid-handle(wgh-cod-pto-contr-im0055dd    ) and
           valid-handle(wgh-cod-desp-im0055dd         ) and
           valid-handle(wgh-cod-emitente-desp-im0055dd) then do:
            FIND FIRST desp-embarque-ext NO-LOCK
                 WHERE desp-embarque-ext.cod-estabel       = wgh-cod-estabel-im0055dd          :screen-value 
                 and   desp-embarque-ext.embarque          = wgh-embarque-im0055dd             :screen-value 
                 and   desp-embarque-ext.cod-itiner        = int(wgh-cod-itiner-im0055dd       :screen-value)
                 and   desp-embarque-ext.cod-pto-contr     = int(wgh-cod-pto-contr-im0055dd    :screen-value)
                 and   desp-embarque-ext.cod-desp          = int(wgh-cod-desp-im0055dd         :screen-value)
                 and   desp-embarque-ext.cod-emitente-desp = int(wgh-cod-emitente-desp-im0055dd:screen-value) no-error.

            IF  AVAIL desp-embarque-ext THEN
                ASSIGN wgh-c-ordem-invest-im0055dd:screen-value = string(desp-embarque-ext.num-ord-inv).
            ELSE
                ASSIGN wgh-c-ordem-invest-im0055dd:screen-value = "0".
        END.
    END.

    if l-modifica-im0055dc then do:
        assign wgh-c-ordem-invest-im0055dd:sensitive = no.
    end.
    /*Katia*/

    IF NOT VALID-HANDLE(h-bocx120) THEN DO:
       RUN cxbo/bocx120.p persistent set h-bocx120.
    END.


    RUN pi-entry-upc.
END.
IF  p-ind-event  = "AFTER-DESTROY-INTERFACE" THEN DO:
    if valid-handle(h-im0055dd-upc) then do:
        delete procedure h-im0055dd-upc.
        assign h-im0055dd-upc = ?.
    end.
    assign l-inclui-im0055dc   = no
           l-modifica-im0055dc = no.
end.
PROCEDURE pi-entry-upc:
/* desp-embarque => ordens-embarque => ordem-compra*/


    FIND FIRST pto-itiner
        WHERE pto-itiner.cod-itiner  = INT(wgh-cod-itiner-im0055dd :SCREEN-VALUE)
        AND pto-itiner.cod-pto-contr = INT(wgh-cod-pto-contr-im0055dd:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAIL pto-itiner THEN
        RUN setalocais in h-bocx120 (input rowid(pto-itiner),
                                     output l-eadi,
                                     output clocal).


    IF clocal = "desembarque/chegada" OR clocal = "desembarque"  THEN DO:
        ASSIGN wgh-c-ordem-invest-im0055dd:SENSITIVE = NO.
        
        IF wgh-c-ordem-invest-im0055dd:SCREEN-VALUE = "0" THEN DO:
            FIND FIRST ordens-embarque
                WHERE ordens-embarque.embarque = wgh-embarque-im0055dd:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAIL ordens-embarque THEN DO:
                FIND FIRST ordem-compra
                    WHERE ordem-compra.numero-ordem = ordens-embarque.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN DO:
                    ASSIGN wgh-c-ordem-invest-im0055dd:SCREEN-VALUE = STRING(ordem-compra.num-ord-inv)
                           wgh-c-ordem-invest-im0055dd:SENSITIVE    = NO /*IF ordem-compra.num-ord-inv = 0 THEN YES ELSE NO*/ .
                END.
                ELSE 
                    ASSIGN wgh-c-ordem-invest-im0055dd:SENSITIVE = YES.
            END.
            ELSE 
                ASSIGN wgh-c-ordem-invest-im0055dd:SENSITIVE = YES.
        END.
    END.
    ELSE
        ASSIGN wgh-c-ordem-invest-im0055dd:SENSITIVE = YES.
    
END PROCEDURE.

procedure pi-btOkUPC:

  

    if int(wgh-c-ordem-invest-im0055dd:screen-value) <> 0 then do:
        if not can-find (first sub-div-ordem
                         where sub-div-ordem.num-ord-magnus = int(wgh-c-ordem-invest-im0055dd:screen-value)) then do:
            RUN utp/ut-msgs.p(INPUT "SHOW":U,
                              INPUT 17006,
                              INPUT "Ordem de Investimento Invÿlida! Favor informar ordem de investimento.").

            return "NOK":U.
        end.
    end.

    
    
    if valid-handle(wgh-btOk-im0055dd) then
        apply "choose" to wgh-btOk-im0055dd.

    if return-value <> "OK":U then
        return "NOK":U.
    else do:
        if valid-handle(wgh-c-ordem-invest-im0055dd) AND int(wgh-c-ordem-invest-im0055dd:screen-value) <> 0 then do:
    
            if valid-handle(wgh-cod-estabel-im0055dd      ) and
               valid-handle(wgh-embarque-im0055dd         ) and
               valid-handle(wgh-cod-itiner-im0055dd       ) and
               valid-handle(wgh-cod-pto-contr-im0055dd    ) and
               valid-handle(wgh-cod-desp-im0055dd         ) and
               valid-handle(wgh-cod-emitente-desp-im0055dd) and 
               valid-handle(wgh-val-desp-im0055dd         ) then do:
                FIND FIRST desp-embarque-ext exclusive-lock
                     WHERE desp-embarque-ext.cod-estabel       = wgh-cod-estabel-im0055dd          :screen-value 
                     and   desp-embarque-ext.embarque          = wgh-embarque-im0055dd             :screen-value 
                     and   desp-embarque-ext.cod-itiner        = int(wgh-cod-itiner-im0055dd       :screen-value)
                     and   desp-embarque-ext.cod-pto-contr     = int(wgh-cod-pto-contr-im0055dd    :screen-value)
                     and   desp-embarque-ext.cod-desp          = int(wgh-cod-desp-im0055dd         :screen-value)
                     and   desp-embarque-ext.cod-emitente-desp = int(wgh-cod-emitente-desp-im0055dd:screen-value) no-error.
            
                IF NOT AVAIL desp-embarque-ext THEN DO:
                    CREATE desp-embarque-ext.
                    ASSIGN desp-embarque-ext.cod-estabel       = wgh-cod-estabel-im0055dd          :screen-value 
                           desp-embarque-ext.embarque          = wgh-embarque-im0055dd             :screen-value 
                           desp-embarque-ext.cod-itiner        = int(wgh-cod-itiner-im0055dd       :screen-value)
                           desp-embarque-ext.cod-pto-contr     = int(wgh-cod-pto-contr-im0055dd    :screen-value)
                           desp-embarque-ext.cod-desp          = int(wgh-cod-desp-im0055dd         :screen-value)
                           desp-embarque-ext.cod-emitente-desp = int(wgh-cod-emitente-desp-im0055dd:screen-value).
                END.
                ASSIGN desp-embarque-ext.num-ord-inv = int(wgh-c-ordem-invest-im0055dd:screen-value)
                       desp-embarque-ext.val-desp    = dec(wgh-val-desp-im0055dd:screen-value).
    
                find first estabelec no-lock
                     where estabelec.cod-estabel = wgh-cod-estabel-im0055dd:screen-value  no-error.
                if avail estabelec then
                    assign desp-embarque-ext.ep-codigo = estabelec.ep-codigo.
                else do:
                    find first param-global no-lock no-error.
                    if avail param-global then
                        assign desp-embarque-ext.ep-codigo = param-global.empresa-prin.
                    else assign desp-embarque-ext.ep-codigo = "0".
                end.
    
                run esp\esapi001.p persistent set h-esapi001.
                if valid-handle(h-esapi001) then do: 
                    run pi-principal in h-esapi001 (input 4,  /*Despesas*/
                                                    input rowid(desp-embarque-ext),
                                                    input 1,
                                                    output table tt-erro).
                    if can-find (first tt-erro) then do:
                        run cdp/cd0669.w (input table tt-erro).
                        return "NOK":U.
                    end.
                    delete procedure h-esapi001.
                    assign h-esapi001 = ?.
                end.
            END.
        end.
    end.
END.

procedure pi-btCancelUPC:
    assign l-inclui-im0055dc   = no
           l-modifica-im0055dc = no.

    apply "choose" to wgh-btCancel-im0055dd.
end procedure.


