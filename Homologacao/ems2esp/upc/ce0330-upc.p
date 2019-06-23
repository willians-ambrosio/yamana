/****************************************************************************
** Programa: ce0330-upc.p                                                  **
** Autor...: Camila Steffen                                                **
** Data....: Set/2010                                                      **
** Objetivo: Adicionar os campos Ponto Encomenda e Qtde-Segur da tabela    **
** (Xtivity) com as informa‡äes gravadas na tabela espec¡fica              **
** ext-item-uni-estab01 - Yamana                                             **
****************************************************************************/
{include/i-prgvrs.i ce0330-upc 2.00.00.001}
{utp/ut-glob.i}
/* Parametros de Entrada da UPC */
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define variable h-objeto     as handle    no-undo.
define variable wgh-objeto   as handle    no-undo.
define variable wgh-objeto-2 as handle    no-undo.
define variable wgh-objeto-3 as handle    no-undo.
define variable c-formato    as character format "x(15)" no-undo.

define new global shared variable h-upc-ce0330                 as handle no-undo.
define new global shared variable wgh-ce0330-it-codigo         as widget-handle no-undo.
define new global shared variable wgh-ce0330-cod-estabel       as widget-handle no-undo.
define new global shared variable wgh-ce0330-cb-perm-saldo-neg as widget-handle no-undo.
define new global shared variable wgh-ce0330-ind-cons-prv      as widget-handle no-undo.
define new global shared variable wgh-ce0330-tempo-segur       as widget-handle no-undo.
define new global shared variable wgh-ce0330-quant-segur       as widget-handle no-undo.
define new global shared variable wh-label                     as widget-handle no-undo.
define new global shared variable wgh-ce0330-fPage3            as widget-handle no-undo.
define new global shared variable wgh-ce0330-fPage2            as widget-handle no-undo.
define new global shared variable text-ce0330-Qtde-Segur       as widget-handle no-undo.
define new global shared variable fi-ce0330-Qtde-Segur         as widget-handle no-undo.
define new global shared variable wgh-ce0330-ponto-encomenda   as widget-handle no-undo.
define new global shared variable text-ce0330-ponto-encomenda  as widget-handle no-undo.
define new global shared variable fi-ce0330-ponto-encomenda    as widget-handle no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE wh-ce0330-cb-demanda     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-demanda-ant            AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ce0330-quant-segur    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-quant-segur-ant        AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-xtivity                AS DECIMAL       NO-UNDO.

DEFINE BUFFER b-item-uni-estab FOR item-uni-estab.
DEFINE BUFFER b-item           FOR ITEM.

/*message "p-ind-event  " p-ind-event  skip
        "p-ind-object " p-ind-object skip
    view-as alert-box info buttons ok.  */

if p-ind-event  = "INITIALIZE":U and
   p-ind-object = "CONTAINER":U then do: 
    
    run upc\ce0330-upc.p persistent set h-upc-ce0330 (input "",            
                                                      input "",            
                                                      input p-wgh-object,  
                                                      input p-wgh-frame,   
                                                      input "",            
                                                      input p-row-table).
end. /**INITIALIZE**/


if p-ind-event  = "BEFORE-INITIALIZE":U and
   p-ind-object = "CONTAINER":U then do: 

    
    run pi-armazena-handle.

    if valid-handle(wgh-ce0330-cb-perm-saldo-neg) and
       valid-handle(wgh-ce0330-ind-cons-prv)      and   
       valid-handle(wgh-ce0330-tempo-segur)       and
       valid-handle(wgh-ce0330-quant-segur)       and
       valid-handle(wgh-ce0330-ponto-encomenda)   then do:

        assign wgh-ce0330-cb-perm-saldo-neg:row = 1.29
               wh-label                         = wgh-ce0330-cb-perm-saldo-neg:side-label-handle
               wh-label                    :row = 1.29 

               wgh-ce0330-ind-cons-prv     :row = 2.29
               wh-label                         = wgh-ce0330-ind-cons-prv:side-label-handle
               wh-label                    :row = 2.29 

               wgh-ce0330-tempo-segur      :row = 3.29
               wh-label                         = wgh-ce0330-tempo-segur:side-label-handle
               wh-label                    :row = 3.29 

               wgh-ce0330-quant-segur      :row = 4.29
               wh-label                         = wgh-ce0330-quant-segur:side-label-handle
               wh-label                    :row = 4.29

               wgh-ce0330-ponto-encomenda:width = 20.
    end.

    find first ext-item-uni-estab01 no-lock no-error.

    create text text-ce0330-Qtde-Segur
    assign frame        = wgh-ce0330-fPage3
           format       = "x(21)"
           width        = 21
           screen-value = "Qtde Segur (Xtivity): "
           row          = 5.41
           col          = 19
           fgcolor      = 0
           visible      = yes.      
    
    create fill-in fi-ce0330-Qtde-Segur
    assign frame              = wgh-ce0330-fPage3
           side-label-handle  = text-ce0330-Qtde-Segur:handle
           format             = "x(15)"   
           width              = 15
           height             = 0.88
           row                = 5.29
           col                = 32.5
           label              = "Qtde Segur (Xtivity): "
           visible            = yes
           sensitive          = NO  .

    create text text-ce0330-ponto-encomenda
    assign frame        = wgh-ce0330-fPage2
           format       = "x(11)"
           width        = 11
           screen-value = "(Xtivity): "
           row          = 4.5
           col          = 54.5
           fgcolor      = 0
           visible      = yes.  

    create fill-in fi-ce0330-ponto-encomenda
    assign frame              = wgh-ce0330-fPage2
           side-label-handle  = text-ce0330-ponto-encomenda:handle
           format             = "x(20)"   
           width              = 20
           height             = 0.88
           row                = 4.38
           col                = 60
           label              = "(Xtivity): "
           visible            = yes
           sensitive          = NO  .
   
end. /**BEFORE-INITIALIZE**/


if p-ind-event  = "BEFORE-DISPLAY":U and
   p-ind-object = "CONTAINER":U then do: 

    run pi-armazena-handle.

    find first ext-item-uni-estab01 where ext-item-uni-estab01.cod-estabel = wgh-ce0330-cod-estabel:screen-value and
                                         ext-item-uni-estab01.it-codigo   = wgh-ce0330-it-codigo:screen-value no-lock no-error.
    if avail ext-item-uni-estab01 then do:

        assign fi-ce0330-Qtde-Segur:screen-value      = string(ext-item-uni-estab01.estoque-seguranca,">>>,>>>,>>9.99")
               fi-ce0330-ponto-encomenda:screen-value = string(ext-item-uni-estab01.ponto-encomenda,">>>,>>>,>>>,>>9.9999").
    end.

end. /**BEFORE-CHANGE-PAGE**/


if p-ind-event  = "BEFORE-CONTROL-TOOL-BAR":U and
   p-ind-object = "CONTAINER":U then do:

    run pi-armazena-handle.

    find first ext-item-uni-estab01 where ext-item-uni-estab01.cod-estabel = wgh-ce0330-cod-estabel:screen-value and
                                         ext-item-uni-estab01.it-codigo   = wgh-ce0330-it-codigo:screen-value no-lock no-error.
    if avail ext-item-uni-estab01 then do:

        assign fi-ce0330-Qtde-Segur:screen-value      = string(ext-item-uni-estab01.estoque-seguranca,">>>,>>>,>>9.99")
               fi-ce0330-ponto-encomenda:screen-value = string(ext-item-uni-estab01.ponto-encomenda,">>>,>>>,>>>,>>9.9999").
    end.
    else 
        assign fi-ce0330-Qtde-Segur:screen-value      = "0.00"
               fi-ce0330-ponto-encomenda:screen-value = "0.0000".

end. /**BEFORE-CONTROL-TOOL-BAR**/

IF p-ind-event  = "BEFORE-ASSIGN" AND
   p-ind-object = "CONTAINER"     THEN
DO:

    FIND b-item-uni-estab NO-LOCK WHERE
         ROWID(b-item-uni-estab) = p-row-table NO-ERROR.
    IF AVAIL b-item-uni-estab THEN DO:
       ASSIGN i-demanda-ant     = b-item-uni-estab.demanda
              i-quant-segur-ant = b-item-uni-estab.quant-segur.

       find first ext-item-uni-estab01 where ext-item-uni-estab01.cod-estabel = b-item-uni-estab.cod-estabel and
                                         ext-item-uni-estab01.it-codigo   = b-item-uni-estab.it-codigo no-lock no-error.
       if avail ext-item-uni-estab01 THEN
           assign i-xtivity      = ext-item-uni-estab01.estoque-seguranca.

    END.
END. /* p-ind-event  = "BEFORE-ASSIGN" */

IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    THEN
DO:

    FIND b-item-uni-estab NO-LOCK WHERE
         ROWID(b-item-uni-estab) = p-row-table NO-ERROR.
    IF AVAIL b-item-uni-estab THEN DO:

        FIND FIRST b-item NO-LOCK WHERE
                   b-item.it-codigo = b-item-uni-estab.it-codigo NO-ERROR.

        IF b-item-uni-estab.demanda <> i-demanda-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "demanda"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-item.fm-codigo
                   ext-audit-item-fam.programa  = "ce0330"
                   ext-audit-item-fam.usuario   = c-seg-usuario.

            CASE i-demanda-ant:
                 WHEN 0 THEN ASSIGN ext-audit-item-fam.valor-ant = "".
                 WHEN 1 THEN ASSIGN ext-audit-item-fam.valor-ant = "Dependente".
                 WHEN 2 THEN ASSIGN ext-audit-item-fam.valor-ant = "Independente".
            END CASE.

            CASE b-item-uni-estab.demanda:
                 WHEN 0 THEN ASSIGN ext-audit-item-fam.valor-atu = "".
                 WHEN 1 THEN ASSIGN ext-audit-item-fam.valor-atu = "Dependente".
                 WHEN 2 THEN ASSIGN ext-audit-item-fam.valor-atu = "Independente".
            END CASE.
        END.

        IF b-item-uni-estab.quant-segur <> i-quant-segur-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "quant-segur"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-item.fm-codigo
                   ext-audit-item-fam.programa  = "ce0330"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-quant-segur-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item-uni-estab.quant-segur).

        END.

        IF i-xtivity <> DEC(fi-ce0330-Qtde-Segur:SCREEN-VALUE) THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "quant-segur (xtivity)"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = b-item.fm-codigo
                   ext-audit-item-fam.programa  = "ce0330"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-xtivity)
                   ext-audit-item-fam.valor-atu = STRING(fi-ce0330-Qtde-Segur:SCREEN-VALUE).

        END.

    END. /* AVAIL b-item-uni-estab */

END. /* p-ind-event  = "AFTER-ASSIGN"  */


procedure pi-armazena-handle:
    assign wgh-objeto = p-wgh-frame:first-child.
    
    do while valid-handle(wgh-objeto):

        case wgh-objeto:type:
            when "fill-in":U then do:
                if wgh-objeto:name = "it-codigo" then
                    assign wgh-ce0330-it-codigo = wgh-objeto:handle.
                if wgh-objeto:name = "cod-estabel" then
                    assign wgh-ce0330-cod-estabel = wgh-objeto:handle.
            end.
            when "frame" then do:
                if wgh-objeto:name = "fPage2" then
                    assign wgh-ce0330-fPage2 = wgh-objeto:handle.
                if wgh-objeto:name = "fPage3" then
                    assign wgh-ce0330-fPage3 = wgh-objeto:handle.
            end.
        end case.

        if valid-handle(wgh-objeto) then
            if wgh-objeto:type = "field-group" then 
                assign wgh-objeto = wgh-objeto:first-child.
            else 
                assign wgh-objeto = wgh-objeto:next-sibling.
        
    end.

    assign wgh-objeto-2 = wgh-ce0330-fPage3:first-child.

    do while valid-handle(wgh-objeto-2):

        case wgh-objeto-2:type:
            when "fill-in":U then do:
                if wgh-objeto-2:name = "ind-cons-prv" then
                    assign wgh-ce0330-ind-cons-prv = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "tempo-segur" then
                    assign wgh-ce0330-tempo-segur = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "quant-segur" then
                    assign wgh-ce0330-quant-segur = wgh-objeto-2:handle.
            end.
            when "combo-box" then do:
                if wgh-objeto-2:name = "cb-perm-saldo-neg" then
                    assign wgh-ce0330-cb-perm-saldo-neg = wgh-objeto-2:handle.
            end.                                                              
        end case.
        
        if valid-handle(wgh-objeto-2) then
            if wgh-objeto-2:type = "field-group" then 
                assign wgh-objeto-2 = wgh-objeto-2:first-child.
            else 
                assign wgh-objeto-2 = wgh-objeto-2:next-sibling.
    end. 

    assign wgh-objeto-3 = wgh-ce0330-fPage2:first-child.

    do while valid-handle(wgh-objeto-3):

        case wgh-objeto-3:type:
            when "fill-in":U then do:
                if wgh-objeto-3:name = "ponto-encomenda" then
                    assign wgh-ce0330-ponto-encomenda = wgh-objeto-3:handle.
            end.
        end case.
        
        if valid-handle(wgh-objeto-3) then
            if wgh-objeto-3:type = "field-group" then 
                assign wgh-objeto-3 = wgh-objeto-3:first-child.
            else 
                assign wgh-objeto-3 = wgh-objeto-3:next-sibling.
    end.
end procedure.
