/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES0001 2.00.00.001}  /*** 010001 ***/

{include/i_fnctrad.i}

{include/i-rpvar.i}

{utp/ut-glob.i}

/* Defini‡Æo e Prepara‡Æo dos Parƒmetros */
def temp-table tt-param
    field destino         as integer
    field arq-destino     as char
    field arq-entrada     as char
    field todos           as integer
    field usuario         as char
    field data-exec       as date
    field hora-exec       as integer.

def temp-table tt-raw-digita
    field raw-digita      as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

 /*Variavel que verifica qual usuario esta logado*/
define new global shared variable c-seg-usuario as character format "x (12)" no-undo.

define variable i-erros        as inte extent 2 format ">>>>>>>9" init 0 no-undo.
define variable c-imp-erro     as character format "x(132)"   no-undo.
define variable h-acomp        as handle    no-undo.
define variable cLinha         as character no-undo.
define variable iLinha         as integer   no-undo.
define variable c-ok           as character format "x(132)" no-undo.
define variable c-alt          as character format "x(25)"  no-undo.
define variable c-cod-estabel  as character no-undo.
define variable c-it-codigo    as character no-undo.
define variable de-estoq-segu  like ext-item-uni-estab01.estoque-seguranca.
define variable de-tempo-ressu like ext-item-uni-estab01.tempo-ressuprimento.
                              
define temp-table tt-erros-valid
  field it-codigo    as character format "x(16)"
  field cod-estabel  as character format "x(03)"
  field c-erro       as character format "x(50)".

form c-imp-erro
     with width 132 no-box frame f-erro stream-io.

form tt-erros-valid.it-codigo   label "Item"    to 16
     tt-erros-valid.cod-estabel label "Estab"   to 22
     tt-erros-valid.c-erro      label "Erro"    
    with width 132 down no-box frame f-erro-1 stream-io.

form c-ok
     with width 132 down no-box frame f-ok stream-io.

run utp/ut-trfrrp.p (input frame f-erro:handle).
run utp/ut-trfrrp.p (input frame f-erro-1:handle).
run utp/ut-trfrrp.p (input frame f-ok:handle).

{utp/ut-liter.i ERROS_NO_ARQUIVO_DE_IMPORTA€ÇO}
assign c-imp-erro:label in frame f-erro = trim(return-value).

{utp/ut-liter.i Importa‡Æo_realizada_com_sucesso.}
assign c-ok:label in frame f-ok = trim(return-value).

def stream s-imp.
input stream s-imp from value(tt-param.arq-entrada).
{include/i-rpout.i &tofile = tt-param.arq-destino} 


/***  Inicio do programa  ***/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input  Return-value ). 


find first param-global no-lock no-error.
find first param-estoq no-lock no-error.
find first tt-param no-lock no-error.

{utp/ut-liter.i Importa‡Æo_da_Tabela_Xtivity}
assign c-titulo-relat = trim(return-value).

assign c-programa    = "ES/0001"
       c-versao      = "2.00"  
       c-revisao     = "00.001".



assign i-erros = 0.
if search(tt-param.arq-entrada) = ? then do:
    run utp/ut-msgs.p (input "msg", /*Alterado*/
                      input 326,
                      input tt-param.arq-entrada).  
end.

input from value(tt-param.arq-entrada) CONVERT TARGET "ibm850" no-echo.
FOR EACH tt-erros-valid:
    DELETE tt-erros-valid.
END.

{include/i-rpcab.i}

view frame f-cabec.
view frame f-rodape. 


run PI-LEITURA-REGISTROS.

IF  CAN-FIND(FIRST tt-erros-valid) THEN DO:
    put "ERROS NO ARQUIVO DE IMPORTA€ÇO" skip(1).
    FOR EACH tt-erros-valid:
        DISP tt-erros-valid.it-codigo    
             tt-erros-valid.cod-estabel  
             tt-erros-valid.c-erro       
            WITH FRAME f-erro-1 WIDTH 132 DOWN STREAM-IO.
        DOWN WITH FRAME f-erro-1.      

        DELETE TT-ERROS-VALID.
    end.
    run pi-finalizar in h-acomp.  

    {include/i-rpclo.i}. 

    return "NOK":U.
end.

if return-value = 'OK' then do:

    disp c-ok 
        WITH FRAME f-ok WIDTH 132 DOWN STREAM-IO.
    DOWN WITH FRAME f-ok.    

    run pi-finalizar in h-acomp.  

    {include/i-rpclo.i}. 
end.

procedure pi-leitura-registros:

    assign iLinha = 0.
    
    if tt-param.arq-entrada <> "" then do: /* Importa‡Æo */

        input from value (tt-param.arq-entrada).
        
        repeat on error undo, leave
               on stop undo, return "nok":u:

            import unformatted cLinha.

            if cLinha = "" then
                next.

            assign iLinha         = iLinha + 1
                   c-cod-estabel  = right-trim(entry(1,cLinha,";"))        /**Estabelecimento**/
                   c-it-codigo    = right-trim(entry(2,cLinha,";"))        /**Item**/  
                   de-estoq-segu  = dec(right-trim(entry(3,cLinha,";")))   /**Estoque de seguran‡a**/
                   de-tempo-ressu = dec(right-trim(entry(4,cLinha,";"))).  /**Tempo de Ressuprimento Total**/ 

            find first item where item.it-codigo = c-it-codigo no-lock no-error.
            if not avail item then do:
                create tt-erros-valid.
                assign tt-erros-valid.it-codigo   = c-it-codigo    
                       tt-erros-valid.cod-estabel = c-cod-estabel
                       tt-erros-valid.c-erro      = "Item inexistente!".
                next.
            end. /* Item */

            find first estabelec where estabelec.cod-estabel = c-cod-estabel no-lock no-error.
            if not avail estabelec then do:
                create tt-erros-valid.
                assign tt-erros-valid.it-codigo    = c-it-codigo    
                       tt-erros-valid.cod-estabel = c-cod-estabel
                       tt-erros-valid.c-erro       = "Estabelecimento inexistente!".
                next.
            end. /* Estabelec */
            
            find first item-uni-estab where item-uni-estab.it-codigo  = c-it-codigo    and
                                             item-uni-estab.cod-estabel = c-cod-estabel no-lock no-error.
            if not avail item-uni-estab then do:
                create tt-erros-valid.
                assign tt-erros-valid.it-codigo   = c-it-codigo    
                       tt-erros-valid.cod-estabel = c-cod-estabel
                       tt-erros-valid.c-erro      = "NÆo existe relacionamento entre Item/Estabelecimento!".
                next.
            end. /* Item-uni-estab */

            find first ext-item-uni-estab01 where ext-item-uni-estab01.it-codigo   = c-it-codigo and
                                                 ext-item-uni-estab01.cod-estabel = c-cod-estabel exclusive-lock no-error.
            if avail ext-item-uni-estab01 then do:
                assign ext-item-uni-estab01.estoque-seguranca   = de-estoq-segu
                       ext-item-uni-estab01.tempo-ressuprimento = de-tempo-ressu.

                create log-ext-item-uni-estab01.
                assign log-ext-item-uni-estab01.usuario             = c-seg-usuario
                       log-ext-item-uni-estab01.data-alteracao      = today
                       log-ext-item-uni-estab01.hora-alteracao      = string(time,"hh:mm:ss")
                       log-ext-item-uni-estab01.cod-estabel	      = c-cod-estabel
                       log-ext-item-uni-estab01.it-codigo	          = c-it-codigo
                       log-ext-item-uni-estab01.estoque-seguranca	  = de-estoq-segu
                       log-ext-item-uni-estab01.tempo-ressuprimento = de-tempo-ressu.
            end.
            else do:
            
                create ext-item-uni-estab01.
                assign ext-item-uni-estab01.it-codigo           = c-it-codigo
                       ext-item-uni-estab01.cod-estabel         = c-cod-estabel
                       ext-item-uni-estab01.estoque-seguranca   = de-estoq-segu
                       ext-item-uni-estab01.tempo-ressuprimento = de-tempo-ressu.

                create log-ext-item-uni-estab01.
                assign log-ext-item-uni-estab01.usuario             = c-seg-usuario
                       log-ext-item-uni-estab01.data-alteracao      = today
                       log-ext-item-uni-estab01.hora-alteracao      = string(time,"hh:mm:ss")
                       log-ext-item-uni-estab01.cod-estabel	      = c-cod-estabel
                       log-ext-item-uni-estab01.it-codigo	          = c-it-codigo
                       log-ext-item-uni-estab01.estoque-seguranca	  = de-estoq-segu
                       log-ext-item-uni-estab01.tempo-ressuprimento = de-tempo-ressu.

            end.
            
            
        end. /* ext-item-uni-estab01 */                                       
    end. /* Importa‡Æo */
    
        input close.


    return "ok":u.

end procedure.
