/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES0003 2.00.00.002}  /*** 010001 ***/

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

define variable h-acomp        as handle    no-undo.
define variable cLinha         as character no-undo.
define variable iLinha         as integer   no-undo.
define variable c-cod-estabel  as character no-undo.
define variable c-it-codigo    as character no-undo.
define variable de-estoq-segu  like ext-item-uni-estab01.estoque-seguranca  .
define variable de-tempo-ressu like ext-item-uni-estab01.tempo-ressuprimento.
DEFINE VARIABLE de-ponto-encom LIKE ext-item-uni-estab01.ponto-encomenda.
                              
define temp-table tt-log
  field linha        as int       format ">>>,>>9"
  field cod-estabel  as character format "x(03)"
  field it-codigo    as character format "x(16)"
  field c-log        as character format "x(80)".

function fLog returns logical ( pLog as char ):

    create tt-log.
    assign tt-log.linha       = iLinha    
           tt-log.it-codigo   = c-it-codigo    
           tt-log.cod-estabel = c-cod-estabel
           tt-log.c-log       = trim(pLog).

    return false.

end function.

form tt-log.linha       label "Linha"     at 01
     tt-log.cod-estabel label "Estab"     at 10
     tt-log.it-codigo   label "Item"      at 20
     tt-log.c-log       label "Historico" at 40
     with width 132 down no-box frame f-log stream-io.

run utp/ut-trfrrp.p (input frame f-log:handle).

def stream s-imp.
input stream s-imp from value(tt-param.arq-entrada).
{include/i-rpout.i &tofile = tt-param.arq-destino} 

/***  Inicio do programa  ***/

find first param-global no-lock no-error.
find first param-estoq no-lock no-error.
find first tt-param no-lock no-error.

{utp/ut-liter.i Importa‡Æo_da_Tabela_Xtivity}
assign c-titulo-relat = trim(return-value).

assign c-programa    = "ES/0001"
       c-versao      = "2.00"  
       c-revisao     = "00.002".

empty temp-table tt-log.

if  search(tt-param.arq-entrada) = ? then do:
    fLog(subst("Arquivo '&1' nÆo encontrado!",tt-param.arq-entrada)).
    run utp/ut-msgs.p (input "msg", /*Alterado*/
                      input 326,
                      input tt-param.arq-entrada). 
end.
else do:

    fLog(subst("Arquivo: '&1'",tt-param.arq-entrada)).
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input  Return-value ). 
    run PI-LEITURA-REGISTROS.
    run pi-finalizar in h-acomp.  

end.

{include/i-rpcab.i}
view frame f-cabec.
view frame f-rodape. 

FOR EACH tt-log:
    DISP tt-log.linha when tt-log.linha > 0
         tt-log.it-codigo    
         tt-log.cod-estabel  
         tt-log.c-log       
        WITH FRAME f-log WIDTH 132 DOWN STREAM-IO.
    DOWN WITH FRAME f-log.      
end.

{include/i-rpclo.i}. 

procedure pi-leitura-registros:

    assign iLinha = 0.
    
    if  tt-param.arq-entrada <> "" then do:

        input from value (tt-param.arq-entrada) no-convert no-echo.
        
        repeat on error undo, leave
               on stop undo, return "nok":u:

            assign iLinha         = iLinha + 1
                   c-cod-estabel  = ""
                   c-it-codigo    = ""
                   cLinha         = "".

            import unformatted cLinha.

            if cLinha = "" then next.

            release tt-log.

            if  num-entries(cLinha,";") < 5 then do:
                fLog("ERRO: Quantidade invalida de campos na linha!").
                next.
            end.

            assign c-cod-estabel  = right-trim(entry(1,cLinha,";"))  no-error.   /** Estabelecimento **/
            assign c-it-codigo    = right-trim(entry(2,cLinha,";"))  no-error.   /** Item **/  
            assign de-estoq-segu  = dec  (trim(entry(3,cLinha,";"))) no-error.   /** Estoque de seguran‡a **/
            assign de-tempo-ressu = int  (trim(entry(4,cLinha,";"))) no-error.   /** Tempo de Ressuprimento Total **/ 
            assign de-ponto-encom = dec  (trim(entry(5,cLinha,";"))) no-error.   /** Ponto de Encomenta **/

            if  not can-find ( first item no-lock where item.it-codigo = c-it-codigo ) then
                fLog("ERRO: Item inexistente!").

            if  not can-find ( first estabelec no-lock where estabelec.cod-estabel = c-cod-estabel ) then
                fLog("ERRO: Estabelecimento inexistente!").
            
            if  not can-find ( first item-uni-estab no-lock 
                               where item-uni-estab.it-codigo   = c-it-codigo   
                               and   item-uni-estab.cod-estabel = c-cod-estabel ) then
                fLog("ERRO: Relacionamento Estab x Item inexistente!").

            if avail tt-log then next.

            fLog("Registro importado com sucesso!").

            create log-ext-item-uni-estab01.
            assign log-ext-item-uni-estab01.cod-estabel	        = c-cod-estabel
                   log-ext-item-uni-estab01.it-codigo	        = c-it-codigo
                   log-ext-item-uni-estab01.estoque-seguranca   = ( if de-estoq-segu  = ? then 0 else de-estoq-segu  )
                   log-ext-item-uni-estab01.tempo-ressuprimento = ( if de-tempo-ressu = ? then 0 else de-tempo-ressu )
                   log-ext-item-uni-estab01.ponto-encomenda     = ( if de-ponto-encom = ? then 0 else de-ponto-encom )
                   log-ext-item-uni-estab01.usuario             = c-seg-usuario
                   log-ext-item-uni-estab01.DataHora            = now.

            find first ext-item-uni-estab01 exclusive-lock of log-ext-item-uni-estab01 no-error.
            if not avail ext-item-uni-estab01 then create ext-item-uni-estab01.
            buffer-copy log-ext-item-uni-estab01 to ext-item-uni-estab01.

            release log-ext-item-uni-estab01.
            release ext-item-uni-estab01.
            
        end.

        input close.

    end.

    return "ok":u.

end procedure.
