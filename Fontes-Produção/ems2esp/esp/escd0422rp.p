/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCD0422RP 12.1.17.000}  /*** 010001 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i escd0422rp MCD}
&ENDIF

/*******************************************************************************
**
**       Programa: ESCD0422RP.P
**
**       Data....: Outubro 2002
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem de Fornecedores.
**
**       Versao..: 1.00.000 
**
*******************************************************************************/
/* Defini‡Æo da temp-tables comuns */
define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as integer
    &IF "{&mguni_version}" >= "2.071" &THEN
    field ep-codigo           LIKE ems2cadme.empresa.ep-codigo
    &ELSE
    field ep-codigo           as integer
    &ENDIF
    field classifica          as integer
    field desc-classifica     as char
    field i-cod-emitente-ini  as integer format ">>>>>>>>9" 
    field i-cod-emitente-fim  as integer format ">>>>>>>>9"
    field c-nome-abrev-ini    as char
    field c-nome-abrev-fim    as char
    field c-cgc-ini           as char
    field c-cgc-fim           as char
    field i-cod-gr-forn-ini   as integer
    field i-cod-gr-forn-fim   as integer
    field l-param-ativos      as logical
    field l-param-restr-cc    as logical
    field l-param-restr-cc-re as logical
    field l-param-inativos    as logical
    field dt-aval-situacao    as date
    field parametro           as logical.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita as raw.

/*Defini‡Æo das Includes*/
{utp/ut-glob.i}
{include/i-rpvar.i}

/*Defini‡Æo das vari veis*/
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

def var i-num-ped-exec-rpw-aux  as CHAR LABEL "Execu‡Æo"                        no-undo.
def var c-lb-selecao            as char LABEL "SELE€ÇO"                         no-undo.
def var c-lb-classificacao      as char LABEL "CLASSIFICA€ÇO"                   no-undo.
def var c-lb-param              as char LABEL "PAR¶METROS"                      no-undo.
def var c-lb-impressao          as CHAR LABEL "IMPRESSÇO"                       no-undo.
def var l-param-ativos-aux      as CHAR LABEL "Ativos"                          no-undo.    
def var l-param-restr-cc-aux    as char LABEL "Restri‡Æo Compras"               no-undo.
def var l-param-restr-cc-re-aux as char LABEL "Restri‡Æo Compras Recebimento"   no-undo.
def var l-param-inativos-aux    as char LABEL "Inativos"                        no-undo.
def var parametro-aux           as char LABEL "Imprimir P gina Parƒmetros"      no-undo.
def var v-cod-destino-impres    as CHAR LABEL "Destino"                         no-undo.

/*Defini‡Æo da FORM*/
FORM
      c-lb-selecao                 colon 25 no-label format "x(20)"skip(1)   
      i-cod-emitente-ini           colon 45 "|< >|"   at 66 i-cod-emitente-fim no-label
      c-nome-abrev-ini             colon 45 "|< >|"   at 66 c-nome-abrev-fim   no-label
      c-cgc-ini                    colon 45 "|< >|"   at 66 c-cgc-fim          no-label format "x(20)"
      i-cod-gr-forn-ini            colon 45 "|< >|"   at 66 i-cod-gr-forn-fim  no-label skip(1)
      c-lb-classificacao           colon 25 no-label format "x(20)" skip(1)
      tt-param.desc-classifica     colon 45 no-label format "x(20)" skip(1)
      c-lb-param                   colon 25 no-label format "x(20)" skip(1) 
      tt-param.l-param-ativos      colon 45 label "Ativos"
      tt-param.l-param-restr-cc    colon 45 label "Restri‡Æo Compras"
      tt-param.l-param-restr-cc-re colon 45 label "Restri‡Æo Compras Recebimento"
      tt-param.l-param-inativos    colon 45 label "Inativos"
      dt-aval-situacao             colon 45 format 99/99/9999 skip(1) 
      c-lb-impressao               colon 25 no-label format "x(20)" skip(1)
      v-cod-destino-impres         colon 45 format "x(10)"   " - " at 60 tt-param.arquivo no-label format "x(40)"    
      i-num-ped-exec-rpw-aux       colon 45 
      tt-param.parametro           colon 45 label "Imprimir P gina Parƒmetros"
      tt-param.usuario             colon 45 
      with stream-io side-labels width 132 frame f-imp.

run utp/ut-trfrrp.p (input frame f-imp:handle).

create tt-param.
raw-transfer raw-param to tt-param.

if  tt-param.classifica = 1 then 
    run esp/escd0422a.p (input raw-param). /* Por C¢digo         */
if  tt-param.classifica = 2 then 
    run esp/escd0422b.p (input raw-param). /* Por Nome Abreviado */
if  tt-param.classifica = 3 then 
    run esp/escd0422c.p (input raw-param). /* Por CGC/CNPJ       */
if  tt-param.classifica = 4 then 
    run esp/escd0422d.p (input raw-param). /* Por Grupo          */
if  tt-param.classifica = 5 then 
    run esp/escd0422e.p (input raw-param). /* Por RazÆo Social   */

/***********************ImpressÆo da p gina de parƒmetros***********************/
{include/i-rpcab.i &stream="str-rp" }
{include/i-rpout.i &stream="stream str-rp" &append=append }

/*****Informa‡äes do cabe‡alho*****/
{utp/ut-liter.i Listagem_Fornecedores * R}
assign c-programa     = "cd0422"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = return-value
       c-sistema      = "".

find first ems2cadme.empresa no-lock
     where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if   avail empresa then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

/*****Informa‡äes dos parƒmetros selecionados*****/
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else do:
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".
end.

if  tt-param.parametro then do:
    assign i-num-ped-exec-rpw-aux  = if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".

    {utp/ut-liter.i Sim/NÆo}
    assign tt-param.l-param-ativos:format      in frame f-imp = trim(return-value)
           tt-param.l-param-restr-cc:format    in frame f-imp = trim(return-value)
           tt-param.l-param-restr-cc-re:format in frame f-imp = trim(return-value)
           tt-param.l-param-inativos:format    in frame f-imp = trim(return-value)
           tt-param.parametro:format           in frame f-imp = trim(return-value).
    
    {utp/ut-liter.i Situa‡Æo}
    assign tt-param.dt-aval-situacao:label in frame f-imp = trim(return-value).
    {utp/ut-liter.i C¢digo}
    assign i-cod-emitente-ini:label        in frame f-imp = trim(return-value).
    {utp/ut-liter.i Nome Abreviado}
    assign c-nome-abrev-ini:label          in frame f-imp = trim(return-value).
    {utp/ut-liter.i CGC/CPF}
    assign c-cgc-ini:label                 in frame f-imp = trim(return-value).
    {utp/ut-liter.i Grupo}
    assign i-cod-gr-forn-in:label          in frame f-imp = trim(return-value).
    {utp/ut-liter.i Por}
    assign tt-param.desc-classifica                       = trim(return-value) + ' ' + tt-param.desc-classifica.

    {utp/ut-liter.i SELE€ÇO}
    assign c-lb-selecao = return-value.
    {utp/ut-liter.i CLASSIFICA€ÇO}
    assign c-lb-classificacao = return-value.
    {utp/ut-liter.i PAR¶METROS}
    assign c-lb-param = return-value.
    {utp/ut-liter.i IMPRESSÇO}
    assign c-lb-impressao = return-value.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    disp stream str-rp 
          c-lb-selecao
          i-cod-emitente-ini     
          i-cod-emitente-fim
          c-nome-abrev-ini
          c-nome-abrev-fim
          c-cgc-ini
          c-cgc-fim
          i-cod-gr-forn-ini
          i-cod-gr-forn-fim
          c-lb-classificacao           
          tt-param.desc-classifica        
          c-lb-param             
          tt-param.l-param-ativos          
          tt-param.l-param-restr-cc        
          tt-param.l-param-restr-cc-re     
          tt-param.l-param-inativos        
          dt-aval-situacao       
          c-lb-impressao           
          v-cod-destino-impres   
          arquivo
          i-num-ped-exec-rpw-aux 
          tt-param.parametro      
          tt-param.usuario       
          with stream-io side-labels overlay row 032 frame f-imp.
end.

else
    output stream str-rp close.

{include/i-rpclo.i &STREAM="stream str-rp"}
