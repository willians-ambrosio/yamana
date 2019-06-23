/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CD0422C 2.00.00.001}  /*** 010001 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i cd0422c MCD}
&ENDIF

/*****************************************************************************
**
**       Programa: cd0422c.p
**
**       Data....: 01/10/2002
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem Fornecedores
**
**       VersÆo..: 1.00.000 
**
**       OBS.....: Este fonte foi gerado pelo Data Viewer
**
****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var c-prog-gerado           as char    no-undo initial "CDR0212".
def var c-branco                as char.
def var h-acomp                 as handle  no-undo.
def var v-cod-destino-impres    as char    no-undo.
def var v-num-point             as int     no-undo.
def var v-num-set               as int     no-undo.
def var v-num-linha             as int     no-undo.
def var l-imprime               as logical no-undo.
def var h-api029                as handle  no-undo.
def var c-emitente              as char    no-undo.
def var c-nome-emit             as char format 'x(20)' no-undo.
def var c-pais                  as char format 'x(18)' no-undo.

def var i-situacao      as integer no-undo.
def var dt-vig-ini      as date    label "Vigˆncia Inicial"   format 99/99/9999 no-undo.
def var dt-vig-fim      as date    label "Vigˆncia Final"     format 99/99/9999 no-undo.
def var c-desc-situacao as char    label "Situa‡Æo"           format "x(30)"    no-undo.

def input param raw-param as raw no-undo.

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/
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
    field i-cod-emitente-ini  as integer 
    field i-cod-emitente-fim  as integer
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

def temp-table tt-editor no-undo
    field linha         as integer
    field conteudo      as character format "x(80)"
    index editor-id is primary unique linha.

{utp/ut-glob.i}
{cdp/cd0669.i}/*defini‡Æo da tt-erro*/

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 
form emitente.cgc           format "x(19)" colon 025
     emitente.ins-estadual  format "X(19)" colon 070
     emitente.cod-emitente  format ">>>>>>>>9" colon 105 skip
     emitente.nome-abrev    format "X(12)" colon 025
     emitente.nome-emit     format "X(40)" colon 070 skip
     emitente.nome-matriz   format "x(12)" colon 025
     emitente.endereco      format "X(40)" colon 070 skip
     emitente.cod-gr-forn   format ">9"    colon 025
     emitente.bairro        format "X(30)" colon 070 skip
     emitente.cidade        format "X(25)" colon 025
     emitente.estado        format "x(4)"  colon 070
     emitente.cep           format "x(12)" colon 105 skip
     emitente.caixa-postal  format "X(10)" colon 025
     emitente.telefax       format "x(15)" colon 070
     emitente.telefone[2]   format "x(15)" colon 105 skip
     emitente.telefone[1]   format "x(15)" colon 025
     emitente.telef-modem   format "x(15)" colon 070
     emitente.ramal-fax     format "x(05)" colon 105 skip
     emitente.telef-fac     format "x(15)" colon 025
     emitente.nat-operacao  format "9.99-xxx" colon 070
     emitente.linha-produt  format "X(08)"  colon 105 skip
     emitente.telex         format "x(15)"  colon 025
     emitente.taxa-financ   format ">>9.99" colon 070
     emitente.cod-cond-pag  format ">>9"    colon 105 skip
     emitente.atividade     format "X(12)"  colon 025
     emitente.cod-transp    format ">>,>>9" colon 070
     emitente.data-taxa     format "99/99/9999" colon 105 skip
     emitente.bonificacao label "Bonifica‡Æo" format ">>9.99" colon 025
     emitente.pais          format "X(20)"  colon 070
     emitente.cod-banco     format "999"    colon 105 skip
     emitente.emissao-ped   format ">9"     colon 025
     emitente.conta-corren  format "x(20)"  colon 070
     emitente.agencia       format "x(8)"   colon 105 skip
     c-desc-situacao        format "x(27)"      colon 025
     dt-vig-ini             format "99/99/9999" colon 070
     dt-vig-fim             format "99/99/9999" colon 105 SKIP
     &if '{&bf_mat_versao_ems}' >= '2.05' &then
     emitente.log-controla-val-max-inss LABEL "Controla Valor Maximo Inss" colon 025
     emitente.cod-inscr-inss format "x(20)"                                colon 070
     &endif
     c-branco no-label  format "x(10)" at 002
     with down width 132 side-labels no-box stream-io frame f-relat-09-132.

run utp/ut-trfrrp.p (input frame f-relat-09-132:handle).

assign emitente.bonificacao:label in frame f-relat-09-132 = "Bonifica‡Æo".
{utp/ut-liter.i Bonifica‡Æo * R}
if length(trim(return-value)) < length(emitente.bonificacao:label in frame f-relat-09-132) then
  assign emitente.bonificacao:label in frame f-relat-09-132 = fill(" ",(length(emitente.bonificacao:label in frame f-relat-09-132) - length(trim(return-value)))) + trim(return-value).
else
  if length(trim(return-value)) >= length(emitente.bonificacao:label in frame f-relat-09-132) then
     assign emitente.bonificacao:label in frame f-relat-09-132 = substring(trim(return-value),1,length(emitente.bonificacao:label in frame f-relat-09-132)).

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

{utp/ut-liter.i Listagem_Fornecedores * R}
assign c-programa     = "cd0422"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = return-value
       c-sistema      = "".

{include/i-rpcab.i &stream="str-rp"}
{include/i-rpout.i &stream="stream str-rp"}

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario.

{varinc/var00002.i}

run utp/ut-acomp.p persistent set h-acomp.
run cdp/cdapi029.p persistent set h-api029 (input "",
                                            input 0,
                                            input ?,
                                            input 0,
                                            output i-situacao,
                                            output dt-vig-ini,
                                            output dt-vig-fim,
                                            output table tt-erro).
find first ems2cadme.empresa no-lock
     where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if   avail empresa then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

assign l-imprime = no.

{utp/ut-liter.i Acompanhamento_Relat¢rio * R}
run pi-inicializar in h-acomp(input return-value).

{utp/ut-liter.i Emitente}
assign c-emitente = return-value.

RUN pi-inicia-excel (INPUT tt-param.arquivo).

for each emitente no-lock
    where emitente.cgc >= tt-param.c-cgc-ini and 
          emitente.cgc <= tt-param.c-cgc-fim and
          emitente.cod-emitente >= tt-param.i-cod-emitente-ini and 
          emitente.cod-emitente <= tt-param.i-cod-emitente-fim and
          emitente.cod-gr-forn >= tt-param.i-cod-gr-forn-ini and 
          emitente.cod-gr-forn <= tt-param.i-cod-gr-forn-fim and
          emitente.nome-abrev >= tt-param.c-nome-abrev-ini and 
          emitente.nome-abrev <= tt-param.c-nome-abrev-fim and
          (emitente.identific  = 2 or
          emitente.identific  = 3)
    break by emitente.cgc:

    run pi-acompanhar in h-acomp(input c-emitente + ": " + STRING(emitente.cod-emitente)).

    run pi-verifica-status-emitente in h-api029 (input "",
                                                 input "",
                                                 input tt-param.dt-aval-situacao,
                                                 input emitente.cod-emitente,
                                                 output i-situacao,
                                                 output dt-vig-ini,
                                                 output dt-vig-fim,
                                                 output table tt-erro).

    case i-situacao:
        when 1 then do:
             if l-param-ativos then
                 assign c-desc-situacao = "Ativo".
             else next.
        end.
        when 2 then do: 
            if l-param-restr-cc then
                assign c-desc-situacao = "Restri‡Æo Compras".
            else next.
        end.
        when 3 then do: 
            if l-param-restr-cc-re then
                assign c-desc-situacao = "Restri‡Æo Compras Recebimento".
            else next.
        end.
        when 4 then do:
            if l-param-inativos then
                assign c-desc-situacao = "Inativo".
            else next.
        end.
    end case.

    assign c-branco = "                   ".
    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.

    RUN pi-dados-excel.

    display stream str-rp emitente.cgc
                          emitente.ins-estadual
                          emitente.cod-emitente
                          emitente.nome-abrev
                          emitente.nome-emit
                          emitente.nome-matriz
                          emitente.endereco
                          emitente.cod-gr-forn
                          emitente.bairro
                          emitente.cidade
                          emitente.estado
                          emitente.cep
                          emitente.caixa-postal
                          emitente.telefax
                          emitente.telefone[2]
                          emitente.telefone[1]
                          emitente.telef-modem
                          emitente.ramal-fax
                          emitente.telef-fac
                          emitente.nat-operacao
                          emitente.linha-produt
                          emitente.telex
                          emitente.taxa-financ
                          emitente.cod-cond-pag
                          emitente.atividade
                          emitente.cod-transp
                          emitente.data-taxa
                          emitente.bonificacao
                          emitente.pais
                          emitente.cod-banco
                          emitente.emissao-ped
                          emitente.conta-corren
                          emitente.agencia
                          c-desc-situacao
                          dt-vig-ini
                          dt-vig-fim
                          &if '{&bf_mat_versao_ems}' >= '2.05' &then
                          emitente.log-controla-val-max-inss
                          emitente.cod-inscr-inss  
                          &endif
                          c-branco
         with stream-io frame f-relat-09-132.
    down stream str-rp with frame f-relat-09-132.
end.

if  l-imprime = no then do:
    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

run pi-finalizar in h-acomp.

if  tt-param.destino <> 1 then
    page stream str-rp.

{include/i-rpclo.i &STREAM="stream str-rp"}

RUN pi-encerra-excel (INPUT tt-param.destino).

{esp/escd0422.i}

/* fim do programa */


procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

return 'OK'.

/* Fim da procedure */
