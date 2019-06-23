/*******************************************************************************
**
** Programa  : ESRE0002RP
**
** Objetivo  : Relatorio de Notas Fiscais Pendentes
**
** Autor     : Renato Oliveira
**
** Data      : Janeiro/2019
**
** Versao    : 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************************/
{include/i-prgvrs.i ESRE0002RP 2.12.00.000}

/**********************  Definiá∆o tabela de ParÉmetros **********************/
define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char
    field usuario           as char
    field data-exec         as date
    field hora-exec         as INTEGER
    field cod-estabel-ini   like doc-fisico.cod-estabel
    field cod-estabel-fim   like doc-fisico.cod-estabel
    field cod-emitente-ini  like doc-fisico.cod-emitente
    field cod-emitente-fim  like doc-fisico.cod-emitente
    field dt-trans-ini      like doc-fisico.dt-trans
    field dt-trans-fim      like doc-fisico.dt-trans
    field nro-docto-ini     like doc-fisico.nro-docto
    field nro-docto-fim     like doc-fisico.nro-docto
    field usuario-ini       like doc-fisico.usuario
    field usuario-fim       like doc-fisico.usuario
    field dt-emissao-ini    like doc-fisico.dt-emissao
    field dt-emissao-fim    like doc-fisico.dt-emissao
    .

define temp-table tt-digita no-undo
    field it-codigo like ITEM.it-codigo
    index id it-codigo.
        
DEFINE temp-table tt-raw-digita
   field raw-digita      as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.    

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end. /* for each tt-raw-digita: */


/* Definicao dos Buffers */

/*------------------- Variaveis Documento --------------------*/
DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR i-contador      AS INT    NO-UNDO.
DEF VAR c-arquivo       AS CHAR   NO-UNDO.
DEF VAR v-num-reg-lidos AS INT    NO-UNDO.

/* In°cio - Bloco Principal */
find first param-global NO-LOCK NO-ERROR.
find first param-compra NO-LOCK NO-ERROR.

{include/i-rpvar.i}

assign c-programa     = "ESRE0002"
       c-versao       = "2.12"
       c-revisao      = "00.000"
       c-empresa      = param-global.grupo
       c-sistema      = "Especifico"
       c-titulo-relat = "Relat¢rio de Documentos".

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.  

RUN pi-inicializar IN h-acomp (INPUT "Aguarde, Gerando...").

{include/i-rpout.i}
{include/i-rpcab.i}
/* VIEW FRAME f-cabec.  */
/* VIEW FRAME f-rodape. */

assign v-num-reg-lidos = 0.

put unformatted 
    "Est;Emitente;Nome Abrev;Nota Fiscal;Valor Mercadoria;Data Emiss∆o;Data Recebimento;Data Atualizaá∆o;Usu†rio;Dias Pendentes;Status"
    SKIP.

for each doc-fisico no-lock
   where doc-fisico.cod-emitente >= tt-param.cod-emitente-ini
     and doc-fisico.cod-emitente <= tt-param.cod-emitente-fim
     and doc-fisico.cod-estabel  >= tt-param.cod-estabel-ini
     and doc-fisico.cod-estabel  <= tt-param.cod-estabel-fim
     and doc-fisico.dt-emissao   >= tt-param.dt-emissao-ini
     and doc-fisico.dt-emissao   <= tt-param.dt-emissao-fim
     and doc-fisico.dt-trans     >= tt-param.dt-trans-ini
     and doc-fisico.dt-trans     <= tt-param.dt-trans-fim
     and doc-fisico.nro-docto    >= tt-param.nro-docto-ini
     and doc-fisico.nro-docto    <= tt-param.nro-docto-fim
     and doc-fisico.usuario      >= tt-param.usuario-ini
     and doc-fisico.usuario      <= tt-param.usuario-fim,
    each emitente no-lock
         where emitente.cod-emitente = doc-fisico.cod-emitente:

    assign v-num-reg-lidos = v-num-reg-lidos + 1.

    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    FIND FIRST it-doc-fisico NO-LOCK 
         WHERE it-doc-fisico.serie-docto  = doc-fisico.serie-docto
           AND it-doc-fisico.nro-docto    = doc-fisico.nro-docto
           AND it-doc-fisico.cod-emitente = doc-fisico.cod-emitente
           AND it-doc-fisico.tipo-nota    = doc-fisico.tipo-nota
           AND it-doc-fisico.quantidade  <> it-doc-fisico.quant-conf NO-ERROR.

    FIND FIRST mgesp.es-doc-fisico-just NO-LOCK OF doc-fisico NO-ERROR.

    put unformatted 
        doc-fisico.cod-estabel  ";"
        doc-fisico.cod-emitente ";"
        emitente.nome-abrev     ";"
        "'" doc-fisico.nro-docto    ";"
        doc-fisico.valor-mercad ";"
        doc-fisico.dt-emissao   ";"
        doc-fisico.dt-trans     ";"
        doc-fisico.dt-atualiza  ";"
        doc-fisico.usuario      ";"
        doc-fisico.dt-trans - doc-fisico.dt-emissao ";".

    IF AVAIL it-doc-fisico THEN DO:

        IF it-doc-fisico.quant-conf = 0 THEN
            put unformatted 
                "Contagem n∆o Realizada" ";".
        ELSE
            put unformatted 
                "Com Divergància" ";".
    END.
    ELSE
        put unformatted 
            "Apto" ";".

    put unformatted 
        skip.
    
end.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.
/* Final DO Programa */
