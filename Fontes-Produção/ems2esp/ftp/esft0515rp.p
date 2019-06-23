/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i ESFT0515RP 2.00.00.045}  /*** 010045 ***/
/*******************************************************************************
**
**  PROGRAMA:  esft0515rp.p
**  DATA    :  Fevereiro de 1997
**  AUTOR   :  DATASUL DESENVOLVIMENTO DE SISTEMAS S.A.
**  OBJETIVO:  Emissao de Notas Fiscais.
**  VERSAO  :  ESFT0515.I99
**
******************************************************************************
**  ALTERADO:  AGOSTO/03 - Adapta‡Æo para EmissÆo de NF no layout da MFB
******************************************************************************/

{cdp/cdcfgdis.i} /*Include de defini‡Æo de Preprocessadores*/
{cdp/cd0620.i1 "' '"}


def temp-table tt-raw-digita 
    field raw-digita as raw.

{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */

def input parameter raw-param as raw no-undo.   
def input parameter table for tt-raw-digita.

{bcp/bcapi004.i}
{cdp/cd0666.i}

/*************************
*   definicao de buffer
*************************/

def buffer b-nota-fiscal     for nota-fiscal.

/****************************
*   definicao de work-file
****************************/

def new shared temp-table item-nota no-undo
               field registro        as rowid
               field it-codigo     like it-nota-fisc.it-codigo
               field aliquota-icm  like it-nota-fisc.aliquota-icm
               field nr-seq-fat    like it-nota-fisc.nr-seq-fat
               field sit-tribut      as integer format ">>>".

/************************************
*   definicao de variaveis globais
************************************/

def new shared var l-aliq-nat        as logical                        no-undo.
def new shared var l-tipo-nota       as logical format "Entrada/Saida" no-undo.
def new shared var de-aliquota-icm like natur-oper.aliquota-icm.
def new shared var r-ped-venda       as rowid.
def new shared var r-pre-fat         as rowid.
def new shared var r-emitente        as rowid.
def new shared var r-estabel         as rowid.
def new shared var r-docum-est       as rowid.

def new shared var r-nota            as rowid.
def new shared var de-cotacao        as decimal format ">>>,>>9.99999999".
def new shared var r-nota-fiscal     as rowid.
def new shared var i-codigo          as integer.
def new shared var i-cont            as integer.
def new shared var de-conv           as decimal format ">>>>9.99".
def new shared var r-item            as rowid.
def new shared var r-natur-oper      as rowid.
def new shared var c-num-nota        as char format "x(16)".
def new shared var c-num-duplic      as char.

def new shared var de-tot-icms-obs      like it-nota-fisc.vl-icms-it.
def new shared var de-tot-icmssubs-obs  like it-nota-fisc.vl-icmsub-it.
def new shared var de-tot-bicmssubs-obs like it-nota-fisc.vl-bsubs-it.
def new shared var de-tot-ipi-dev-obs   like it-nota-fisc.vl-ipi-it.
def new shared var de-tot-ipi-calc      like it-nota-fisc.vl-ipi-it.
def new shared var de-tot-ipi-nota      like it-nota-fisc.vl-ipi-it.

/**********************************
*   definicao de variaveis shared
**********************************/

def new shared var c-ser           like nota-fiscal.serie.
def new shared var c-est           like nota-fiscal.cod-estabel.
def new shared var da-dt-emis      like nota-fiscal.dt-emis-nota.
def new shared var i-nr-nota       like nota-fiscal.nr-nota-fis.
def new shared var i-nota-ini      as integer format ">>>,>>9".
def new shared var i-nota-fim      as integer format "999,999".
def new shared var i-embarq-ini    like nota-fiscal.cdd-embarq.
def new shared var i-embarq-fim    like nota-fiscal.cdd-embarq.
def new shared var l-dt              as logica.
def new shared var dt-saida          as date format "99/99/9999".
def new shared var hr-saida          as char format "xx:xx:xx".

/**********************************
*   definicao de tabelas globais
**********************************/

def new shared var i-parcela   as integer                             extent 6.
def new shared var i-fatura    as char      format "x(16)"            extent 6.
def new shared var da-venc-dup as date      format "99/99/9999"       extent 6.
def new shared var de-vl-dup   as decimal   format ">>>>>,>>>,>>9.99" extent 6.

/************************************
*   definicao das variaveis locais
************************************/

def var l-resposta             as logical format "Sim/Nao" init yes.
def var i-cont4                as integer.
def var i-cont6                as integer.
def var l-tem-ipi              as logical.
def var i                      as integer.
def var i-qt-volumes           as integer extent 5 format ">>,>>9".
def var i-cont-item            as integer.
def var i-dup                  as integer.
def var l-sub                  as logical.
def var de-aliquota-iss      like it-nota-fisc.aliquota-iss.
def var de-desc              like it-nota-fisc.vl-preori.
def var c-cod-suframa-est    like estabelec.cod-suframa.
def var c-cod-suframa-cli    like emitente.cod-suframa.
def var de-qt-fatur            as decimal format ">>>>,>>9.9999".
def var de-tot-icmssubs      like it-nota-fisc.vl-icmsub-it.
def var de-tot-bicmssubs     like it-nota-fisc.vl-bsubs-it.
def var de-tot-bas-icm       like it-nota-fisc.vl-icms-it.
def var de-tot-bas-iss       like it-nota-fisc.vl-iss-it.
def var de-tot-bas-ipi       like it-nota-fisc.vl-ipi-it.
def var de-tot-icm           like it-nota-fisc.vl-icms-it.
def var de-tot-iss           like it-nota-fisc.vl-iss-it.
def var de-tot-ipi           like it-nota-fisc.vl-ipi-it.
def var de-vl-bipi-it        like it-nota-fisc.vl-bipi-it.
def var de-vl-ipi-it         like it-nota-fisc.vl-ipi-it.
def var l-frete-bipi           as log.
def var i-cep                like nota-fiscal.cep.
def var c-pago                 as character format "x" init " ".
def var c-opcao                as character.
def var c-class-fiscal         as character format {cdp/cd0603.i3}.
def var c-mensagem1            as character format "x(380)".
def var c-mensagem2            as character format "x(152)".
def var c-especie              as character extent 5 format "x(30)".
def var c-desc-prod            as character format "x(42)".
def var c-repres               as character format "x(15)".
def var c-redesp               as character .
def var c-nat                  as character format "x.xxx".
def var c-un-fatur             as character format "x(2)".
def var c-tipo-venda           as character extent 6 initial
    ["1 - Laticin.","2 - Corantes","3 - Frigorif.","4 - Export.",
     "5 - Prest.Serv.","6 - Outros"] format "x(13)".
def var r-it-nota              as rowid.
def var i-sit-nota-ini         as integer.
def var i-sit-nota-fim         as integer.
def var c-formato-cfop         as char.

def var h-acomp  as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.

/* Integracao Modulo de Importacao */
def temp-table tt-desp
    field cod-desp  as integer format ">>>>9"
    field descricao as char    format "x(30)"
    field val-desp  as decimal format ">>>>>,>>>,>>9.99999"
    index codigo is unique primary
        cod-desp.
def var l-importacao as logical no-undo.

/**************************
*   definicao de tabelas
**************************/

def var c-mess     as character format "x(83)" extent 8 init "  ".
def var c-cgc      like estabelec.cgc.

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field identific        as char
    field ini-cod-estabel  as char
    field fim-cod-estabel  as char
    field ini-serie        as char
    field fim-serie        as char
    field ini-cdd-embarq  as integer
    field fim-cdd-embarq  as integer
    field ini-nr-nota-fis  as char
    field fim-nr-nota-fis  as char
    field rs-imprime       as integer
    field banco            as integer
    field cod-febraban     as integer
    field cod-portador     as integer
    field prox-bloq        as char
    field c-instrucao      as char extent 5
    field imprime-bloq     as logical.

define shared temp-table tt-notas-impressas
    field r-nota as rowid.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

create tt-param.
raw-transfer raw-param to tt-param.

define stream arq-erro.
define var c-arquivo as char format "X(40)".

assign c-arquivo =   substring(string(time,"HH:MM:SS"),1,2)
                   + substring(string(time,"HH:MM:SS"),4,2)
                   + string(day(today))
                   + string(month(today))
                   + ".lst".

/************************
*   definicao de telas
************************/
{include/i-rpvar.i}

find first param-global no-lock no-error.
if  available param-global then
    assign c-empresa = grupo.
else
    return "OK".

find first para-ped no-lock no-error.

{utp/ut-liter.i EmissÆo_de_Notas_Fiscais * C}
assign c-titulo-relat = return-value.
 
{utp/ut-liter.i Faturamento * C}
assign c-sistema = return-value.
assign c-programa = "FT/0515"
       c-versao   = "01.00"
       c-revisao  = "01".

{include/i-rpcab.i}
{include/i-rpout.i} 

{include/tt-edit.i}
{include/pi-edit.i}

run pi-inicializar in h-acomp (input c-titulo-relat).

for each tt-notas-impressas:
    delete tt-notas-impressas.
end.

if   tt-param.rs-imprime = 1 then
     assign i-sit-nota-ini = 1
            i-sit-nota-fim = 1.
else assign i-sit-nota-ini = 2
            i-sit-nota-fim = 7.

find first tt-raw-digita no-error.

if  avail tt-raw-digita then do:
    for each tt-raw-digita:
        create tt-notas-geradas.
        raw-transfer tt-raw-digita.raw-digita to tt-notas-geradas.
    end.

    for each tt-notas-geradas,
        each nota-fiscal
        where rowid(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal no-lock:

        run pi-imprime-nota.
    end.
end.
else     
    for each  nota-fiscal use-index ch-nota
        where nota-fiscal.cod-estabel   = tt-param.ini-cod-estabel
        and   nota-fiscal.serie         = tt-param.ini-serie
        and   nota-fiscal.nr-nota-fis  >= tt-param.ini-nr-nota-fis
        and   nota-fiscal.nr-nota-fis  <= tt-param.fim-nr-nota-fis
        and   nota-fiscal.cdd-embarq  >= tt-param.ini-cdd-embarq
        and   nota-fiscal.cdd-embarq  <= tt-param.fim-cdd-embarq
        and   nota-fiscal.ind-sit-nota >= i-sit-nota-ini 
        and   nota-fiscal.ind-sit-nota <= i-sit-nota-fim no-lock
        break by nota-fiscal.cod-estabel
              by nota-fiscal.serie
              by nota-fiscal.nr-nota-fis:
 
        run pi-imprime-nota.
    end.

run pi-finalizar in h-acomp.
{include/i-rpclo.i}
return "OK".



/* Procedure para impressao da nota fiscal */
procedure pi-imprime-nota:
    find b-nota-fiscal
         where rowid(b-nota-fiscal) = rowid(nota-fiscal)
         EXCLUSIVE-LOCK no-error.
    if  tt-param.data-exec <> ? then
        assign b-nota-fiscal.dt-saida = tt-param.data-exec
               dt-saida               = tt-param.data-exec
               hr-saida               = string(tt-param.hora-exec,"999999")
               l-dt                   = yes.

    find ped-venda
         where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
         and   ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
         no-lock no-error.

    find estabelec
         where estabelec.cod-estabel = nota-fiscal.cod-estabel
         no-lock no-error.

    find emitente
         where emitente.nome-abrev  = nota-fiscal.nome-ab-cli
         no-lock no-error.

    find pre-fatur use-index ch-embarque
         where pre-fatur.cdd-embarq = nota-fiscal.cdd-embarq
         and   pre-fatur.nome-abrev  = nota-fiscal.nome-ab-cli
         and   pre-fatur.nr-pedcli   = nota-fiscal.nr-pedcli
         and   pre-fatur.nr-resumo   = nota-fiscal.nr-resumo
         no-lock no-error.

    find natur-oper
         where natur-oper.nat-operacao = nota-fiscal.nat-operacao
         no-lock no-error.

    assign r-estabel    = rowid(estabelec)
           r-ped-venda  = rowid(ped-venda)
           r-emitente   = rowid(emitente)
           r-natur-oper = rowid(natur-oper)
           r-pre-fat    = if   avail pre-fatur
                          then rowid(pre-fatur)
                          else ?
           l-tipo-nota  = no.

/*----------------------------------------------------------------------------*/
                        
    assign de-conv = 1.

    find first cidade-zf where cidade-zf.cidade = nota-fiscal.cidade
                         and   cidade-zf.estado = nota-fiscal.estado
                         no-lock no-error.

    if  avail cidade-zf 
    and dec(substr(natur-oper.char-2,66,5)) > 0 then
        assign de-conv = (100 - dec(substr(natur-oper.char-2,66,5))) / 100.
        /* valor para tratamento de ZFM */
       
    {ftp/esft0515.i1}
    
    /* muda o status da nota fiscal. */
    assign r-nota = rowid(nota-fiscal).
    run "ftp/ft0503a.p".

    create tt-notas-impressas.
    assign tt-notas-impressas.r-nota = rowid(nota-fiscal).

    /* GERA ETIQUETAS PARA O MODULO DE COLETA DE DADOS */

    if  avail param-global
    and param-global.modulo-cl
    and (   tt-param.rs-imprime      = 1
         or nota-fiscal.ind-tip-nota = 2) /* tipo de nota-fiscal Manual */
    then do:
        create tt-prog-bc.
        assign tt-prog-bc.cod-prog-dtsul        = "ft0513"
               tt-prog-bc.cod-versao-integracao = 1
               tt-prog-bc.usuario               = tt-param.usuario
               tt-prog-bc.opcao                 = 1.

        run bcp/bcapi004.p (input-output table tt-prog-bc,
                            input-output table tt-erro).

        find first tt-prog-bc no-error.

        assign  c-arquivo = tt-prog-bc.nome-dir-etiq + "/" + c-arquivo.

        if  return-value = "OK" then do:

            {utp/ut-liter.i Gerando_Etiquetas  MRE R}
            run pi-acompanhar in h-acomp (input return-value).

            erro:
            do  on stop     undo erro,leave erro
                on quit     undo erro,leave erro
                on error    undo erro,leave erro
                on endkey   undo erro,leave erro:

                run value(tt-prog-bc.prog-criacao)(input tt-prog-bc.cd-trans,
                                                   input rowid(nota-fiscal),
                                                   input-output table tt-erro) no-error.

                if  ERROR-STATUS:ERROR 
                or  (    error-status:get-number(1) <> 138
                     and error-status:num-messages  <> 0)
                then do:
                    output stream arq-erro to value(c-arquivo) append.

                    {utp/ut-liter.i Ocorreu_na_Gera‡Æo_de_Etiquetas_-_Progress MRE R}
                    put stream arq-erro "***" return-value skip.
                    {utp/ut-liter.i Programa * R}
                    put stream arq-erro error-status:get-message(1) skip.
                    put stream arq-erro return-value ": " tt-prog-bc.prog-criacao skip.
                    put stream arq-erro nota-fiscal.serie                           at 1.
                    put stream arq-erro nota-fiscal.nr-nota-fis                     at 7.
                    put stream arq-erro nota-fiscal.cod-estabel                     at 24.

                    output stream arq-erro close.
                end.

                if  return-value = "NOK" then do:
                    find first tt-erro no-error.
                    if  avail tt-erro
                    then do:
                        output stream arq-erro to value(c-arquivo) append.

                        {utp/ut-liter.i Ocorreu_na_Gera‡Æo_de_Etiquetas MRE R}
                        put stream arq-erro "***" return-value skip.
                        for each tt-erro:
                            put stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                        end.
                        put stream arq-erro skip.

                        output stream arq-erro close.
                    end.
                end.
            end.
        end.
        else do:
            /**** caso tenha integra‡Æo com o coleta e ocorreu erros ***/
            find first tt-erro no-error.
            if  avail tt-erro then do:
                output stream arq-erro to value(c-arquivo) append.

                {utp/ut-liter.i Ocorreu_na_Gera‡Æo_de_Etiquetas MRE R}
                put stream arq-erro "***" return-value skip.
                for each tt-erro:
                    put  stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                end.
                put stream arq-erro skip.
                output stream arq-erro close.
            end.
        end.
    end.
    /*************************************************/
end procedure.

