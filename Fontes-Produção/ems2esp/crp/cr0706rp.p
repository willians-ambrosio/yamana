/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i CR0706RP 2.00.00.013}  /*** 010013 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i cr0706rp MCR}
&ENDIF

/******************************************************************************
***
***  crp/cr0706rp.p - Listagem Clientes
***
*******************************************************************************/

{utp/ut-glob.i}
{crp/cr0706d.i}

{cdp/cdcfgfin.i}

define temp-table tt-estat no-undo
       field destino              as integer
       field arquivo              as char format "x(35)"
       field usuario              as char format "x(12)"
       field data-exec            as date
       field hora-exec            as integer
       field tipo-relat           as INTEGER
       FIELD cod-emitente         LIKE titulo.cod-emitente
       FIELD nome-abrev           LIKE titulo.nome-abrev
       FIELD fi-periodo           as char   FORMAT "99/9999"
       field da-data-conver       as date   FORMAT "99/99/9999"
       FIELD dt-maior-tit         AS DATE   FORMAT "99/99/9999"
       FIELD c-sigla-4            AS CHAR   FORMAT "X(5)"
       FIELD vl-maior-tit         AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD dt-ult-tit           AS DATE   FORMAT "99/99/9999"
       FIELD c-sigla-5            AS CHAR   FORMAT "X(5)"
       FIELD vl-ult-tit           AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-1            AS CHAR   FORMAT "X(5)"
       FIELD fi-vendas            AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-2            AS CHAR   FORMAT "X(5)"
       FIELD fi-vendas-acumuladas AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-3            AS CHAR   FORMAT "X(5)"
       FIELD fi-saldo-aberto      AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-6            AS CHAR   FORMAT "X(5)"
       FIELD fi-saldo-aberto-venc AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       field fi-label-periodo     as char   format "x(08)"
       FIELD fi-periodo-1         as char   format "x(08)"
       FIELD fi-periodo-2         as char   format "x(08)"
       field fi-periodo-3         as char   format "x(08)"
       field fi-periodo-4         as char   format "x(08)"
       field fi-periodo-5         as char   format "x(08)"
       field fi-periodo-6         as char   format "x(08)"
       field fi-periodo-7         as char   format "x(08)"
       field fi-periodo-8         as char   format "x(08)"
       field fi-periodo-9         as char   format "x(08)"
       field fi-periodo-10        as char   format "x(08)"
       field fi-periodo-11        as char   format "x(08)"
       field fi-periodo-12        as char   format "x(08)"
       FIELD fi-label-media       AS CHAR   FORMAT "x(15)"
       FIELD fi-label-atm         AS CHAR   FORMAT "x(15)"
       field fi-atm-1             as INT    format "->>9"
       field fi-atm-2             as INT    format "->>9"
       field fi-atm-3             as INT    format "->>9"
       field fi-atm-4             as INT    format "->>9"
       field fi-atm-5             as INT    format "->>9"
       field fi-atm-6             as INT    FORMAT "->>9"
       FIELD fi-atm-7             as INT    format "->>9"
       field fi-atm-8             as INT    format "->>9"
       field fi-atm-9             as INT    format "->>9"
       field fi-atm-10            as INT    format "->>9"
       field fi-atm-11            as INT    format "->>9"
       field fi-atm-12            as INT    format "->>9"
       field fi-atm-media         as INT    format "->>9"
       FIELD fi-label-pmr         AS CHAR   FORMAT "x(15)"
       field fi-pmr-1             as INT    format "->>9"
       field fi-pmr-2             as INT    format "->>9"
       field fi-pmr-3             as INT    format "->>9"
       field fi-pmr-4             as INT    format "->>9"
       field fi-pmr-5             as INT    format "->>9"
       field fi-pmr-6             as INT    FORMAT "->>9"
       field fi-pmr-7             as INT    format "->>9"
       FIELD fi-pmr-8             as INT    format "->>9"
       field fi-pmr-9             as INT    format "->>9"
       field fi-pmr-10            as INT    format "->>9"
       field fi-pmr-11            as INT    format "->>9"
       field fi-pmr-12            as INT    format "->>9"
       field fi-pmr-media         as INT    format "->>9"
       FIELD fi-label-vendas      AS CHAR   FORMAT "x(15)"
       field fi-vendas-1          as dec    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-2          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-3          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-4          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-5          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-6          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-7          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-8          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-9          as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-10         as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-11         as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-12         as DEC    format "->>>,>>>,>>>,>>9.99"
       field fi-vendas-media      as DEC    format "->>>,>>>,>>>,>>9.99".

define temp-table tt-liq no-undo
       FIELD c-barra         AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2        LIKE c-barra           INIT "/"
       FIELD cod-emitente    LIKE titulo.cod-emitente
       FIELD nome-abrev      LIKE titulo.nome-abrev
       FIELD ep-codigo       LIKE mov-tit.ep-codigo
       FIELD cod-estabel     LIKE mov-tit.cod-estabel
       FIELD cod-esp         LIKE mov-tit.cod-esp
       FIELD serie           LIKE mov-tit.serie
       FIELD nr-docto        LIKE mov-tit.nr-docto
       FIELD parcela         LIKE mov-tit.parcela 
       FIELD vl-baixa        LIKE mov-tit.vl-baixa
       FIELD dt-credito      LIKE mov-tit.dt-credito
       FIELD dt-baixa        LIKE mov-tit.dt-baixa 
       FIELD cod-portador    LIKE mov-tit.cod-portador
       FIELD de-total-baixa  LIKE mov-tit.vl-baixa
       FIELD c-modalidade    AS CHAR
       FIELD vl-baixa-me     LIKE mov-tit.vl-baixa-me
       FIELD mo-codigo       LIKE mov-tit.mo-codigo
       FIELD cotacao-dia     LIKE mov-tit.cotacao-dia
       FIELD vl-desconto     LIKE mov-tit.vl-desconto
       FIELD vl-desconto-me  LIKE mov-tit.vl-desconto-me  
       FIELD vl-juros-rec    LIKE mov-tit.vl-juros-rec
       FIELD vl-juros-rec-me LIKE mov-tit.vl-juros-rec-me
       FIELD vl-desp-banc    LIKE mov-tit.vl-desp-banc 
       FIELD vl-desp-banc-me LIKE mov-tit.vl-desp-banc-me
       FIELD vl-abatimen     LIKE mov-tit.vl-abatimen
       FIELD vl-abatimen-me  LIKE mov-tit.vl-abatimen-me
       FIELD dt-vencimen     LIKE mov-tit.dt-vencimen
       FIELD esp-antecip     LIKE mov-tit.esp-antecip
       FIELD serie-antecip   LIKE mov-tit.serie-antecip
       FIELD doc-antecip     LIKE mov-tit.doc-antecip
       FIELD parc-antecip    LIKE mov-tit.parc-antecip 
       FIELD vl-antecip      LIKE mov-tit.vl-antecip
       FIELD vl-antecip-me   LIKE mov-tit.vl-antecip-me
       FIELD referencia      LIKE mov-tit.referencia 
       FIELD i-dias-atraso   AS INT.

define temp-table tt-hist no-undo
       FIELD cod-emitente LIKE titulo.cod-emitente
       FIELD nome-abrev   LIKE titulo.nome-abrev
       FIELD dt-his-emit  LIKE his-emit.dt-his-emit
       FIELD horario      LIKE his-emit.horario
       FIELD historico    LIKE his-emit.historico.

define temp-table tt-doc no-undo
       FIELD c-barra                      AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2                     AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-estabel                  LIKE titulo.cod-estabel
       FIELD cod-esp                      LIKE titulo.cod-esp
       FIELD serie                        LIKE titulo.serie
       FIELD nr-docto                     LIKE titulo.nr-docto
       FIELD parcela                      LIKE titulo.parcela
       FIELD cod-portador                 LIKE mov-tit.cod-portador
       FIELD c-modalidade                 AS CHAR
       FIELD vl-saldo                     LIKE titulo.vl-saldo
       FIELD vl-original                  LIKE titulo.vl-original
       field de-juros                     as dec
       FIELD de-total-juros               AS DEC
       FIELD dt-emissao                   LIKE titulo.dt-emissao
       FIELD dt-vencimen                  LIKE titulo.dt-vencimen
       FIELD cod-emitente                 LIKE titulo.cod-emitente
       FIELD nome-abrev                   LIKE titulo.nome-abrev
       FIELD i-dias                       AS int
       FIELD de-total-saldo-aberto        AS DEC
       FIELD de-saldo-anterior-do-cliente AS DEC.

/*** Defini‡Æo do Parametr“s do Programa ***/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.
def input param table for tt-liq.
def input param table for tt-hist.
def input param table for tt-estat.
DEF INPUT PARAM TABLE FOR tt-doc.
def input param table for tt-documento.
def input param r-emitente as rowid no-undo.
def input param fi-total-titulos  as dec format "->,>>>,>>>,>>>,>>9.99" no-undo.
def input param fi-total-matriz   as dec format "->,>>>,>>>,>>>,>>9.99" no-undo.
def input param fi-saldo-anterior as dec format "->,>>>,>>>,>>>,>>9.99" no-undo.
def input param cod-escolha AS INT NO-UNDO.

/*** Defini‡äes de Vari veis ***/

DEF VAR c-titulo-tela                    AS CHAR NO-UNDO.
DEF VAR c-hora-proc                      AS CHAR NO-UNDO.
DEF VAR da-data-proc                     AS DATE NO-UNDO.
DEF VAR c-label-cliente                  AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR c-label-total-das-baixas         AS CHAR FORMAT "X(20)" no-undo.
DEF VAR c-label-total-saldo-aberto       AS CHAR FORMAT "x(19)" NO-UNDO.
DEF VAR c-label-sald-anterior-do-cliente AS CHAR FORMAT "x(31)" NO-UNDO.
DEF VAR de-tot-baixa                     AS DEC FORMAT "->>>,>>>,>>>,>>>,>>9.99" no-undo.
DEF VAR i-codigo                         AS INT FORMAT ">>>>>>>>9" NO-UNDO.
DEF VAR l-venct-matriz                   AS LOG NO-UNDO.
DEF VAR i-cod-emitente            AS INT NO-UNDO.

/*--- Form's do Relat¢rio ---*/
{crp/cr0706d.i3}

FORM
         "+-----------------------------------------------------+" AT 42 SKIP
          tt-estat.cod-emitente NO-LABEL  "-"
          tt-estat.nome-abrev   NO-LABEL
         "|Periodo       ATM    PMR             Valor das Vendas|" AT 42
         "+--------------------------------------+" AT 1
         "+-----------------------------------------------------+" AT 42
         "|            Per¡odo:" AT 1 tt-estat.fi-periodo AT 23 "| |" AT 40
         tt-estat.fi-periodo-1 AT 43 tt-estat.fi-atm-1 AT 56 tt-estat.fi-pmr-1 AT 63 tt-estat.fi-vendas-1 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "|    Data Conversäes:" AT 1 tt-estat.da-data-conver AT 23 "| |" AT 40   
         tt-estat.fi-periodo-2 AT 43 tt-estat.fi-atm-2 AT 56 tt-estat.fi-pmr-2 AT 63 tt-estat.fi-vendas-2 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "+--------------------------------------+" AT 1 "|" AT 42 
         tt-estat.fi-periodo-3 AT 43 tt-estat.fi-atm-3 AT 56 tt-estat.fi-pmr-3 AT 63 tt-estat.fi-vendas-3 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "|  Data Maior T¡tulo:" AT 1 tt-estat.dt-maior-tit AT 23 "| |" AT 40 
         tt-estat.fi-periodo-4 AT 43 tt-estat.fi-atm-4 AT 56 tt-estat.fi-pmr-4 AT 63 tt-estat.fi-vendas-4 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "|    Vl Maior T¡tulo:" AT 1 tt-estat.vl-maior-tit FORMAT ">>>,>>>,>>>,>>9.99"  AT 22 "| |" AT 40
         tt-estat.fi-periodo-5 AT 43 tt-estat.fi-atm-5 AT 56 tt-estat.fi-pmr-5 AT 63 tt-estat.fi-vendas-5 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "| Data éltimo T¡tulo:" AT 1 tt-estat.dt-ult-tit AT 23 "| |" AT 40
         tt-estat.fi-periodo-6 AT 43 tt-estat.fi-atm-6 AT 56 tt-estat.fi-pmr-6 AT 63 tt-estat.fi-vendas-6 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96 
         "|   Vl éltimo T¡tulo:" AT 1 tt-estat.vl-ult-tit FORMAT ">>>,>>>,>>>,>>9.99" AT 22 "| |" AT 40    
         tt-estat.fi-periodo-7 AT 43 tt-estat.fi-atm-7 AT 56 tt-estat.fi-pmr-7 AT 63 tt-estat.fi-vendas-7 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96 
         "|                                      |" AT 1 "|" AT 42
         tt-estat.fi-periodo-8 AT 43 tt-estat.fi-atm-8 AT 56 tt-estat.fi-pmr-8 AT 63 tt-estat.fi-vendas-8 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "+--------------------------------------+" AT 1 "|" AT 42 
         tt-estat.fi-periodo-9 AT 43 tt-estat.fi-atm-9 AT 56 tt-estat.fi-pmr-9 AT 63 tt-estat.fi-vendas-9 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "|                                      |" AT 1 "|" AT 42 
         tt-estat.fi-periodo-10 AT 43 tt-estat.fi-atm-10 AT 56 tt-estat.fi-pmr-10 AT 63 tt-estat.fi-vendas-10 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96
         "|       Vendas em R$:" AT 1 tt-estat.fi-vendas FORMAT ">>>,>>>,>>>,>>9.99" AT 22 "| |" AT 40 
         tt-estat.fi-periodo-11 AT 43 tt-estat.fi-atm-11 AT 56 tt-estat.fi-pmr-11 AT 63 tt-estat.fi-vendas-11 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96 
         "|     Vendas Acum R$:" AT 1 tt-estat.fi-vendas-acumuladas FORMAT ">>>,>>>,>>>,>>9.99" AT 22 "| |" AT 40
         tt-estat.fi-periodo-12 AT 43 tt-estat.fi-atm-12 AT 56 tt-estat.fi-pmr-12 AT 63 tt-estat.fi-vendas-12 FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96 
         "| Saldo em Aberto R$:" AT 1 tt-estat.fi-saldo-aberto FORMAT ">>>,>>>,>>>,>>9.99" AT 22 "|" AT 40  
         "|-----------------------------------------------------|" AT 42
         "|     Sl.Ab.Venc. R$:" AT 1 tt-estat.fi-saldo-aberto-venc FORMAT ">>>,>>>,>>>,>>9.99" AT 22 "| |" AT 40  
         " M‚dia:" AT 43 tt-estat.fi-atm-media AT 56 tt-estat.fi-pmr-media AT 63 tt-estat.fi-vendas-media FORMAT "->>>,>>>,>>>,>>9.99" AT 77 "|" AT 96 
         "+--------------------------------------+" AT 1 
         "+-----------------------------------------------------+" AT 42 
         WITH NO-LABELS WIDTH 97 NO-BOX NO-ATTR-SPACE  STREAM-IO FRAME f-imp-est.

form
         c-label-cliente  NO-LABEL AT 1 
         tt-liq.cod-emitente NO-LABEL AT 14 "-"
         tt-liq.nome-abrev NO-LABEL 
         WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 80 ROW 2 FRAME f-cliente.
FORM
        tt-liq.ep-codigo     COLUMN-LABEL "Emp" AT 9
        tt-liq.cod-estabel   COLUMN-LABEL "Est"  
        tt-liq.cod-esp       COLUMN-LABEL "Esp"
        tt-liq.serie         COLUMN-LABEL "S‚rie"
        tt-liq.nr-docto      COLUMN-LABEL "Documento" SPACE(0)
        tt-liq.c-barra       COLUMN-LABEL "/"         SPACE(0)
        tt-liq.parcela       COLUMN-LABEL "P"         
        tt-liq.cod-port      COLUMN-LABEL "Port"      SPACE(0)
        tt-liq.c-barra2      COLUMN-LABEL "/"         SPACE(0)
        tt-liq.c-modalidade  COLUMN-LABEL "M"
        tt-liq.dt-vencimen   COLUMN-LABEL "Dt Vencimento"
        tt-liq.dt-baixa      COLUMN-LABEL "Dt Baixa"
        tt-liq.i-dias-atraso COLUMN-LABEL "Atraso"
        tt-liq.vl-baixa      COLUMN-LABEL "Vl Liquida‡Æo"
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 118 FRAME f-liq TITLE " Baixas do Cliente ".

FORM 
        c-label-total-das-baixas NO-LABEL AT 67
        de-tot-baixa NO-LABEL
        WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 118 ROW 2 FRAME f-liq-total.


form
         c-label-cliente      NO-LABEL 
         tt-hist.cod-emitente NO-LABEL "-"
         tt-hist.nome-abrev   NO-LABEL 
         WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 80 ROW 2 FRAME f-cliente-hist.

form
        tt-hist.dt-his-emit COLUMN-LABEL "Dt Hist¢rico" AT 9
        tt-hist.horario     COLUMN-LABEL "Hora"  
        tt-hist.historico   COLUMN-LABEL "Hist¢rico"
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 118 FRAME f-hist.

FORM
        tt-doc.cod-emitente COLUMN-LABEL "C¢digo" 
        tt-doc.nome-abrev   COLUMN-LABEL "Nome Abreviado"  
        SKIP(1)
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 118 FRAME f-cliente-doc.

form
        tt-doc.cod-estabel    COLUMN-LABEL "Est"  
        tt-doc.cod-esp        COLUMN-LABEL "Esp"
        tt-doc.serie          COLUMN-LABEL "S‚rie"
        tt-doc.nr-docto       COLUMN-LABEL "Documento" SPACE(0)
        tt-doc.c-barra        COLUMN-LABEL "/"         SPACE(0)
        tt-doc.parcela        COLUMN-LABEL "P"
        tt-doc.cod-port       COLUMN-LABEL "Port"      SPACE(0)
        tt-doc.c-barra2       COLUMN-LABEL "/"         SPACE(0)
        tt-doc.c-modalidade   COLUMN-LABEL "M"
        tt-doc.vl-saldo       COLUMN-LABEL "Valor Saldo" FORMAT "->>,>>>,>>9.99"
        tt-doc.de-juros       COLUMN-LABEL "Juros"       FORMAT "->>,>>9.99"
        tt-doc.de-total-juros COLUMN-LABEL "Total Saldo" FORMAT "->>,>>>,>>9.99"
        tt-doc.dt-emissao     COLUMN-LABEL "EmissÆo"
        tt-doc.i-dias         COLUMN-LABEL "Dias"        FORMAT "->>>>9"
        tt-doc.vl-original    COLUMN-LABEL "Vl Original" FORMAT "->>,>>>,>>>,>>9.99"
        tt-doc.dt-vencimen    COLUMN-LABEL "Vencto"
        WITH DOWN NO-ATTR-SPACE STREAM-IO WIDTH 139 FRAME f-doc.

FORM
       SKIP(1)
       tt-documento.cod-emitente SPACE(0)
       "-"
       tt-documento.nome-abrev NO-LABEL
       WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 50 FRAME f-cliente-documento.

FORM    
        c-label-total-saldo-aberto       NO-LABEL AT  59
        fi-total-matriz                  NO-LABEL
        c-label-sald-anterior-do-cliente NO-LABEL AT 52
        fi-saldo-anterior                NO-LABEL
        WITH SIDE-LABELS OVERLAY NO-ATTR-SPACE STREAM-IO WIDTH 118 ROW 2 FRAME f-doc-total.

{include/i-rpvar.i}
find first empresa no-lock where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  not avail empresa then
    return.

{utp/ut-liter.i CONTAS_A_RECEBER}
assign c-programa = "CR0706D":U
       c-empresa  = empresa.nome  
       c-sistema  = trim(return-value).

create tt-param.
raw-transfer raw-param to tt-param. 

run utp/ut-trfrrp.p (input frame f-doc-total:handle).
run utp/ut-trfrrp.p (input frame f-doc:handle).
run utp/ut-trfrrp.p (input frame f-cliente-documento:handle).
run utp/ut-trfrrp.p (input frame f-cliente-doc:handle).
run utp/ut-trfrrp.p (input frame f-hist:handle).
run utp/ut-trfrrp.p (input frame f-cliente-hist:handle).
run utp/ut-trfrrp.p (input frame f-liq-total:handle).
run utp/ut-trfrrp.p (input frame f-cliente:handle).
run utp/ut-trfrrp.p (input frame f-imp-est:handle).
{include/i-rpcab.i}
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.
ASSIGN l-venct-matriz = NO.
&IF DEFINED(BF_FIN_VENCT_MATRIZ) &THEN
    ASSIGN l-venct-matriz = YES.
&else
    IF CAN-FIND(funcao
                WHERE funcao.cd-funcao = 'spp-venct-matriz' 
                AND   funcao.ativo = yes) THEN
        ASSIGN l-venct-matriz = YES.
&ENDIF

IF cod-escolha = 1  THEN
DO:
    {utp/ut-liter.i Consulta_de_Estat¡stica}
    assign c-titulo-relat = trim(return-value).

    FIND FIRST tt-estat NO-LOCK NO-ERROR.

    IF AVAIL tt-estat THEN DO:


       DISP  tt-estat.cod-emitente
             tt-estat.nome-abrev
             tt-estat.fi-atm-media 
             tt-estat.fi-pmr-media 
             tt-estat.fi-vendas-media
             tt-estat.fi-periodo 
             tt-estat.da-data-conver
             tt-estat.fi-periodo-1 
             tt-estat.fi-atm-1 
             tt-estat.fi-pmr-1 
             tt-estat.fi-vendas-1 
             tt-estat.fi-periodo-2
             tt-estat.fi-atm-2
             tt-estat.fi-pmr-2
             tt-estat.fi-vendas-2 
             tt-estat.dt-maior-tit
             tt-estat.fi-periodo-3
             tt-estat.fi-atm-3 
             tt-estat.fi-pmr-3 
             tt-estat.fi-vendas-3 
             tt-estat.fi-periodo-4 
             tt-estat.fi-atm-4 
             tt-estat.fi-pmr-4
             tt-estat.fi-vendas-4
             tt-estat.vl-maior-tit
             tt-estat.fi-periodo-5
             tt-estat.fi-atm-5 
             tt-estat.fi-pmr-5
             tt-estat.fi-vendas-5
             tt-estat.dt-ult-tit 
             tt-estat.fi-periodo-6 
             tt-estat.fi-atm-6 
             tt-estat.fi-pmr-6 
             tt-estat.fi-vendas-6 
             tt-estat.vl-ult-tit 
             tt-estat.fi-periodo-7 
             tt-estat.fi-atm-7 
             tt-estat.fi-pmr-7 
             tt-estat.fi-vendas-7 
             tt-estat.fi-periodo-8 
             tt-estat.fi-atm-8 
             tt-estat.fi-pmr-8 
             tt-estat.fi-vendas-8 
             tt-estat.fi-vendas 
             tt-estat.fi-vendas-acumuladas 
             tt-estat.fi-saldo-aberto
             tt-estat.fi-saldo-aberto-venc
             tt-estat.fi-periodo-9 
             tt-estat.fi-atm-9 
             tt-estat.fi-pmr-9 
             tt-estat.fi-vendas-9 
             tt-estat.fi-periodo-10 
             tt-estat.fi-atm-10 
             tt-estat.fi-pmr-10 
             tt-estat.fi-vendas-10 
             tt-estat.fi-periodo-11 
             tt-estat.fi-atm-11 
             tt-estat.fi-pmr-11 
             tt-estat.fi-vendas-11 
             tt-estat.fi-periodo-12 
             tt-estat.fi-atm-12 
             tt-estat.fi-pmr-12 
             tt-estat.fi-vendas-12
             WITH FRAME f-imp-est WIDTH 300.
    END.
END.
ELSE 
DO:
    IF cod-escolha = 2  THEN
    DO:
        {utp/ut-liter.i Consulta_de_Liquida‡äes}
        assign c-titulo-relat = trim(return-value).

        {utp/ut-liter.i Cliente:}
        ASSIGN c-label-cliente = trim(RETURN-VALUE).

            ASSIGN de-tot-baixa = 0.

            FOR EACH tt-liq NO-LOCK BREAK BY tt-liq.cod-emitente:
                IF FIRST-OF(tt-liq.cod-emitente) THEN
                    DISP c-label-cliente
                        tt-liq.cod-emitente
                        tt-liq.nome-abrev
                        WITH FRAME f-cliente.
                    DISP tt-liq.ep-codigo 
                         tt-liq.cod-estabel  
                         tt-liq.cod-esp         
                         tt-liq.serie           
                         tt-liq.nr-docto        
                         tt-liq.parcela  
                         tt-liq.cod-portador
                         tt-liq.c-modalidade
                         tt-liq.dt-vencimen
                         tt-liq.dt-baixa
                         tt-liq.i-dias-atraso
                         tt-liq.vl-baixa        
                         WITH FRAME f-liq.
                    DOWN WITH FRAME f-liq.
                    ASSIGN de-tot-baixa = de-tot-baixa + tt-liq.vl-baixa.

            END.
            {utp/ut-liter.i Total_das_Baixas:}
            ASSIGN c-label-total-das-baixas = TRIM (RETURN-VALUE).
            DISP c-label-total-das-baixas
                 de-tot-baixa
                 WITH FRAME f-liq-total.

    END.

    ELSE DO:
        IF cod-escolha = 3  THEN DO:
            {utp/ut-liter.i Consulta_de_Hist¢ricos}
            assign c-titulo-relat = trim(return-value).

            {utp/ut-liter.i Cliente:}
            ASSIGN c-label-cliente = trim(RETURN-VALUE).

            FOR EACH tt-hist NO-LOCK BREAK BY tt-hist.cod-emitente:
                IF FIRST-OF(tt-hist.cod-emitente) THEN
                    DISP c-label-cliente
                        tt-hist.cod-emitente
                        tt-hist.nome-abrev
                        WITH FRAME f-cliente-hist.
                    DISP tt-hist.dt-his-emit      
                         tt-hist.horario 
                         tt-hist.historico 
                         WITH FRAME f-hist.
                    DOWN WITH FRAME f-hist.
            END.
        END.
        ELSE DO:
            IF cod-escolha = 4 THEN DO:
                {utp/ut-liter.i Listando_dados_do_Cliente}
                assign c-titulo-relat = trim(return-value).

                ASSIGN de-tot-original       = 0
                       de-tot-saldo          = 0
                       de-tot-juros          = 0
                       de-total-saldo        = 0
                       i-cod-emitente        = ?.

                FOR EACH tt-documento  NO-LOCK                         
                    BREAK BY tt-documento.cod-emitente:

                    FIND FIRST tt-doc NO-LOCK
                        WHERE tt-doc.cod-estabel = tt-documento.cod-estabel
                          AND tt-doc.cod-esp     = tt-documento.cod-esp    
                          AND tt-doc.serie       = tt-documento.serie      
                          AND tt-doc.nr-docto    = tt-documento.nr-docto   
                          AND tt-doc.parcela     = tt-documento.parcela 
                          AND tt-doc.cod-port    = tt-documento.cod-port NO-ERROR.
                    IF AVAIL tt-doc THEN DO:
                        IF tt-doc.cod-emitente <> i-cod-emitente THEN DO:
                            DISP tt-doc.cod-emitente                      
                                 tt-doc.nome-abrev                        
                                WITH FRAME f-cliente-doc.
                            ASSIGN i-cod-emitente = tt-doc.cod-emitente.
                        END.

                        IF FIRST-OF(tt-documento.cod-emitente) THEN DO:
                               DISP tt-documento.cod-emitente
                                    tt-documento.nome-abrev
                               WITH FRAME f-cliente-documento.
                        END.

                        DISP tt-doc.cod-estabel  
                             tt-doc.cod-esp         
                             tt-doc.serie           
                             tt-doc.nr-docto        
                             tt-doc.parcela
                             tt-doc.cod-portador
                             tt-doc.c-modalidade
                             tt-doc.dt-vencimen 
                             tt-doc.vl-saldo
                             tt-doc.vl-original
                             tt-doc.de-juros
                             tt-doc.de-total-juros
                             tt-doc.dt-emissao 
                             tt-doc.dt-vencimen 
                             tt-doc.i-dias WITH FRAME f-doc.
                             DOWN WITH FRAME f-doc.

                        /* Totalizaza‡Æo */
                        FOR FIRST esp-doc FIELDS(tipo) NO-LOCK
                            WHERE esp-doc.cod-esp = tt-doc.cod-esp:
                                IF esp-doc.tipo = 2 THEN
                                    ASSIGN de-tot-saldo    = de-tot-saldo    - tt-doc.vl-saldo
                                           de-tot-juros    = de-tot-juros    - tt-doc.de-juros
                                           de-total-saldo  = de-total-saldo  - tt-doc.de-total-juros
                                           de-tot-original = de-tot-original - tt-doc.vl-original.
                                ELSE       
                                    ASSIGN de-tot-saldo    = de-tot-saldo    + tt-doc.vl-saldo
                                           de-tot-juros    = de-tot-juros    + tt-doc.de-juros
                                           de-total-saldo  = de-total-saldo  + tt-doc.de-total-juros
                                           de-tot-original = de-tot-original + tt-doc.vl-original.
                        END.
                    END.
                END.

                {utp/ut-liter.i Total_Saldo_Aberto:}
                ASSIGN c-label-total-saldo-aberto = TRIM (RETURN-VALUE).

                {utp/ut-liter.i Saldo_Anterior_do_Cliente:}
                ASSIGN c-label-sald-anterior-do-cliente = TRIM (RETURN-VALUE).

                /* ImpressÆo dos Totalizadores *************************************************/
                UNDERLINE tt-doc.vl-saldo
                          tt-doc.vl-original
                          tt-doc.de-juros
                          tt-doc.de-total-juros WITH FRAME f-doc.

                DISP de-tot-saldo    @ tt-doc.vl-saldo       FORMAT "->>,>>>,>>9.99"
                     de-tot-juros    @ tt-doc.de-juros       FORMAT "->>,>>9.99"
                     de-total-saldo  @ tt-doc.de-total-juros FORMAT "->>,>>>,>>9.99"
                     de-tot-original @ tt-doc.vl-original    FORMAT "->>,>>>,>>>,>>9.99" WITH FRAME f-doc.
                /*******************************************************************************/

                DISP c-label-total-saldo-aberto
                     fi-saldo-anterior format "->,>>>,>>>,>>>,>>9.99"
                     c-label-sald-anterior-do-cliente
                     fi-total-matriz format "->,>>>,>>>,>>>,>>9.99"
                     WITH FRAME f-doc-total.
            END.
            ELSE DO:
                /*----------- INÖCIO IMPRESSÇO -----------*/
                IF cod-escolha = 5 THEN DO: 
                    find first emitente 
                     where rowid(emitente) = r-emitente NO-LOCK no-error.
                    if  avail emitente then do:

                        {utp/ut-liter.i Listando_dados_do_Cliente * C}
                        assign c-titulo-relat = trim(return-value).

                        disp emitente.cod-emitente
                             emitente.nome-abrev 
                             with frame f-emitente.
                        down with frame f-emitente.

                        if  tt-param.tipo-relat = 1 then
                            view frame f-cab-corpo.
                        else    
                            view frame f-cab-corpo-resumido.    
                        assign de-tot-original = 0
                               de-tot-saldo    = 0
                               de-tot-juros    = 0
                               de-total-saldo  = 0.

                        IF l-venct-matriz THEN DO:
                            for each tt-documento NO-LOCK
                                BY tt-documento.dt-vencimen:
                                RUN pi-imprime-docto.
                            END.
                        END.
                        ELSE DO:
                            for each tt-documento:
                                RUN pi-imprime-docto.
                            end.
                        END.


                        if tt-param.tipo-relat = 1 then do:
                            underline tt-documento.vl-original    
                                      tt-documento.de-saldo       
                                      tt-documento.de-juros       
                                      tt-documento.de-total-juros with frame f-documento.    

                            disp de-tot-original @ tt-documento.vl-original     format "->>,>>>,>>>,>>9.99" no-label
                                 de-tot-saldo    @ tt-documento.de-saldo        format "->>,>>>,>>>,>>9.99" no-label
                                 de-tot-juros    @ tt-documento.de-juros        format "->>,>>>,>>>,>>9.99" no-label
                                 de-total-saldo  @ tt-documento.de-total-juros  format "->>,>>>,>>>,>>9.99" no-label
                                 with frame f-documento.
                        end.
                        else do:
                            underline tt-documento.vl-original    
                                      tt-documento.de-saldo       
                                      tt-documento.de-juros       
                                      tt-documento.de-total-juros with frame f-documento-resumido.    

                            disp de-tot-original @ tt-documento.vl-original     format "->>,>>>,>>>,>>9.99" no-label
                                 de-tot-saldo    @ tt-documento.de-saldo        format "->>,>>>,>>9.99"     no-label
                                 de-tot-juros    @ tt-documento.de-juros        format "->>,>>9.99"         no-label
                                 de-total-saldo  @ tt-documento.de-total-juros  format "->>,>>>,>>9.99"     no-label
                                 with frame f-documento-resumido.    
                        end.         

                        disp fi-total-matriz
                             fi-saldo-anterior 
                             with frame f-total.
                        down with frame f-total.     
                    end.
                END.
            END.
        END.
    END.
END.


{include/i-rpclo.i} 

return "OK".

PROCEDURE pi-imprime-docto:
    if  tt-param.tipo-relat = 1 then do:
        disp tt-documento.cod-estabel    format "x(4)"            no-label
             tt-documento.cod-esp                                 no-label
             tt-documento.serie          format "x(6)"            no-label
             tt-documento.nr-docto                                no-label
             tt-documento.parcela        format "x(3)"            no-label
             tt-documento.cod-port       format ">>>>9"           no-label
             tt-documento.modalidade                              no-label
             tt-documento.de-saldo       format "->>>,>>>,>>9.99" no-label
             tt-documento.de-juros       format "->>>,>>>,>>9.99" no-label
             tt-documento.de-total-juros format "->>>,>>>,>>9.99" no-label
             tt-documento.vl-original    format "->>>,>>>,>>9.99" no-label
             tt-documento.dt-emissao                              no-label
             tt-documento.dt-vencimen                             no-label 
             tt-documento.i-dias         format "->>>>9"          no-label 
             with frame f-documento.
        down with frame f-documento.
    end.
    else do:
        disp tt-documento.cod-estabel    format "x(4)"            no-label
             tt-documento.cod-esp                                 no-label
             tt-documento.serie          format "x(6)"            no-label
             tt-documento.nr-docto                                no-label
             tt-documento.parcela        format "x(2)"            no-label
             tt-documento.cod-port       format ">>>>9"           no-label
             tt-documento.modalidade                              no-label
             tt-documento.de-saldo                                no-label
             tt-documento.de-juros       format "->>,>>9.99"      no-label
             tt-documento.de-total-juros format "->>,>>>,>>9.99"  no-label
             tt-documento.vl-original                             no-label
             tt-documento.dt-emissao                              no-label
             tt-documento.dt-vencimen                             no-label 
             tt-documento.i-dias                                  no-label 
             with frame f-documento-resumido.
        down with frame f-documento-resumido.
    end.        

    find esp-doc where esp-doc.cod-esp = tt-documento.cod-esp no-lock no-error.       

    if  avail esp-doc then do:
        if (esp-doc.tipo = 2) then do:
            assign de-tot-original = de-tot-original - tt-documento.vl-original
                   de-tot-saldo    = de-tot-saldo    - tt-documento.de-saldo
                   de-tot-juros    = de-tot-juros    - tt-documento.de-juros
                   de-total-saldo  = de-total-saldo  - tt-documento.de-total-juros.
        end.           
        else do: 
             assign de-tot-original = de-tot-original + tt-documento.vl-original
                    de-tot-saldo    = de-tot-saldo    + tt-documento.de-saldo
                    de-tot-juros    = de-tot-juros    + tt-documento.de-juros
                    de-total-saldo  = de-total-saldo  + tt-documento.de-total-juros.
        end.
    end.        
    if  line-counter >= PAGE-SIZE then do:
        page.
        if  tt-param.tipo-relat = 1 then
            view frame f-cab-corpo.
        else    
            view frame f-cab-corpo-resumido.

    end.    

END.
