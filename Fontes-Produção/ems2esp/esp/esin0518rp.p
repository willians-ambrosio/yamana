/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIN0518RP 2.00.00.015}  /*** 010015 ***/

{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: ESIN0518rp.p
**
**   Data....: Setembro de 2000.
**
**   Autor...: DATASUL S.A. Alan Koerbel
**
**   Objetivo: Listagem da Verba da Ordem Investimento - Detalhado.
**  
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-epc200.i esin0518rp}

{include/i-rpvar.i}

def var h-acomp as handle no-undo.
def var c-titulo-i       as char format "x(20)"  no-undo.
def var c-titulo-p       as char format "x(20)"  no-undo.
def var c-destino        as char format "x(7)"   no-undo.             
def var c-titulo-s       as char format "x(20)"  no-undo.   
def var c-titulo-c       as char format "x(20)"  no-undo.   
def var c-titulo-par     as char format "x(20)"  no-undo.
def var l-imprime        as log  initial yes     no-undo.
def var c-geral          as char format "x(20)"  no-undo.
def var c-verba          as char format "x(20)"  no-undo.
def var c-situacao       as char format "x(11)"  no-undo.
def var c-situacao-1     as char format "x(11)"  no-undo.
def var c-situacao-2     as char format "x(11)"  no-undo.
def var c-exib-moeda     as char format "x(20)"  no-undo.
def var c-lb-exib-moeda  as char format "x(25)"  no-undo.
def var c-emp-sel        as char format "x(10)"  no-undo.
def var c-est-sel        as char format "x(10)"  no-undo.
def var c-proj-sel       as char format "x(10)"  no-undo.
def var c-ord-sel        as char format "x(10)"  no-undo.
def var c-imprimindo     as char format "x(13)"  no-undo. 
def var c-processando    as char format "x(13)"  no-undo. 
def var c-moeda-controle as char format "x(15)"  no-undo.
def var c-moedas-inv     as char format "x(15)"  no-undo extent 3.
def var c-lb-proj        as char format "x(5)"   no-undo.
def var i-cod-moeda-par  as integer              no-undo.
def var c-lb-dest        as char                 no-undo.
def var c-lb-usuar       as char                 no-undo.
def var c-lb-atualiza    as char                 no-undo.
def var c-atualiza       as char                 no-undo.
def var c-lb-moeda-controle as char format "x(10)" no-undo.
def var c-lb-situacao    as char format "x(10)"  no-undo.
def var l-imprimiu       as logical              no-undo.
def var c-lb-total       as char format "x(10)" no-undo.
def var c-lb-dt-trans    as char format "x(10)" no-undo.
def var c-lb-tipo-doc    as char format "x(10)" no-undo.
def var c-lb-serie       as char format "x(05)" no-undo.
def var c-lb-docto       as char format "x(25)" no-undo.
def var c-lb-ent-comp    as char format "x(14)" no-undo.    
def var c-lb-sai-comp    as char format "x(14)" no-undo.
def var c-lb-ent-real    as char format "x(14)" no-undo.
def var c-lb-sai-real    as char format "x(14)" no-undo.
def var c-lb-acum-comp   as char format "x(16)" no-undo. 
def var c-lb-acum-real   as char format "x(16)" no-undo. 
def var c-lb-solicitacao as char                no-undo.
def var c-lb-ordem       as char                no-undo.
def var c-lb-pedido      as char                no-undo.
def var c-lb-especie     as char                no-undo.     
def var c-lb-item        as char                no-undo.
def var c-lb-num-ord-magnus as char             no-undo.

{cdp/cd9731.i}  /* l-integra-cn-in */
{cdp/cd9731.i1} /* l-integra-cn-in-medicao */

def buffer b-controle-verba for controle-verba.
def var de-acum-comp     as dec                 no-undo.
def var de-acum-real     as dec                 no-undo.
def var de-acum-tot      as dec                 no-undo.
def var de-total         as dec                 no-undo.
DEF VAR c-nome LIKE emitente.nome-emit.
DEF STREAM st.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classif-1        as integer
    field desc-classifica  as char format "x(40)"
    field i-ep-ini         LIKE empresa.ep-codigo 
    field i-ep-fim         LIKE empresa.ep-codigo
    field c-est-ini        as char
    field c-est-fim        as char
    field i-proj-ini       as int 
    field i-proj-fim       as int
    field i-ord-ini        as int
    field i-ord-fim        as int
    field i-moeda-par      as int
    FIELD i-data-ini       AS DATE
    FIELD i-data-fim       AS DATE
    field arquivo-destino  as char format "x(35)"
    field atualiza         as logical.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem. 

def temp-table tt-raw-digita                   
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.


/* Defini‡Æo das frames e tradu‡Æo*/
{esp/esin0518.i2}

find first param-global no-lock no-error.

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "ESIN0518" 
       c-versao       = "I.00"
       c-revisao      = "000"
       c-sistema      = return-value.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN  c-destino = RETURN-VALUE.
&else
    ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.
&endif
run INIC-FORMS.  /* gera cabecalho e rodape de 233 cols */

run utp/ut-acomp.p persistent set h-acomp.

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

/* Monta valores na temp-table */
{esp/esin0518.i1}

RUN pi-principal.
RUN pi-parametros.

{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return 'ok'.

{esp/esin0518.i5}

PROCEDURE pi-principal:

FOR EACH t-controle NO-LOCK:
    ASSIGN t-controle.valor = t-controle.ent-comp.
    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN
        ASSIGN t-controle.valor = t-controle.ent-real
               t-controle.ent-comp = t-controle.ent-real.

    RELEASE contrato-for. 
    IF t-controle.nr-contrato <> 0 THEN DO:
        FIND FIRST contrato-for OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
            ASSIGN t-controle.dt-contrato = contrato-for.dt-ini-validade
                   t-controle.valor = contrato-for.dec-2
                   t-controle.valor-cont = contrato-for.dec-2.
    END.

    IF t-controle.num-pedido <> 0 THEN DO:
        FIND FIRST pedido-compr OF t-controle NO-LOCK NO-ERROR.
        IF AVAIL pedido-compr THEN
            ASSIGN t-controle.cod-emitente = pedido-compr.cod-emitente.
    END.

    IF t-controle.tipo-doc = "imd" OR
       t-controle.tipo-doc = "pef" THEN DO:
        ASSIGN t-controle.cod-emitente = int(ENTRY(4,t-controle.num-docto,"/")).
    END.

    FIND FIRST t-controle-1 OF t-controle NO-ERROR.
    IF NOT AVAIL t-controle-1 THEN DO:
        CREATE t-controle-1.
        BUFFER-COPY t-controle TO t-controle-1.
    END.
    ELSE DO:
        ASSIGN t-controle-1.ent-comp = t-controle-1.ent-comp + t-controle.ent-comp.
               
        IF NOT AVAIL contrato-for THEN
            ASSIGN t-controle-1.valor = t-controle-1.valor + t-controle.valor.

        IF t-controle-1.descricao = "" THEN 
            ASSIGN t-controle-1.descricao = t-controle.descricao.
        ELSE
            ASSIGN t-controle-1.descricao = t-controle-1.descricao + " / " + t-controle.descricao.
    END.
END.

OUTPUT STREAM st TO VALUE(tt-param.arquivo-destino) CONVERT TARGET "iso8859-1". /* D:/TEMP/20000047/ESIN0518.LST.*/
FOR EACH t-controle-1.
    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = t-controle-1.cod-emitente NO-ERROR.
    IF AVAIL emitente THEN
        ASSIGN c-nome = emitente.nome-emit.
    ELSE
        ASSIGN c-nome = "".

    ASSIGN t-controle-1.descricao = REPLACE(REPLACE(t-controle-1.descricao,CHR(10)," "),CHR(13)," ").
    DISP t-controle-1.ep-codigo
         t-controle-1.cod-est-exec
         t-controle-1.num-projeto
         t-controle-1.num-ord-magnus
         t-controle-1.num-pedido
         t-controle-1.nr-contrato 
         t-controle-1.cod-emitente
         t-controle-1.ent-comp  COLUMN-LABEL "Comprometido"
         t-controle-1.dt-trans
         t-controle-1.valor-cont
         t-controle-1.dt-contrato
         t-controle-1.tipo-doc
         /*t-controle-1.num-ord-comp*/
        WITH WIDTH 333 STREAM-IO.

    PUT  STREAM st UNFORMATTED 
         t-controle-1.cod-est-exec FORMAT "x(3)"
         t-controle-1.num-projeto  FORMAT "999"
         t-controle-1.num-ord-magnus FORMAT "999999"
         t-controle-1.cod-emitente FORMAT "999999"
         t-controle-1.num-pedido FORMAT "999999" ";"
         t-controle-1.cod-est-exec FORMAT "x(3)"
         t-controle-1.num-projeto  FORMAT "999"
         t-controle-1.num-ord-magnus FORMAT "999999" ";"
         t-controle-1.num-ord-magnus ";"
         t-controle-1.nr-contrato ";"
         t-controle-1.num-pedido ";"
         t-controle-1.cod-emitente ";"
         c-nome ";"
         t-controle-1.descricao ";"
         t-controle-1.ent-comp ";"
         t-controle-1.dt-trans ";"
         t-controle-1.valor-cont ";"
         t-controle-1.dt-contrato SKIP.
END.
OUTPUT STREAM st CLOSE.

    RETURN "OK":U.
END PROCEDURE.
    
PROCEDURE pi-parametros:
    PAGE.
    put unformatted
        c-titulo-s           skip(1)
        c-emp-sel            at 5  ": "
    tt-param.i-ep-ini    at 30 "|<  " at 43 
        c-est-sel            at 5  ": "
        tt-param.c-est-ini   at 30 "|< >| " at 43 tt-param.c-est-fim
        c-proj-sel           at 5  ": "
        tt-param.i-proj-ini  at 30 "|< >| " at 43 tt-param.i-proj-fim
        c-ord-sel         at 5  ": "
        tt-param.i-ord-ini   at 30 "|< >| " at 43 tt-param.i-ord-fim 
        "Periodo"            at 5  ": "
        tt-param.i-data-ini FORMAT "99/99/9999"   at 30 "|< >| " at 43 tt-param.i-data-fim FORMAT "99/99/9999" skip(2)
        
/*
        c-titulo-c           skip(1)
        c-geral              at 5  ": " tt-param.desc-classifica skip(2)
        
        c-titulo-p           skip(1)
        c-lb-exib-moeda      at 5  ": " c-exib-moeda
        c-lb-atualiza        at 5  ": " c-atualiza skip(2)
*/
        c-titulo-i           skip(1)
        c-lb-dest            at 5  ": " c-destino + " - " + tt-param.arquivo
        "Arquivo"            at 5  ": " tt-param.arquivo-destino
        c-lb-usuar           at 5  ": " tt-param.usuario skip(2).    
    RETURN "OK":U.
END.
