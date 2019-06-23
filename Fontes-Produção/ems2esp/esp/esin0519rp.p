/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i esin0519RP 2.00.00.015}  /*** 010015 ***/
{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: esin0519rp.p
**
**   Data....: Setembro de 2000.
**
**   Autor...: DATASUL S.A. Alan Koerbel
**
**   Objetivo: Listagem da Verba da Ordem Investimento - Detalhado.
**  
*******************************************************************************/
{include/i-epc200.i esin0519rp}

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

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classif-1        as integer
    field desc-classifica  as char format "x(40)"
    field i-ep-ini         as char 
    field i-ep-fim         as char
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
{esp/esin0519.i2}

find first param-global no-lock no-error.

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "esin0519" 
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

/* Monta valores na temp-table */
{esp/esin0519.i1}

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.


DEF TEMP-TABLE tt-nota NO-UNDO
    FIELD cod-emitente AS INT
    FIELD num-docto AS CHAR
    FIELD serie-docto AS CHAR
    FIELD nro-docto AS CHAR
    FIELD nat-operacao AS CHAR
    FIELD valor AS DEC
    FIELD tot-nota AS DEC
    FIELD tot-imp AS DEC
    INDEX chave IS UNIQUE PRIMARY
        cod-emitente
        num-docto.

FOR EACH t-controle NO-LOCK
    WHERE t-controle.l-nota = NO
    AND t-controle.num-pedido <> 0
    AND NOT CAN-FIND(FIRST tt-nota OF t-controle):
    CREATE tt-nota.
    BUFFER-COPY t-controle TO tt-nota.
END.

FOR EACH tt-nota,
    EACH t-controle
        WHERE t-controle.cod-emitente = tt-nota.cod-emitente
        AND t-controle.num-docto = tt-nota.num-docto
        AND t-controle.num-pedido <> 0:
    IF t-controle.ent-real > 0 THEN
        ASSIGN tt-nota.valor = tt-nota.valor + t-controle.ent-real.
    ELSE 
        ASSIGN tt-nota.valor = tt-nota.valor + t-controle.sai-real.
END.


FOR EACH tt-nota:
    ASSIGN tt-nota.serie-docto = ENTRY(1,tt-nota.num-docto,"/")
           tt-nota.nro-docto = ENTRY(2,tt-nota.num-docto,"/")
           tt-nota.nat-operacao = ENTRY(3,tt-nota.num-docto,"/").
END.

FOR EACH tt-nota,
    FIRST docum-est OF tt-nota NO-LOCK:
    ASSIGN tt-nota.tot-nota = docum-est.tot-valor.
END.

DEF VAR de-aux AS DEC.
FOR EACH tt-nota
    WHERE (tt-nota.valor / tt-nota.tot-nota) > 0.9,
    EACH t-controle
        WHERE t-controle.cod-emitente = tt-nota.cod-emitente
        AND t-controle.num-docto = tt-nota.num-docto
        AND t-controle.num-pedido <> 0:
    IF t-controle.ent-real > 0 THEN
        ASSIGN de-aux = t-controle.ent-real
               t-controle.ent-real = round(t-controle.ent-real / tt-nota.valor * tt-nota.tot-nota,2)
               t-controle.imposto = t-controle.ent-real - de-aux .
    ELSE 
        ASSIGN de-aux = t-controle.sai-real
               t-controle.sai-real = round(t-controle.sai-real / tt-nota.valor * tt-nota.tot-nota,2)
               t-controle.imposto = t-controle.sai-real - de-aux .
END.

FOR EACH tt-nota
    WHERE (tt-nota.valor / tt-nota.tot-nota) <= 0.9,
    FIRST docum-est OF tt-nota NO-LOCK,
    EACH item-doc-est OF docum-est NO-LOCK:
          
    case substring(item-doc-est.char-2,21,1):
        when "1" then assign tt-nota.tot-imp = tt-nota.tot-imp + dec(substring(item-doc-est.char-2,41,14)).
        when "4" then assign tt-nota.tot-imp = tt-nota.tot-imp + dec(substring(item-doc-est.char-2,41,14)).
/*                    when "5" then assign t-controle.ent-real = t-controle.ent-real - dec(substring(item-doc-est.char-2,41,14)).
*/                end.   
    case SUBSTRING(item-doc-est.char-2,83,1):
        when "1" then assign tt-nota.tot-imp = tt-nota.tot-imp + dec(substring(item-doc-est.char-2,103,14)).
        when "4" then assign tt-nota.tot-imp = tt-nota.tot-imp + dec(substring(item-doc-est.char-2,103,14)).
/*                    when "5" then assign t-controle.ent-real = t-controle.ent-real - dec(substring(item-doc-est.char-2,103,14)).
*/                end.
    case item-doc-est.cd-trib-icm:
        when 1 then assign tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-icm[1].
        when 3 then ASSIGN tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-icm[1].
        when 4 then assign tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-icm[1].
/*                    when 5 then assign t-controle.ent-real = t-controle.ent-real - item-doc-est.valor-icm[1].
*/                end.
    
    case item-doc-est.cd-trib-ipi:
        when 1 then assign tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-ipi[1].
        when 3 then assign tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-ipi[1].
        when 4 then assign tt-nota.tot-imp = tt-nota.tot-imp + item-doc-est.valor-ipi[1].
/*                    when 5 then assign t-controle.ent-real = t-controle.ent-real - item-doc-est.valor-ipi[1].
*/                end.


END.
/*
FOR EACH tt-nota
    WHERE (tt-nota.valor / tt-nota.tot-nota) <= 0.9:
    DISP tt-nota WITH WIDTH 333.
    DISP (tt-nota.valor / tt-nota.tot-nota) * 100
         tt-nota.tot-imp / tt-nota.tot-nota * 100.
END.
*/
FOR EACH tt-nota
    WHERE (tt-nota.valor / tt-nota.tot-nota) <= 0.9
    AND tt-nota.tot-imp > 0,
    EACH t-controle
        WHERE t-controle.cod-emitente = tt-nota.cod-emitente
        AND t-controle.num-docto = tt-nota.num-docto
        AND t-controle.num-pedido <> 0:
    IF t-controle.ent-real > 0 THEN
        ASSIGN de-aux = t-controle.ent-real
               t-controle.ent-real = t-controle.ent-real + round(t-controle.ent-real * (tt-nota.tot-imp / tt-nota.tot-nota),2)
               t-controle.imposto = t-controle.ent-real - de-aux .
    ELSE 
        ASSIGN de-aux = t-controle.sai-real
               t-controle.sai-real = t-controle.sai-real + round(t-controle.sai-real * (tt-nota.tot-imp / tt-nota.tot-nota),2)
               t-controle.imposto = t-controle.sai-real - de-aux .
END.




FOR EACH t-controle NO-LOCK:
    RELEASE t-controle-1.
    FIND FIRST t-controle-1 
        WHERE t-controle-1.ep-codigo = t-controle.ep-codigo
        AND t-controle-1.cod-est-exec = t-controle.cod-est-exec
        AND t-controle-1.num-projeto = t-controle.num-projeto
        AND t-controle-1.num-ord-magnus = t-controle.num-ord-magnus
        AND t-controle-1.cod-emitente = t-controle.cod-emitente
        AND t-controle-1.num-pedido = t-controle.num-pedido
        AND t-controle-1.num-docto = t-controle.num-docto NO-ERROR.
    IF NOT AVAIL t-controle-1 THEN DO:
        CREATE t-controle-1.
        BUFFER-COPY t-controle TO t-controle-1.
    END.
    ELSE DO:
        ASSIGN t-controle-1.imposto = t-controle-1.imposto + t-controle.imposto
               t-controle-1.ent-real = t-controle-1.ent-real + t-controle.ent-real
               t-controle-1.sai-real = t-controle-1.sai-real + t-controle.sai-real.

        IF t-controle-1.descricao = "" THEN 
            ASSIGN t-controle-1.descricao = t-controle.descricao.
        ELSE
            ASSIGN t-controle-1.descricao = t-controle-1.descricao + " / " + t-controle.descricao.
    END.
END.

FOR EACH t-controle-1:
    ASSIGN t-controle-1.valor = t-controle-1.ent-real - t-controle-1.sai-real.
END.



DEF STREAM st.
OUTPUT STREAM st TO VALUE(tt-param.arquivo-destino) CONVERT TARGET "iso8859-1". /* D:/TEMP/20000047/esin0519.LST.*/
FOR EACH t-controle-1.

   /* ASSIGN t-controle-1.num-docto = ENTRY(2,t-controle-1.num-docto,"/").*/
    DISP t-controle-1.ep-codigo
         t-controle-1.cod-est-exec
         t-controle-1.num-projeto
         t-controle-1.num-ord-magnus COLUMN-LABEL "Ordem EMS"
         t-controle-1.num-pedido
         t-controle-1.cod-emitente 
         t-controle-1.num-docto FORMAT "x(30)"
         t-controle-1.valor FORMAT "->,>>>,>>>,>>9.99" COLUMN-LABEL "Realizado"
         t-controle-1.imposto FORMAT "->,>>>,>>>,>>9.99" COLUMN-LABEL "Imposto"
         t-controle-1.dt-trans
         t-controle-1.nr-contrato
         t-controle-1.tipo-doc
        WITH WIDTH 333 STREAM-IO.

    PUT  STREAM st 
         t-controle-1.cod-est-exec FORMAT "x(3)"
         t-controle-1.num-projeto  FORMAT "999"
         t-controle-1.num-ord-magnus FORMAT "999999"
         t-controle-1.cod-emitente FORMAT "999999"
         t-controle-1.num-pedido FORMAT "999999" ";"
         t-controle-1.cod-est-exec FORMAT "x(3)"
         t-controle-1.num-projeto  FORMAT "999"
         t-controle-1.num-ord-magnus FORMAT "999999" ";"
         t-controle-1.num-docto FORMAT "x(30)" ";"
         t-controle-1.valor ";"
         t-controle-1.dt-trans ";"
         t-controle-1.nr-contrato
          SKIP.

END.
OUTPUT STREAM st CLOSE.
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
    "Arquivo"             at 5  ": " tt-param.arquivo-destino
    c-lb-usuar           at 5  ": " tt-param.usuario skip(2).    

{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return 'ok'.


{esp/esin0519.i5}
