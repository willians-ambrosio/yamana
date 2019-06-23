/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCN0206RP 12.1.13.001 } /*** 010011 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i ESCN0206RP MCN}
&ENDIF


/***************************************************************************** 
** 
**    Programa: CN0206rp.P
** 
**    Data....: NOVEMBRO DE 1997
** 
**    Autor...: DATASUL -  Desenvolvimento de Sistemas S/A. 
** 
**    Objetivo: Importacao de Contratos           
** 
**    Versao..: 1.00.000 - Maria Carolina
**
*****************************************************************************/ 

define temp-table tt-param no-undo
   field destino          as integer
   field arq-destino      as char
   field arq-entrada      as char
   field todos            as integer
   field usuario          as char
   field data-exec        as date
   field hora-exec        as integer
   field da-transacao     as date format "99/99/9999" 
   field c-todos          as char
   field c-destino        as char.
     
define temp-table tt-contrato-matriz no-undo
   field im-cod-emitente      like contrato-for.cod-emitente
   field im-nr-contrat        like contrato-for.nr-contrato
   field im-cod-tipo-contrat  like contrato-for.cod-tipo-contrat
   field im-des-contrat       like contrato-for.des-contrat.

define temp-table tt-problema-un-negoc no-undo 
   field c-desc-prob as char format "x(120)". 
     
def temp-table tt-raw-digita no-undo
    field raw-digita as raw.
    
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var de-tipo               as char    format "x(2)"   no-undo.
def var de-sequencia          as integer format "99"     no-undo.
def var c-param               as char    format "x(15)"  no-undo.
def var c-linha               as char    no-undo.
def var c-linha-cc-ant        as char    no-undo.
DEF VAR c-it-codigo-ic00      AS CHAR    NO-UNDO.
def var h-acomp               as handle  no-undo.

DEFINE VARIABLE i-num-seq-item LIKE item-contrat.num-seq-item NO-UNDO.

def var i-percent-mc like matriz-rat-contr.perc-rateio initial 100.
def var i-percent-mi like matriz-rat-contr.perc-rateio initial 100.

/* Variaveis p/ Pagina Parametros */
def var l-param       like param-global.exp-cep no-undo.
def var c-lb-param    as char no-undo.
def var c-lb-data     as char no-undo.
def var c-lb-import   as char no-undo.
def var c-lb-obsolet  as char no-undo.
def var c-lb-log      as char no-undo.
def var c-lb-impr     as char no-undo.
def var c-lb-usuar    as char no-undo.
def var c-lb-dest     as char no-undo.
{utp/ut-liter.i Data_Transaá∆o * r}
assign c-lb-data = trim(return-value).
{utp/ut-liter.i Importar_Ordem/Cotaá∆o_para_Itens_Obsoletos * r}
assign c-lb-obsolet = trim(return-value).
{utp/ut-liter.i Arquivo_Importaá∆o * r}
assign c-lb-import = trim(return-value).
{utp/ut-liter.i PAR∂METROS * r}
assign c-lb-param = trim(return-value).
{utp/ut-liter.i LOG * r}
assign c-lb-log = trim(return-value).
{utp/ut-liter.i Imprime * r}
assign c-lb-impr = trim(return-value).
{utp/ut-liter.i Usu†rio * r}
assign c-lb-usuar = trim(return-value).
{utp/ut-liter.i Destino * r}
assign c-lb-dest = trim(return-value).
def stream s-imp.

{utp/ut-glob.i}
DEFINE BUFFER bf-hist-alter FOR hist-alter.
DEFINE BUFFER bf-contrato-for FOR contrato-for.

{cnp/esp/escn0206tt.i}

DEFINE TEMP-TABLE tt-importacao NO-UNDO
    FIELD nr-contrato LIKE contrato-for.nr-contrato
    FIELD linha       AS   INTEGER
    FIELD desc-erro   AS   CHARACTER FORMAT "X(85)".

FORM tt-importacao.nr-contrato LABEL "Contrato"
     tt-importacao.linha       LABEL "Linha"
     tt-importacao.desc-erro   LABEL "Situaá∆o"
     WITH FRAME f-relat-contrat DOWN STREAM-IO WIDTH 132. 

{cdp/cdcfgmat.i}

{include/i-rpvar.i} 

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Importaá∆o de Contratos").

create tt-param.
raw-transfer raw-param to tt-param.

find first param-global no-lock no-error.
find first param-compra no-lock no-error.
assign c-empresa  = (if avail param-global then param-global.grupo else "")
       c-programa = "ESCN/0206" 
       c-versao   = "1.00" 
       c-revisao  = "000"
       de-tipo    = "". 

{utp/ut-liter.i Importaá∆o_de_Contratos * r}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i Contratos * r}
assign c-sistema = trim(return-value).
 
{include/i-rpcab.i}
   
input stream s-imp from value(tt-param.arq-entrada).

{include/i-rpout.i &tofile=tt-param.arq-destino}

view frame f-cabec. 
view frame f-rodape. 

DEFINE VARIABLE c-documento    AS   CHARACTER NO-UNDO.
DEFINE VARIABLE i-acao         AS   INTEGER   NO-UNDO.
DEFINE VARIABLE i-registro     AS   INTEGER   NO-UNDO.
DEFINE VARIABLE i-cont         AS   INTEGER   NO-UNDO.
DEFINE VARIABLE l-erro-doc     AS   LOGICAL   NO-UNDO.
DEFINE VARIABLE i-numero-ordem LIKE ordem-compra.numero-ordem NO-UNDO.
DEFINE VARIABLE i-num-ord      AS   INTEGER   NO-UNDO.
DEFINE VARIABLE i-cont-pe      AS   INTEGER   NO-UNDO.
DEFINE VARIABLE i-empresa      AS   CHARACTER NO-UNDO.
DEFINE VARIABLE h-cdapi024     AS   HANDLE    NO-UNDO.
DEFINE VARIABLE i-cont-moeda   AS   INTEGER   NO-UNDO.
DEFINE VARIABLE h-esapi001     AS   HANDLE    NO-UNDO.
DEFINE VARIABLE i-nr-contrato  AS   INTEGER   NO-UNDO.
DEFINE VARIABLE i-nr-contrato-vinc AS INTEGER NO-UNDO.
DEFINE VARIABLE c-cod-estabel-vinc AS CHARACTER NO-UNDO.

DEF VAR i-lista-estoque    AS INT    NO-UNDO.
def var h_api_cta_ctbl     as handle no-undo.
def var v_cod_cta_ctbl     as char   no-undo.
def var v_des_cta_ctbl     as char   no-undo.
def var v_num_tip_cta_ctbl as int    no-undo.
def var v_num_sit_cta_ctbl as int    no-undo.
def var v_ind_finalid_cta  as char   no-undo.
DEF VAR icont              AS INT    NO-UNDO.
def var v_log_utz_ccusto   AS LOGICAL init no NO-UNDO.
def var h_api_ccusto     as handle no-undo.

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as int  format ">>>>,>>9" label "N£mero"         column-label "Número"
    field ttv_des_msg_ajuda as char format "x(40)"    label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as char format "x(60)"    label "Mensagem Erro"  column-label "Inconsistºncia".

DEF TEMP-TABLE tt-erro-aux NO-UNDO
        FIELD i-sequen AS INT              
        FIELD cd-erro  AS INT
        FIELD mensagem AS CHAR FORMAT "x(255)".

DEFINE TEMP-TABLE tt-ordem-compra-auto NO-UNDO LIKE ordem-compra.

DEF NEW GLOBAL SHARED VAR i-num-linha-escn0206  AS INTEGER LABEL "Linha"                NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-motivo-alter        LIKE hist-alter.des-motivo-alter        NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-alter-origem        AS CHARACTER FORMAT "x(76)"             NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-alterado            AS CHARACTER FORMAT "x(76)"             NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-tipo-alter          AS CHARACTER FORMAT "!" INITIAL "C"     NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-texto-orig          LIKE hist-tex-ori.alter-texto-origem    NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-texto               LIKE hist-tex-des.alter-texto-destino   NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-contrato-for       AS ROWID                                NO-UNDO.

DEF VAR l-relacionamento AS LOGICAL INIT NO NO-UNDO.

DEFINE TEMP-TABLE tt-linha NO-UNDO
    FIELD id-registro  AS INTEGER
    FIELD cod-registro AS CHARACTER
    FIELD acao         AS INTEGER
    FIELD num-linha    AS INTEGER 
    FIELD linha        AS CHARACTER FORMAT "X(5000)"
    INDEX id-registro cod-registro
    INDEX linha id-registro num-linha.

DEFINE TEMP-TABLE tt-erro-inv NO-UNDO
    FIELD i-sequen AS INTEGER
    FIELD cd-erro  AS INTEGER
    FIELD mensagem AS CHARACTER FORMAT "x(255)"
    FIELD c-param  AS CHARACTER.

DEFINE VARIABLE i-cod-emitente LIKE emitente.cod-emitente.


DEFINE BUFFER bf-tt-linha FOR tt-linha.
DEFINE BUFFER bf2-tt-linha FOR tt-linha.
DEFINE BUFFER b-contrato-for FOR contrato-for.
DEFINE BUFFER b-pedido-compr FOR pedido-compr.
DEFINE BUFFER b2-pedido-compr FOR pedido-compr.

ASSIGN i-registro = 0.

/* =============== SERGIO LUIZ - DSC PRAXIS - 07/02/2017======*/
DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.

RUN cnp/esp/escn0206a.p (INPUT tt-param.arq-entrada,
                         OUTPUT c-arquivo).


/* ASSIGN tt-param.arq-entrada = c-arquivo. */
/* ===========================================================*/

ASSIGN i-num-seq-item = 0.


INPUT stream s-imp FROM VALUE(c-arquivo) NO-ECHO.

REPEAT:

    IMPORT STREAM s-imp UNFORMATTED c-linha.

    IF c-linha = "" THEN
        NEXT.

    ASSIGN c-documento = ENTRY(1,c-linha,";")
           i-cont      = i-cont + 1.

    IF c-documento = "CCIN" THEN
        ASSIGN i-registro = i-registro + 1
               i-acao     = INT(ENTRY(2,c-linha,";")).

    RUN pi-acompanhar IN h-acomp (INPUT "Importando registro: " + STRING(i-registro)).

    CREATE tt-linha.
    ASSIGN tt-linha.id-registro  = i-registro
           tt-linha.cod-registro = c-documento
           tt-linha.acao         = i-acao
           tt-linha.num-linha    = i-cont
           tt-linha.linha        = c-linha.
END.

INPUT STREAM s-imp CLOSE.

DO i-cont = 1 TO i-registro:

    INICIO:
    FOR FIRST bf-tt-linha NO-LOCK
        WHERE bf-tt-linha.id-registro  = i-cont
          AND bf-tt-linha.cod-registro = "CCIN" TRANSACTION:

        FOR EACH tt-linha USE-INDEX linha NO-LOCK
           WHERE tt-linha.id-registro = bf-tt-linha.id-registro:

            IF tt-linha.cod-registro = "CCIN" THEN
                NEXT.

            ASSIGN de-tipo      = substr(entry(1, tt-linha.linha, ";"),1,2)
                   de-sequencia = integer(substr(entry(1, tt-linha.linha, ";"),3,2)).

            RUN pi-acompanhar IN h-acomp (INPUT "Criando registros - " + de-tipo + " Seq: " + STRING(i-num-seq-item)).
    
            if  de-tipo <> "CC":U and   /* Contrato */ 
                de-tipo <> "IC":U and   /* Item */ 
                de-tipo <> "MC":U and   /* Matriz Rateio Contrato */ 
                de-tipo <> "MI":U and   /* Matriz Rateio Item  */ 
                de-tipo <> "FR":U and   /* Formula Rajuste */ 
                de-tipo <> "EM":U and   /* Evento Modelo */ 
                de-tipo <> "PE":U and   /* Pedidos de Compra */
                de-tipo <> "OR":U and   /* Ordens de Compra */
                de-tipo <> "PA":U and   /* Parcelas da Ordem */
                de-tipo <> "CT":U and   /* Cotacoes */
                de-tipo <> "RE":U and   /* Recebimento */
                de-tipo <> "CE":U and   /* Condicao Especifica */
                de-tipo <> "DI":U and   /* Desp-item-contrat*/
                de-tipo <> "DC":U and   /* do:  -Desp-cotacao-item- CÛdigo comentado devido ao chamado TGRDJW */
                de-tipo <> "AC":U and 
                de-tipo <> "PD":U then 
                NEXT.
    
            ASSIGN l-erro-doc           = NO
                   i-num-linha-escn0206 = tt-linha.num-linha.

            if  de-tipo       = "PE":U
            AND tt-linha.acao = 1 THEN DO:

                RUN pi-gera-pe.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            IF  de-tipo       = "CC":U
            AND tt-linha.acao = 1 THEN DO:

                IF de-sequencia = 01 THEN DO:
                END.
            
                RUN pi-gera-cc.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.
    
            IF  de-tipo = "IC":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) THEN DO:
            
                RUN pi-gera-ic.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            IF  de-tipo       = "IC":U
            AND tt-linha.acao = 3 THEN DO:

                RUN pi-gera-ai.

                IF VALID-HANDLE(h-esapi001) THEN DO:
                    DELETE PROCEDURE h-esapi001.
                    ASSIGN h-esapi001 = ?.
                END.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.
    
            IF  de-tipo = "MC":U
            AND tt-linha.acao = 1 THEN DO:

                ASSIGN i-percent-mc = 0.

                FOR EACH bf2-tt-linha NO-LOCK
                   WHERE bf2-tt-linha.id-registro = tt-linha.id-registro
                     AND SUBSTRING(bf2-tt-linha.cod-registro,1,2) = "MC":

                    ASSIGN i-percent-mc = i-percent-mc + DEC(ENTRY(05,bf2-tt-linha.linha,";")).
                END.

                IF i-percent-mc <> 100 THEN DO:

                    RUN pi-gera-log-imp (INPUT DEC(ENTRY(02,tt-linha.linha,";")),
                                         INPUT 30). /* codigo erro */
                    UNDO inicio, NEXT inicio.
                END.
                ELSE DO:
                   
                    RUN pi-gera-mc.

                    IF VALID-HANDLE(h_api_cta_ctbl) THEN
                        DELETE PROCEDURE h_api_cta_ctbl.

                    IF VALID-HANDLE(h_api_ccusto) THEN
                        DELETE PROCEDURE h_api_ccusto.

                    IF RETURN-VALUE = "NOK":U THEN
                        UNDO inicio, NEXT inicio.
                END.
            END.
    
            IF  de-tipo = "MI":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                ASSIGN i-percent-mi = 0.

                FOR EACH bf2-tt-linha NO-LOCK
                   WHERE bf2-tt-linha.id-registro = tt-linha.id-registro
                     AND SUBSTRING(bf2-tt-linha.cod-registro,1,2) = "MI":

                    IF  INT(ENTRY(02,bf2-tt-linha.linha,";")) = INT(ENTRY(02,tt-linha.linha,";"))
                    AND INT(ENTRY(06,bf2-tt-linha.linha,";")) = INT(ENTRY(06,tt-linha.linha,";")) THEN
                        ASSIGN i-percent-mi = i-percent-mi + DEC(ENTRY(05,bf2-tt-linha.linha,";")).
                END.

                IF i-percent-mi <> 100 THEN DO:

                    RUN pi-gera-log-imp (INPUT DEC(ENTRY(02,tt-linha.linha,";")),
                                         INPUT 30). /* codigo erro */
                    UNDO inicio, NEXT inicio.
                END.
                ELSE DO:
                
                    RUN pi-gera-mi.

                    IF VALID-HANDLE(h_api_cta_ctbl) THEN
                        DELETE PROCEDURE h_api_cta_ctbl.

                    IF VALID-HANDLE(h_api_ccusto) THEN
                        DELETE PROCEDURE h_api_ccusto.

                    IF RETURN-VALUE = "NOK":U THEN
                        UNDO inicio, NEXT inicio.
                END.
            END.

            if  de-tipo = "FR":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) THEN DO:

                RUN pi-gera-fr.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.
            
            if  de-tipo = "RE":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-re.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "EM":U 
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-em.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo       = "CE":U
            AND tt-linha.acao = 1 then DO:

                RUN pi-gera-ce.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "OR":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-or.

                IF VALID-HANDLE(h_api_cta_ctbl) THEN
                    DELETE PROCEDURE h_api_cta_ctbl.

                IF VALID-HANDLE(h_api_ccusto) THEN
                    DELETE PROCEDURE h_api_ccusto.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "PA":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-pa.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "CT":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-ct.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "IE":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-ie.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "DI":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) THEN DO:

                RUN pi-gera-di.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "DC":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) THEN DO:

                RUN pi-gera-dc.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "AC":U
            AND (tt-linha.acao = 1 OR tt-linha.acao = 2) then DO:

                RUN pi-gera-ac.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "PD":U
            AND (tt-linha.acao = 1) then DO:

                RUN pi-gera-pd.

                IF RETURN-VALUE = "NOK":U THEN
                    UNDO inicio, NEXT inicio.
            END.

            if  de-tipo = "" then leave.
        END.
    END.


END.

FOR EACH tt-importacao NO-LOCK:

    DISP tt-importacao.nr-contrato
         tt-importacao.linha      
         tt-importacao.desc-erro  
         WITH FRAME f-relat-contrat.
    DOWN WITH FRAME f-relat-contrat STREAM-IO .

END.

run pi-finalizar in h-acomp.

page.
put unformatted
    c-lb-param  skip(1)
    c-lb-data   at 5 ": " tt-param.da-transacao
    c-lb-import at 5 ": " c-arquivo /*tt-param.arq-entrada*/ skip(1).

put unformatted
    c-lb-log    skip(1)
    c-lb-impr   at 5 ": " tt-param.c-todos
    c-lb-dest   at 5 ": " tt-param.c-destino " - " tt-param.arq-destino
    c-lb-usuar  at 5 ": " tt-param.usuario.
{include/i-rpclo.i}

return 'ok':U.

PROCEDURE pi-gera-log-imp:

    DEFINE INPUT PARAM p-nr-contrato LIKE contrato-for.nr-contrato NO-UNDO.
    DEFINE INPUT PARAM p-cod-erro    AS   INTEGER                  NO-UNDO.

    CREATE tt-importacao.
    ASSIGN tt-importacao.nr-contrato = p-nr-contrato
           tt-importacao.linha       = tt-linha.num-linha.

    CASE p-cod-erro:
        WHEN 0 THEN
            ASSIGN tt-importacao.desc-erro = "Contrato " + STRING(p-nr-contrato) + " importado com sucesso.".
        WHEN 95 THEN DO:
            ASSIGN tt-importacao.desc-erro = "Contrato " + STRING(p-nr-contrato) + " : Condiá∆o de pagamento inativa.".
        END.
        WHEN 96 THEN DO:
            ASSIGN tt-importacao.desc-erro = "Item j† cadastrado em outro contrato do mesmo fornecedor. Contrato: " + STRING(i-nr-contrato-vinc) + " Est: " + c-cod-estabel-vinc.
        END.
        WHEN 97 THEN DO:
            ASSIGN tt-importacao.desc-erro = "Os itens controlados por Mediá∆o dever∆o ser do tipo dÇbito direto.".
        END.
        WHEN 98 THEN DO:
            ASSIGN tt-importacao.desc-erro = "Preáo Fornecedor deve ser diferente de 0.".
        END.
        WHEN 99 THEN DO:
            FIND FIRST tt-problema-un-negoc NO-ERROR.
            ASSIGN tt-importacao.desc-erro = SUBSTRING(tt-problema-un-negoc.c-desc-prob,1,70).
        END.
        OTHERWISE
            ASSIGN tt-importacao.desc-erro = l-descr-prob[p-cod-erro].
    END CASE.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-cc:

    DEFINE VARIABLE c-empresa-inv AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-erro-inv    AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tt-contrato.
    CREATE tt-contrato.

    IF de-sequencia = 00 THEN DO:

        IF tt-linha.acao = 1 THEN DO:

            FIND LAST contrato-for USE-INDEX contrato NO-LOCK NO-ERROR.
    
            IF AVAIL contrato-for THEN
                ASSIGN i-nr-contrato = contrato-for.nr-contrato + 1.
            ELSE
                ASSIGN i-nr-contrato = 1.

            ASSIGN i-cod-emitente = integer(ENTRY(28,tt-linha.linha,";")).
        END.

        ASSIGN tt-contrato.im-cod-emitente     = i-cod-emitente
               tt-contrato.im-nr-contrat       = integer(ENTRY(02,tt-linha.linha,";"))
               tt-contrato.im-impr-contrat     =     if (ENTRY(03,tt-linha.linha,";")) = "s" then yes else no 
               tt-contrato.im-cod-tipo-contrat = integer(ENTRY(04,tt-linha.linha,";"))
               tt-contrato.im-natureza         = integer(ENTRY(05,tt-linha.linha,";"))
               tt-contrato.im-gestor-tecnico   =         ENTRY(06,tt-linha.linha,";")
               tt-contrato.im-frete            = integer(ENTRY(07,tt-linha.linha,";"))
               tt-contrato.im-variacao-qtd     =     dec(ENTRY(08,tt-linha.linha,";"))
               tt-contrato.im-variacao-preco   =     dec(ENTRY(09,tt-linha.linha,";"))
               tt-contrato.im-cod-mensagem     = integer(ENTRY(10,tt-linha.linha,";"))
               tt-contrato.im-cod-cond-pag     = integer(ENTRY(11,tt-linha.linha,";"))
               tt-contrato.im-cod-transp       = integer(ENTRY(12,tt-linha.linha,";"))
               tt-contrato.im-via-transp       = integer(ENTRY(13,tt-linha.linha,";"))
               tt-contrato.im-val-total        =     dec(ENTRY(14,tt-linha.linha,";"))
               tt-contrato.im-cod-comprado     =         ENTRY(15,tt-linha.linha,";")
               tt-contrato.im-ind-sit-contrat  = 1
               tt-contrato.im-qtd-total        =     dec(ENTRY(17,tt-linha.linha,";"))
               tt-contrato.im-sld-qtd          =     dec(ENTRY(18,tt-linha.linha,";"))
               tt-contrato.im-sld-val          =     dec(ENTRY(19,tt-linha.linha,";"))
               tt-contrato.im-acum-rec-qtd     =     dec(ENTRY(20,tt-linha.linha,";"))
               tt-contrato.im-acum-rec-val     =     dec(ENTRY(21,tt-linha.linha,";"))
               tt-contrato.im-sld-qtd-liber    =     dec(ENTRY(22,tt-linha.linha,";"))
               tt-contrato.im-sld-val-liber    =     dec(ENTRY(23,tt-linha.linha,";"))
               tt-contrato.im-val-fatur-minimo =     dec(ENTRY(24,tt-linha.linha,";"))
               tt-contrato.im-dat-ini-validade =    date(ENTRY(25,tt-linha.linha,";")) 
               tt-contrato.im-dat-fim-validade =    date(ENTRY(26,tt-linha.linha,";"))  
               tt-contrato.im-dat-contrat      =    date(ENTRY(27,tt-linha.linha,";"))
               tt-contrato.im-dec-2            =     DEC(ENTRY(33,tt-linha.linha,";"))
               tt-contrato.im-cod-estabel      =         ENTRY(29,tt-linha.linha,";")
               tt-contrato.im-cod-estab-cobr   =         ENTRY(30,tt-linha.linha,";")
               tt-contrato.im-cod-estab-orig   =         ENTRY(31,tt-linha.linha,";")
               tt-contrato.im-cod-estab-entr   =         ENTRY(32,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-contrato.im-nr-contrat = i-nr-contrato.

        FIND FIRST contrato-for EXCLUSIVE-LOCK
             WHERE contrato-for.nr-contrato = tt-contrato.im-nr-contrat NO-ERROR.

        IF AVAIL contrato-for THEN DO:
            
            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 48). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF tt-linha.acao = 1 THEN DO:
           FIND FIRST cond-pagto
                WHERE cond-pagto.cod-cond-pag = tt-contrato.im-cod-cond-pag
                NO-LOCK NO-ERROR.
           IF NOT AVAIL cond-pagto THEN DO:
           
               RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                    INPUT 10). /* codigo erro */
               RETURN "NOK":U.
           END.
           ELSE DO:
              FIND FIRST es-cond-pagto 
                   WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                   NO-LOCK NO-ERROR.
              IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                 RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                       INPUT 95). /* codigo erro */
                 RETURN "NOK":U.
              END.
           END.

           ASSIGN i-cod-emitente = tt-contrato.im-cod-emitente.
           
           CREATE contrato-for.
           ASSIGN contrato-for.cod-emitente     = tt-contrato.im-cod-emitente
                  contrato-for.nr-contrato      = tt-contrato.im-nr-contrat
                  contrato-for.impr-contrat     = tt-contrato.im-impr-contrat
                  contrato-for.cod-tipo-contrat = tt-contrato.im-cod-tipo-contrat
                  contrato-for.gestor-tecnico   = tt-contrato.im-gestor-tecnico
                  contrato-for.frete            = tt-contrato.im-frete
                  contrato-for.variacao-qtd     = tt-contrato.im-variacao-qtd
                  contrato-for.variacao-preco   = tt-contrato.im-variacao-preco
                  contrato-for.cod-mensagem     = tt-contrato.im-cod-mensagem
                  contrato-for.cod-cond-pag     = tt-contrato.im-cod-cond-pag
                  contrato-for.cod-estabel      = tt-contrato.im-cod-estabel
                  contrato-for.cod-estab-cobr   = tt-contrato.im-cod-estab-cobr
                  contrato-for.cod-estab-orig   = tt-contrato.im-cod-estab-orig
                  contrato-for.cod-transp       = tt-contrato.im-cod-transp
                  contrato-for.via-transp       = tt-contrato.im-via-transp
                  contrato-for.val-total        = tt-contrato.im-val-total
                  contrato-for.cod-comprado     = tt-contrato.im-cod-comprado
                  contrato-for.cod-estab-entr   = tt-contrato.im-cod-estab-entr
                  contrato-for.dt-ini-validade  = tt-contrato.im-dat-ini-validade
                  contrato-for.dt-ter-validade  = tt-contrato.im-dat-fim-validade
                  contrato-for.dt-contrato      = tt-contrato.im-dat-contrat
                  contrato-for.ind-sit-contrat  = tt-contrato.im-ind-sit-contrat
                  contrato-for.qtd-total        = tt-contrato.im-qtd-total
                  contrato-for.sld-qtd          = tt-contrato.im-sld-qtd
                  contrato-for.sld-val          = tt-contrato.im-sld-val
                  contrato-for.acum-rec-qtd     = tt-contrato.im-acum-rec-qtd
                  contrato-for.acum-rec-val     = tt-contrato.im-acum-rec-val
                  contrato-for.sld-qtd-liber    = tt-contrato.im-sld-qtd-liber
                  contrato-for.sld-val-liber    = tt-contrato.im-sld-val-liber
                  contrato-for.val-fatur-minimo = tt-contrato.im-val-fatur-minimo
                  contrato-for.natureza         = tt-contrato.im-natureza
                  contrato-for.dec-2            = tt-contrato.im-dec-2.
        END.

        RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    IF de-sequencia = 01 THEN DO:

        ASSIGN tt-contrato.im-nr-contrat        = integer(ENTRY(02,tt-linha.linha,";"))
               tt-contrato.im-des-contrat       =         ENTRY(03,tt-linha.linha,";")
               tt-contrato.im-acum-val-pago     =     dec(ENTRY(04,tt-linha.linha,";"))
               tt-contrato.im-mo-codigo         = integer(ENTRY(05,tt-linha.linha,";"))
               tt-contrato.im-log-libera        =     if (ENTRY(06,tt-linha.linha,";")) = "S" then yes else no
               tt-contrato.im-tp-fornecim       = integer(ENTRY(07,tt-linha.linha,";"))
               tt-contrato.im-contato           =         ENTRY(17,tt-linha.linha,";")
               tt-contrato.im-ind-control-rec   = integer(ENTRY(09,tt-linha.linha,";"))
               tt-contrato.im-sld-qtd-med       =     dec(ENTRY(10,tt-linha.linha,";"))
               tt-contrato.im-sld-qtd-liber-med =     dec(ENTRY(11,tt-linha.linha,";"))
               tt-contrato.im-sld-val-med       =     dec(ENTRY(12,tt-linha.linha,";"))
               tt-contrato.im-sld-val-liber-med =     dec(ENTRY(13,tt-linha.linha,";"))
               tt-contrato.im-cod-projeto       =         ENTRY(14,tt-linha.linha,";")
               tt-contrato.im-cod-cond-fatur    = integer(ENTRY(15,tt-linha.linha,";"))
               tt-contrato.im-dat-revisao       =    date(ENTRY(16,tt-linha.linha,";"))
               tt-contrato.im-narrat-contrat    =         ENTRY(18,tt-linha.linha,";")
               tt-contrato.im-num-ord-inv       =     INT(ENTRY(19,tt-linha.linha,";"))
               tt-contrato.im-perc-alert-saldo  =     DEC(ENTRY(20,tt-linha.linha,";"))
               tt-contrato.im-email-alert       =         ENTRY(21,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-contrato.im-nr-contrat = i-nr-contrato.

        FIND FIRST contrato-for EXCLUSIVE-LOCK
             WHERE contrato-for.nr-contrato = tt-contrato.im-nr-contrat NO-ERROR.

        IF NOT AVAIL contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.

        if (ENTRY(06,tt-linha.linha,";")) = "" THEN DO:
            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 73). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF  tt-contrato.im-num-ord-inv <> 0
        AND contrato-for.dec-2          = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 63 ). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF  tt-contrato.im-perc-alert-saldo = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 68). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF  tt-contrato.im-email-alert  = "" THEN DO:

            RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                                 INPUT 69). /* codigo erro */
            RETURN "NOK":U.
        END.

        
             


        ASSIGN contrato-for.des-contrat       = tt-contrato.im-des-contrat 
               contrato-for.acum-val-pago     = tt-contrato.im-acum-val-pago 
               contrato-for.dat-revisao       = tt-contrato.im-dat-revisao
               contrato-for.mo-codigo         = tt-contrato.im-mo-codigo 
               contrato-for.log-libera        = tt-contrato.im-log-libera
               contrato-for.tp-fornecim       = tt-contrato.im-tp-fornecim         
               contrato-for.contato           = tt-contrato.im-contato
               contrato-for.ind-control-rec   = tt-contrato.im-ind-control-rec 
               contrato-for.sld-qtd-med       = tt-contrato.im-sld-qtd-med         
               contrato-for.sld-val-med       = tt-contrato.im-sld-val-med
               contrato-for.sal-qtd-liber-med = tt-contrato.im-sld-qtd-liber-med 
               contrato-for.sld-val-liber-med = tt-contrato.im-sld-val-liber-med 
               contrato-for.cod-projeto       = tt-contrato.im-cod-projeto         
               contrato-for.cod-cond-fatur    = tt-contrato.im-cod-cond-fatur
               contrato-for.sld-val-receb     = tt-contrato.im-sld-val-receb
               contrato-for.narrat-contrat    = tt-contrato.im-narrat-contrat
               contrato-for.num-ord-inv       = tt-contrato.im-num-ord-inv
               contrato-for.perc-alert-saldo  = tt-contrato.im-perc-alert-saldo
               contrato-for.email-alert       = tt-contrato.im-email-alert.

        /*** REALIZAR INTEGRAÄ«O COM INVESTIMENTOS ***/
        IF contrato-for.num-ord-inv <> 0 THEN DO:

            EMPTY TEMP-TABLE tt-erro-inv.
        
            IF NOT VALID-HANDLE(h-esapi001) THEN
                RUN esp\esapi001.p PERSISTENT SET h-esapi001.

            FIND FIRST estabelec NO-LOCK
                 WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-ERROR.

            IF AVAIL estabelec THEN
                ASSIGN c-empresa-inv = estabelec.ep-codigo.
            ELSE
                ASSIGN c-empresa-inv = param-global.empresa-prin.

            FIND FIRST param-inv NO-LOCK
                 WHERE param-inv.ep-codigo = c-empresa-inv NO-ERROR.

            CREATE contrato-for-ext.
            ASSIGN contrato-for-ext.nr-contrato = contrato-for.nr-contrato
                   contrato-for-ext.num-ord-inv = contrato-for.num-ord-inv
                   contrato-for-ext.ep-codigo   = c-empresa-inv.

            RUN pi-atualiza-verba IN h-esapi001 (INPUT  1, /* Valida Verba */
                                                 INPUT  c-empresa-inv,
                                                 INPUT  contrato-for.num-ord-inv,
                                                 INPUT  contrato-for.dt-contrato,
                                                 INPUT  param-inv.moeda-inv,
                                                 INPUT  contrato-for.dec-2,
                                                 INPUT  0,
                                                 OUTPUT TABLE tt-erro-inv).

            FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

            IF AVAIL tt-erro-inv THEN DO:

                ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.

                CREATE tt-importacao.
                ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                       tt-importacao.linha       = tt-linha.num-linha
                       tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).

                IF VALID-HANDLE(h-esapi001) THEN DO:
                    DELETE PROCEDURE h-esapi001.
                    ASSIGN h-esapi001 = ?.
                END.

                RETURN "NOK":U.
            END.

            /*FOR EACH  controle-inv-esp EXCLUSIVE-LOCK
                WHERE controle-inv-esp.ep-codigo    = contrato-for-ext.ep-codigo
                  AND controle-inv-esp.num-ord-inv  = contrato-for-ext.num-ord-inv
                  AND controle-inv-esp.nr-contrato  = contrato-for-ext.nr-contrato:

                IF VALID-HANDLE(h-esapi001) THEN DO:

                    RUN pi-atualiza-verba in h-esapi001 (INPUT 2,
                                                         INPUT  controle-inv-esp.ep-codigo,
                                                         INPUT  controle-inv-esp.num-ord-inv,
                                                         INPUT  controle-inv-esp.dt-trans,
                                                         INPUT  param-inv.moeda-inv,
                                                         INPUT  controle-inv-esp.ent-comp * -1,
                                                         INPUT  controle-inv-esp.ent-real * -1,
                                                         OUTPUT TABLE tt-erro-inv).
                    FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

                    IF AVAIL tt-erro-inv THEN DO:
        
                        ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.
        
                        CREATE tt-importacao.
                        ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                               tt-importacao.linha       = tt-linha.num-linha
                               tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).
        
                        IF VALID-HANDLE(h-esapi001) THEN DO:
                            DELETE PROCEDURE h-esapi001.
                            ASSIGN h-esapi001 = ?.
                        END.
        
                        RETURN "NOK":U.
                    END.
                END.
                DELETE controle-inv-esp.
            END.
            DELETE contrato-for-ext.*/

            RUN pi-principal in h-esapi001 (INPUT  1,
                                            INPUT  ROWID(contrato-for),
                                            INPUT  1,
                                            OUTPUT TABLE tt-erro-inv).

            FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

            IF AVAIL tt-erro-inv THEN DO:

                ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.

                CREATE tt-importacao.
                ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                       tt-importacao.linha       = tt-linha.num-linha
                       tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).

                IF VALID-HANDLE(h-esapi001) THEN DO:
                    DELETE PROCEDURE h-esapi001.
                    ASSIGN h-esapi001 = ?.
                END.

                RETURN "NOK":U.
            END.

            IF VALID-HANDLE(h-esapi001) THEN DO:
                DELETE PROCEDURE h-esapi001.
                ASSIGN h-esapi001 = ?.
            END.
        END.

        RUN pi-gera-log-imp (INPUT tt-contrato.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-pe:

    EMPTY TEMP-TABLE tt-pedido.
    CREATE tt-pedido.

    ASSIGN i-cont-pe = 0.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-pedido.im-num-pedido    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-pedido.im-natureza-ped  = integer(ENTRY(03,tt-linha.linha,";"))
               tt-pedido.im-emitente-ped  = integer(ENTRY(18,tt-linha.linha,";")) 
               tt-pedido.im-condicao-ped  = integer(ENTRY(04,tt-linha.linha,";"))
               tt-pedido.im-data-pedido   =    date(ENTRY(05,tt-linha.linha,";")) 
               tt-pedido.im-situacao-ped  = integer(ENTRY(06,tt-linha.linha,";"))
               tt-pedido.im-responsavel   =         ENTRY(07,tt-linha.linha,";")
               tt-pedido.im-frete-ped     = integer(ENTRY(08,tt-linha.linha,";"))
               tt-pedido.im-cod-transp    = integer(ENTRY(09,tt-linha.linha,";"))
               tt-pedido.im-via-transp    = integer(ENTRY(10,tt-linha.linha,";"))
               tt-pedido.im-cod-mensagem  = integer(ENTRY(11,tt-linha.linha,";"))
               tt-pedido.im-impr-pedido   =      if ENTRY(12,tt-linha.linha,";") = "S" then yes else no
               tt-pedido.im-emergencial   =      if ENTRY(13,tt-linha.linha,";") = "S" then yes else no
               tt-pedido.im-nr-prox-ped   = integer(ENTRY(14,tt-linha.linha,";"))
               tt-pedido.im-contr-forn    =      if ENTRY(15,tt-linha.linha,";") = "S" then yes else no
               tt-pedido.im-nr-processo   = integer(ENTRY(16,tt-linha.linha,";"))
               tt-pedido.im-nr-contrato   = integer(ENTRY(17,tt-linha.linha,";"))
               tt-pedido.im-end-entrega   =         ENTRY(19,tt-linha.linha,";")
               tt-pedido.im-end-cobranca  =         ENTRY(20,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-pedido.im-nr-contrato = i-nr-contrato.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-pedido.im-nr-contrat NO-ERROR. 

        IF NOT AVAIL contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.

        ASSIGN i-cod-emitente = contrato-for.cod-emitente.

            
        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = tt-pedido.im-emitente-ped NO-ERROR.

        IF NOT AVAIL emitente THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 02). /* codigo erro */
            RETURN "NOK":U.

        END.
            
        IF tt-pedido.im-condicao-ped <> 0 THEN DO:

            FIND FIRST cond-pagto
                 WHERE cond-pagto.cod-cond-pag = tt-pedido.im-condicao-ped NO-ERROR.

            IF NOT AVAIL cond-pagto THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 10). /* codigo erro */
                RETURN "NOK":U.
            END.
            ELSE DO:
               FIND FIRST es-cond-pagto
                    WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                    NO-LOCK NO-ERROR.
               IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                        INPUT 95). /* codigo erro */
                  RETURN "NOK":U.
               END.
            END.
        END.

        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = tt-pedido.im-end-entrega NO-ERROR.

        IF NOT AVAIL estabelec THEN DO:

            IF NOT AVAIL cond-pagto THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 12). /* codigo erro */
                RETURN "NOK":U.
            END.
            ELSE DO:
               FIND FIRST es-cond-pagto WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                    NO-LOCK NO-ERROR.
               IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                        INPUT 95). /* codigo erro */
                  RETURN "NOK":U.
               END.
            END.
        END.
            
        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = tt-pedido.im-end-cobranca NO-ERROR.

        IF NOT AVAIL estabelec THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 12). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF  tt-pedido.im-natureza-ped <> 1
        AND tt-pedido.im-natureza-ped <> 2
        AND tt-pedido.im-natureza-ped <> 3 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 24). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF  tt-pedido.im-situacao-ped <> 1
        AND tt-pedido.im-situacao-ped <> 2
        AND tt-pedido.im-situacao-ped <> 3 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 41). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND FIRST comprador NO-LOCK
             WHERE comprador.cod-comprado = tt-pedido.im-responsavel NO-ERROR.

        IF NOT AVAIL comprador THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 09). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND FIRST transporte NO-LOCK
             WHERE transporte.cod-transp = tt-pedido.im-cod-transp NO-ERROR.

        IF NOT AVAIL transporte THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 26). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF  tt-pedido.im-via-transp <> 1
        AND tt-pedido.im-via-transp <> 2
        AND tt-pedido.im-via-transp <> 3
        AND tt-pedido.im-via-transp <> 4
        AND tt-pedido.im-via-transp <> 5
        AND tt-pedido.im-via-transp <> 6
        AND tt-pedido.im-via-transp <> 7
        AND tt-pedido.im-via-transp <> 8 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 27). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF tt-pedido.im-nr-processo <> 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 28). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF CAN-FIND (FIRST pedido-compr 
                     WHERE pedido-compr.nr-contrato = tt-pedido.im-nr-contrato                       
                       AND pedido-compr.end-entrega = tt-pedido.im-end-entrega) THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 50). /* codigo erro */
            RETURN "NOK":U.
        END.            

        FIND FIRST pedido-compr EXCLUSIVE-LOCK
             WHERE pedido-compr.num-pedido = tt-pedido.im-num-pedido NO-ERROR.

        IF NOT AVAIL pedido-compr THEN DO:

            CREATE pedido-compr.
            ASSIGN pedido-compr.num-pedido   = tt-pedido.im-num-pedido    
                   pedido-compr.natureza     = tt-pedido.im-natureza-ped   
                   pedido-compr.cod-emitente = tt-pedido.im-emitente-ped 
                   pedido-compr.cod-cond-pag = tt-pedido.im-condicao-ped 
                   pedido-compr.data-pedido  = tt-pedido.im-data-pedido 
                   pedido-compr.situacao     = tt-pedido.im-situacao-ped  
                   pedido-compr.responsavel  = tt-pedido.im-responsavel
                   pedido-compr.end-entrega  = tt-pedido.im-end-entrega   
                   pedido-compr.end-cobranca = tt-pedido.im-end-cobranca
                   pedido-compr.frete        = tt-pedido.im-frete-ped
                   pedido-compr.cod-transp   = tt-pedido.im-cod-transp
                   pedido-compr.via-transp   = tt-pedido.im-via-transp
                   pedido-compr.cod-mensagem = tt-pedido.im-cod-mensagem
                   pedido-compr.impr-pedido  = tt-pedido.im-impr-pedido
                   pedido-compr.emergencial  = tt-pedido.im-emergencial
                   pedido-compr.nr-prox-ped  = tt-pedido.im-nr-prox-ped
                   pedido-compr.contr-forn   = tt-pedido.im-contr-forn
                   pedido-compr.nr-processo  = tt-pedido.im-nr-processo
                   pedido-compr.cod-estabel  = tt-pedido.im-end-entrega
                   pedido-compr.nr-contrato  = tt-pedido.im-nr-contrato.

            /* ----------- Geracao de Ordens para o Pedido do contrato -------------*/
            IF tt-pedido.im-num-pedido = 0 THEN
                FOR EACH item-contrat
                   WHERE item-contrat.nr-contrat = pedido-compr.nr-contrat
                     AND (  item-contrat.ind-tipo-control = 1 OR 
                            item-contrat.ind-tipo-control = 3 ) NO-LOCK:
                    {cnp/esp/escn0206.i2}
                END.

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
        ELSE DO:

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 29). /* codigo erro */
            RETURN "NOK":U.
        END.
    END.

    IF de-sequencia = 01 THEN DO:
               
        ASSIGN tt-pedido.im-num-pedido   = INTEGER(ENTRY(02,tt-linha.linha,";"))
               tt-pedido.im-nome-ass[1]  =         ENTRY(03,tt-linha.linha,";")
               tt-pedido.im-nome-ass[2]  =         ENTRY(04,tt-linha.linha,";")
               tt-pedido.im-nome-ass[3]  =         ENTRY(05,tt-linha.linha,";")
               tt-pedido.im-cargo-ass[1] =         ENTRY(06,tt-linha.linha,";")
               tt-pedido.im-cargo-ass[2] =         ENTRY(07,tt-linha.linha,";")
               tt-pedido.im-cargo-ass[3] =         ENTRY(08,tt-linha.linha,";").
    
        if tt-pedido.im-nome-ass[1] <> "" then 
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-nome-ass[2] <> "" then 
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-nome-ass[3] <> "" then
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-cargo-ass[1] <> "" then
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-cargo-ass[2] <> "" then 
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-cargo-ass[3] <> "" then 
           assign i-cont-pe = i-cont-pe + 1.
    
        IF i-cont-pe > 0 THEN DO:

            FIND FIRST pedido-compr EXCLUSIVE-LOCK
                 WHERE pedido-compr.num-pedido = tt-pedido.im-num-pedido NO-ERROR.

            IF NOT AVAIL pedido-compr THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 29). /* codigo erro */
                RETURN "NOK":U.
            END.
                
            ASSIGN pedido-compr.cargo-ass[1] = tt-pedido.im-cargo-ass[1] 
                   pedido-compr.cargo-ass[2] = tt-pedido.im-cargo-ass[2]  
                   pedido-compr.cargo-ass[3] = tt-pedido.im-cargo-ass[3]
                   pedido-compr.nome-ass[1]  = tt-pedido.im-nome-ass[1]
                   pedido-compr.nome-ass[2]  = tt-pedido.im-nome-ass[2]
                   pedido-compr.nome-ass[3]  = tt-pedido.im-nome-ass[3].

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 02 THEN DO:

        assign tt-pedido.im-num-pedido  = integer(ENTRY(02,tt-linha.linha,";"))
               tt-pedido.im-mot-elimina =         ENTRY(03,tt-linha.linha,";")
               tt-pedido.im-comentario  =         ENTRY(04,tt-linha.linha,";").
    
        if tt-pedido.im-mot-elimina <> "" then
           assign i-cont-pe = i-cont-pe + 1.
        if tt-pedido.im-comentario <> "" then
            assign i-cont-pe = i-cont-pe + 1.
    
        if  i-cont-pe > 0 then do:
            
            FIND FIRST pedido-compr EXCLUSIVE-LOCK
                 WHERE pedido-compr.num-pedido = tt-pedido.im-num-pedido NO-ERROR.

            IF NOT AVAIL pedido-compr THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 29). /* codigo erro */
                RETURN "NOK":U.
            END.
                
            ASSIGN pedido-compr.mot-elimina = tt-pedido.im-mot-elimina
                   pedido-compr.comentario  = tt-pedido.im-comentario.

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 03 THEN DO:

        assign tt-pedido.im-num-pedido    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-pedido.im-comentario    =         ENTRY(03,tt-linha.linha,";").
    
        if  tt-pedido.im-comentario <> "" then 
            assign i-cont-pe = i-cont-pe + 1.
    
        if  i-cont-pe > 0 then do:

            FIND FIRST pedido-compr EXCLUSIVE-LOCK
                 WHERE pedido-compr.num-pedido = tt-pedido.im-num-pedido NO-ERROR.

            IF NOT AVAIL pedido-compr THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 29). /* codigo erro */
                RETURN "NOK":U.
            END.
                
            ASSIGN pedido-compr.comentario = tt-pedido.im-comentario.

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 04 THEN DO:

        ASSIGN tt-pedido.im-num-pedido  = integer(ENTRY(02,tt-linha.linha,";"))
               tt-pedido.im-comentario  =         ENTRY(03,tt-linha.linha,";").
    
        IF tt-pedido.im-comentario <> "" THEN
            ASSIGN i-cont-pe = i-cont-pe + 1.
    
        IF i-cont-pe > 0 THEN DO:
            
            FIND FIRST pedido-compr EXCLUSIVE-LOCK
                 WHERE pedido-compr.num-pedido = tt-pedido.im-num-pedido NO-ERROR. 

            IF NOT AVAIL pedido-compr THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                     INPUT 29). /* codigo erro */
                RETURN "NOK":U.
            END.
                
            ASSIGN pedido-compr.comentario = tt-pedido.im-comentario.

            RUN pi-gera-log-imp (INPUT tt-pedido.im-nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

DEFINE VARIABLE i-ind-tipo-control LIKE tt-item-contrato.im-ind-tipo-control.

PROCEDURE pi-gera-ic:

    DEFINE VARIABLE c-empresa-inv AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-erro-inv    AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tt-item-contrato.
    CREATE tt-item-contrato.

    IF de-sequencia = 00 THEN DO:

        ASSIGN i-ind-tipo-control = 0.

        ASSIGN i-num-seq-item = INTEGER(ENTRY(16,tt-linha.linha,";")).
        
        IF tt-linha.acao = 1 THEN DO:
           ASSIGN i-num-seq-item = integer(ENTRY(16,tt-linha.linha,";")).
        END.
        ELSE DO:
           IF i-num-seq-item = 0 THEN DO:
              IF NOT CAN-FIND (FIRST item-contrat WHERE item-contrat.nr-contrato = int(ENTRY(02,tt-linha.linha,";"))) THEN
                 ASSIGN i-num-seq-item = 1.
              ELSE 
                 blk1:
                 FOR EACH item-contrat
                          WHERE item-contrat.nr-contrato = int(ENTRY(02,tt-linha.linha,";"))
                          NO-LOCK
                          BY item-contrat.num-seq-item DESCENDING:
                 
                    ASSIGN i-cod-emitente = item-contrat.cod-emitente.
                 
                    ASSIGN i-num-seq-item = item-contrat.num-seq-item + 1.
                    LEAVE blk1.
                 END.
           END.
           /*ELSE DO:
              ASSIGN i-num-seq-item = i-num-seq-item + 1.
           END. */
        END.

        ASSIGN tt-item-contrato.im-cod-emitente         = i-cod-emitente
               tt-item-contrato.im-nr-contrat           =     int(ENTRY(02,tt-linha.linha,";"))
               tt-item-contrato.im-preco-unit           = decimal(ENTRY(03,tt-linha.linha,";"))
               tt-item-contrato.im-qtd-minima           = decimal(ENTRY(04,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val              = decimal(ENTRY(05,tt-linha.linha,";"))
               tt-item-contrato.im-val-fatur-minimo     = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-item-contrato.im-mo-codigo            = integer(ENTRY(07,tt-linha.linha,";"))
               tt-item-contrato.im-log-libera           =     if (ENTRY(08,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-it-codigo            =         ENTRY(09,tt-linha.linha,";")
               tt-item-contrato.im-val-total            = decimal(ENTRY(10,tt-linha.linha,";"))
               tt-item-contrato.im-cod-refer            =         ENTRY(11,tt-linha.linha,";")
               tt-item-contrato.im-codigo-ipi           =     if (ENTRY(12,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-codigo-icm           = integer(ENTRY(13,tt-linha.linha,";"))
               tt-item-contrato.im-un                   =         ENTRY(14,tt-linha.linha,";")
               tt-item-contrato.im-contato              =         ENTRY(15,tt-linha.linha,";")
               tt-item-contrato.im-num-seq-item         = i-num-seq-item
               tt-item-contrato.im-frequencia           = integer(ENTRY(17,tt-linha.linha,";"))
               tt-item-contrato.im-ind-sit-item         = 1
               tt-item-contrato.im-qtd-total            = decimal(ENTRY(19,tt-linha.linha,";"))
               tt-item-contrato.im-ind-un-contrato      = integer(ENTRY(20,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd              = decimal(ENTRY(21,tt-linha.linha,";"))
               tt-item-contrato.im-acum-rec-val         = decimal(ENTRY(22,tt-linha.linha,";"))
               tt-item-contrato.im-acum-rec-qtd         = decimal(ENTRY(23,tt-linha.linha,";"))
               tt-item-contrato.im-ind-tipo-control-val =     int(ENTRY(24,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd-liber        = decimal(ENTRY(25,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val-liber        = decimal(ENTRY(26,tt-linha.linha,";"))
               tt-item-contrato.im-log-control-event    =     if (ENTRY(27,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-ind-caract-item      = integer(ENTRY(28,tt-linha.linha,";"))
               tt-item-contrato.im-log-obrig-item       =     if (ENTRY(29,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-log-ind-multa        =     if (ENTRY(30,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-perc-multa-dia       = decimal(ENTRY(31,tt-linha.linha,";"))
               tt-item-contrato.im-perc-multa-limite    = decimal(ENTRY(32,tt-linha.linha,";"))
               tt-item-contrato.im-cod-depos            =         ENTRY(33,tt-linha.linha,";")
               tt-item-contrato.im-aliquota-icm         = decimal(ENTRY(34,tt-linha.linha,";"))
               tt-item-contrato.im-aliquota-ipi         = decimal(ENTRY(35,tt-linha.linha,";"))
               tt-item-contrato.im-aliquota-iss         = decimal(ENTRY(36,tt-linha.linha,";"))
               tt-item-contrato.im-tp-despesa           = integer(ENTRY(37,tt-linha.linha,";"))
               tt-item-contrato.im-cod-cond-pag         = integer(ENTRY(38,tt-linha.linha,";"))
               tt-item-contrato.im-frete-ped            =     if (ENTRY(39,tt-linha.linha,";")) = "S" then yes else no.

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-item-contrato.im-nr-contrat = i-nr-contrato.

        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = tt-item-contrato.im-cod-emitente NO-ERROR.

        IF NOT AVAIL emitente THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 02). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF (ENTRY(08,tt-linha.linha,";")) = "" THEN DO:
            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 77). /* codigo erro */
            RETURN "NOK":U.
        END.
           
        FIND FIRST moeda NO-LOCK
             WHERE moeda.mo-codigo = tt-item-contrato.im-mo-codigo NO-ERROR.

        IF NOT AVAIL moeda THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 03). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND FIRST item NO-LOCK
             WHERE item.it-codigo = tt-item-contrato.im-it-codigo NO-ERROR.

        IF NOT AVAIL item THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:
            ASSIGN c-it-codigo-ic00 = tt-item-contrato.im-it-codigo.
        END.
            
        /*IF  AVAIL ITEM
        AND ITEM.tipo-contr <> 4 
        AND tt-item-contrato.im-ind-tipo-control-val = 1 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 97). /* codigo erro */
            RETURN "NOK":U.
        END.*/

        FIND FIRST tab-unidade NO-LOCK
             WHERE tab-unidade.un = tt-item-contrato.im-un NO-ERROR.

        IF NOT AVAIL tab-unidade THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 05). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND FIRST deposito NO-LOCK
             WHERE deposito.cod-depos = tt-item-contrato.im-cod-depos NO-ERROR.

        IF NOT AVAIL deposito THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 08). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF tt-linha.acao = 1 THEN DO:
           FIND FIRST cond-pagto NO-LOCK
                WHERE cond-pagto.cod-cond-pag = tt-item-contrato.im-cod-cond-pag NO-ERROR.
           
           IF NOT AVAIL cond-pagto THEN DO:
           
               RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                    INPUT 10). /* codigo erro */
               RETURN "NOK":U.
           END.
           ELSE DO:
              FIND FIRST es-cond-pagto WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                   NO-LOCK NO-ERROR.
              IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                 RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                       INPUT 95). /* codigo erro */
                 RETURN "NOK":U.
              END.
           END.
        END.
            
        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-item-contrato.im-nr-contrat NO-ERROR.

        IF AVAIL contrato-for THEN DO:

            IF CAN-FIND(FIRST item-contrat NO-LOCK
                        WHERE item-contrat.nr-contrato  = tt-item-contrato.im-nr-contrat
                          AND item-contrat.num-seq-item = tt-item-contrato.im-num-seq-item) THEN DO:

                RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                     INPUT 06). /* codigo erro */
                RETURN "NOK":U.
            END.

            IF tt-item-contrato.im-it-codigo = "" OR
              (AVAIL ITEM AND 
               item.tipo-contr = 4 AND
               NOT CAN-FIND (FIRST item-fornec WHERE item-fornec.it-codigo = item.it-codigo
                             AND   item-fornec.cod-emitente = contrato-for.cod-emitente
                             AND   item-fornec.ativo) )
            THEN .
            ELSE DO:
                FIND FIRST item-contrat NO-LOCK
                     WHERE item-contrat.it-codigo     = tt-item-contrato.im-it-codigo  
                      AND  item-contrat.nr-contrato   = tt-item-contrato.im-nr-contrat 
                      AND  item-contrat.ind-sit-item <> 3                              
                      AND  item-contrat.ind-sit-item <> 4 NO-ERROR.

                IF AVAIL item-contrat THEN DO:

                    RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                         INPUT 06). /* codigo erro */
                    RETURN "NOK":U.

                END.
                   
                FOR EACH item-contrat NO-LOCK
                   WHERE item-contrat.it-codigo         = tt-item-contrato.im-it-codigo 
                     AND item-contrat.ind-tipo-control <> 1                              
                     AND item-contrat.ind-sit-item     <> 3                             
                     AND item-contrat.ind-sit-item     <> 4:

                    FIND  b-contrato-for NO-LOCK
                    WHERE b-contrato-for.nr-contrato      = item-contrat.nr-contrato     
                      AND b-contrato-for.cod-emitente     = contrato-for.cod-emitente    
                      AND b-contrato-for.dt-ini-validade <= contrato-for.dt-ter-validade 
                      AND b-contrato-for.dt-ter-validade >= contrato-for.dt-ini-validade 
                      AND b-contrato-for.log-libera       = YES NO-ERROR.

                    IF AVAIL b-contrato-for THEN DO:

                        FOR EACH b-pedido-compr FIELD (nr-contrat end-entrega)
                           WHERE b-pedido-compr.nr-contrat = contrato-for.nr-contrato NO-LOCK:

                            FOR FIRST b2-pedido-compr NO-LOCK
                                WHERE b2-pedido-compr.end-entrega = b-pedido-compr.end-entrega  
                                  AND b2-pedido-compr.nr-contrato = b-contrato-for.nr-contrato:

                                ASSIGN i-nr-contrato-vinc = b2-pedido-compr.nr-contrato
                                       c-cod-estabel-vinc = b2-pedido-compr.end-entrega.

                                RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                                     INPUT 96). /* codigo erro */
                                RETURN "NOK":U.
                            END.
                        END.
                    END.
                END.
            END.
        END.  
        ELSE DO: 

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
        
        CREATE item-contrat.        
        ASSIGN item-contrat.cod-emitente         = tt-item-contrato.im-cod-emitente     
               item-contrat.nr-contrat           = tt-item-contrato.im-nr-contrat 
               item-contrat.preco-unit           = tt-item-contrato.im-preco-unit /*/ 100000*/
               item-contrat.qtd-minima           = tt-item-contrato.im-qtd-minima /*/ 10000*/
               item-contrat.sld-val              = tt-item-contrato.im-sld-val /*/ 10000*/
               item-contrat.val-fatur-minimo     = tt-item-contrato.im-val-fatur-minimo /*/ 10000*/
               item-contrat.mo-codigo            = tt-item-contrato.im-mo-codigo      
               item-contrat.log-libera           = tt-item-contrato.im-log-libera
               item-contrat.it-codigo            = tt-item-contrato.im-it-codigo        
               item-contrat.val-total            = tt-item-contrato.im-val-total /*/ 10000       */
               item-contrat.cod-refer            = tt-item-contrato.im-cod-refer        
               item-contrat.codigo-ipi           = tt-item-contrato.im-codigo-ipi
               item-contrat.codigo-icm           = tt-item-contrato.im-codigo-icm
               item-contrat.un                   = tt-item-contrato.im-un               
               item-contrat.contato              = tt-item-contrato.im-contato          
               item-contrat.num-seq-item         = i-num-seq-item     
               item-contrat.frequencia           = tt-item-contrato.im-frequencia       
               item-contrat.ind-sit-item         = tt-item-contrato.im-ind-sit-item     
               item-contrat.qtd-total            = tt-item-contrato.im-qtd-total /*/ 10000      */
               item-contrat.ind-un-contrato      = tt-item-contrato.im-ind-un-contrato  
               item-contrat.sld-qtd              = tt-item-contrato.im-sld-qtd /*/ 10000        */
               item-contrat.acum-rec-val         = tt-item-contrato.im-acum-rec-val /*/ 10000     */
               item-contrat.acum-rec-qtd         = tt-item-contrato.im-acum-rec-qtd /*/ 10000  */
               item-contrat.ind-tipo-control-val = tt-item-contrato.im-ind-tipo-control-val
               item-contrat.sld-qtd-liber        = tt-item-contrato.im-sld-qtd-liber /*/ 10000   */
               item-contrat.sld-val-liber        = tt-item-contrato.im-sld-val-liber /*/ 10000*/
               item-contrat.log-control-event    = tt-item-contrato.im-log-control-event
               item-contrat.ind-caract-item      = int(tt-item-contrato.im-ind-caract-item)
               item-contrat.log-obrig-item       = tt-item-contrato.im-log-obrig-item
               item-contrat.log-ind-multa        = tt-item-contrato.im-log-ind-multa      
               item-contrat.perc-multa-dia       = tt-item-contrato.im-perc-multa-dia /*/ 100*/
               item-contrat.perc-multa-limite    = tt-item-contrato.im-perc-multa-limite /*/ 100*/
               item-contrat.cod-depos            = tt-item-contrato.im-cod-depos
               item-contrat.aliquota-icm         = tt-item-contrato.im-aliquota-icm /*/ 100*/
               item-contrat.aliquota-ipi         = tt-item-contrato.im-aliquota-ipi /*/ 100*/
               item-contrat.aliquota-iss         = tt-item-contrato.im-aliquota-iss /*/ 100*/
               item-contrat.cod-cond-pag         = tt-item-contrato.im-cod-cond-pag
               item-contrat.frete                = tt-item-contrato.im-frete-ped
               item-contrat.tp-despesa           = tt-item-contrato.im-tp-despesa.

        IF tt-linha.acao = 2 THEN
           RUN pi-gera-aditivo.

        ASSIGN i-ind-tipo-control = tt-item-contrato.im-ind-tipo-control.

        RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    IF de-sequencia = 01 THEN DO:

        ASSIGN tt-item-contrato.im-nr-contrat       =     int(ENTRY(02,tt-linha.linha,";"))
               tt-item-contrato.im-num-seq-item     = integer(ENTRY(03,tt-linha.linha,";"))
               tt-item-contrato.im-preco-fornec     = decimal(ENTRY(04,tt-linha.linha,";"))
               tt-item-contrato.im-taxa-financ      =     if (ENTRY(05,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-val-frete        = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-item-contrato.im-val-taxa         = decimal(ENTRY(07,tt-linha.linha,";"))
               tt-item-contrato.im-prazo-ent        = integer(ENTRY(08,tt-linha.linha,";"))
               tt-item-contrato.im-dat-cotac        =    date(ENTRY(09,tt-linha.linha,";"))
               tt-item-contrato.im-preco-base       = decimal(ENTRY(10,tt-linha.linha,";"))
               tt-item-contrato.im-cod-comprado     =         ENTRY(11,tt-linha.linha,";")
               tt-item-contrato.im-perc-desconto    = decimal(ENTRY(12,tt-linha.linha,";"))
               tt-item-contrato.im-narrat-compra    =         ENTRY(13,tt-linha.linha,";")
               tt-item-contrato.im-ind-tipo-control =     int(ENTRY(14,tt-linha.linha,";"))
               tt-item-contrato.im-pre-unit-for     = decimal(ENTRY(15,tt-linha.linha,";")) 
               tt-item-contrato.im-dat-base         =    date(ENTRY(16,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd-receb    = decimal(ENTRY(17,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val-receb    = decimal(ENTRY(18,tt-linha.linha,";"))
               tt-item-contrato.im-ordem-base       = integer(ENTRY(19,tt-linha.linha,";"))
               tt-item-contrato.im-narrat-item      =         ENTRY(20,tt-linha.linha,";")
               tt-item-contrato.im-perc-compra      = decimal(ENTRY(21,tt-linha.linha,";"))
               tt-item-contrato.im-num-ord-invest   = integer(ENTRY(22,tt-linha.linha,";")).

/*         IF tt-item-contrato.im-codigo-ipi = NO THEN DO:                                                                                                                       */
/*            ASSIGN tt-item-contrato.im-pre-unit-for = tt-item-contrato.im-preco-fornec + ROUND(tt-item-contrato.im-preco-fornec * (tt-item-contrato.im-aliquota-ipi / 100),2). */
/*         END.                                                                                                                                                                  */
/*         ELSE DO:                                                                                                                                                              */
/*            ASSIGN tt-item-contrato.im-pre-unit-for = tt-item-contrato.im-preco-fornec.                                                                                        */
/*         END.                                                                                                                                                                  */


/*         MESSAGE "de-sequencia => IC" de-sequencia skip                                */
/*            "tt-item-contrato.im-preco-fornec" tt-item-contrato.im-preco-fornec SKIP   */
/*            "tt-item-contrato.im-pre-unit-for " tt-item-contrato.im-pre-unit-for  SKIP */
/*           " tt-item-contrato.im-num-seq-item"  tt-item-contrato.im-num-seq-item       */
/*                                                                                       */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                          */

 FIND FIRST item-contrat EXCLUSIVE-LOCK
                WHERE item-contrat.nr-contrat   = tt-item-contrato.im-nr-contrat   
                  AND item-contrat.num-seq-item = tt-item-contrato.im-num-seq-item NO-ERROR.


        IF tt-linha.acao = 1 THEN DO:
           ASSIGN tt-item-contrato.im-nr-contrat = i-nr-contrato.

           FIND FIRST item-contrat EXCLUSIVE-LOCK
                WHERE item-contrat.nr-contrat   = tt-item-contrato.im-nr-contrat   
                  AND item-contrat.num-seq-item = tt-item-contrato.im-num-seq-item NO-ERROR.
           
           IF NOT AVAIL item-contrat THEN DO:
           
               RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                    INPUT 07). /* codigo erro */
               RETURN "NOK":U.
           END.

           IF tt-contrato.im-ind-control-rec = 1 THEN DO:
               IF tt-item-contrato.im-ind-tipo-control <> 1 THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                       INPUT 72). /* codigo erro */
                   RETURN "NOK":U.
               END.
           END.
        END.
        
        IF tt-linha.acao = 1 THEN DO:
           CASE i-ind-tipo-control:
                WHEN 1 THEN DO:
                   IF CAN-FIND(FIRST tt-item-contrato
                               WHERE (tt-item-contrato.im-ind-un-contrato      <> 1 ) OR
                                     (tt-item-contrato.im-ind-caract-item      <> 1 ) OR
                                     (tt-item-contrato.im-ind-tipo-control-val =  1 )
                               NO-LOCK) THEN DO:
                      RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                           INPUT 72). /* codigo erro */
                      RETURN "NOK":U.
                   END.
                END.
                WHEN 2 OR WHEN 3 THEN DO:
                   IF CAN-FIND(FIRST tt-item-contrato
                               WHERE (tt-item-contrato.im-ind-un-contrato      <> 1 ) OR
                                     (tt-item-contrato.im-ind-caract-item      <> 1 ) OR
                                     (tt-item-contrato.im-ind-tipo-control-val <> 1 )
                               NO-LOCK) THEN DO:
                      RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                           INPUT 72). /* codigo erro */
                      RETURN "NOK":U.
                   END.
                END.
           END CASE.
        END.

        FIND FIRST ITEM NO-LOCK
             WHERE ITEM.it-codigo = tt-item-contrato.im-it-codigo 
             NO-ERROR.

        IF  AVAIL ITEM
        AND ITEM.tipo-contr <> 4 
        AND tt-item-contrato.im-ind-tipo-control = 1 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 97). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF tt-linha.acao = 1 THEN DO:
           FIND FIRST comprador NO-LOCK
                WHERE comprador.cod-comprado = tt-item-contrato.im-cod-comprado NO-ERROR.
           
           IF NOT AVAIL comprador THEN DO:
           
               RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                    INPUT 09). /* codigo erro */
               RETURN "NOK":U.
           END.
        END.
            
        IF  tt-item-contrato.im-num-ord-invest <> 0
        AND item-contrat.val-total              = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 62). /* codigo erro */
            RETURN "NOK":U.
        END.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-item-contrato.im-nr-contrat NO-ERROR.

        IF  AVAIL contrato-for
        AND contrato-for.num-ord-inv           <> 0
        AND tt-item-contrato.im-num-ord-invest <> 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 65). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF item.tipo-contr <> 4 THEN DO:
        
            IF tt-item-contrato.im-perc-compra <= 0
            OR tt-item-contrato.im-perc-compra >  100 THEN DO:
    
                RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                     INPUT 60). /* codigo erro */
                RETURN "NOK":U.
            END.
            ELSE DO:
    
                FIND FIRST item-fornec NO-LOCK
                     WHERE item-fornec.it-codigo    = item-contrat.it-codigo
                       AND item-fornec.cod-emitente = item-contrat.cod-emitente NO-ERROR.
    
                IF AVAIL item-fornec THEN DO:
    
                    IF item-fornec.perc-compra <> tt-item-contrato.im-perc-compra THEN DO:
                        
                        RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                             INPUT 61). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                END.
                ELSE DO:
    
                    IF tt-item-contrato.im-perc-compra <> 100 THEN DO:
    
                        RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                             INPUT 64). /* codigo erro */
                        RETURN "NOK":U.
                    END.
    
                    FOR EACH item-fornec EXCLUSIVE-LOCK
                       WHERE item-fornec.it-codigo = item-contrat.it-codigo:
    
                        ASSIGN item-fornec.perc-compra = 0.
                    END.
    
                    CREATE item-fornec.
                    ASSIGN item-fornec.it-codigo           = item-contrat.it-codigo
                           item-fornec.cod-emitente        = item-contrat.cod-emitente
                           item-fornec.item-do-forn        = item.it-codigo
                           item-fornec.unid-med-for        = item.un
                           item-fornec.fator-conver        = 1
                           item-fornec.contr-forn          = YES
                           item-fornec.ativo               = YES
                           item-fornec.lote-minimo         = 1
                           item-fornec.lote-mul-for        = 1
                           item-fornec.classe-repro        = 1
                           item-fornec.perc-compra         = tt-item-contrato.im-perc-compra
                           item-fornec.idi-tributac-pis    = 2
                           item-fornec.idi-tributac-cofins = 2.
                END.
            END.
        END.
            
        ASSIGN item-contrat.preco-fornec     = tt-item-contrato.im-preco-fornec /*/ 100000     */
               item-contrat.taxa-financ      = tt-item-contrato.im-taxa-financ
               item-contrat.val-frete        = tt-item-contrato.im-val-frete /*/ 10000*/
               item-contrat.val-taxa         = tt-item-contrato.im-val-taxa  /*/ 10000*/
               item-contrat.prazo-ent        = tt-item-contrato.im-prazo-ent
               item-contrat.dat-cotac        = tt-item-contrato.im-dat-cotac
               item-contrat.preco-base       = tt-item-contrato.im-preco-base /*/ 100000*/
               item-contrat.cod-comprado     = tt-item-contrato.im-cod-comprado
               item-contrat.perc-desconto    = tt-item-contrato.im-perc-desconto /*/ 100*/
               item-contrat.narrat-compra    = tt-item-contrato.im-narrat-compra
               item-contrat.ind-tipo-control = tt-item-contrato.im-ind-tipo-control
               item-contrat.pre-unit-for     = tt-item-contrato.im-pre-unit-for /*/ 100000*/
               item-contrat.dat-base         = tt-item-contrato.im-dat-base
               item-contrat.sld-qtd-receb    = tt-item-contrato.im-sld-qtd-receb /*/ 10000*/
               item-contrat.sld-val-receb    = tt-item-contrato.im-sld-val-receb /*/ 10000*/
               item-contrat.narrat-item      = tt-item-contrato.im-narrat-item
               item-contrat.num-ord-inv      = tt-item-contrato.im-num-ord-invest.

        RUN pi-calcula-preco.


        /*** REALIZAR INTEGRAÄ«O COM INVESTIMENTOS AQUI ***/
        IF item-contrat.num-ord-inv <> 0 THEN DO:

            EMPTY TEMP-TABLE tt-erro-inv.

            IF NOT VALID-HANDLE(h-esapi001) THEN
                RUN esp\esapi001.p PERSISTENT SET h-esapi001.

            IF AVAIL contrato-for THEN DO:

                FIND FIRST estabelec NO-LOCK
                     WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-ERROR.

                IF AVAIL estabelec THEN
                    ASSIGN c-empresa-inv = estabelec.ep-codigo.
            END.
            ELSE DO:

                FIND FIRST param-global NO-LOCK NO-ERROR.
                ASSIGN c-empresa-inv = param-global.empresa-prin.
            END.

            FIND FIRST param-inv NO-LOCK
                 WHERE param-inv.ep-codigo = c-empresa-inv NO-ERROR.

            CREATE item-contrat-ext.
            ASSIGN item-contrat-ext.nr-contrato  = item-contrat.nr-contrato
                   item-contrat-ext.num-seq-item = item-contrat.num-seq-item
                   item-contrat-ext.num-ord-inv  = item-contrat.num-ord-inv
                   item-contrat-ext.ep-codigo    = c-empresa-inv.

            RUN pi-atualiza-verba IN h-esapi001 (INPUT  1, /* Valida Verba */
                                                 INPUT  c-empresa-inv,
                                                 INPUT  item-contrat-ext.num-ord-inv,
                                                 INPUT  contrato-for.dt-contrato,
                                                 INPUT  param-inv.moeda-inv,
                                                 INPUT  item-contrat.val-total,
                                                 INPUT  0,
                                                 OUTPUT TABLE tt-erro-inv).
                
            FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

            IF AVAIL tt-erro-inv THEN DO:

                ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.

                CREATE tt-importacao.
                ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                       tt-importacao.linha       = tt-linha.num-linha
                       tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).

                IF VALID-HANDLE(h-esapi001) THEN DO:
                    DELETE PROCEDURE h-esapi001.
                    ASSIGN h-esapi001 = ?.
                END.

                RETURN "NOK":U.
            END.

            /*FOR EACH controle-inv-esp EXCLUSIVE-LOCK
               WHERE controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
                 AND controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                 AND controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
                 AND controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:

                RUN pi-atualiza-verba IN h-esapi001 (INPUT 2,
                                                     INPUT controle-inv-esp.ep-codigo,
                                                     INPUT controle-inv-esp.num-ord-inv,
                                                     INPUT controle-inv-esp.dt-trans,
                                                     INPUT param-inv.moeda-inv,
                                                     INPUT controle-inv-esp.ent-comp * -1,
                                                     INPUT controle-inv-esp.ent-real * -1,
                                                     OUTPUT TABLE tt-erro-inv).

                FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

                IF AVAIL tt-erro-inv THEN DO:
    
                    ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.
    
                    CREATE tt-importacao.
                    ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                           tt-importacao.linha       = tt-linha.num-linha
                           tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).
    
                    IF VALID-HANDLE(h-esapi001) THEN DO:
                        DELETE PROCEDURE h-esapi001.
                        ASSIGN h-esapi001 = ?.
                    END.
    
                    RETURN "NOK":U.
                END.

                DELETE controle-inv-esp.
            END.
            DELETE item-contrat-ext.*/

            RUN pi-principal IN h-esapi001 (INPUT 2,
                                            INPUT ROWID(item-contrat),
                                            INPUT 1,
                                            OUTPUT TABLE tt-erro-inv).

            FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

            IF AVAIL tt-erro-inv THEN DO:

                ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.

                CREATE tt-importacao.
                ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                       tt-importacao.linha       = tt-linha.num-linha
                       tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).

                IF VALID-HANDLE(h-esapi001) THEN DO:
                    DELETE PROCEDURE h-esapi001.
                    ASSIGN h-esapi001 = ?.
                END.

                RETURN "NOK":U.
            END.
        END.

        IF (item-contrat.ind-tipo-control = 1
        OR  item-contrat.ind-tipo-control = 3)
        AND item-contrat.preco-fornec     > 0 THEN DO:

            FIND FIRST param-contrat NO-LOCK NO-ERROR.
            
            FOR EACH pedido-compr NO-LOCK
               WHERE pedido-compr.nr-contrato = item-contrat.nr-contrato:

                IF contrato-for.ind-control-rec = 1 THEN
                    FIND FIRST ordem-compra NO-LOCK
                         WHERE ordem-compra.nr-contrato = contrato-for.nr-contrato
                           AND ordem-compra.cod-estabel = pedido-compr.cod-estabel
                           AND ordem-compra.it-codigo   = param-contrat.it-codigo NO-ERROR.

                IF  contrato-for.ind-control-rec = 1
                AND AVAIL ordem-compra THEN NEXT.

                RUN pi-gera-ordem-auto.

                IF RETURN-VALUE = "NOK":U THEN
                    RETURN "NOK":U.
            END.
        END.

        RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-mc:

    EMPTY TEMP-TABLE tt_log_erro.
    EMPTY TEMP-TABLE tt-erro-aux.
    EMPTY TEMP-TABLE tt-matriz-contrato.
    CREATE tt-matriz-contrato.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-matriz-contrato.im-nr-contrat      =     int(ENTRY(02,tt-linha.linha,";"))
               tt-matriz-contrato.im-ct-codigo       =    trim(ENTRY(03,tt-linha.linha,";"))
               tt-matriz-contrato.im-sc-codigo       =    trim(ENTRY(04,tt-linha.linha,";"))
               tt-matriz-contrato.im-perc-rateio     = decimal(ENTRY(05,tt-linha.linha,";")).
               tt-matriz-contrato.im-cod-unid-negoc  =         ENTRY(06,tt-linha.linha,";").

        IF  NOT CAN-FIND(FIRST unid-negoc
                         WHERE unid-negoc.cod-unid-negoc = tt-matriz-contrato.im-cod-unid-negoc) THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 58). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-matriz-contrato.im-nr-contrat = i-nr-contrato.

        RUN prgint\utb\utb743za.py PERSISTENT SET h_api_cta_ctbl.
        RUN prgint\utb\utb742za.py PERSISTENT SET h_api_ccusto.
    
        FIND FIRST matriz-rat-contr EXCLUSIVE-LOCK
             WHERE matriz-rat-contr.nr-contrat     = tt-matriz-contrato.im-nr-contrat
               AND matriz-rat-contr.ct-codigo      = tt-matriz-contrato.im-ct-codigo
               AND matriz-rat-contr.sc-codigo      = tt-matriz-contrato.im-sc-codigo
               AND matriz-rat-contr.cod-unid-negoc = tt-matriz-contrato.im-cod-unid-negoc NO-ERROR.

        IF AVAIL matriz-rat-contr THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 47). /* codigo erro */
            RETURN "NOK":U.
        END.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-matriz-contrato.im-nr-contrat NO-ERROR.

        IF NOT AVAIL contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF AVAIL contrato-for THEN DO:

            FIND FIRST pedido-compr NO-LOCK
                 WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-ERROR.

            IF AVAIL pedido-compr THEN DO:

                FIND FIRST estabelec NO-LOCK
                     WHERE estabelec.cod-estabel = pedido-compr.end-entrega NO-ERROR.
            END.
            ELSE
                FIND FIRST estabelec NO-LOCK
                     WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-ERROR.

            IF AVAIL estabelec THEN
                    ASSIGN i-empresa = estabelec.ep-codigo.
        END.

        ASSIGN v_cod_cta_ctbl  = tt-matriz-contrato.im-ct-codigo
               i-lista-estoque = 0.
        
        run pi_busca_dados_cta_ctbl in h_api_cta_ctbl (input        STRING(i-empresa),  /* EMPRESA EMS2 */
                                                       input        "",                 /* PLANO DE CONTAS */
                                                       input-output v_cod_cta_ctbl,     /* CONTA */
                                                       input        today,              /* DATA TRANSACAO */   
                                                       output       v_des_cta_ctbl,     /* DESCRICAO CONTA */
                                                       output       v_num_tip_cta_ctbl, /* TIPO DA CONTA */
                                                       output       v_num_sit_cta_ctbl, /* SITUA∞ÄO DA CONTA */
                                                       output       v_ind_finalid_cta,  /* FINALIDADES DA CONTA */
                                                       output table tt_log_erro).       /* ERROS */
													   
		run pi_busca_dados_cta_ctbl_integr in h_api_cta_ctbl (input        STRING(i-empresa), /* EMPRESA EMS2 */
                                                              input        "CEP",             /* M‡DULO */
                                                              input        "",                /* PLANO DE CONTAS */
                                                              input        v_cod_cta_ctbl,    /* CONTA */
                                                              input        today,             /* DATA TRANSACAO */   
                                                              output       v_ind_finalid_cta, /* FINALIDADES DA CONTA */
                                                              output table tt_log_erro).      /* ERROS */ 

        FIND FIRST tt_log_erro NO-ERROR.

        IF NOT AVAIL tt_log_erro THEN
            ASSIGN i-lista-estoque = {adinc/i05ad049.i 6 v_ind_finalid_cta}.
        
        IF AVAIL tt_log_erro THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 13). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO: 
            IF v_num_tip_cta_ctbl  = 6 or
               v_num_sit_cta_ctbl <> 3 or
               i-lista-estoque     = 0 or
               i-lista-estoque     = 9 THEN DO:

                RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                     INPUT 14). /* codigo erro */
                RETURN "NOK":U.
            END.
        END.

        RUN pi_verifica_utilizacao_ccusto in h_api_ccusto (input i-empresa,           /* EMPRESA EMS 2 */
                                                           input estabelec.cod-estabel,/* ESTABELECIMENTO EMS2 */
                                                           input "",                  /* PLANO CONTAS */
                                                           input v_cod_cta_ctbl,      /* CONTA */
                                                           input today,               /* DT TRANSACAO */
                                                           output v_log_utz_ccusto,    /* UTILIZA CCUSTO ? */
                                                           output table tt_log_erro). /* ERROS */
        
        IF v_log_utz_ccusto = no and tt-matriz-contrato.im-sc-codigo <> "" THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 59). /* codigo erro */
            RETURN "NOK":U.

        END.
            
        IF tt-matriz-contrato.im-cod-unid-negoc <> "" THEN DO:

            IF AVAIL contrato-for THEN DO:

                FIND FIRST pedido-compr NO-LOCK
                      WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-ERROR.

                IF AVAIL pedido-compr THEN DO:

                    RUN cdp/cdapi024.p persistent set h-cdapi024.
                     
                     /*UN X Estabelecimento*/
                    RUN ValidaUnidadeNegocioEstabel IN h-cdapi024 (INPUT pedido-compr.end-entrega,
                                                                   INPUT TODAY,
                                                                   INPUT tt-matriz-contrato.im-cod-unid-negoc,
                                                                   OUTPUT TABLE tt-erro-aux).
                    IF  CAN-FIND(tt-erro-aux) THEN DO:

                        RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                             INPUT 56). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                    
                    /*UN X Usu†rio*/
                    RUN ValidaUnidadeNegocioUsuario IN h-cdapi024 (INPUT contrato-for.cod-comprado,
                                                                   INPUT tt-matriz-contrato.im-cod-unid-negoc,
                                                                   OUTPUT TABLE tt-erro-aux).
                    IF CAN-FIND(tt-erro-aux) THEN DO:

                        RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                             INPUT 57). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                        
                    /*UN X Restriá‰es EMS5*/ 
                    IF tt-matriz-contrato.im-ct-codigo <> "" THEN DO:

                        RUN ValidaRestricoesUnidadeNegocio in h-cdapi024 (input  contrato-for.cod-estabel,
                                                                          input  tt-matriz-contrato.im-ct-codigo,
                                                                          input  tt-matriz-contrato.im-sc-codigo,
                                                                          input  tt-matriz-contrato.im-cod-unid-negoc,
                                                                          input  today,
                                                                          input  no,
                                                                          output table tt-erro-aux).

                        IF CAN-FIND(FIRST tt-erro-aux) THEN DO:

                            FOR EACH tt-erro-aux:
                                CREATE tt-problema-un-negoc.
                                ASSIGN tt-problema-un-negoc.c-desc-prob = string(tt-erro-aux.cd-erro) + " - " + tt-erro-aux.mensagem.
                            END.

                            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                                 INPUT 99). /* codigo erro */
                            RETURN "NOK":U.
                        END.
                    END.
                    

                    RUN pi-finalizar IN h-cdapi024.
                END.
            END.
        END.

        IF  tt-matriz-contrato.im-cod-unid-negoc <> ""
        AND NOT CAN-FIND(FIRST unid-negoc
                         WHERE unid-negoc.cod-unid-negoc = tt-matriz-contrato.im-cod-unid-negoc) THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                                 INPUT 58). /* codigo erro */
            RETURN "NOK":U.
        END.

        CREATE matriz-rat-contr.
        ASSIGN matriz-rat-contr.nr-contrat     = tt-matriz-contrato.im-nr-contrat
               matriz-rat-contr.ct-codigo      = tt-matriz-contrato.im-ct-codigo
               matriz-rat-contr.sc-codigo      = tt-matriz-contrato.im-sc-codigo
               matriz-rat-contr.perc-rateio    = tt-matriz-contrato.im-perc-rateio
               matriz-rat-contr.cod-unid-negoc = tt-matriz-contrato.im-cod-unid-negoc.

        &if defined(bf_mat_fech_estab) &then
             if avail matriz-rat-contr then
                assign matriz-rat-contr.ct-codigo = tt-matriz-contrato.im-ct-codigo
                       matriz-rat-contr.sc-codigo = tt-matriz-contrato.im-sc-codigo.
        &else
            if avail matriz-rat-contr then
                assign matriz-rat-contr.ct-codigo = int(tt-matriz-contrato.im-ct-codigo)
                       matriz-rat-contr.sc-codigo = int(tt-matriz-contrato.im-sc-codigo).
        &endif
        
        RUN pi-gera-log-imp (INPUT tt-matriz-contrato.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-mi:

    EMPTY TEMP-TABLE tt_log_erro.
    EMPTY TEMP-TABLE tt-erro-aux.
    EMPTY TEMP-TABLE tt-matriz-item.
    CREATE tt-matriz-item.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-matriz-item.im-nr-contrat     =     int(ENTRY(02,tt-linha.linha,";"))
               tt-matriz-item.im-ct-codigo      =    trim(ENTRY(03,tt-linha.linha,";"))
               tt-matriz-item.im-sc-codigo      =    trim(ENTRY(04,tt-linha.linha,";"))
               tt-matriz-item.im-perc-rateio    = decimal(ENTRY(05,tt-linha.linha,";"))
               tt-matriz-item.im-num-seq-item   = integer(ENTRY(06,tt-linha.linha,";"))
               tt-matriz-item.im-it-codigo      =         ENTRY(07,tt-linha.linha,";")
               tt-matriz-item.im-cod-unid-negoc =         ENTRY(08,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-matriz-item.im-nr-contrat = i-nr-contrato.
            
        run prgint\utb\utb743za.py persistent set h_api_cta_ctbl.
        run prgint\utb\utb742za.py persistent set h_api_ccusto.
    
        find first contrato-for where
             contrato-for.nr-contrato = tt-matriz-item.im-nr-contrat no-lock no-error.

        if  not avail contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.

        if avail contrato-for then do:
           find first pedido-compr
                where pedido-compr.nr-contrato = contrato-for.nr-contrato no-lock no-error.
           if avail pedido-compr then do:
              find first estabelec
                   where estabelec.cod-estabel = pedido-compr.end-entrega no-lock no-error.
           end.
           ELSE
               FIND FIRST estabelec NO-LOCK
                    WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-ERROR.

           if avail estabelec then
                 assign i-empresa = estabelec.ep-codigo.
        end.
        
        ASSIGN v_cod_cta_ctbl  = tt-matriz-item.im-ct-codigo
               i-lista-estoque = 0.
        run pi_busca_dados_cta_ctbl in h_api_cta_ctbl (input        STRING(i-empresa),  /* EMPRESA EMS2 */
                                                       input        "",                 /* PLANO DE CONTAS */
                                                       input-output v_cod_cta_ctbl,     /* CONTA */
                                                       input        today,              /* DATA TRANSACAO */   
                                                       output       v_des_cta_ctbl,     /* DESCRICAO CONTA */
                                                       output       v_num_tip_cta_ctbl, /* TIPO DA CONTA */
                                                       output       v_num_sit_cta_ctbl, /* SITUA∞ÄO DA CONTA */
                                                       output       v_ind_finalid_cta,  /* FINALIDADES DA CONTA */
                                                       output table tt_log_erro).       /* ERROS */
													   
		run pi_busca_dados_cta_ctbl_integr in h_api_cta_ctbl (input        STRING(i-empresa), /* EMPRESA EMS2 */
                                                              input        "CEP",             /* M‡DULO */
                                                              input        "",                /* PLANO DE CONTAS */
                                                              input        v_cod_cta_ctbl,    /* CONTA */
                                                              input        today,             /* DATA TRANSACAO */   
                                                              output       v_ind_finalid_cta, /* FINALIDADES DA CONTA */
                                                              output table tt_log_erro).      /* ERROS */ 

        FIND FIRST tt_log_erro NO-ERROR.
        IF NOT AVAIL tt_log_erro THEN DO:
            assign i-lista-estoque = {adinc/i05ad049.i 6 v_ind_finalid_cta}.
        END.
        
        if avail tt_log_erro THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 13). /* codigo erro */
            RETURN "NOK":U.
        END.
        else 
            if v_num_tip_cta_ctbl  = 6 or
               v_num_sit_cta_ctbl <> 3 or
               i-lista-estoque     = 0 or
               i-lista-estoque     = 9 THEN DO:

                RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                     INPUT 14). /* codigo erro */
                RETURN "NOK":U.
            END.
        
        RUN pi_verifica_utilizacao_ccusto in h_api_ccusto (input i-empresa,           /* EMPRESA EMS 2 */
                                                           input estabelec.cod-estabel,/* ESTABELECIMENTO EMS2 */
                                                           input "",                  /* PLANO CONTAS */
                                                           input v_cod_cta_ctbl,      /* CONTA */
                                                           input today,               /* DT TRANSACAO */
                                                           output v_log_utz_ccusto,    /* UTILIZA CCUSTO ? */
                                                           output table tt_log_erro). /* ERROS */
            
        IF v_log_utz_ccusto = no and tt-matriz-item.im-sc-codigo <> "" THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 59). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first item-contrat where
            item-contrat.nr-contrat   = tt-matriz-item.im-nr-contrat and
            item-contrat.num-seq-item = tt-matriz-item.im-num-seq-item           
            no-lock no-error.
        if not avail item-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 07). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first item where 
             item.it-codigo = tt-matriz-item.im-it-codigo 
             no-lock no-error.
        if not avail item THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF  tt-matriz-item.im-cod-unid-negoc <> "" THEN DO:
            IF  AVAIL contrato-for THEN DO:
                FIND FIRST pedido-compr NO-LOCK
                     WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-ERROR.
                IF  AVAIL pedido-compr THEN DO:
                    RUN cdp/cdapi024.p persistent set h-cdapi024.
                    /*UN X Estabelecimento*/
                    RUN ValidaUnidadeNegocioEstabel IN h-cdapi024 (INPUT  pedido-compr.end-entrega,
                                                                   INPUT  TODAY,
                                                                   INPUT  tt-matriz-item.im-cod-unid-negoc,
                                                                   OUTPUT TABLE tt-erro-aux).
                    IF  CAN-FIND(tt-erro-aux) THEN DO:

                        RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                             INPUT 56). /* codigo erro */
                        RETURN "NOK":U.

                    END.
                    
                    /*UN X Usu†rio*/
                    RUN ValidaUnidadeNegocioUsuario IN h-cdapi024 (INPUT  contrato-for.cod-comprado,
                                                                   INPUT  tt-matriz-item.im-cod-unid-negoc,
                                                                   OUTPUT TABLE tt-erro-aux).
                    IF  CAN-FIND(tt-erro-aux) THEN DO:

                        RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                             INPUT 57). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                        
                    /*UN X Restriá‰es EMS5*/ 
                    if  tt-matriz-item.im-ct-codigo <> "" then do:
                        run ValidaRestricoesUnidadeNegocio in h-cdapi024 (input  contrato-for.cod-estabel,
                                                                          input  tt-matriz-item.im-ct-codigo,
                                                                          input  tt-matriz-item.im-sc-codigo,
                                                                          input  tt-matriz-item.im-cod-unid-negoc,
                                                                          input  today,
                                                                          input  no,
                                                                          output table tt-erro-aux).

                        if can-find(first tt-erro-aux) then do:
                            for each tt-erro-aux:
                                create tt-problema-un-negoc.
                                assign tt-problema-un-negoc.c-desc-prob = string(tt-erro-aux.cd-erro) + " - " + tt-erro-aux.mensagem.
                            end.

                            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                                 INPUT 99). /* codigo erro */
                            RETURN "NOK":U.
                        end.
                    end.
                    
                    RUN pi-finalizar IN h-cdapi024.
                END.
            END.
        END.
        
        IF  /*tt-matriz-item.im-cod-unid-negoc <> ""
        AND*/ NOT CAN-FIND(FIRST unid-negoc
                         WHERE unid-negoc.cod-unid-negoc = tt-matriz-item.im-cod-unid-negoc) THEN DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 58). /* codigo erro */
            RETURN "NOK":U.
        END.
        
        find matriz-rat-item
             where matriz-rat-item.nr-contrat     = tt-matriz-item.im-nr-contrat
             and   matriz-rat-item.ct-codigo      = tt-matriz-item.im-ct-codigo
             and   matriz-rat-item.sc-codigo      = tt-matriz-item.im-sc-codigo
             and   matriz-rat-item.num-seq-item   = tt-matriz-item.im-num-seq-item
             and   matriz-rat-item.cod-unid-negoc = tt-matriz-item.im-cod-unid-negoc no-lock no-error.

        if avail matriz-rat-item then DO:

            RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                                 INPUT 47). /* codigo erro */
            RETURN "NOK":U.
        END.

        create matriz-rat-item.              
        assign matriz-rat-item.nr-contrat   = tt-matriz-item.im-nr-contrat
               matriz-rat-item.ct-codigo    = tt-matriz-item.im-ct-codigo
               matriz-rat-item.sc-codigo    = tt-matriz-item.im-sc-codigo
               matriz-rat-item.perc-rateio  = tt-matriz-item.im-perc-rateio
               matriz-rat-item.it-codigo    = tt-matriz-item.im-it-codigo
               matriz-rat-item.num-seq-item = tt-matriz-item.im-num-seq-item
               matriz-rat-item.cod-unid-negoc = tt-matriz-item.im-cod-unid-negoc.
        

        &if defined(bf_mat_fech_estab) &then
             if avail matriz-rat-item then
                assign matriz-rat-item.ct-codigo = tt-matriz-item.im-ct-codigo
                       matriz-rat-item.sc-codigo = tt-matriz-item.im-sc-codigo.
        &else
            if avail matriz-rat-item then
                assign matriz-rat-item.ct-codigo = int(tt-matriz-item.im-ct-codigo)
                       matriz-rat-item.sc-codigo = int(tt-matriz-item.im-sc-codigo).
        &endif
        
        RUN pi-gera-log-imp (INPUT tt-matriz-item.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-fr:

    EMPTY TEMP-TABLE tt-formula.
    CREATE tt-formula.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-formula.im-nr-contrat     =     int(ENTRY(02,tt-linha.linha,";"))
               tt-formula.im-dat-preco-base =    date(ENTRY(03,tt-linha.linha,";"))
               tt-formula.im-num-seq-item   =      IF ENTRY(15,tt-linha.linha,";") <> "" THEN integer(ENTRY(15,tt-linha.linha,";")) ELSE integer(ENTRY(04,tt-linha.linha,";"))
               tt-formula.im-mo-codigo1[1]  = integer(ENTRY(05,tt-linha.linha,";"))
               tt-formula.im-mo-codigo1[2]  = integer(ENTRY(06,tt-linha.linha,";"))
               tt-formula.im-mo-codigo1[3]  = integer(ENTRY(07,tt-linha.linha,";"))
               tt-formula.im-mo-codigo1[4]  = integer(ENTRY(08,tt-linha.linha,";"))
               tt-formula.im-mo-codigo1[5]  = integer(ENTRY(09,tt-linha.linha,";"))
               tt-formula.im-dat-prox-reaj  =    date(ENTRY(10,tt-linha.linha,";"))
               tt-formula.im-periodo        = integer(ENTRY(11,tt-linha.linha,";"))
               tt-formula.im-log-reajuste   =     if (ENTRY(12,tt-linha.linha,";")) = "S" then yes else no
               tt-formula.im-dat-indice     =    date(ENTRY(13,tt-linha.linha,";"))
               tt-formula.im-perc-reaj      = decimal(ENTRY(14,tt-linha.linha,";")).

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-formula.im-nr-contrat = i-nr-contrato.

        find first formula-reaj where
             formula-reaj.nr-contrat   = tt-formula.im-nr-contrat and
             formula-reaj.num-seq-item = tt-formula.im-num-seq-item EXCLUSIVE-LOCK no-error.
        
        if avail formula-reaj THEN DO:

            RUN pi-gera-log-imp (INPUT tt-formula.im-nr-contrat,
                                 INPUT 19). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first contrato-for where
             contrato-for.nr-contrato = tt-formula.im-nr-contrat no-lock no-error.

        if  not avail contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-formula.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first item-contrat where
            item-contrat.nr-contrat   = tt-formula.im-nr-contrat and
            item-contrat.num-seq-item = tt-formula.im-num-seq-item no-lock no-error.

        if not avail item-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-formula.im-nr-contrat,
                                 INPUT 07). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        do i-cont-moeda = 1 to 5:
            find first moeda where 
                 moeda.mo-codigo = tt-formula.im-mo-codigo1[i-cont-moeda]
                 no-lock no-error.
            if not avail moeda THEN DO:

                RUN pi-gera-log-imp (INPUT tt-formula.im-nr-contrat,
                                     INPUT 03). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        create formula-reaj.              
        assign formula-reaj.nr-contrat     = tt-formula.im-nr-contrat
               formula-reaj.dat-preco-base = tt-formula.im-dat-preco-base
               formula-reaj.num-seq-item   = tt-formula.im-num-seq-item
               formula-reaj.mo-codigo[1]   = tt-formula.im-mo-codigo1[1] 
               formula-reaj.mo-codigo[2]   = tt-formula.im-mo-codigo1[2]
               formula-reaj.mo-codigo[3]   = tt-formula.im-mo-codigo1[3]
               formula-reaj.mo-codigo[4]   = tt-formula.im-mo-codigo1[4]
               formula-reaj.mo-codigo[5]   = tt-formula.im-mo-codigo1[5]
               formula-reaj.dat-prox-reaj  = tt-formula.im-dat-prox-reaj
               formula-reaj.periodo        = tt-formula.im-periodo
               formula-reaj.log-reajuste   = tt-formula.im-log-reajuste
               formula-reaj.dat-indice[1]  = tt-formula.im-dat-indice[1]
               formula-reaj.perc-reaj[1]   = tt-formula.im-perc-reaj[1].
        
        RUN pi-gera-log-imp (INPUT tt-formula.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-re:

    EMPTY TEMP-TABLE tt-receb.
    CREATE tt-receb.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-receb.im-cod-emitente = integer(ENTRY(31,tt-linha.linha,";")) 
               tt-receb.im-cod-movto    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-receb.im-codigo-rejei = integer(ENTRY(03,tt-linha.linha,";")) 
               tt-receb.im-data-atualiz =    date(ENTRY(04,tt-linha.linha,";"))
               tt-receb.im-data-movto   =    date(ENTRY(05,tt-linha.linha,";"))
               tt-receb.im-data-nota    =    date(ENTRY(06,tt-linha.linha,";"))
               tt-receb.im-hora-atualiz =         ENTRY(07,tt-linha.linha,";") 
               tt-receb.im-it-codigo    =         ENTRY(08,tt-linha.linha,";") 
               tt-receb.im-num-pedido   = integer(ENTRY(09,tt-linha.linha,";")) 
               tt-receb.im-numero-nota  = LEFT-TRIM(STRING(DEC(ENTRY(10,tt-linha.linha,";")),">>>>>>>>>9999999"))
               tt-receb.im-numero-ordem = integer(ENTRY(11,tt-linha.linha,";"))
               tt-receb.im-parcela      = integer(ENTRY(12,tt-linha.linha,";"))
               tt-receb.im-pre-unit-for = decimal(ENTRY(13,tt-linha.linha,";"))
               tt-receb.im-preco-unit   = decimal(ENTRY(14,tt-linha.linha,";"))
               tt-receb.im-qtd-rec-forn = decimal(ENTRY(15,tt-linha.linha,";"))
               tt-receb.im-qtd-rej-forn = decimal(ENTRY(16,tt-linha.linha,";"))
               tt-receb.im-quant-receb  = decimal(ENTRY(17,tt-linha.linha,";"))
               tt-receb.im-quant-rejeit = decimal(ENTRY(18,tt-linha.linha,";"))
               tt-receb.im-recebedor    =         ENTRY(19,tt-linha.linha,";") 
               tt-receb.im-serie-nota   =         ENTRY(20,tt-linha.linha,";") 
               tt-receb.im-usuario      =         ENTRY(21,tt-linha.linha,";") 
               tt-receb.im-valor-total  = decimal(ENTRY(22,tt-linha.linha,";"))
               tt-receb.im-sit-ord      = integer(ENTRY(23,tt-linha.linha,";"))
               tt-receb.im-sit-par      = integer(ENTRY(24,tt-linha.linha,";"))             
               tt-receb.im-aliquota-icm = decimal(ENTRY(25,tt-linha.linha,";"))
               tt-receb.im-aliquota-ipi = decimal(ENTRY(26,tt-linha.linha,";"))
               tt-receb.im-aliquota-iss = decimal(ENTRY(27,tt-linha.linha,";"))
               tt-receb.im-valor-icm    = decimal(ENTRY(28,tt-linha.linha,";"))
               tt-receb.im-valor-ipi    = decimal(ENTRY(29,tt-linha.linha,";"))
               tt-receb.im-valor-iss    = decimal(ENTRY(30,tt-linha.linha,";")).

        find ordem-compra
            where ordem-compra.numero-ordem = tt-receb.im-numero-ordem no-lock no-error.

        if  not avail ordem-compra then do:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 36). /* codigo erro */
            RETURN "NOK":U.
        end.

        find first emitente
            where emitente.cod-emitente = tt-receb.im-cod-emitente no-lock no-error.

        if  not avail emitente then do:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        end.

        find pedido-compr
            where pedido-compr.num-pedido = tt-receb.im-num-pedido
            no-lock no-error.

        if  not avail pedido-compr then do:
            
            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 35). /* codigo erro */
            RETURN "NOK":U.
        end.
        
        if  avail pedido-compr and avail ordem-compra then do:
            if  pedido-compr.num-pedido <> ordem-compra.num-pedido then do:
                run utp/ut-msgs.p (input "msg":U, input 8444, input "").
                
                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 38). /* codigo erro */
                RETURN "NOK":U.
            end.
        end.

        find prazo-compra
            where prazo-compra.parcela      = tt-receb.im-parcela
            and   prazo-compra.numero-ordem = tt-receb.im-numero-ordem
            no-lock no-error. 

        if  not avail prazo-compra then do:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 37). /* codigo erro */
            RETURN "NOK":U.
        end.

        find item
            where item.it-codigo = tt-receb.im-it-codigo no-lock no-error. 

        if  not avail item then do:
            
            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 39). /* codigo erro */
            RETURN "NOK":U.
        end.

        CREATE recebimento. 
        ASSIGN recebimento.cod-emitente = tt-receb.im-cod-emitente 
               recebimento.cod-movto    = tt-receb.im-cod-movto 
               recebimento.codigo-rejei = tt-receb.im-codigo-rejei 
               recebimento.data-atualiz = tt-receb.im-data-atualiz 
               recebimento.data-movto   = tt-receb.im-data-movto 
               recebimento.data-nota    = tt-receb.im-data-nota 
               recebimento.hora-atualiz = tt-receb.im-hora-atualiz 
               recebimento.it-codigo    = tt-receb.im-it-codigo 
               recebimento.num-pedido   = tt-receb.im-num-pedido 
               recebimento.numero-nota  = tt-receb.im-numero-nota 
               recebimento.numero-ordem = tt-receb.im-numero-ordem 
               recebimento.parcela      = tt-receb.im-parcela 
               recebimento.pre-unit-for = tt-receb.im-pre-unit-for 
               recebimento.preco-unit   = tt-receb.im-preco-unit 
               recebimento.qtd-rec-forn = tt-receb.im-qtd-rec-forn 
               recebimento.qtd-rej-forn = tt-receb.im-qtd-rej-forn 
               recebimento.quant-receb  = tt-receb.im-quant-receb 
               recebimento.quant-rejeit = tt-receb.im-quant-rejeit 
               recebimento.recebedor    = tt-receb.im-recebedor 
               recebimento.serie-nota   = tt-receb.im-serie-nota 
               recebimento.usuario      = tt-receb.im-usuario 
               recebimento.valor-total  = tt-receb.im-valor-total
               recebimento.aliquota-icm = tt-receb.im-aliquota-icm 
               recebimento.aliquota-ipi = tt-receb.im-aliquota-ipi 
               recebimento.aliquota-iss = tt-receb.im-aliquota-iss
               recebimento.valor-icm    = tt-receb.im-valor-icm 
               recebimento.valor-ipi    = tt-receb.im-valor-ipi  
               recebimento.valor-iss    = tt-receb.im-valor-iss.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-em:

    EMPTY TEMP-TABLE tt-modelo-evento.
    CREATE tt-modelo-evento.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-modelo-evento.im-nr-contrat        =     int(ENTRY(02,tt-linha.linha,";"))
               tt-modelo-evento.im-cod-cond-pag      = integer(ENTRY(03,tt-linha.linha,";"))
               tt-modelo-evento.im-dat-event         =    date(ENTRY(04,tt-linha.linha,";"))
               tt-modelo-evento.im-qtd-prevista      = decimal(ENTRY(05,tt-linha.linha,";")) 
               tt-modelo-evento.im-val-previsto      = decimal(ENTRY(06,tt-linha.linha,";")) 
               tt-modelo-evento.im-perc-lib-previsto = decimal(ENTRY(07,tt-linha.linha,";")) 
               tt-modelo-evento.im-log-dest-event    = integer(ENTRY(08,tt-linha.linha,";"))
               tt-modelo-evento.im-num-seq-event-mod = integer(ENTRY(09,tt-linha.linha,";"))   
               tt-modelo-evento.im-num-seq-item      = integer(ENTRY(10,tt-linha.linha,";"))
               tt-modelo-evento.im-it-codigo         =         ENTRY(11,tt-linha.linha,";")
               tt-modelo-evento.im-prazo-dat-realiz  = integer(ENTRY(12,tt-linha.linha,";")).

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-modelo-evento.im-nr-contrat = i-nr-contrato.

        find first event-mod-contrat where
             event-mod-contrat.nr-contrat        = tt-modelo-evento.im-nr-contrat and
             event-mod-contrat.num-seq-item      = tt-modelo-evento.im-num-seq-item and
             event-mod-contrat.num-seq-event-mod = tt-modelo-evento.im-num-seq-event-mod NO-LOCK no-error.

        if avail event-mod-contrat THEN DO:
            
            RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                 INPUT 21). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first contrato-for where
             contrato-for.nr-contrato = tt-modelo-evento.im-nr-contrat no-lock no-error.

        if  not avail contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.

        END.
            
        find first item-contrat where
            item-contrat.nr-contrat   = tt-modelo-evento.im-nr-contrat and
            item-contrat.num-seq-item = tt-modelo-evento.im-num-seq-item no-lock no-error.

        if not avail item-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                 INPUT 07). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first cond-pagto where 
             cond-pagto.cod-cond-pag = tt-modelo-evento.im-cod-cond-pag no-lock no-error.

        if not avail cond-pagto THEN DO:

            RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                 INPUT 10). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:
           FIND FIRST es-cond-pagto WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                NO-LOCK NO-ERROR.
           IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
              RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                    INPUT 95). /* codigo erro */
              RETURN "NOK":U.
           END.
        END.
            
        find first item where 
             item.it-codigo = tt-modelo-evento.im-it-codigo no-lock no-error.

        if not avail item THEN DO:

            RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.

        create event-mod-contrat.              
        assign event-mod-contrat.nr-contrat        = tt-modelo-evento.im-nr-contrat
               event-mod-contrat.cod-cond-pag      = tt-modelo-evento.im-cod-cond-pag
               event-mod-contrat.cod-comprado      = tt-modelo-evento.im-cod-comprado
               event-mod-contrat.dat-event         = tt-modelo-evento.im-dat-event
               event-mod-contrat.qtd-prevista      = tt-modelo-evento.im-qtd-prevista
               event-mod-contrat.val-previsto      = tt-modelo-evento.im-val-previsto
               event-mod-contrat.perc-lib-previsto = tt-modelo-evento.im-perc-lib-previsto
               event-mod-contrat.ind-dest-event    = tt-modelo-evento.im-log-dest-event
               event-mod-contrat.num-seq-event-mod = tt-modelo-evento.im-num-seq-event-mod                                                           
               event-mod-contrat.num-seq-item      = tt-modelo-evento.im-num-seq-item
               event-mod-contrat.it-codigo         = tt-modelo-evento.im-it-codigo
               event-mod-contrat.prazo-dat-realiz  = tt-modelo-evento.im-prazo-dat-realiz.

        RUN pi-gera-log-imp (INPUT tt-modelo-evento.im-nr-contrat,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-ce:

    DEFINE VARIABLE i-cont-cond AS INTEGER INIT 0 NO-UNDO.
    DEFINE VARIABLE i-tot       AS INTEGER INIT 0 NO-UNDO.

    EMPTY TEMP-TABLE tt-condicao.
    CREATE tt-condicao.

    IF de-sequencia = 00 THEN DO:

        IF tt-condicao.im-condicao-ped = 0 THEN DO: 

            ASSIGN tt-condicao.im-ce-np-condicao    = integer(ENTRY(02,tt-linha.linha,";"))
                   tt-condicao.im-ce-data-pagto[1]  =    date(ENTRY(03,tt-linha.linha,";"))
                   tt-condicao.im-ce-data-pagto[2]  =    date(ENTRY(04,tt-linha.linha,";")) 
                   tt-condicao.im-ce-data-pagto[3]  =    date(ENTRY(05,tt-linha.linha,";"))
                   tt-condicao.im-ce-data-pagto[4]  =    date(ENTRY(06,tt-linha.linha,";")) 
                   tt-condicao.im-ce-data-pagto[5]  =    date(ENTRY(07,tt-linha.linha,";")) 
                   tt-condicao.im-ce-data-pagto[6]  =    date(ENTRY(08,tt-linha.linha,";"))
                   tt-condicao.im-ce-perc-pagto[1]  = integer(ENTRY(09,tt-linha.linha,";"))
                   tt-condicao.im-ce-perc-pagto[2]  = integer(ENTRY(10,tt-linha.linha,";")) 
                   tt-condicao.im-ce-perc-pagto[3]  = integer(ENTRY(11,tt-linha.linha,";")) 
                   tt-condicao.im-ce-perc-pagto[4]  = integer(ENTRY(12,tt-linha.linha,";")) 
                   tt-condicao.im-ce-perc-pagto[5]  = integer(ENTRY(13,tt-linha.linha,";")) 
                   tt-condicao.im-ce-perc-pagto[6]  = integer(ENTRY(14,tt-linha.linha,";"))
                   tt-condicao.im-ce-coment-cond[1] =         ENTRY(15,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[2] =         ENTRY(16,tt-linha.linha,";").

            do i-cont-cond = 1 to 6:
                assign i-tot = i-tot + tt-condicao.im-ce-perc-pagto[i-cont-cond].
            end.

            if i-tot <> 100 THEN DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 30). /* codigo erro */
                RETURN "NOK":U.
            END.

            find first pedido-compr where
                 pedido-compr.num-pedido = tt-condicao.im-ce-np-condicao no-lock no-error.

            if not avail pedido-compr then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 11). /* codigo erro */
                RETURN "NOK":U.
            END.
            else if pedido-compr.cod-cond-pag <> 0 THEN DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 31). /* codigo erro */
                RETURN "NOK":U.
            END.

            find cond-especif 
                 where cond-especif.num-pedido  = tt-condicao.im-ce-np-condicao EXCLUSIVE-LOCK no-error. 

            if not avail cond-especif then 
               create cond-especif.
            ELSE DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 32). /* codigo erro */
                RETURN "NOK":U.
            END.

            assign cond-especif.num-pedido    = tt-condicao.im-ce-np-condicao 
                   cond-especif.perc-pagto[1] = if tt-condicao.im-ce-perc-pagto[1] > 0 then
                                                   tt-condicao.im-ce-perc-pagto[1]
                                                else 0
                   cond-especif.perc-pagto[2] = if tt-condicao.im-ce-perc-pagto[2] > 0 then
                                                   tt-condicao.im-ce-perc-pagto[2] 
                                                else 0 
                   cond-especif.perc-pagto[3] = if tt-condicao.im-ce-perc-pagto[3] > 0 then 
                                                   tt-condicao.im-ce-perc-pagto[3]
                                                else 0
                   cond-especif.perc-pagto[4] = if tt-condicao.im-ce-perc-pagto[4] > 0 then
                                                   tt-condicao.im-ce-perc-pagto[4] 
                                                else 0 
                   cond-especif.perc-pagto[5] = if tt-condicao.im-ce-perc-pagto[5] > 0 then 
                                                   tt-condicao.im-ce-perc-pagto[5] 
                                                else 0 
                   cond-especif.perc-pagto[6] = if tt-condicao.im-ce-perc-pagto[6] > 0 then 
                                                   tt-condicao.im-ce-perc-pagto[6] 
                                                else 0.

            if  tt-condicao.im-ce-data-pagto[1] <> 12/31/9999 then 
                assign cond-especif.data-pagto[1]   = tt-condicao.im-ce-data-pagto[1]. 
            if  tt-condicao.im-ce-data-pagto[2] <> 12/31/9999 then 
                assign cond-especif.data-pagto[2]   = tt-condicao.im-ce-data-pagto[2]. 
            if  tt-condicao.im-ce-data-pagto[3] <> 12/31/9999 then 
                assign cond-especif.data-pagto[3]   = tt-condicao.im-ce-data-pagto[3]. 
            if  tt-condicao.im-ce-data-pagto[4] <> 12/31/9999 then                 
                assign cond-especif.data-pagto[4]   = tt-condicao.im-ce-data-pagto[4]. 
            if  tt-condicao.im-ce-data-pagto[5] <> 12/31/9999 then 
                assign cond-especif.data-pagto[5]   = tt-condicao.im-ce-data-pagto[5]. 
            if  tt-condicao.im-ce-data-pagto[6] <> 12/31/9999 then 
                assign cond-especif.data-pagto[6]   = tt-condicao.im-ce-data-pagto[6]. 

            assign cond-especif.comentario[1] = tt-condicao.im-ce-coment-cond[1]
                   cond-especif.comentario[2] = tt-condicao.im-ce-coment-cond[2].

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
       END.
    END.

    IF de-sequencia = 01 THEN DO:

        IF tt-condicao.im-condicao-ped = 0 THEN
            ASSIGN tt-condicao.im-ce-coment-cond[3] = ENTRY(02,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[4] = ENTRY(03,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[5] = ENTRY(04,tt-linha.linha,";").

        find cond-especif 
            where cond-especif.num-pedido  = tt-condicao.im-ce-np-condicao EXCLUSIVE-LOCK no-error. 

        if not avail cond-especif then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 33). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:

            assign cond-especif.comentario[3] = tt-condicao.im-ce-coment-cond[3]
                   cond-especif.comentario[4] = tt-condicao.im-ce-coment-cond[4]
                   cond-especif.comentario[5] = tt-condicao.im-ce-coment-cond[5].

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 02 THEN DO:

        IF tt-condicao.im-condicao-ped = 0 THEN
            ASSIGN tt-condicao.im-ce-coment-cond[6] = ENTRY(02,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[7] = ENTRY(03,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[8] = ENTRY(04,tt-linha.linha,";").

        find cond-especif 
            where cond-especif.num-pedido  = tt-condicao.im-ce-np-condicao EXCLUSIVE-LOCK no-error. 

        if not avail cond-especif then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 33). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:

           assign cond-especif.comentario[6] = tt-condicao.im-ce-coment-cond[6]
                  cond-especif.comentario[7] = tt-condicao.im-ce-coment-cond[7]
                  cond-especif.comentario[8] = tt-condicao.im-ce-coment-cond[8].

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 03 THEN DO:

        IF tt-condicao.im-condicao-ped = 0 THEN
            ASSIGN tt-condicao.im-ce-coment-cond[9]  = ENTRY(02,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[10] = ENTRY(03,tt-linha.linha,";")
                   tt-condicao.im-ce-coment-cond[11] = ENTRY(04,tt-linha.linha,";").

        find cond-especif 
            where cond-especif.num-pedido  = tt-condicao.im-ce-np-condicao EXCLUSIVE-LOCK no-error. 

        if not avail cond-especif then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 33). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:
        
            ASSIGN cond-especif.comentario[9]  = tt-condicao.im-ce-coment-cond[9]
                   cond-especif.comentario[10] = tt-condicao.im-ce-coment-cond[10]
                   cond-especif.comentario[11] = tt-condicao.im-ce-coment-cond[11].

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    IF de-sequencia = 04 THEN DO:

        IF tt-condicao.im-condicao-ped = 0 THEN
            ASSIGN tt-condicao.im-ce-coment-cond[12] = ENTRY(02,tt-linha.linha,";").

        find cond-especif 
            where cond-especif.num-pedido  = tt-condicao.im-ce-np-condicao EXCLUSIVE-LOCK no-error. 

        if not avail cond-especif then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 33). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE DO:
        
            assign cond-especif.comentario[12] = tt-condicao.im-ce-coment-cond[12].

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-or:

    DEFINE VARIABLE c-cod-unid-negoc AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tt-ordem-compra.
    EMPTY TEMP-TABLE tt_log_erro.
    EMPTY TEMP-TABLE tt-erro-aux.
    EMPTY TEMP-TABLE tt-problema-un-negoc.

    CREATE tt-ordem-compra.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-ordem-compra.im-or-num-ordem    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-ordem-compra.im-or-it-codigo    =         ENTRY(03,tt-linha.linha,";")
               tt-ordem-compra.im-or-natureza     = integer(ENTRY(04,tt-linha.linha,";"))
               tt-ordem-compra.im-or-situacao     = integer(ENTRY(05,tt-linha.linha,";"))
               tt-ordem-compra.im-or-origem       = integer(ENTRY(06,tt-linha.linha,";"))
               tt-ordem-compra.im-or-op-codigo    = integer(ENTRY(07,tt-linha.linha,";"))
               tt-ordem-compra.im-or-data-emissao =    date(ENTRY(08,tt-linha.linha,";"))
               tt-ordem-compra.im-or-ct-codigo    =         ENTRY(09,tt-linha.linha,";")
               tt-ordem-compra.im-or-sc-codigo    =         ENTRY(10,tt-linha.linha,";")
               tt-ordem-compra.im-or-requisitante =         ENTRY(11,tt-linha.linha,";")
               tt-ordem-compra.im-or-dep-almoxar  =         ENTRY(12,tt-linha.linha,";")
               tt-ordem-compra.im-or-ordem-servic = integer(ENTRY(13,tt-linha.linha,";"))
               tt-ordem-compra.im-or-cod-comprado =         ENTRY(14,tt-linha.linha,";")
               tt-ordem-compra.im-or-num-pedido   = integer(ENTRY(15,tt-linha.linha,";"))
               tt-ordem-compra.im-or-data-pedido  =    date(ENTRY(16,tt-linha.linha,";"))
               tt-ordem-compra.im-or-cod-emitente = integer(ENTRY(37,tt-linha.linha,";"))
               tt-ordem-compra.im-or-data-cotacao =    date(ENTRY(17,tt-linha.linha,";"))
               tt-ordem-compra.im-or-preco-orig   = decimal(ENTRY(18,tt-linha.linha,";"))
               tt-ordem-compra.im-or-preco-unit   = decimal(ENTRY(19,tt-linha.linha,";"))
               tt-ordem-compra.im-or-pre-unit-for = decimal(ENTRY(20,tt-linha.linha,";"))
               tt-ordem-compra.im-or-preco-fornec = decimal(ENTRY(21,tt-linha.linha,";"))
               tt-ordem-compra.im-or-nr-alt-preco = integer(ENTRY(22,tt-linha.linha,";"))
               tt-ordem-compra.im-or-mo-codigo    = integer(ENTRY(23,tt-linha.linha,";"))
               tt-ordem-compra.im-or-codigo-ipi   =     if (ENTRY(24,tt-linha.linha,";")) = "S" then yes else no
               tt-ordem-compra.im-or-aliquota-ipi = decimal(ENTRY(25,tt-linha.linha,";"))
               tt-ordem-compra.im-or-codigo-icm   = integer(ENTRY(26,tt-linha.linha,";"))
               tt-ordem-compra.im-or-aliquota-icm = decimal(ENTRY(27,tt-linha.linha,";"))
               tt-ordem-compra.im-or-frete        =     if (ENTRY(28,tt-linha.linha,";")) = "S" then yes else no
               tt-ordem-compra.im-or-valor-frete  = decimal(ENTRY(29,tt-linha.linha,";"))
               tt-ordem-compra.im-or-taxa-financ  =     if (ENTRY(30,tt-linha.linha,";")) = "S" then yes else no 
               tt-ordem-compra.im-or-valor-taxa   = decimal(ENTRY(31,tt-linha.linha,";"))
               tt-ordem-compra.im-or-perc-descto  = decimal(ENTRY(32,tt-linha.linha,";"))
               tt-ordem-compra.im-or-cod-cond-pag = integer(ENTRY(33,tt-linha.linha,";"))
               tt-ordem-compra.im-or-prazo-entreg = integer(ENTRY(34,tt-linha.linha,";"))
               tt-ordem-compra.im-or-contato      =         ENTRY(35,tt-linha.linha,";")
               tt-ordem-compra.im-or-impr-ficha   =     if (ENTRY(36,tt-linha.linha,";")) = "S" then yes else no.

        if tt-ordem-compra.im-or-num-pedido > 0 then do:

            find first pedido-compr where 
                 pedido-compr.num-pedido = tt-ordem-compra.im-or-num-pedido no-lock no-error.

            if not avail pedido-compr then do:
                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 11). /* codigo erro */
                RETURN "NOK":U.
            end.
        end.

        find item where item.it-codigo = tt-ordem-compra.im-or-it-codigo no-lock no-error. 

        if  not avail item then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.
        ELSE
            if  item.cod-obsoleto = 2 
            or  item.cod-obsoleto = 3 then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 34). /* codigo erro */
                RETURN "NOK":U.
            END.
                
        if  tt-ordem-compra.im-or-ct-codigo <> "" or
            tt-ordem-compra.im-or-sc-codigo <> "" then do: 

            if avail pedido-compr then do:
                find first estabelec
                     where estabelec.cod-estabel = pedido-compr.end-entrega no-lock no-error.

                if avail estabelec THEN
                    assign i-empresa = estabelec.ep-codigo.
            END.
            
            ASSIGN v_cod_cta_ctbl  = tt-ordem-compra.im-or-ct-codigo
                   i-lista-estoque = 0.

            RUN prgint\utb\utb743za.py PERSISTENT SET h_api_cta_ctbl.
            RUN prgint\utb\utb742za.py PERSISTENT SET h_api_ccusto.
    
            run pi_busca_dados_cta_ctbl in h_api_cta_ctbl (input        STRING(i-empresa),  /* EMPRESA EMS2 */
                                                           input        "",                 /* PLANO DE CONTAS */
                                                           input-output v_cod_cta_ctbl,     /* CONTA */
                                                           input        today,              /* DATA TRANSACAO */   
                                                           output       v_des_cta_ctbl,     /* DESCRICAO CONTA */
                                                           output       v_num_tip_cta_ctbl, /* TIPO DA CONTA */
                                                           output       v_num_sit_cta_ctbl, /* SITUA∞ÄO DA CONTA */
                                                           output       v_ind_finalid_cta,  /* FINALIDADES DA CONTA */
                                                           output table tt_log_erro).       /* ERROS */
                                                           
            run pi_busca_dados_cta_ctbl_integr in h_api_cta_ctbl (input        STRING(i-empresa), /* EMPRESA EMS2 */
                                                                  input        "CEP",             /* M‡DULO */
                                                                  input        "",                /* PLANO DE CONTAS */
                                                                  input        v_cod_cta_ctbl,    /* CONTA */
                                                                  input        today,             /* DATA TRANSACAO */   
                                                                  output       v_ind_finalid_cta, /* FINALIDADES DA CONTA */
                                                                  output table tt_log_erro).      /* ERROS */ 

            FIND FIRST tt_log_erro NO-ERROR.
            IF NOT AVAIL tt_log_erro THEN DO:
                assign i-lista-estoque = {adinc/i05ad049.i 6 v_ind_finalid_cta}.
            END.
    
            if  AVAIL tt_log_erro then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 13). /* codigo erro */
                RETURN "NOK":U.
            END.
            else do: 
                if  (v_num_tip_cta_ctbl <> 1 
                and v_num_tip_cta_ctbl <> 4 
                and v_num_tip_cta_ctbl <> 5) 
                or  v_num_sit_cta_ctbl <> 3 then DO:

                    RUN pi-gera-log-imp (INPUT 0,
                                         INPUT 35). /* codigo erro */
                    RETURN "NOK":U.
                END.
                    
                if (i-lista-estoque <> 0 
                and i-lista-estoque <> 1 
                and i-lista-estoque <> 2 
                and i-lista-estoque <> 3 
                and i-lista-estoque <> 5 
                and i-lista-estoque <> 6) then DO:

                    RUN pi-gera-log-imp (INPUT 0,
                                         INPUT 14). /* codigo erro */
                    RETURN "NOK":U.
                END.
            end. 
        end. 

        find comprador where comprador.cod-comprado = tt-ordem-compra.im-or-cod-comprado no-lock no-error. 

        if  not avail comprador then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 09). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        if  tt-ordem-compra.im-or-dep-almoxar <> "" then do: 

            find deposito where deposito.cod-depos = tt-ordem-compra.im-or-dep-almoxar no-lock no-error. 

            if  not avail deposito then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 08). /* codigo erro */
                RETURN "NOK":U.
            END.
        end. 

        if tt-ordem-compra.im-or-natureza <> 1 and
           tt-ordem-compra.im-or-natureza <> 2 and
           tt-ordem-compra.im-or-natureza <> 3 then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 36). /* codigo erro */
            RETURN "NOK":U.
        END.
           
        if tt-ordem-compra.im-or-situacao <> 1 and
           tt-ordem-compra.im-or-situacao <> 2 and
           tt-ordem-compra.im-or-situacao <> 3 and
           tt-ordem-compra.im-or-situacao <> 4 and
           tt-ordem-compra.im-or-situacao <> 5 and
           tt-ordem-compra.im-or-situacao <> 6 then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 37). /* codigo erro */
            RETURN "NOK":U.
        END.
           
        if tt-ordem-compra.im-or-origem <> 1 and 
           tt-ordem-compra.im-or-origem <> 2 and 
           tt-ordem-compra.im-or-origem <> 3 then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 38). /* codigo erro */
            RETURN "NOK":U.
        END.
           
        find ordem-compra where 
             ordem-compra.numero-ordem = tt-ordem-compra.im-or-num-ordem  and 
             ordem-compra.it-codigo    = tt-ordem-compra.im-or-it-codigo exclusive-lock no-error. 

        if avail ordem-compra then do:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 39). /* codigo erro */
            RETURN "NOK":U.
        end.

        if tt-ordem-compra.im-or-mo-codigo > 0 then do:

            find first moeda where 
                moeda.mo-codigo = tt-ordem-compra.im-or-mo-codigo no-lock no-error.

            if not avail moeda then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 03). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.
        
        create ordem-compra.
        assign ordem-compra.numero-ordem = tt-ordem-compra.im-or-num-ordem  
               ordem-compra.num-pedido   = tt-ordem-compra.im-or-num-pedido    
               ordem-compra.it-codigo    = tt-ordem-compra.im-or-it-codigo     
               ordem-compra.ct-codigo    = tt-ordem-compra.im-or-ct-codigo     
               ordem-compra.sc-codigo    = tt-ordem-compra.im-or-sc-codigo      
               ordem-compra.natureza     = tt-ordem-compra.im-or-natureza       
               ordem-compra.situacao     = tt-ordem-compra.im-or-situacao       
               ordem-compra.origem       = tt-ordem-compra.im-or-origem         
               ordem-compra.requisitante = tt-ordem-compra.im-or-requisitante   
               ordem-compra.dep-almoxar  = tt-ordem-compra.im-or-dep-almoxar    
               ordem-compra.ordem-servic = tt-ordem-compra.im-or-ordem-servic   
               ordem-compra.cod-comprado = tt-ordem-compra.im-or-cod-comprado   
               ordem-compra.impr-ficha   = tt-ordem-compra.im-or-impr-ficha
               ordem-compra.aliquota-icm = tt-ordem-compra.im-or-aliquota-icm   
               ordem-compra.aliquota-ipi = tt-ordem-compra.im-or-aliquota-ipi   
               ordem-compra.cod-cond-pag = tt-ordem-compra.im-or-cod-cond-pag   
               ordem-compra.cod-emitente = tt-ordem-compra.im-or-cod-emitente    
               ordem-compra.codigo-icm   = tt-ordem-compra.im-or-codigo-icm
               ordem-compra.codigo-ipi   = tt-ordem-compra.im-or-codigo-ipi
               ordem-compra.contato      = tt-ordem-compra.im-or-contato
               ordem-compra.data-emissao = tt-ordem-compra.im-or-data-emissao 
               ordem-compra.data-cotacao = tt-ordem-compra.im-or-data-cotacao 
               ordem-compra.data-pedido  = tt-ordem-compra.im-or-data-pedido
               ordem-compra.op-codigo    = tt-ordem-compra.im-or-op-codigo   
               ordem-compra.perc-descto  = tt-ordem-compra.im-or-perc-descto  
               ordem-compra.prazo-entreg = tt-ordem-compra.im-or-prazo-entreg 
               ordem-compra.pre-unit-for = tt-ordem-compra.im-or-pre-unit-for 
               ordem-compra.preco-fornec = tt-ordem-compra.im-or-preco-fornec 
               ordem-compra.preco-orig   = tt-ordem-compra.im-or-preco-orig 
               ordem-compra.preco-unit   = tt-ordem-compra.im-or-preco-unit 
               ordem-compra.taxa-financ  = tt-ordem-compra.im-or-taxa-financ
               ordem-compra.valor-frete  = tt-ordem-compra.im-or-valor-frete
               ordem-compra.valor-taxa   = tt-ordem-compra.im-or-valor-taxa  
               ordem-compra.frete        = tt-ordem-compra.im-or-frete
               ordem-compra.nr-alt-preco = tt-ordem-compra.im-or-nr-alt-preco 
               ordem-compra.mo-codigo    = tt-ordem-compra.im-or-mo-codigo.    

        if avail ordem-compra then
           assign ordem-compra.ct-codigo = tt-ordem-compra.im-or-ct-codigo
                  ordem-compra.sc-codigo = tt-ordem-compra.im-or-sc-codigo.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    IF de-sequencia = 01 THEN DO:

        assign tt-ordem-compra.im-or-num-ordem    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-ordem-compra.im-or-it-codigo    =         ENTRY(03,tt-linha.linha,";")
               tt-ordem-compra.im-or-comentarios  =         ENTRY(04,tt-linha.linha,";") 
               tt-ordem-compra.im-or-hora-atualiz =         ENTRY(05,tt-linha.linha,";")
               tt-ordem-compra.im-or-nr-ord-orig  = integer(ENTRY(06,tt-linha.linha,";"))
               tt-ordem-compra.im-or-ind-reajuste = decimal(ENTRY(07,tt-linha.linha,";"))
               tt-ordem-compra.im-or-linha        = integer(ENTRY(08,tt-linha.linha,";"))
               tt-ordem-compra.im-or-cod-refer    =         ENTRY(09,tt-linha.linha,";")
               tt-ordem-compra.im-or-sit-oc       = integer(ENTRY(10,tt-linha.linha,";"))
               tt-ordem-compra.im-or-num-seq      = integer(ENTRY(11,tt-linha.linha,";"))
               tt-ordem-compra.im-or-data-atualiz =    date(ENTRY(12,tt-linha.linha,";"))
               tt-ordem-compra.im-nr-contrato     =     int(ENTRY(13,tt-linha.linha,";"))
               tt-ordem-compra.im-or-cod-estabel  =         ENTRY(14,tt-linha.linha,";") .

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-ordem-compra.im-nr-contrato = i-nr-contrato.

        find ordem-compra where 
             ordem-compra.numero-ordem = tt-ordem-compra.im-or-num-ordem exclusive-lock no-error.

        if not avail ordem-compra then DO:

            RUN pi-gera-log-imp (INPUT tt-ordem-compra.im-nr-contrato,
                                 INPUT 15). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find estabelec where estabelec.cod-estabel = tt-ordem-compra.im-or-cod-estabel no-lock no-error. 

        if  not avail estabelec then DO:

            RUN pi-gera-log-imp (INPUT tt-ordem-compra.im-nr-contrato,
                                 INPUT 12). /* codigo erro */
            RETURN "NOK":U.
        END.

        assign ordem-compra.hora-atualiz  = tt-ordem-compra.im-or-hora-atualiz 
               ordem-compra.nr-ord-orig   = tt-ordem-compra.im-or-nr-ord-orig  
               ordem-compra.usuario       = tt-ordem-compra.im-or-usuario
               ordem-compra.cod-estabel   = tt-ordem-compra.im-or-cod-estabel    
               ordem-compra.comentarios   = tt-ordem-compra.im-or-comentarios
               ordem-compra.ind-reajuste  = tt-ordem-compra.im-or-ind-reajuste
               ordem-compra.linha         = tt-ordem-compra.im-or-linha
               ordem-compra.data-atualiz  = tt-ordem-compra.im-or-data-atualiz
               ordem-compra.nr-contrato   = tt-ordem-compra.im-nr-contrato
               ordem-compra.num-seq-item  = tt-ordem-compra.im-or-num-seq.
        
        FIND FIRST item-contrat NO-LOCK
             WHERE item-contrat.nr-contrato  = ordem-compra.nr-contrato
               AND item-contrat.num-seq-item = ordem-compra.num-seq-item NO-ERROR.

        IF  AVAIL item-contrat AND item-contrat.ind-tipo-control <> 1 THEN DO:

            RUN cdp/cdapi024.p persistent set h-cdapi024.
            
            IF  VALID-HANDLE(h-cdapi024) THEN DO:
                EMPTY TEMP-TABLE tt-erro-aux.
                
                RUN RetornaUnidadeNegocioExternaliz IN h-cdapi024 (INPUT ordem-compra.cod-estabel,
                                                                   INPUT item-contrat.it-codigo,
                                                                   INPUT item-contrat.cod-depos,
                                                                   OUTPUT c-cod-unid-negoc).
                ASSIGN ordem-compra.cod-unid-negoc = c-cod-unid-negoc.
                
                /*UN X Estabelecimento*/
                RUN ValidaUnidadeNegocioEstabel IN h-cdapi024 (INPUT ordem-compra.cod-estabel,
                                                               INPUT ordem-compra.data-emissao,
                                                               INPUT c-cod-unid-negoc, 
                                                               OUTPUT TABLE tt-erro-aux).
                IF  CAN-FIND(tt-erro-aux) THEN DO:

                    RUN pi-gera-log-imp (INPUT tt-ordem-compra.im-nr-contrato,
                                         INPUT 56). /* codigo erro */
                    RETURN "NOK":U.
                END.
                
                /*UN X Usu†rio*/
                RUN ValidaUnidadeNegocioUsuario IN h-cdapi024 (INPUT ordem-compra.requisitante,
                                                               INPUT c-cod-unid-negoc, 
                                                               OUTPUT TABLE tt-erro-aux).
                IF  CAN-FIND(tt-erro-aux) THEN DO:

                    RUN pi-gera-log-imp (INPUT tt-ordem-compra.im-nr-contrato,
                                         INPUT 57). /* codigo erro */
                    RETURN "NOK":U.
                END.
                    
                /*UN X Restriá‰es EMS5*/ 
                if ordem-compra.ct-codigo <> "" then do:

                    run ValidaRestricoesUnidadeNegocio in h-cdapi024 (input  ordem-compra.cod-estabel,
                                                                      input  ordem-compra.ct-codigo,
                                                                      input  ordem-compra.sc-codigo,
                                                                      input  c-cod-unid-negoc,
                                                                      input  ordem-compra.data-emissao,
                                                                      input  no,
                                                                      output table tt-erro-aux).

                    if can-find(first tt-erro-aux) then do:
                        for each tt-erro-aux:
                            create tt-problema-un-negoc.
                            assign tt-problema-un-negoc.c-desc-prob = string(tt-erro-aux.cd-erro) + " - " + tt-erro-aux.mensagem.
                        end.

                        RUN pi-gera-log-imp (INPUT tt-ordem-compra.im-nr-contrato,
                                             INPUT 99). /* codigo erro */
                        RETURN "NOK":U.
                    end.
                end.
                
                RUN pi-finalizar IN h-cdapi024.
            END.
        END.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-pa:

    CREATE tt-parcela.

    IF de-sequencia = 00 THEN DO:

        assign tt-parcela.im-or-num-ordem    = integer(ENTRY(02,tt-linha.linha,";"))
               tt-parcela.im-pa-parcela      = integer(ENTRY(03,tt-linha.linha,";"))
               tt-parcela.im-or-it-codigo    =         ENTRY(04,tt-linha.linha,";")
               tt-parcela.im-pa-quantidade   = decimal(ENTRY(05,tt-linha.linha,";"))
               tt-parcela.im-pa-un           =         ENTRY(06,tt-linha.linha,";")
               tt-parcela.im-pa-data-entrega =    date(ENTRY(07,tt-linha.linha,";"))
               tt-parcela.im-pa-nome-abrev   =         ENTRY(08,tt-linha.linha,";")
               tt-parcela.im-pa-pedido-clien =         ENTRY(09,tt-linha.linha,";")
               tt-parcela.im-pa-situacao     = integer(ENTRY(10,tt-linha.linha,";"))
               tt-parcela.im-pa-qtd-do-forn  = decimal(ENTRY(11,tt-linha.linha,";"))
               tt-parcela.im-pa-qtd-rec-forn = decimal(ENTRY(12,tt-linha.linha,";"))
               tt-parcela.im-pa-qtd-rej-forn = decimal(ENTRY(13,tt-linha.linha,";"))
               tt-parcela.im-pa-qtd-sal-forn = decimal(ENTRY(14,tt-linha.linha,";"))
               tt-parcela.im-pa-quant-receb  = decimal(ENTRY(15,tt-linha.linha,";"))
               tt-parcela.im-pa-quant-rejeit = decimal(ENTRY(16,tt-linha.linha,";"))
               tt-parcela.im-pa-quant-saldo  = decimal(ENTRY(17,tt-linha.linha,";"))
               tt-parcela.im-pa-quantid-orig = decimal(ENTRY(18,tt-linha.linha,";"))
               tt-parcela.im-pa-cod-alter    =     if (ENTRY(19,tt-linha.linha,";")) = "S" then yes else no
               tt-parcela.im-pa-data-orig    =    date(ENTRY(20,tt-linha.linha,";"))
               tt-parcela.im-pa-data-alter   =    date(ENTRY(21,tt-linha.linha,";"))
               tt-parcela.im-pa-usuario-alt  =         ENTRY(22,tt-linha.linha,";")
               tt-parcela.im-pa-nr-alt-data  = integer(ENTRY(23,tt-linha.linha,";"))
               tt-parcela.im-pa-nr-alt-quant = integer(ENTRY(24,tt-linha.linha,";"))
               tt-parcela.im-pa-natureza     = integer(ENTRY(25,tt-linha.linha,";"))
               tt-parcela.im-pa-cod-refer    =         ENTRY(26,tt-linha.linha,";").

        find ordem-compra where 
             ordem-compra.numero-ordem = tt-parcela.im-or-num-ordem no-lock no-error. 

        if not avail ordem-compra THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 15). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find item where 
             item.it-codigo = tt-parcela.im-or-it-codigo no-lock no-error. 

        if not avail item then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.
        else do:
            if  item.un <> tt-parcela.im-pa-un 
            and item.tipo-contr <> 4  then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 40). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        if tt-parcela.im-pa-natureza <> 1 and
           tt-parcela.im-pa-natureza <> 2 and
           tt-parcela.im-pa-natureza <> 3 then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 36). /* codigo erro */
            RETURN "NOK":U.
        END.

        find prazo-compra where 
             prazo-compra.numero-ordem = tt-parcela.im-or-num-ordem and 
             prazo-compra.parcela = tt-parcela.im-pa-parcela no-lock no-error. 

        if  avail prazo-compra then do:

            for each prazo-compra where 
                prazo-compra.numero-ordem = tt-parcela.im-or-num-ordem EXCLUSIVE-LOCK. 


                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 41). /* codigo erro */
                RETURN "NOK":U.
            end. 
        end. 

        if im-pa-situacao <> 1 and
           im-pa-situacao <> 2 and
           im-pa-situacao <> 3 and
           im-pa-situacao <> 4 and
           im-pa-situacao <> 5 and
           im-pa-situacao <> 6 then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 37). /* codigo erro */
            RETURN "NOK":U.
        END.
        
        /* Verificar se item Ç controlado por programaá∆o de entrega - a quantidade deve ser zero */
        find first item-contrat
            where item-contrat.nr-contrato  = ordem-compra.nr-contrato
              and item-contrat.num-seq-item = ordem-compra.num-seq-item no-lock no-error.

        if  avail item-contrat
        and item-contrat.ind-tipo-control <> 3 then do: /* programaá∆o de entrega */
            if  tt-parcela.im-pa-quantidade = 0 then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 42). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        if  tt-parcela.im-or-it-codigo = ? or tt-parcela.im-or-it-codigo = "" then do:
            find tab-unidade where tt-parcela.im-pa-un = tab-unidade.un
                 no-lock no-error.
            
            if  not avail tab-unidade THEN DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 43). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        create prazo-compra. 
        assign prazo-compra.numero-ordem = ordem-compra.numero-ordem
               prazo-compra.parcela      = tt-parcela.im-pa-parcela 
               prazo-compra.it-codigo    = ordem-compra.it-codigo 
               prazo-compra.situacao     = tt-parcela.im-pa-situacao 
               prazo-compra.quantidade   = tt-parcela.im-pa-quantidade 
               prazo-compra.un           = tt-parcela.im-pa-un 
               prazo-compra.data-entrega = tt-parcela.im-pa-data-entrega 
               prazo-compra.nome-abrev   = tt-parcela.im-pa-nome-abrev 
               prazo-compra.pedido-clien = tt-parcela.im-pa-pedido-clien 
               prazo-compra.cod-alter    = tt-parcela.im-pa-cod-alter
               prazo-compra.qtd-do-forn  = tt-parcela.im-pa-qtd-do-forn 
               prazo-compra.qtd-rec-forn = tt-parcela.im-pa-qtd-rec-forn 
               prazo-compra.qtd-rej-forn = tt-parcela.im-pa-qtd-rej-forn 
               prazo-compra.qtd-sal-forn = tt-parcela.im-pa-qtd-sal-forn 
               prazo-compra.quant-receb  = tt-parcela.im-pa-quant-receb 
               prazo-compra.quant-rejeit = tt-parcela.im-pa-quant-rejeit 
               prazo-compra.quant-saldo  = tt-parcela.im-pa-quant-saldo 
               prazo-compra.quantid-orig = tt-parcela.im-pa-quantid-orig
               prazo-compra.usuario-alt  = tt-parcela.im-pa-usuario-alt  
               prazo-compra.nr-alt-data  = tt-parcela.im-pa-nr-alt-data  
               prazo-compra.nr-alt-quant = tt-parcela.im-pa-nr-alt-quant 
               prazo-compra.natureza     = tt-parcela.im-pa-natureza    
               prazo-compra.cod-refer    = tt-parcela.im-pa-cod-refer    
               prazo-compra.data-orig    = tt-parcela.im-pa-data-orig 
               prazo-compra.data-alter   = tt-parcela.im-pa-data-alter.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-ct:

    CREATE tt-cotacao.

    IF de-sequencia = 00 THEN DO:

        assign tt-cotacao.im-or-it-codigo    =         ENTRY(02,tt-linha.linha,";")
               tt-cotacao.im-co-cod-emitente = integer(ENTRY(28,tt-linha.linha,";"))
               tt-cotacao.im-or-num-ordem    = integer(ENTRY(03,tt-linha.linha,";"))
               tt-cotacao.im-co-data-cotacao =    date(ENTRY(04,tt-linha.linha,";"))
               tt-cotacao.im-co-un           =         ENTRY(05,tt-linha.linha,";") 
               tt-cotacao.im-co-preco-unit   = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-cotacao.im-co-pre-unit-for = decimal(ENTRY(07,tt-linha.linha,";"))
               tt-cotacao.im-co-preco-fornec = decimal(ENTRY(08,tt-linha.linha,";")) 
               tt-cotacao.im-co-mo-codigo    = integer(ENTRY(09,tt-linha.linha,";")) 
               tt-cotacao.im-co-codigo-ipi   =     if (ENTRY(10,tt-linha.linha,";")) = "S" then yes else no
               tt-cotacao.im-co-aliquota-ipi = decimal(ENTRY(11,tt-linha.linha,";")) 
               tt-cotacao.im-co-codigo-icm   = integer(ENTRY(12,tt-linha.linha,";"))
               tt-cotacao.im-co-aliquota-icm = decimal(ENTRY(13,tt-linha.linha,";")) 
               tt-cotacao.im-co-aliquota-iss = decimal(ENTRY(14,tt-linha.linha,";")) 
               tt-cotacao.im-co-frete        =     if (ENTRY(15,tt-linha.linha,";")) = "S" then yes else no
               tt-cotacao.im-co-valor-frete  = decimal(ENTRY(16,tt-linha.linha,";")) 
               tt-cotacao.im-co-taxa-financ  =     if (ENTRY(17,tt-linha.linha,";")) = "S" then yes else no
               tt-cotacao.im-co-valor-taxa   = decimal(ENTRY(18,tt-linha.linha,";")) 
               tt-cotacao.im-co-perc-descto  = decimal(ENTRY(19,tt-linha.linha,";")) 
               tt-cotacao.im-co-cod-cond-pag = integer(ENTRY(20,tt-linha.linha,";")) 
               tt-cotacao.im-co-prazo-entreg = integer(ENTRY(21,tt-linha.linha,";")) 
               tt-cotacao.im-co-contato      =         ENTRY(22,tt-linha.linha,";")
               tt-cotacao.im-co-cot-aprovada =     if (ENTRY(23,tt-linha.linha,";")) = "S" then yes else no
               tt-cotacao.im-co-aprovador    =         ENTRY(24,tt-linha.linha,";") 
               tt-cotacao.im-co-usuario      =         ENTRY(25,tt-linha.linha,";")
               tt-cotacao.im-co-dt-atualiz   =    date(ENTRY(26,tt-linha.linha,";"))
               tt-cotacao.im-co-hora-atualiz =         ENTRY(27,tt-linha.linha,";").
               
        FIND FIRST ordem-compra
             WHERE ordem-compra.numero-ordem = tt-cotacao.im-or-num-ordem
               AND ordem-compra.cod-emitente = tt-cotacao.im-co-cod-emitente NO-LOCK NO-ERROR.

        IF AVAIL ordem-compra THEN DO:

            FIND FIRST item-contrat
                 WHERE item-contrat.nr-contrat   = ordem-compra.nr-contrato
                   AND item-contrat.num-seq-item = ordem-compra.num-seq-item NO-LOCK NO-ERROR.

            IF AVAIL item-contrat THEN
                ASSIGN tt-cotacao.im-co-cod-comprado = item-contrat.cod-comprado.
        END.

        find emitente where 
             emitente.cod-emitente = tt-cotacao.im-co-cod-emitente no-lock no-error. 

        if  not avail emitente then do: 

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 44). /* codigo erro */
            RETURN "NOK":U.
        end. 

        find first ordem-compra where
             ordem-compra.numero-ordem = tt-cotacao.im-or-num-ordem no-lock no-error.

        if not avail ordem-compra then do:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 15). /* codigo erro */
            RETURN "NOK":U.
        end.

        find item where item.it-codigo = tt-cotacao.im-or-it-codigo no-lock no-error. 

        if not avail item then DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.
        else do:
            if  item.cod-obsoleto = 2 
            or  item.cod-obsoleto = 3 then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 34). /* codigo erro */
                RETURN "NOK":U.
            END.
               
            if  item.un <> tt-cotacao.im-co-un 
            and item.tipo-contr <> 4  then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 40). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        if tt-cotacao.im-co-mo-codigo > 0 then do:

            find first moeda where 
                 moeda.mo-codigo = tt-cotacao.im-co-mo-codigo no-lock no-error.

            if not avail moeda then DO:

                RUN pi-gera-log-imp (INPUT 0,
                                     INPUT 03). /* codigo erro */
                RETURN "NOK":U.
            END.
        end.

        find cotacao-item where 
             cotacao-item.numero-ordem = tt-cotacao.im-or-num-ordem     and 
             cotacao-item.it-codigo    = tt-cotacao.im-or-it-codigo     and 
             cotacao-item.data-cotacao = tt-cotacao.im-co-data-cotacao  and 
             cotacao-item.cod-emitente = tt-cotacao.im-co-cod-emitente  EXCLUSIVE-LOCK no-error. 

        if not avail cotacao-item then
            create cotacao-item.
        ELSE DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 45). /* codigo erro */
            RETURN "NOK":U.
        END.

        assign cotacao-item.numero-ordem  = tt-cotacao.im-or-num-ordem 
               cotacao-item.it-codigo     = tt-cotacao.im-or-it-codigo
               cotacao-item.cod-emitente  = tt-cotacao.im-co-cod-emitente 
               cotacao-item.data-cotacao  = tt-cotacao.im-co-data-cotacao 
               cotacao-item.un            = tt-cotacao.im-co-un 
               cotacao-item.preco-fornec  = tt-cotacao.im-co-preco-fornec 
               cotacao-item.preco-unit    = tt-cotacao.im-co-preco-unit   
               cotacao-item.pre-unit-for  = tt-cotacao.im-co-pre-unit-for 
               cotacao-item.mo-codigo     = tt-cotacao.im-co-mo-codigo 
               cotacao-item.codigo-ipi    = tt-cotacao.im-co-codigo-ipi
               cotacao-item.aliquota-ipi  = tt-cotacao.im-co-aliquota-ipi 
               cotacao-item.codigo-icm    = tt-cotacao.im-co-codigo-icm 
               cotacao-item.aliquota-icm  = tt-cotacao.im-co-aliquota-icm 
               cotacao-item.aliquota-iss  = tt-cotacao.im-co-aliquota-iss 
               cotacao-item.frete         = tt-cotacao.im-co-frete
               cotacao-item.valor-frete   = tt-cotacao.im-co-valor-frete 
               cotacao-item.taxa-financ   = tt-cotacao.im-co-taxa-financ
               cotacao-item.valor-taxa    = tt-cotacao.im-co-valor-taxa 
               cotacao-item.perc-descto   = tt-cotacao.im-co-perc-descto 
               cotacao-item.cod-cond-pag  = tt-cotacao.im-co-cod-cond-pag 
               cotacao-item.prazo-entreg  = tt-cotacao.im-co-prazo-entreg 
               cotacao-item.contato       = tt-cotacao.im-co-contato 
               cotacao-item.cod-comprado  = tt-cotacao.im-co-cod-comprado 
               cotacao-item.cot-aprovada  = tt-cotacao.im-co-cot-aprovada
               cotacao-item.aprovador     = tt-cotacao.im-co-aprovador 
               cotacao-item.usuario       = tt-cotacao.im-co-usuario     
               cotacao-item.hora-atualiz  = tt-cotacao.im-co-hora-atualiz
               cotacao-item.data-atualiz  = tt-cotacao.im-co-dt-atualiz.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    IF de-sequencia = 01 THEN DO:

        assign tt-cotacao.im-or-it-codigo    =         ENTRY(02,tt-linha.linha,";")
               tt-cotacao.im-co-cod-emitente = integer(ENTRY(06,tt-linha.linha,";"))
               tt-cotacao.im-or-num-ordem    = integer(ENTRY(03,tt-linha.linha,";"))
               tt-cotacao.im-co-motivo-apr   =         ENTRY(04,tt-linha.linha,";") 
               tt-cotacao.im-co-ind-reajuste = decimal(ENTRY(05,tt-linha.linha,";")).

        find cotacao-item  
             where cotacao-item.numero-ordem = tt-cotacao.im-or-num-ordem 
             and   cotacao-item.it-codigo    = tt-cotacao.im-or-it-codigo 
             and   cotacao-item.cod-emitente = tt-cotacao.im-co-cod-emitente EXCLUSIVE-LOCK no-error.

        if not avail cotacao-item THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 46). /* codigo erro */
            RETURN "NOK":U.
        END.

        assign cotacao-item.ind-reajuste  = tt-cotacao.im-co-ind-reajuste
               cotacao-item.motivo-apr    = tt-cotacao.im-co-motivo-apr. 

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-ie:

    CREATE tt-item-contrat-estab.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-item-contrat-estab.im-acum-rec-qtd  = dec(ENTRY(02,tt-linha.linha,";"))
               tt-item-contrat-estab.im-acum-rec-val  = dec(ENTRY(03,tt-linha.linha,";"))
               tt-item-contrat-estab.im-cod-emitente  = dec(ENTRY(04,tt-linha.linha,";"))
               tt-item-contrat-estab.im-it-codigo     =     ENTRY(06,tt-linha.linha,";")
               tt-item-contrat-estab.im-nr-contrato   = int(ENTRY(07,tt-linha.linha,";"))
               tt-item-contrat-estab.im-num-seq-item  = int(ENTRY(08,tt-linha.linha,";"))                                                      
               tt-item-contrat-estab.im-qtd-total     = dec(ENTRY(09,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-qtd       = dec(ENTRY(10,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-qtd-liber = dec(ENTRY(11,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-qtd-receb = dec(ENTRY(12,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-val       = dec(ENTRY(13,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-val-liber = dec(ENTRY(14,tt-linha.linha,";"))
               tt-item-contrat-estab.im-sld-val-receb = dec(ENTRY(15,tt-linha.linha,";"))
               tt-item-contrat-estab.im-val-total     = dec(ENTRY(16,tt-linha.linha,";"))
               tt-item-contrat-estab.im-cod-estabel   =     ENTRY(16,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-item-contrat-estab.im-nr-contrato = i-nr-contrato.

        find item-contrat-estab where 
             item-contrat-estab.cod-estabel = tt-item-contrat-estab.im-cod-estabel and 
             item-contrat-estab.it-codigo   = tt-item-contrat-estab.im-it-codigo no-lock no-error. 

        if avail ordem-compra then do:

            RUN pi-gera-log-imp (INPUT tt-item-contrat-estab.im-nr-contrato,
                                 INPUT 39). /* codigo erro */
            RETURN "NOK":U.
        end.
        
        create item-contrat-estab.
        assign item-contrat-estab.acum-rec-qtd  = tt-item-contrat-estab.im-acum-rec-qtd 
               item-contrat-estab.acum-rec-val  = tt-item-contrat-estab.im-acum-rec-val 
               item-contrat-estab.cod-emitente  = tt-item-contrat-estab.im-cod-emitente 
               item-contrat-estab.cod-estabel   = tt-item-contrat-estab.im-cod-estabel
               item-contrat-estab.it-codigo     = tt-item-contrat-estab.im-it-codigo 
               item-contrat-estab.nr-contrato   = tt-item-contrat-estab.im-nr-contrato 
               item-contrat-estab.num-seq-item  = tt-item-contrat-estab.im-num-seq-item 
               item-contrat-estab.qtd-total     = tt-item-contrat-estab.im-qtd-total
               item-contrat-estab.sld-qtd       = tt-item-contrat-estab.im-sld-qtd
               item-contrat-estab.sld-qtd-liber = tt-item-contrat-estab.im-sld-qtd-liber
               item-contrat-estab.sld-qtd-receb = tt-item-contrat-estab.im-sld-qtd-receb
               item-contrat-estab.sld-val       = tt-item-contrat-estab.im-sld-val
               item-contrat-estab.sld-val-liber = tt-item-contrat-estab.im-sld-val-liber
               item-contrat-estab.sld-val-receb = tt-item-contrat-estab.im-sld-val-receb
               item-contrat-estab.val-total     = tt-item-contrat-estab.im-val-total.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.


PROCEDURE pi-gera-di:

    CREATE tt-desp-item-contrat.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-desp-item-contrat.im-nr-contrato   = integer(ENTRY(02,tt-linha.linha,";"))
               tt-desp-item-contrat.im-num-seq-item  = integer(ENTRY(03,tt-linha.linha,";"))
               tt-desp-item-contrat.im-num-seq-desp  = integer(ENTRY(04,tt-linha.linha,";"))
               tt-desp-item-contrat.im-cod-despesa   =         ENTRY(05,tt-linha.linha,";")
               tt-desp-item-contrat.im-val-despesa   = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-desp-item-contrat.im-val-per-desp  = decimal(ENTRY(07,tt-linha.linha,";"))
               tt-desp-item-contrat.im-dsl-narrativa =    trim(ENTRY(08,tt-linha.linha,";")).

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-desp-item-contrat.im-nr-contrato = i-nr-contrato.

        FIND contrato-for WHERE
             contrato-for.nr-contrato = tt-desp-item-contrat.im-nr-contrat NO-LOCK NO-ERROR.

        IF NOT AVAIL contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
    
        FIND item-contrat WHERE 
             item-contrat.nr-contrato  = tt-desp-item-contrat.im-nr-contrato AND
             item-contrat.num-seq-item = tt-desp-item-contrat.im-num-seq-item NO-LOCK NO-ERROR.

        IF NOT AVAIL item-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 07). /* codigo erro */
            RETURN "NOK":U.
        END.

        FIND despesa WHERE
             despesa.cod-despesa = tt-desp-item-contrat.im-cod-despesa NO-LOCK NO-ERROR.

        IF NOT AVAIL despesa THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 51). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        if  avail despesa 
        and despesa.cdn-tp-calculo = 2 
        and tt-desp-item-contrat.im-val-despesa = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 55). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF  avail despesa 
        and despesa.cdn-tp-calculo = 1 
        and tt-desp-item-contrat.im-val-per-desp = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 54). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND desp-item-contrat WHERE
             desp-item-contrat.nr-contrato  = tt-desp-item-contrat.im-nr-contrat   AND
             desp-item-contrat.num-seq-item = tt-desp-item-contrat.im-num-seq-item AND
             desp-item-contrat.num-seq-desp = tt-desp-item-contrat.im-num-seq-desp AND
             desp-item-contrat.cod-despesa  = tt-desp-item-contrat.im-cod-despesa NO-LOCK NO-ERROR.

        IF AVAIL desp-item-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                                 INPUT 53). /* codigo erro */
            RETURN "NOK":U.
        END.

        CREATE desp-item-contrat.
        ASSIGN desp-item-contrat.nr-contrato   = tt-desp-item-contrat.im-nr-contrato
               desp-item-contrat.num-seq-item  = tt-desp-item-contrat.im-num-seq-item
               desp-item-contrat.num-seq-desp  = tt-desp-item-contrat.im-num-seq-desp
               desp-item-contrat.cod-despesa   = tt-desp-item-contrat.im-cod-despesa
               desp-item-contrat.val-despesa   = tt-desp-item-contrat.im-val-despesa 
               desp-item-contrat.val-per-desp  = tt-desp-item-contrat.im-val-per-desp
               desp-item-contrat.dsl-narrativa = tt-desp-item-contrat.im-dsl-narrativa.

        RUN pi-gera-log-imp (INPUT tt-desp-item-contrat.im-nr-contrato,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-dc:

    CREATE tt-desp-cotacao-item.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-desp-cotacao-item.im-numero-ordem  = integer(ENTRY(02,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-cod-emitente  = integer(ENTRY(03,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-it-codigo     =         ENTRY(04,tt-linha.linha,";")
               tt-desp-cotacao-item.im-seq-cotac     = integer(ENTRY(05,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-num-seq-desp  = integer(ENTRY(06,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-cod-despesa   =         ENTRY(07,tt-linha.linha,";")
               tt-desp-cotacao-item.im-val-despesa   = Decimal(ENTRY(08,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-val-per-desp  = Decimal(ENTRY(09,tt-linha.linha,";"))
               tt-desp-cotacao-item.im-dsl-narrativa =         ENTRY(10,tt-linha.linha,";").
    
        FIND FIRST ordem-compra WHERE 
                   ordem-compra.numero-ordem = tt-desp-cotacao-item.im-numero-ordem NO-LOCK NO-ERROR.

        IF NOT AVAIL ordem-compra THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 15). /* codigo erro */
            RETURN "NOK":U.
        END.
    
        FIND emitente WHERE
             emitente.cod-emitente = tt-desp-cotacao-item.im-cod-emitente NO-LOCK NO-ERROR.

        IF NOT AVAIL emitente THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 02). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND item WHERE
             item.it-codigo = tt-desp-cotacao-item.im-it-codigo NO-LOCK NO-ERROR.

        IF NOT AVAIL item THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 04). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        FIND despesa WHERE
             despesa.cod-despesa = tt-desp-cotacao-item.im-cod-despesa NO-LOCK NO-ERROR.

        IF NOT AVAIL despesa THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 51). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        if  avail despesa 
        and despesa.cdn-tp-calculo = 2
        and tt-desp-cotacao-item.im-val-despesa = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 55). /* codigo erro */
            RETURN "NOK":U.
        END.
    
        if  avail despesa 
        and despesa.cdn-tp-calculo = 1 
        and tt-desp-cotacao-item.im-val-per-desp = 0 THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 54). /* codigo erro */
            RETURN "NOK":U.
        END.
    
        FIND cotacao-item WHERE
             cotacao-item.numero-ordem = tt-desp-cotacao-item.im-numero-ordem AND
             cotacao-item.cod-emitente = tt-desp-cotacao-item.im-cod-emitente AND
             cotacao-item.it-codigo    = tt-desp-cotacao-item.im-it-codigo    AND                                                  
             cotacao-item.seq-cotac    = tt-desp-cotacao-item.im-seq-cotac NO-LOCK NO-ERROR.

        IF NOT AVAIL cotacao-item THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 46). /* codigo erro */
            RETURN "NOK":U.
        END.

        FIND desp-cotacao-item WHERE
             desp-cotacao-item.numero-ordem = tt-desp-cotacao-item.im-numero-ordem AND
             desp-cotacao-item.cod-emitente = tt-desp-cotacao-item.im-cod-emitente AND
             desp-cotacao-item.it-codigo    = tt-desp-cotacao-item.im-it-codigo    AND
             desp-cotacao-item.seq-cotac    = tt-desp-cotacao-item.im-seq-cotac    AND
             desp-cotacao-item.num-seq-desp = tt-desp-cotacao-item.im-num-seq-desp AND
             desp-cotacao-item.cod-despesa  = tt-desp-cotacao-item.im-cod-despesa NO-LOCK NO-ERROR.

        IF AVAIL desp-cotacao-item THEN DO:

            RUN pi-gera-log-imp (INPUT 0,
                                 INPUT 52). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        CREATE desp-cotacao-item.
        ASSIGN desp-cotacao-item.numero-ordem  = tt-desp-cotacao-item.im-numero-ordem
               desp-cotacao-item.cod-emitente  = tt-desp-cotacao-item.im-cod-emitente
               desp-cotacao-item.it-codigo     = tt-desp-cotacao-item.im-it-codigo
               desp-cotacao-item.seq-cotac     = tt-desp-cotacao-item.im-seq-cotac
               desp-cotacao-item.num-seq-desp  = tt-desp-cotacao-item.im-num-seq-desp 
               desp-cotacao-item.cod-despesa   = tt-desp-cotacao-item.im-cod-despesa
               desp-cotacao-item.val-despesa   = tt-desp-cotacao-item.im-val-despesa
               desp-cotacao-item.val-per-desp  = tt-desp-cotacao-item.im-val-per-desp
               desp-cotacao-item.dsl-narrativa = tt-desp-cotacao-item.im-dsl-narrativa.

        RUN pi-gera-log-imp (INPUT 0,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.


PROCEDURE pi-gera-ac:

    DEFINE VARIABLE t-hora AS DATETIME NO-UNDO.

    ASSIGN t-hora = DATETIME(TODAY, MTIME).

    CREATE tt-anexo-contrat.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-anexo-contrat.i-num-seq-anexo = integer(ENTRY(02,tt-linha.linha,";"))
               tt-anexo-contrat.c-usuario       =         ENTRY(03,tt-linha.linha,";")
               tt-anexo-contrat.i-nr-contrato   = integer(ENTRY(04,tt-linha.linha,";"))
               tt-anexo-contrat.c-des-anexo     =         ENTRY(05,tt-linha.linha,";") 
               tt-anexo-contrat.c-narrat-anexo  =         ENTRY(06,tt-linha.linha,";").

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-anexo-contrat.i-nr-contrato = i-nr-contrato.

        find first anexo-contrat
             where anexo-contrat.num-seq-anexo = tt-anexo-contrat.i-num-seq-anexo
               and anexo-contrat.nr-contrato   = tt-anexo-contrat.i-nr-contrato no-lock no-error.

        if avail anexo-contrat THEN DO:

            RUN pi-gera-log-imp (INPUT tt-anexo-contrat.i-nr-contrato,
                                 INPUT 47). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        find first contrato-for where
             contrato-for.nr-contrato = tt-anexo-contrat.i-nr-contrato no-lock no-error.

        if  not avail contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-anexo-contrat.i-nr-contrato,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.

        create anexo-contrat.
        assign anexo-contrat.num-seq-anexo = tt-anexo-contrat.i-num-seq-anexo
               anexo-contrat.nr-contrato   = tt-anexo-contrat.i-nr-contrato
               anexo-contrat.des-anexo     = tt-anexo-contrat.c-des-anexo
               anexo-contrat.narrat-anexo  = tt-anexo-contrat.c-narrat-anexo
               anexo-contrat.usuario       = tt-anexo-contrat.c-usuario
               anexo-contrat.dat-revisao   = TODAY
               anexo-contrat.hra-alter     = SUBSTR(STRING(t-hora), 12,5).

        RUN pi-gera-log-imp (INPUT tt-anexo-contrat.i-nr-contrato,
                             INPUT 00). /* codigo erro */
        RETURN "OK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-ai:

    DEFINE VARIABLE c-empresa-inv AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-erro-inv    AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE tt-erro-inv.

    EMPTY TEMP-TABLE tt-item-contrato.
    CREATE tt-item-contrato.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-item-contrato.im-cod-emitente         = i-cod-emitente
               tt-item-contrato.im-nr-contrat           =     int(ENTRY(02,tt-linha.linha,";"))
               tt-item-contrato.im-preco-unit           = decimal(ENTRY(03,tt-linha.linha,";"))
               tt-item-contrato.im-qtd-minima           = decimal(ENTRY(04,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val              = decimal(ENTRY(05,tt-linha.linha,";"))
               tt-item-contrato.im-val-fatur-minimo     = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-item-contrato.im-mo-codigo            = integer(ENTRY(07,tt-linha.linha,";"))
               tt-item-contrato.im-log-libera           =     if (ENTRY(08,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-it-codigo            =         ENTRY(09,tt-linha.linha,";")
               tt-item-contrato.im-val-total            = decimal(ENTRY(10,tt-linha.linha,";"))
               tt-item-contrato.im-cod-refer            =         ENTRY(11,tt-linha.linha,";")
               tt-item-contrato.im-codigo-ipi           =     if (ENTRY(12,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-codigo-icm           = integer(ENTRY(13,tt-linha.linha,";"))
               tt-item-contrato.im-un                   =         ENTRY(14,tt-linha.linha,";")
               tt-item-contrato.im-contato              =         ENTRY(15,tt-linha.linha,";")
               tt-item-contrato.im-num-seq-item         = integer(ENTRY(16,tt-linha.linha,";"))
               tt-item-contrato.im-frequencia           = integer(ENTRY(17,tt-linha.linha,";"))
               tt-item-contrato.im-ind-sit-item         =     1
               tt-item-contrato.im-qtd-total            = decimal(ENTRY(19,tt-linha.linha,";"))
               tt-item-contrato.im-ind-un-contrato      = integer(ENTRY(20,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd              = decimal(ENTRY(21,tt-linha.linha,";"))
               tt-item-contrato.im-acum-rec-val         = decimal(ENTRY(22,tt-linha.linha,";"))
               tt-item-contrato.im-acum-rec-qtd         = decimal(ENTRY(23,tt-linha.linha,";"))
               tt-item-contrato.im-ind-tipo-control-val =     int(ENTRY(24,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd-liber        = decimal(ENTRY(25,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val-liber        = decimal(ENTRY(26,tt-linha.linha,";"))
               tt-item-contrato.im-log-control-event    =     if (ENTRY(27,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-ind-caract-item      = integer(ENTRY(28,tt-linha.linha,";"))
               tt-item-contrato.im-log-obrig-item       =     if (ENTRY(29,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-log-ind-multa        =     if (ENTRY(30,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-perc-multa-dia       = decimal(ENTRY(31,tt-linha.linha,";"))
               tt-item-contrato.im-perc-multa-limite    = decimal(ENTRY(32,tt-linha.linha,";"))
               tt-item-contrato.im-cod-depos            =         ENTRY(33,tt-linha.linha,";")
               tt-item-contrato.im-aliquota-icm         = decimal(ENTRY(34,tt-linha.linha,";"))
               tt-item-contrato.im-aliquota-ipi         = decimal(ENTRY(35,tt-linha.linha,";"))
               tt-item-contrato.im-aliquota-iss         = decimal(ENTRY(36,tt-linha.linha,";"))
               tt-item-contrato.im-tp-despesa           = integer(ENTRY(37,tt-linha.linha,";"))
               tt-item-contrato.im-cod-cond-pag         = integer(ENTRY(38,tt-linha.linha,";"))
               tt-item-contrato.im-frete-ped            =     if (ENTRY(39,tt-linha.linha,";")) = "S" then yes else no.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-item-contrato.im-nr-contrat NO-ERROR.
        
        IF AVAIL contrato-for THEN DO:
            
            FIND FIRST item NO-LOCK
                 WHERE item.it-codigo = tt-item-contrato.im-it-codigo NO-ERROR.
            
            /*** REALIZAR ALTERAÄ«O DO PRECO UNITµRIO DO ITEM ***/
            FIND FIRST item-contrat EXCLUSIVE-LOCK
                 WHERE item-contrat.it-codigo     = tt-item-contrato.im-it-codigo  
                 AND   item-contrat.nr-contrato   = tt-item-contrato.im-nr-contrat 
                 AND   item-contrat.num-seq-item  = tt-item-contrato.im-num-seq-item
                 AND   item-contrat.ind-sit-item <> 3                              
                 AND   item-contrat.ind-sit-item <> 4 NO-ERROR.

            IF AVAIL item-contrat THEN DO:

                IF tt-item-contrato.im-preco-unit <> item-contrat.preco-unit THEN DO:

                    IF item-contrat.ind-sit-item = 2 THEN DO:

                        /*** GERAR ADITIVO ***/
                        ASSIGN c-tipo-alter     = "C"
                               c-texto-orig     = ""
                               c-texto          = ""
                               c-alter-origem   = STRING(item-contrat.preco-unit)
                               c-alterado       = STRING(tt-item-contrato.im-preco-unit)
                               gr-contrato-for  = ROWID(contrato-for)
                               l-relacionamento = YES.
                
                        {utp/ut-liter.i Alteraá∆o_Preáo_Itens}
                        ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
                
                        /* Atualiza preáo. */
                        ASSIGN item-contrat.preco-unit = tt-item-contrato.im-preco-unit.
    
                        RUN cnp/cnapi002.p ("item-contrat",
                                            ROWID(item-contrat),
                                            "item-contrat.preco-unit",
                                            l-relacionamento).
                    END.
                    ELSE DO:

                        ASSIGN item-contrat.preco-unit = tt-item-contrato.im-preco-unit.
                    END.

                    /* Atualiza preªo da ordem de compra e cotaªío. */
                    RUN cnp/cn9002.p (INPUT "item-contrat",
                                      INPUT ROWID(item-contrat),
                                      INPUT "item-contrat.preco-unit,item-contrat.preco-fornec",
                                      INPUT NO).

                    RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                         INPUT 00). /* codigo erro */
                    RETURN "OK":U.
                END.

                IF tt-item-contrato.im-val-total <> item-contrat.val-total THEN DO:

                    /* Validar verba no investimento */
                    IF item-contrat.num-ord-inv <> 0 THEN DO:

                        FIND FIRST estabelec NO-LOCK
                             WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-ERROR.

                        IF AVAIL estabelec THEN
                            ASSIGN c-empresa-inv = estabelec.ep-codigo.

                        ELSE DO:

                            FIND FIRST param-global NO-LOCK NO-ERROR.

                            ASSIGN c-empresa-inv = param-global.empresa-prin.
                        END.

                        FIND FIRST param-inv NO-LOCK
                             WHERE param-inv.ep-codigo = c-empresa-inv NO-ERROR.

                        FIND FIRST item-contrat-ext EXCLUSIVE-LOCK
                             WHERE item-contrat-ext.nr-contrato  = item-contrat.nr-contrato
                               AND item-contrat-ext.num-seq-item = item-contrat.num-seq-item NO-ERROR.

                        IF AVAIL item-contrat-ext THEN DO:

                            ASSIGN item-contrat-ext.num-ord-inv = item-contrat.num-ord-inv
                                   item-contrat-ext.ep-codigo   = c-empresa-inv.
                        END.
                        ELSE DO:

                            CREATE item-contrat-ext.
                            ASSIGN item-contrat-ext.nr-contrato  = item-contrat.nr-contrato
                                   item-contrat-ext.num-seq-item = item-contrat.num-seq-item
                                   item-contrat-ext.num-ord-inv  = item-contrat.num-ord-inv
                                   item-contrat-ext.ep-codigo    = c-empresa-inv.
                        END.

                        EMPTY TEMP-TABLE tt-erro-inv.

                        IF NOT VALID-HANDLE(h-esapi001) THEN
                            RUN esp\esapi001.p PERSISTENT SET h-esapi001.

                        RUN pi-atualiza-verba IN h-esapi001 (INPUT  1, /* Valida Verba */
                                                             INPUT  c-empresa-inv,
                                                             INPUT  item-contrat-ext.num-ord-inv,
                                                             INPUT  contrato-for.dt-contrato,
                                                             INPUT  param-inv.moeda-inv,
                                                             INPUT  (tt-item-contrato.im-val-total - item-contrat.val-total),
                                                             INPUT  0,
                                                             OUTPUT TABLE tt-erro-inv).

                        FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

                        IF AVAIL tt-erro-inv THEN DO:
            
                            ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.
            
                            CREATE tt-importacao.
                            ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                                   tt-importacao.linha       = tt-linha.num-linha
                                   tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).
            
                            IF VALID-HANDLE(h-esapi001) THEN DO:
                                DELETE PROCEDURE h-esapi001.
                                ASSIGN h-esapi001 = ?.
                            END.
            
                            RETURN "NOK":U.
                        END.
                    END.

                    IF item-contrat.ind-sit-item = 2 THEN DO:

                        /*** GERAR ADITIVO ***/
                        ASSIGN c-tipo-alter     = "C"
                               c-texto-orig     = ""
                               c-texto          = ""
                               c-alter-origem   = STRING(item-contrat.val-total)
                               c-alterado       = STRING(tt-item-contrato.im-val-total)
                               gr-contrato-for  = ROWID(contrato-for)
                               l-relacionamento = YES.
                
                        {utp/ut-liter.i Limite_Valor}
                        ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
                
                        /* Atualiza preáo. */
                        ASSIGN item-contrat.val-total = tt-item-contrato.im-val-total.
    
                        RUN cnp/cnapi002.p ("item-contrat",
                                            ROWID(item-contrat),
                                            "item-contrat.val-total",
                                            l-relacionamento).

                        RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                             INPUT 00). /* codigo erro */
                        RETURN "OK":U.
                    END.
                    ELSE DO:

                        ASSIGN item-contrat.val-total = tt-item-contrato.im-val-total.

                        IF item-contrat.num-ord-inv <> 0 THEN DO:

                            EMPTY TEMP-TABLE tt-erro-inv.
        
                            IF NOT VALID-HANDLE(h-esapi001) THEN
                                RUN esp\esapi001.p PERSISTENT SET h-esapi001.

                            IF AVAIL item-contrat-ext THEN DO:

                                FOR EACH controle-inv-esp EXCLUSIVE-LOCK
                                   WHERE controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
                                     AND controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                                     AND controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
                                     AND controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:

                                    RUN pi-atualiza-verba IN h-esapi001 (INPUT 2,
                                                                         INPUT controle-inv-esp.ep-codigo,
                                                                         INPUT controle-inv-esp.num-ord-inv,
                                                                         INPUT controle-inv-esp.dt-trans,
                                                                         INPUT param-inv.moeda-inv,
                                                                         INPUT controle-inv-esp.ent-comp * -1,
                                                                         INPUT controle-inv-esp.ent-real * -1,
                                                                         OUTPUT TABLE tt-erro-inv).
                                        
                                    FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.

                                    IF AVAIL tt-erro-inv THEN DO:

                                        ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.
                        
                                        CREATE tt-importacao.
                                        ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                                               tt-importacao.linha       = tt-linha.num-linha
                                               tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).
                        
                                        IF VALID-HANDLE(h-esapi001) THEN DO:
                                            DELETE PROCEDURE h-esapi001.
                                            ASSIGN h-esapi001 = ?.
                                        END.
                        
                                        RETURN "NOK":U.
                                    END.

                                    DELETE controle-inv-esp.
                                END.
                            END.

                            RUN pi-principal IN h-esapi001 (INPUT 2,
                                                            INPUT ROWID(item-contrat),
                                                            INPUT 2,
                                                            OUTPUT TABLE tt-erro-inv).
        
                            FIND FIRST tt-erro-inv NO-LOCK NO-ERROR.
    
                            IF AVAIL tt-erro-inv THEN DO:

                                ASSIGN c-erro-inv = STRING(tt-erro-inv.cd-erro) + "-" + tt-erro-inv.mensagem.
                
                                CREATE tt-importacao.
                                ASSIGN tt-importacao.nr-contrato = contrato-for.nr-contrato
                                       tt-importacao.linha       = tt-linha.num-linha
                                       tt-importacao.desc-erro   = SUBSTRING(c-erro-inv,1,68).
                
                                IF VALID-HANDLE(h-esapi001) THEN DO:
                                    DELETE PROCEDURE h-esapi001.
                                    ASSIGN h-esapi001 = ?.
                                END.
                
                                RETURN "NOK":U.
                            END.

                            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                                 INPUT 00). /* codigo erro */
                            RETURN "OK":U.
                        END.
                        ELSE DO:

                            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                                 INPUT 00). /* codigo erro */
                            RETURN "OK":U.
                        END.
                    END.
                END.
            END.
            ELSE DO:
                RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                     INPUT 07). /* codigo erro */
                RETURN "NOK":U.
            END.
        END.
        ELSE DO: 

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
    END.

    IF de-sequencia = 01 THEN DO:

        ASSIGN tt-item-contrato.im-nr-contrat       =     int(ENTRY(02,tt-linha.linha,";"))
               tt-item-contrato.im-num-seq-item     = integer(ENTRY(03,tt-linha.linha,";"))
               tt-item-contrato.im-preco-fornec     = decimal(ENTRY(04,tt-linha.linha,";"))
               tt-item-contrato.im-taxa-financ      =     if (ENTRY(05,tt-linha.linha,";")) = "S" then yes else no
               tt-item-contrato.im-val-frete        = decimal(ENTRY(06,tt-linha.linha,";"))
               tt-item-contrato.im-val-taxa         = decimal(ENTRY(07,tt-linha.linha,";"))
               tt-item-contrato.im-prazo-ent        = integer(ENTRY(08,tt-linha.linha,";"))
               tt-item-contrato.im-dat-cotac        =    date(ENTRY(09,tt-linha.linha,";"))
               tt-item-contrato.im-preco-base       = decimal(ENTRY(10,tt-linha.linha,";"))
               tt-item-contrato.im-cod-comprado     =         ENTRY(11,tt-linha.linha,";")
               tt-item-contrato.im-perc-desconto    = decimal(ENTRY(12,tt-linha.linha,";"))
               tt-item-contrato.im-narrat-compra    =         ENTRY(13,tt-linha.linha,";")
               tt-item-contrato.im-ind-tipo-control =     int(ENTRY(14,tt-linha.linha,";"))
               tt-item-contrato.im-pre-unit-for     = decimal(ENTRY(15,tt-linha.linha,";"))
               tt-item-contrato.im-dat-base         =    date(ENTRY(16,tt-linha.linha,";"))
               tt-item-contrato.im-sld-qtd-receb    = decimal(ENTRY(17,tt-linha.linha,";"))
               tt-item-contrato.im-sld-val-receb    = decimal(ENTRY(18,tt-linha.linha,";"))
               tt-item-contrato.im-ordem-base       = integer(ENTRY(19,tt-linha.linha,";"))
               tt-item-contrato.im-narrat-item      =         ENTRY(20,tt-linha.linha,";")
               tt-item-contrato.im-perc-compra      = decimal(ENTRY(21,tt-linha.linha,";"))
               tt-item-contrato.im-num-ord-invest   = integer(ENTRY(22,tt-linha.linha,";")).

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-item-contrato.im-nr-contrat NO-ERROR.
        
        IF AVAIL contrato-for THEN DO:
            
            FIND FIRST item NO-LOCK
                 WHERE item.it-codigo = tt-item-contrato.im-it-codigo NO-ERROR.

            FIND FIRST item-contrat EXCLUSIVE-LOCK
                 WHERE item-contrat.nr-contrato   = tt-item-contrato.im-nr-contrat 
                 AND   item-contrat.num-seq-item  = tt-item-contrato.im-num-seq-item
                 AND   item-contrat.ind-sit-item <> 3                              
                 AND   item-contrat.ind-sit-item <> 4 NO-ERROR.

            IF AVAIL item-contrat THEN DO:

                IF tt-item-contrato.im-preco-fornec <> item-contrat.preco-fornec THEN DO:

                    IF item-contrat.ind-sit-item = 2 THEN DO:

                        /*** GERAR ADITIVO ***/
                        ASSIGN c-tipo-alter     = "C"
                               c-texto-orig     = ""
                               c-texto          = ""
                               c-alter-origem   = STRING(item-contrat.preco-fornec)
                               c-alterado       = STRING(tt-item-contrato.im-preco-fornec)
                               gr-contrato-for  = ROWID(contrato-for)
                               l-relacionamento = YES.
                
                        {utp/ut-liter.i Alteraá∆o_Preáo_Itens}
                        ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
                
                        /* Atualiza preáo. */
                        ASSIGN item-contrat.preco-fornec = tt-item-contrato.im-preco-fornec.

                        RUN cnp/cnapi002.p ("item-contrat",
                                            ROWID(item-contrat),
                                            "item-contrat.preco-fornec",
                                            l-relacionamento).
                    END.
                    ELSE DO:

                        ASSIGN item-contrat.preco-fornec = tt-item-contrato.im-preco-fornec.

                    END.

                    RUN pi-calcula-preco.

                    /* Atualiza preªo da ordem de compra e cotaªío. */
                    RUN cnp/cn9002.p (INPUT "item-contrat",
                                      INPUT ROWID(item-contrat),
                                      INPUT "item-contrat.preco-unit,item-contrat.preco-fornec",
                                      INPUT NO).

                    RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                         INPUT 00). /* codigo erro */
                    RETURN "OK":U.
                END.

                IF tt-item-contrato.im-preco-base <> item-contrat.preco-base THEN DO:

                    IF item-contrat.ind-sit-item = 2 THEN DO:

                        /*** GERAR ADITIVO ***/
                        ASSIGN c-tipo-alter     = "C"
                               c-texto-orig     = ""
                               c-texto          = ""
                               c-alter-origem   = STRING(item-contrat.preco-base)
                               c-alterado       = STRING(tt-item-contrato.im-preco-base)
                               gr-contrato-for  = ROWID(contrato-for)
                               l-relacionamento = YES.
                
                        {utp/ut-liter.i Alteraá∆o_Preáo_Itens}
                        ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
                
                        /* Atualiza preáo. */
                        ASSIGN item-contrat.preco-base = tt-item-contrato.im-preco-base.
    
                        RUN cnp/cnapi002.p ("item-contrat",
                                            ROWID(item-contrat),
                                            "item-contrat.preco-base",
                                            l-relacionamento).
                    END.
                    ELSE DO:

                        ASSIGN item-contrat.preco-base = tt-item-contrato.im-preco-base.
                    END.

                    RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                         INPUT 00). /* codigo erro */
                    RETURN "OK":U.
                END.

                IF tt-item-contrato.im-pre-unit-for <> item-contrat.pre-unit-for THEN DO:

                    IF item-contrat.ind-sit-item = 2 THEN DO:

                        /*** GERAR ADITIVO ***/
                        ASSIGN c-tipo-alter     = "C"
                               c-texto-orig     = ""
                               c-texto          = ""
                               c-alter-origem   = STRING(item-contrat.pre-unit-for)
                               c-alterado       = STRING(tt-item-contrato.im-pre-unit-for)
                               gr-contrato-for  = ROWID(contrato-for)
                               l-relacionamento = YES.
                
                        {utp/ut-liter.i Alteraá∆o_Preáo_Itens}
                        ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
                
                        /* Atualiza preáo. */
                        ASSIGN item-contrat.pre-unit-for = tt-item-contrato.im-pre-unit-for.

                        RUN pi-calcula-preco.

                        RUN cnp/cnapi002.p ("item-contrat",
                                            ROWID(item-contrat),
                                            "item-contrat.pre-unit-for",
                                            l-relacionamento).
                    END.
                    ELSE DO:

                        ASSIGN item-contrat.pre-unit-for = tt-item-contrato.im-pre-unit-for.

                        RUN pi-calcula-preco.

                    END.

                    RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                         INPUT 00). /* codigo erro */
                    RETURN "OK":U.
                END.
            END.
            ELSE DO:
                RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                     INPUT 07). /* codigo erro */
                RETURN "NOK":U.
            END.
        END.
        ELSE DO: 

            RUN pi-gera-log-imp (INPUT tt-item-contrato.im-nr-contrat,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
    END.
                
    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-calcula-preco:

    DEFINE VARIABLE de-preco AS DECIMAL NO-UNDO.

    find first param-compra no-lock no-error.

    assign item-contrat.pre-unit-for = item-contrat.preco-fornec.

    if item-contrat.taxa-financ then do:
        if item-contrat.codigo-ipi then do:

           if item-contrat.perc-desconto > 0 then
               assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).
        end.
        else do:
             if (param-compra.ipi-sobre-preco = 1) = no then do:

                if item-contrat.perc-desconto > 0 then
                    assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).

                assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 + (item-contrat.aliquota-ipi / 100)).
             end.
             else do:
                  assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 + (item-contrat.aliquota-ipi / 100)).

                  if item-contrat.perc-desconto > 0 then 
                     assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).
             end.
        end.
    end.
    else do:
        if item-contrat.codigo-ipi then do:
           if (param-compra.ipi-sobre-preco = 1) = no then do:
              if item-contrat.perc-desconto  > 0 then 
                 assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).

              run ccp/cc9020.p (input yes,
                                input item-contrat.cod-cond-pag,
                                input item-contrat.val-taxa,
                                input item-contrat.nr-dias-taxa,
                                input item-contrat.pre-unit-for,
                                output de-preco).
              assign item-contrat.pre-unit-for = de-preco.
           end.
           else do:
                run ccp/cc9020.p (input yes,
                                  input item-contrat.cod-cond-pag,
                                  input item-contrat.val-taxa,
                                  input item-contrat.nr-dias-taxa,
                                  input item-contrat.pre-unit-for,
                                  output de-preco).
                assign item-contrat.pre-unit-for = de-preco.

                if item-contrat.perc-desconto  > 0 then 
                   assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).
           end.
        end.
        else do:
           if (param-compra.ipi-sobre-preco = 1) = no then do:
              if item-contrat.perc-desconto > 0 then
                 assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).

              run ccp/cc9020.p (input yes,
                                input item-contrat.cod-cond-pag,
                                input item-contrat.val-taxa,
                                input item-contrat.nr-dias-taxa,
                                input item-contrat.pre-unit-for,
                                output de-preco).

              assign item-contrat.pre-unit-for = de-preco.                  

              assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 + (item-contrat.aliquota-ipi / 100)).
           end.
           else do:
                run ccp/cc9020.p (input yes,
                                  input item-contrat.cod-cond-pag,
                                  input item-contrat.val-taxa,
                                  input item-contrat.nr-dias-taxa,
                                  input item-contrat.pre-unit-for,
                                  output de-preco).

                assign item-contrat.pre-unit-for = de-preco.                 

                assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 + (item-contrat.aliquota-ipi / 100)).

                if item-contrat.perc-desconto > 0 then
                   assign item-contrat.pre-unit-for = item-contrat.pre-unit-for * (1 - (item-contrat.perc-desconto / 100)).
           end.
        end.
    end.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-pd:

    DEFINE VARIABLE i-cont-pedido AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-pedido      AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE tt-pedido-auto.
    CREATE tt-pedido-auto.

    IF de-sequencia = 00 THEN DO:

        ASSIGN tt-pedido-auto.im-end-entrega    =         ENTRY(02,tt-linha.linha,";")
               tt-pedido-auto.im-end-cobranca   =         ENTRY(03,tt-linha.linha,";")
               tt-pedido-auto.im-condicao-ped   = INTEGER(ENTRY(04,tt-linha.linha,";"))
               tt-pedido-auto.im-cod-emit-terc  = INTEGER(ENTRY(05,tt-linha.linha,";")).

        IF tt-linha.acao = 1 THEN DO:
           FIND FIRST estabelec
                WHERE estabelec.cod-estabel = tt-pedido-auto.im-end-entrega
                NO-LOCK NO-ERROR.
           IF AVAILABLE(estabelec) THEN DO:
              IF estabelec.ep-codigo <> i-ep-codigo-usuario THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                       INPUT 70). /* codigo erro */
                  RETURN "NOK":U.
              END.
           END.
        END.

        IF tt-linha.acao = 1 THEN
            ASSIGN tt-pedido-auto.im-nr-contrato = i-nr-contrato.

        /* Localiza pr¢ximo n£mero de pedido dispon°vel */
        FIND FIRST param-compra NO-LOCK NO-ERROR.
        FIND LAST b-pedido-compr NO-LOCK NO-ERROR.

        IF AVAIL b-pedido-compr
             AND b-pedido-compr.num-pedido <= param-compra.num-pedido-fim
             AND b-pedido-compr.num-pedido >= param-compra.num-pedido-ini THEN DO:

            ASSIGN i-cont-pedido = 0.

            IF b-pedido-compr.num-pedido = 0 THEN DO:

                ASSIGN i-cont-pedido = 1.

                REPEAT:

                    FIND PREV b-pedido-compr NO-LOCK NO-ERROR.

                    IF AVAIL b-pedido-compr THEN DO:

                        IF b-pedido-compr.num-pedido = 0 THEN
                            ASSIGN i-cont-pedido = i-cont-pedido + 1.
                        ELSE LEAVE.
                    END.
                    ELSE DO:

                        FIND LAST b-pedido-compr NO-LOCK
                            WHERE b-pedido-compr.num-pedido <= param-compra.num-pedido-fim
                              AND b-pedido-compr.num-pedido >= param-compra.num-pedido-ini NO-ERROR.                                        
                        LEAVE.
                    END.
                END.
            END.
        
            IF i-cont-pedido > 0 THEN DO:

                IF AVAILABLE b-pedido-compr THEN DO:

                    IF (b-pedido-compr.num-pedido + i-cont-pedido + 1) > param-compra.num-pedido-fim OR 
                       (b-pedido-compr.num-pedido + i-cont-pedido + 1) < param-compra.num-pedido-ini THEN DO:

                        ASSIGN i-pedido = param-compra.num-pedido-ini.
                    END.
                    ELSE                
                        ASSIGN i-pedido = b-pedido-compr.num-pedido + i-cont-pedido + 1.
                END.                                  
                ELSE 
                    ASSIGN i-pedido = param-compra.num-pedido-ini.
            END.
            ELSE DO:

                IF AVAILABLE b-pedido-compr THEN DO:

                    IF (b-pedido-compr.num-pedido + 1) > param-compra.num-pedido-fim OR
                       (b-pedido-compr.num-pedido + 1) < param-compra.num-pedido-ini THEN DO:

                        ASSIGN i-pedido = param-compra.num-pedido-ini.
                    END.
                    ELSE
                        ASSIGN i-pedido = b-pedido-compr.num-pedido + 1.  
                END.                                  
                ELSE
                    ASSIGN i-pedido = param-compra.num-pedido-ini.
            END.                                                          
        END.
        ELSE 
            ASSIGN i-pedido = param-compra.num-pedido-ini.
        
         /* acha proximo numero disponivel ao chegar ao ultimo parametrizado */
        IF i-pedido = param-compra.num-pedido-ini THEN DO:

            REPEAT:

                FIND  b-pedido-compr
                WHERE b-pedido-compr.num-pedido = i-pedido NO-LOCK NO-ERROR.

                IF AVAIL b-pedido-compr THEN DO:

                    ASSIGN i-pedido = i-pedido + 1.

                    IF i-pedido > param-compra.num-pedido-fim THEN DO:

                        RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                             INPUT 66). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                END.
                ELSE LEAVE.
            END.
        END.
        
        ASSIGN tt-pedido-auto.im-num-pedido = i-pedido.

        FIND FIRST contrato-for NO-LOCK
             WHERE contrato-for.nr-contrato = tt-pedido-auto.im-nr-contrat NO-ERROR. 

        IF NOT AVAIL contrato-for THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                 INPUT 01). /* codigo erro */
            RETURN "NOK":U.
        END.
            
        IF tt-pedido-auto.im-condicao-ped <> 0 THEN DO:

            FIND FIRST cond-pagto
                 WHERE cond-pagto.cod-cond-pag = tt-pedido-auto.im-condicao-ped NO-ERROR.

            IF NOT AVAIL cond-pagto THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                     INPUT 10). /* codigo erro */
                RETURN "NOK":U.
            END.
            ELSE DO:
               FIND FIRST es-cond-pagto WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                    NO-LOCK NO-ERROR.
               IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                        INPUT 95). /* codigo erro */
                  RETURN "NOK":U.
               END.
            END.
        END.

        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = tt-pedido-auto.im-end-entrega NO-ERROR.

        IF NOT AVAIL estabelec THEN DO:

            IF NOT AVAIL cond-pagto THEN DO:

                RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                     INPUT 12). /* codigo erro */
                RETURN "NOK":U.
            END.
            ELSE DO:
               FIND FIRST es-cond-pagto WHERE es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag
                    NO-LOCK NO-ERROR.
               IF AVAILABLE(es-cond-pagto) AND es-cond-pagto.cd-ativa = YES THEN DO:
                  RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                        INPUT 95). /* codigo erro */
                  RETURN "NOK":U.
               END.
            END.
        END.
            
        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = tt-pedido-auto.im-end-cobranca NO-ERROR.

        IF NOT AVAIL estabelec THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                 INPUT 12). /* codigo erro */
            RETURN "NOK":U.
        END.

        IF CAN-FIND (FIRST pedido-compr 
                     WHERE pedido-compr.nr-contrato = tt-pedido-auto.im-nr-contrato                       
                       AND pedido-compr.end-entrega = tt-pedido-auto.im-end-entrega) THEN DO:

            RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                 INPUT 50). /* codigo erro */
            RETURN "NOK":U.
        END.            

        FIND FIRST pedido-compr EXCLUSIVE-LOCK
             WHERE pedido-compr.num-pedido = tt-pedido-auto.im-num-pedido NO-ERROR.

        IF NOT AVAIL pedido-compr THEN DO:

            CREATE pedido-compr.
            ASSIGN pedido-compr.num-pedido       = tt-pedido-auto.im-num-pedido    
                   pedido-compr.nr-contrato      = tt-pedido-auto.im-nr-contrato
                   pedido-compr.end-entrega      = tt-pedido-auto.im-end-entrega   
                   pedido-compr.end-cobranca     = tt-pedido-auto.im-end-cobranca
                   pedido-compr.cod-cond-pag     = tt-pedido-auto.im-condicao-ped 
                   pedido-compr.cod-estabel      = tt-pedido-auto.im-end-entrega
                   pedido-compr.cod-emit-terc    = tt-pedido-auto.im-cod-emit-terc
                   pedido-compr.cod-emitente     = contrato-for.cod-emitente
                   pedido-compr.situacao         = 2
                   pedido-compr.natureza         = contrato-for.natureza
                   pedido-compr.cod-mensagem     = contrato-for.cod-mensagem
                   pedido-compr.via-transp       = contrato-for.via-transp
                   pedido-compr.cod-transp       = contrato-for.cod-transp
                   pedido-compr.frete            = contrato-for.frete
                   pedido-compr.impr-pedido      = YES
                   pedido-compr.data-pedido      = contrato-for.dt-contrato
                   pedido-compr.responsavel      = contrato-for.cod-comprado
                   pedido-compr.cod-estab-gestor = contrato-for.cod-estab-gestor.

            RUN pi-gera-log-imp (INPUT pedido-compr.nr-contrato,
                                 INPUT 00). /* codigo erro */
            RETURN "OK":U.
        END.
        ELSE DO:

            RUN pi-gera-log-imp (INPUT tt-pedido-auto.im-nr-contrato,
                                 INPUT 29). /* codigo erro */
            RETURN "NOK":U.
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-gera-ordem-auto:

    DEFINE BUFFER b-item-contrat FOR item-contrat.

    DEFINE VARIABLE l-preco-bruto    AS   LOGICAL INIT NO                       NO-UNDO.
    DEFINE VARIABLE de-indice        AS   DECIMAL                               NO-UNDO.
    DEFINE VARIABLE i-numero-ordem   AS   INTEGER format "999999" label "Ordem" NO-UNDO.
    DEFINE VARIABLE i-num-ord        AS   INTEGER                               NO-UNDO.
    DEFINE VARIABLE de-aliq-icm      LIKE natur-oper.aliquota-icm               NO-UNDO.
    DEFINE VARIABLE de-preco-dif     LIKE ordem-compra.preco-unit               NO-UNDO.
    DEFINE VARIABLE l-unid-neg       AS   LOGICAL                               NO-UNDO. 
    DEFINE VARIABLE c-cod-unid-negoc AS   CHARACTER                             NO-UNDO.
    DEFINE VARIABLE c-cod-compr-un   AS   CHARACTER                             NO-UNDO.
    DEFINE VARIABLE h-cdapi024       AS   HANDLE                                NO-UNDO.
    DEFINE VARIABLE l-valida-online  AS   LOGICAL                               NO-UNDO.
    DEFINE VARIABLE h_api_cta_ctbl   AS   HANDLE                                NO-UNDO.

    {cdp/cd9590.i}    /*Include verificaá∆o Unidade Neg¢cio*/

    EMPTY TEMP-TABLE tt-erro-aux.
    EMPTY TEMP-TABLE tt-ordem-compra-auto.
    EMPTY TEMP-TABLE tt_log_erro.
    EMPTY TEMP-TABLE tt-problema-un-negoc.

    IF NOT VALID-HANDLE(h-cdapi024) THEN
        RUN cdp/cdapi024.p PERSISTENT SET h-cdapi024.
    
    IF NOT AVAIL param-global THEN
        FIND FIRST param-global NO-LOCK NO-ERROR.
    
    ASSIGN l-valida-online = param-global.log-validac-ems5.

    FIND FIRST param-compra  NO-LOCK NO-ERROR.
    FIND FIRST param-contrat NO-LOCK NO-ERROR.
    
    IF contrato-for.ind-control-rec = 1 THEN
        FIND item WHERE item.it-codigo = param-contrat.it-codigo NO-LOCK NO-ERROR.
    ELSE
        FIND item WHERE item.it-codigo = item-contrat.it-codigo NO-LOCK NO-ERROR.

    if avail param-compra then do:
    
            RUN ccp/ccapi333.p(INPUT  YES,
                               OUTPUT i-numero-ordem).
    end.

    find first ordem-compra
         where ordem-compra.numero-ordem = i-numero-ordem no-wait no-error.

    if  avail ordem-compra or locked ordem-compra then do:
        
        RUN pi-gera-log-imp (INPUT pedido-compr.nr-contrato,
                             INPUT 67). /* codigo erro */
        RETURN "NOK":U.
    end.
    
    find first ordem-compra no-lock
         where ordem-compra.cod-estabel  = pedido-compr.cod-estabel
           and ordem-compra.nr-contrato  = pedido-compr.nr-contrat
           and ordem-compra.num-seq-item = 0
           and ordem-compra.it-codigo    = param-contrat.it-codigo no-error.
    
    if  avail ordem-compra and contrato-for.ind-control-rec = 1 then .
        /* Ordem ja foi cadastrada para o estabelecimento */ 
    else do:
        create cotacao-item.
        assign cotacao-item.cot-aprovada = yes
               cotacao-item.it-codigo    = item-contrat.it-codigo
               cotacao-item.data-cotacao = item-contrat.dat-cotac
               cotacao-item.cod-emitente = contrato-for.cod-emitente
               cotacao-item.un           = item-contrat.un
               cotacao-item.preco-fornec = item-contrat.preco-fornec
               cotacao-item.pre-unit-for = item-contrat.pre-unit-for
               cotacao-item.preco-unit   = item-contrat.preco-unit
               cotacao-item.mo-codigo    = item-contrat.mo-codigo
               cotacao-item.codigo-ipi   = item-contrat.codigo-ipi
               cotacao-item.aliquota-ipi = item-contrat.aliquota-ipi
               cotacao-item.codigo-icm   = item-contrat.codigo-icm
               cotacao-item.aliquota-icm = item-contrat.aliquota-icm
               cotacao-item.aliquota-iss = item-contrat.aliquota-iss
               cotacao-item.frete        = item-contrat.frete
               cotacao-item.valor-frete  = item-contrat.val-frete
               cotacao-item.taxa-financ  = item-contrat.taxa-financ
               cotacao-item.valor-taxa   = item-contrat.val-taxa
               cotacao-item.nr-dias-taxa = item-contrat.nr-dias-taxa
               cotacao-item.perc-descto  = item-contrat.perc-desconto
               cotacao-item.cod-cond-pag = item-contrat.cod-cond-pag
               cotacao-item.prazo-entreg = item-contrat.prazo-ent
               cotacao-item.contato      = item-contrat.contato
               cotacao-item.cod-comprado = item-contrat.cod-comprado
               cotacao-item.numero-ordem = i-numero-ordem.
    
        create tt-ordem-compra-auto.
        
        if  contrato-for.ind-control-rec  = 1 then do:

            find first b-item-contrat where 
                 b-item-contrat.nr-contrato = contrato-for.nr-contrato and
                 b-item-contrat.ind-sit-item < 3
                 use-index item-contrat no-lock no-error.

            assign tt-ordem-compra-auto.numero-ordem = i-numero-ordem
                   tt-ordem-compra-auto.situacao     = 2
                   tt-ordem-compra-auto.cod-comprado = contrato-for.cod-comprado
                   tt-ordem-compra-auto.requisitante = contrato-for.cod-comprado
                   tt-ordem-compra-auto.cod-cond-pag = contrato-for.cod-cond-pag
                   tt-ordem-compra-auto.cod-emitente = contrato-for.cod-emitente
                   tt-ordem-compra-auto.cod-estabel  = pedido-compr.cod-estabel
                   tt-ordem-compra-auto.it-codigo    = param-contrat.it-codigo
                   tt-ordem-compra-auto.num-pedido   = pedido-compr.num-pedido
                   tt-ordem-compra-auto.data-pedido  = pedido-compr.data-pedido
                   tt-ordem-compra-auto.narrativa    = item.desc-item
                   tt-ordem-compra-auto.dep-almoxar  = item.deposito-pad
                   tt-ordem-compra-auto.aliquota-icm = 0
                   tt-ordem-compra-auto.aliquota-ipi = 0
                   tt-ordem-compra-auto.aliquota-iss = 0
                   tt-ordem-compra-auto.valor-frete  = 0
                   tt-ordem-compra-auto.valor-taxa   = 0
                   tt-ordem-compra-auto.perc-descto  = 0
                   tt-ordem-compra-auto.mo-codigo    = contrato-for.mo-codigo
                   tt-ordem-compra-auto.preco-unit   = 0
                   tt-ordem-compra-auto.preco-orig   = 0
                   tt-ordem-compra-auto.tp-despesa   = b-item-contrat.tp-despesa
                   i-num-ord                         = i-numero-ordem / 100.
    
            IF  l-unidade-negocio THEN DO:
               EMPTY TEMP-TABLE tt-erro-aux.
    
               IF  VALID-HANDLE(h-cdapi024) THEN DO:
                   RUN RetornaUnidadeNegocioExternaliz IN h-cdapi024 (INPUT pedido-compr.cod-estabel,
                                                                      INPUT item-contrat.it-codigo,
                                                                      INPUT item-contrat.cod-depos,
                                                                      OUTPUT c-cod-unid-negoc).
                   /*UN X Estabelecimento*/
                   RUN ValidaUnidadeNegocioEstabel IN h-cdapi024 (INPUT pedido-compr.cod-estabel,
                                                                  INPUT tt-ordem-compra-auto.data-emissao,
                                                                  INPUT c-cod-unid-negoc, 
                                                                  OUTPUT TABLE tt-erro-aux).
                   /*UN X Usu†rio*/
                   RUN ValidaUnidadeNegocioUsuario IN h-cdapi024 (INPUT contrato-for.cod-comprado,
                                                                  INPUT c-cod-unid-negoc, 
                                                                  OUTPUT TABLE tt-erro-aux APPEND).
               end.
               IF CAN-FIND(FIRST tt-erro-aux) THEN DO:

                   CREATE tt-problema-un-negoc.

                   FOR EACH tt-erro-aux:
                       ASSIGN tt-problema-un-negoc.c-desc-prob = tt-problema-un-negoc.c-desc-prob + tt-erro-aux.mensagem.
                   END.

                   RUN pi-gera-log-imp (INPUT pedido-compr.nr-contrato,
                                        INPUT 99). /* codigo erro */
                   RETURN "NOK":U.
               END.
               ASSIGN tt-ordem-compra-auto.cod-unid-negoc = c-cod-unid-negoc.
            END.
        end.
        else do:
    
            /* calcula indice */
            if  avail item-contrat then do:
                find item where item.it-codigo = item-contrat.it-codigo no-lock no-error.
                assign de-indice = 1.
                if  avail item then do:
                    if  item.tipo-contr = 4 and
                        not can-find (item-fornec where
                                      item-fornec.it-codigo = item-contrat.it-codigo and
                                      item-fornec.cod-emitente = contrato-for.cod-emitente and
                                      item-fornec.ativo)
                        then do:
                        if  item-contrat.un <> item.un then do:
                            find tab-conv-un where tab-conv-un.un           = item.un and 
                                                   tab-conv-un.unid-med-for = item-contrat.un no-lock no-error.
                            assign de-indice = if avail tab-conv-un then tab-conv-un.fator-conver / exp(10, tab-conv-un.num-casa-dec)
                                               else 1.
                        end.
                        else assign de-indice = 1.
                    end.
                    else do:
                        find item-fornec where 
                             item-fornec.it-codigo = item.it-codigo and 
                             item-fornec.cod-emitente = contrato-for.cod-emitente no-lock no-error.
                        if avail item-fornec then do:
                           assign de-indice = item-fornec.fator-conver / exp(10, item-fornec.num-casa-dec).
                        end.
                    end.
                end.
                if de-indice = 0 then
                   de-indice = 1.
            end.
    
            assign tt-ordem-compra-auto.numero-ordem = i-numero-ordem
                   tt-ordem-compra-auto.situacao     = 2
                   tt-ordem-compra-auto.cod-emitente = contrato-for.cod-emitente
                   tt-ordem-compra-auto.data-cotacao = item-contrat.dat-cotac
                   tt-ordem-compra-auto.pre-unit-for = item-contrat.pre-unit-for
                   tt-ordem-compra-auto.preco-fornec = item-contrat.preco-fornec
                   tt-ordem-compra-auto.preco-unit   = item-contrat.preco-unit
                   tt-ordem-compra-auto.preco-orig   = item-contrat.preco-unit
                   tt-ordem-compra-auto.mo-codigo    = item-contrat.mo-codigo
                   tt-ordem-compra-auto.codigo-ipi   = item-contrat.codigo-ipi
                   tt-ordem-compra-auto.aliquota-ipi = item-contrat.aliquota-ipi
                   tt-ordem-compra-auto.codigo-icm   = item-contrat.codigo-icm
                   tt-ordem-compra-auto.aliquota-icm = item-contrat.aliquota-icm
                   tt-ordem-compra-auto.aliquota-iss = item-contrat.aliquota-iss
                   tt-ordem-compra-auto.frete        = item-contrat.frete
                   tt-ordem-compra-auto.valor-frete  = item-contrat.val-frete
                   tt-ordem-compra-auto.taxa-financ  = item-contrat.taxa-financ
                   tt-ordem-compra-auto.valor-taxa   = item-contrat.val-taxa
                   tt-ordem-compra-auto.nr-dias-taxa = item-contrat.nr-dias-taxa
                   tt-ordem-compra-auto.perc-descto  = item-contrat.perc-desconto
                   tt-ordem-compra-auto.cod-cond-pag = item-contrat.cod-cond-pag
                   tt-ordem-compra-auto.prazo-entreg = item-contrat.prazo-ent
                   tt-ordem-compra-auto.contato      = item-contrat.contato
                   tt-ordem-compra-auto.cod-comprado = item-contrat.cod-comprado
                   tt-ordem-compra-auto.requisitante = item-contrat.cod-comprado
                   tt-ordem-compra-auto.it-codigo    = item-contrat.it-codigo
                   tt-ordem-compra-auto.num-pedido   = pedido-compr.num-pedido
                   tt-ordem-compra-auto.data-pedido  = pedido-compr.data-pedido
                   tt-ordem-compra-auto.cod-estabel  = pedido-compr.cod-estabel
                   tt-ordem-compra-auto.dep-almoxar  = item-contrat.cod-depos
                   tt-ordem-compra-auto.narrativa    = item-contrat.narrat-item
                   tt-ordem-compra-auto.sequencia    = item-contrat.num-seq-item
                   tt-ordem-compra-auto.tp-despesa   = item-contrat.tp-despesa
                   i-num-ord                         = i-numero-ordem / 100.
    
            &if defined(bf_mat_unidade_negocio) &then
                
                IF  l-unidade-negocio THEN DO:
                    EMPTY TEMP-TABLE tt-erro-aux.
        
                    IF  VALID-HANDLE(h-cdapi024) THEN DO:
        
                        RUN RetornaUnidadeNegocioExternaliz IN h-cdapi024 (INPUT pedido-compr.cod-estabel,
                                                                           INPUT item-contrat.it-codigo,
                                                                           INPUT item-contrat.cod-depos,
                                                                           OUTPUT c-cod-unid-negoc).
        
                        /*UN X Estabelecimento*/
                        RUN ValidaUnidadeNegocioEstabel IN h-cdapi024 (INPUT pedido-compr.cod-estabel,
                                                                       INPUT tt-ordem-compra-auto.data-emissao,
                                                                       INPUT c-cod-unid-negoc, 
                                                                       OUTPUT TABLE tt-erro-aux).
        
                        IF  contrato-for.ind-control-rec  = 1 THEN
                            ASSIGN c-cod-compr-un = contrato-for.cod-comprado.
                        ELSE
                            ASSIGN c-cod-compr-un = item-contrat.cod-comprado.
        
                        /*UN X Usu†rio*/
                        RUN ValidaUnidadeNegocioUsuario IN h-cdapi024 (INPUT c-cod-compr-un,
                                                                       INPUT c-cod-unid-negoc, 
                                                                       OUTPUT TABLE tt-erro-aux APPEND).
                    end.
                    
                    IF CAN-FIND(FIRST tt-erro-aux) THEN DO:
                        
                        CREATE tt-problema-un-negoc.

                        FOR EACH tt-erro-aux:
                            ASSIGN tt-problema-un-negoc.c-desc-prob = tt-problema-un-negoc.c-desc-prob + tt-erro-aux.mensagem.
                        END.
                        
                        RUN pi-gera-log-imp (INPUT pedido-compr.nr-contrato,
                                             INPUT 99). /* codigo erro */
                        RETURN "NOK":U.
                    END.
                    ASSIGN tt-ordem-compra-auto.cod-unid-negoc = c-cod-unid-negoc.
                END.
            &endif
    
            IF  contrato-for.natureza = 1 OR
                contrato-for.natureza = 3 THEN
            
                if param-contrat.int-1 = 1 THEN DO:
                  /* Determina Aliquota ICMS */
                   assign de-preco-dif = 0.
        
                   find emitente
                        where emitente.cod-emitente = tt-ordem-compra-auto.cod-emitente no-lock no-error.
                   find first estabelec
                        where estabelec.cod-estabel = tt-ordem-compra-auto.cod-estabel no-lock no-error.
                   find first natur-oper
                        where natur-oper.nat-operacao = emitente.nat-operacao no-lock no-error.
                   run cdp/cd4301.p ( (if avail emitente then 
                                       emitente.contrib-icms
                                    else
                                        yes),
                                    (if  avail emitente then 
                                         emitente.natureza
                                     else 2 /*"J"*/),
                                     emitente.estado,
                                     emitente.pais,
                                     estabelec.estado,
                                     tt-ordem-compra-auto.it-codigo,
                                     if avail natur-oper then 
                                        natur-oper.aliquota-icm 
                                        else 0,
                                     output de-aliq-icm ).    
        
                   if de-aliq-icm <> ? then do:
                      if de-aliq-icm <> 0  then
                         assign de-preco-dif = ( item-contrat.preco-fornec 
                                             *  (1 - (tt-ordem-compra-auto.aliquota-icm / 100)))
                                             /  (1 - (de-aliq-icm / 100)).
                      else
                         assign de-preco-dif = item-contrat.preco-fornec 
                                             *  (1 - (tt-ordem-compra-auto.aliquota-icm / 100)).
    
                      assign cotacao-item.aliquota-icm = de-aliq-icm
                             tt-ordem-compra-auto.aliquota-icm = de-aliq-icm.
                      
                      if de-preco-dif <> ?
                      or de-preco-dif <> 0 then do:
                         assign cotacao-item.preco-fornec = de-preco-dif.
                         {cdp/cd9300.i "cotacao-item"}
                         assign tt-ordem-compra-auto.preco-fornec = cotacao-item.preco-fornec
                                tt-ordem-compra-auto.pre-unit-for = cotacao-item.pre-unit-for
                                tt-ordem-compra-auto.preco-unit   = cotacao-item.pre-unit-for * de-indice
                                tt-ordem-compra-auto.preco-unit   = cotacao-item.pre-unit-for * de-indice
                                tt-ordem-compra-auto.preco-orig   = cotacao-item.preco-unit.
        
                      end.                            
                   end.
                END.
        end.
    
        /* Investimentos Multi-empresa */
        find first param-global no-lock no-error.
    
        find estabelec where estabelec.cod-estabel = tt-ordem-compra-auto.cod-estabel no-lock no-error.
        if  avail tt-ordem-compra-auto and param-global.modulo-in then do:
            if  avail estabelec then
                assign tt-ordem-compra-auto.ep-codigo = estabelec.ep-codigo.
        end.
    
        /* Inclus∆o de Ordens de Compra ao Contrato */
        assign tt-ordem-compra-auto.nr-contrato  = contrato-for.nr-contrato
               cotacao-item.numero-ordem = tt-ordem-compra-auto.numero-ordem.
        if  contrato-for.ind-control-rec = 1 then
            assign tt-ordem-compra-auto.num-seq-item = 0.
        else
            assign tt-ordem-compra-auto.num-seq-item = item-contrat.num-seq-item.
    
        assign tt-ordem-compra-auto.it-codigo   = (if contrato-for.ind-control-rec = 1
                                              then param-contrat.it-codigo
                                              else item-contrat.it-codigo)
               tt-ordem-compra-auto.num-pedido  = tt-ordem-compra-auto.num-pedido
               tt-ordem-compra-auto.cod-estabel = tt-ordem-compra-auto.cod-estabel
               tt-ordem-compra-auto.sit-ordem-contrat = 2
               tt-ordem-compra-auto.natureza = contrato-for.natureza.
    
        if  item.tipo-contr = 1 or 
            item.tipo-contr = 4 then DO:
            if  tt-ordem-compra-auto.ct-codigo = '' AND
                item-contrat.ind-tipo-control = 3 /* programaÁ„o */ then
                assign tt-ordem-compra-auto.ct-codigo = item.ct-codigo
    			       tt-ordem-compra-auto.sc-codigo = item.sc-codigo.
    
            if  tt-ordem-compra-auto.ct-codigo = '' AND
                item-contrat.ind-tipo-control = 1 /* mediá∆o */ AND 
                ((NOT param-contrat.log-rat-medi AND contrato-for.ind-control-rec = 2) OR
                  NOT param-contrat.log-rat-contrat-tot AND contrato-for.ind-control-rec = 1) then
                assign tt-ordem-compra-auto.ct-codigo = item.ct-codigo
    			       tt-ordem-compra-auto.sc-codigo = item.sc-codigo.
        END.
    
        /*Validacao Restricoes Unidade Negocio*/
        &if '{&bf_mat_versao_ems}' >= '2.062' &then
            if  l-unidade-negocio 
            /*and l-valida-online */
            and tt-ordem-compra-auto.ct-codigo <> "" then do:
    
                /** A partir da unificaá∆o de conceitos n∆o Ç mais chamada a procedure ValidaRestricoesUnidadeNegocio.**/
                empty temp-table tt_log_erro.
    
                run prgint/utb/utb743za.py persistent set h_api_cta_ctbl.
                RUN pi_valida_conta_contabil IN h_api_cta_ctbl (INPUT  estabelec.ep-codigo,         /* EMPRESA EMS2 */
                                                                INPUT  tt-ordem-compra-auto.cod-estabel,    /* ESTABELECIMENTO EMS2 */
                                                                INPUT  tt-ordem-compra-auto.cod-unid-negoc, /* UNIDADE NEGOCIO */
                                                                INPUT  "",                          /* PLANO CONTAS */ 
                                                                INPUT  tt-ordem-compra-auto.ct-codigo,      /* CONTA */
                                                                INPUT  "",                          /* PLANO CCUSTO */ 
                                                                INPUT  tt-ordem-compra-auto.sc-codigo,      /* CCUSTO */
                                                                INPUT  tt-ordem-compra-auto.data-emissao,   /* DATA TRANSACAO */
                                                                OUTPUT TABLE tt_log_erro).          /* ERROS */
                delete object h_api_cta_ctbl.
    
                if  can-find(first tt_log_erro) then do:

                    CREATE tt-problema-un-negoc.

                    FOR EACH tt_log_erro:
                        ASSIGN tt-problema-un-negoc.c-desc-prob = tt-problema-un-negoc.c-desc-prob + tt_log_erro.ttv_des_msg_erro.
                    END.
                    
                    RUN pi-gera-log-imp (INPUT pedido-compr.nr-contrato,
                                         INPUT 99). /* codigo erro */
                    RETURN "NOK":U.
                end.
    
            end.
        &endif
        
        &if defined(bf_mat_contratos) &then
            assign tt-ordem-compra-auto.cod-estab-gestor = contrato-for.cod-estab-gestor.
        &endif
        
        
        create ordem-compra.
        buffer-copy tt-ordem-compra-auto to ordem-compra.
        find first ordem-compra where ordem-compra.numero-ordem = tt-ordem-compra-auto.numero-ordem no-lock no-error.
        
        for each tt-ordem-compra-auto exclusive-lock:
            delete tt-ordem-compra-auto.
        end.

        IF AVAIL ordem-compra THEN DO:
            create prazo-compra.
            assign prazo-compra.numero-ordem = ordem-compra.numero-ordem
                   prazo-compra.parcela      = 1
                   prazo-compra.it-codigo    = ordem-compra.it-codigo
                   prazo-compra.natureza     = ordem-compra.natureza
                   prazo-compra.situacao     = 2
                   prazo-compra.data-alter   = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
                   prazo-compra.data-orig    = today
                   prazo-compra.data-entrega = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE TODAY &ENDIF
                   prazo-compra.un           = item.un
                   prazo-compra.nr-contrato  = ordem-compra.nr-contrato.
        
            release ordem-compra.
        END.
    end.
    
    if  valid-handle(h-cdapi024) then do:
        delete procedure h-cdapi024.
        assign h-cdapi024 = ?.
    end.

    RETURN "OK":U.
    
END PROCEDURE.



PROCEDURE pi-gera-aditivo:
   DEFINE VARIABLE i-cont           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE i-cont-alt       AS INTEGER   NO-UNDO.

   ASSIGN i-cont     = 1
          i-cont-alt = 1.

   blk:
   FOR EACH bf-hist-alter
            WHERE bf-hist-alter.nr-contrato   = item-contrat.nr-contrato   AND
                  bf-hist-alter.num-seq-item  = item-contrat.num-seq-item  
            BY bf-hist-alter.num-seq-alter DESCENDING:
      ASSIGN i-cont = bf-hist-alter.num-seq-alter + 1.
      LEAVE blk.
   END.

   blk1:
   FOR EACH bf-hist-alter
            WHERE bf-hist-alter.nr-contrato   = item-contrat.nr-contrato   AND
                  bf-hist-alter.num-seq-item  = item-contrat.num-seq-item  
            BY bf-hist-alter.nr-alter DESCENDING:
      ASSIGN i-cont-alt = bf-hist-alter.nr-alter + 1.
      LEAVE blk1.
   END.


   FIND FIRST bf-contrato-for 
        WHERE bf-contrato-for.nr-contrato = item-contrat.nr-contrato
        NO-LOCK NO-ERROR.
   IF AVAILABLE(bf-contrato-for) THEN DO:
      CREATE hist-alter.
      ASSIGN hist-alter.num-seq-adit        = 0       
             hist-alter.usuario             = c-seg-usuario       
             hist-alter.cod-emitente        = bf-contrato-for.cod-emitente       
             hist-alter.nr-contrato         = bf-contrato-for.nr-contrato       
             hist-alter.hra-alter           = STRING(TIME,"hh:mm:ss")       
             hist-alter.dat-alter           = TODAY       
             hist-alter.des-motivo-alter    = "Inclus∆o de Item - ESCN0206"      
             hist-alter.it-codigo           = item-contrat.it-codigo
             hist-alter.alter-destino       = "2"       
             hist-alter.num-seq-item        = item-contrat.num-seq-item       
             hist-alter.nr-alter            = i-cont-alt        
             hist-alter.num-seq-anexo       = 0       
             hist-alter.num-seq-clausula    = 0       
             hist-alter.ind-dest-event      = 1       
             hist-alter.log-reaj-formula    = NO       
             hist-alter.log-reaj-pend       = NO
             hist-alter.num-seq-alter       = i-cont       
             hist-alter.numero-ordem        = 0       
             hist-alter.num-seq-event       = 0       
             hist-alter.num-seq-medicao     = 0
             hist-alter.des-tipo-alter      = "item-contrat.ind-sit-item" 
             hist-alter.alter-origem        = "1".
   END.
END PROCEDURE.
