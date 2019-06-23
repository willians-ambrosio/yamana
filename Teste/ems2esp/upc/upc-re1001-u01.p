/**============================================================**
** Alteraá∆o...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar/Trocar Natureza de Operaá∆o
** ............:  
**=============================================================**/
{include/i-prgvrs.i UPC-RE1001-U01 11.5.11.001}
{utp/ut-glob.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cdcfgcex.i}

/** ParÉmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* Bisneto */
/*--------------------------------------------------------------------------------------*/
DEFINE NEW GLOBAL SHARED VARIABLE c-serie-docto     LIKE docum-est.serie-docto    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-nro-docto       LIKE docum-est.nro-docto      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cod-emitente    LIKE docum-est.cod-emitente   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-nat-operacao    LIKE docum-est.nat-operacao   NO-UNDO.
DEF VAR                           c-tipo            AS CHAR                       NO-UNDO.
DEF VAR                           c-embarque        LIKE embarque-imp.embarque    NO-UNDO.
DEF VAR                           c-est-embarque    LIKE embarque-imp.cod-estabel NO-UNDO.
/*--------------------------------------------------------------------------------------*/

/** Local **/
DEFINE BUFFER bf-docum-est       FOR docum-est.
DEFINE BUFFER cte-docum-est      FOR docum-est.
DEFINE BUFFER bdocum-est         FOR docum-est.

DEFINE BUFFER bf-item-doc-est     FOR item-doc-est.
DEFINE BUFFER natur-item-doc-est  FOR item-doc-est.
DEFINE BUFFER bitem-doc-est       FOR item-doc-est.
DEFINE BUFFER rateio-item-doc-est FOR item-doc-est.
DEFINE BUFFER cte-item-doc-est    FOR item-doc-est.

DEFINE BUFFER b-cfop-rat-docum    FOR rat-docum.

DEFINE VARIABLE l-natur-nota-propria AS LOGICAL     NO-UNDO.

DEFINE BUFFER brat-docum         FOR rat-docum.
DEFINE BUFFER bdupli-apagar      FOR dupli-apagar.
DEFINE BUFFER bdupli-imp         FOR dupli-imp.

/*Begins REV01 */
DEFINE BUFFER bdocum-estImpto    FOR docum-est.
DEFINE BUFFER bdupli-apagarImpto FOR dupli-apagar.
DEFINE BUFFER bdupli-impImpto    FOR dupli-imp.
/* End REV01 */

DEFINE BUFFER bnatur-oper        FOR natur-oper.
DEFINE BUFFER rateio-natur-oper  FOR natur-oper.

DEFINE BUFFER bemitente          FOR emitente.
DEFINE BUFFER bestabelec         FOR estabelec.
DEFINE BUFFER bunid-feder        FOR ems2cadme.unid-feder.
DEFINE BUFFER beunid-feder       FOR ems2cadme.unid-feder.

DEFINE BUFFER brat-ordem         FOR rat-ordem.

DEFINE BUFFER b-del-es-item-doc-est-natoper FOR es-item-doc-est-natoper.

DEFINE VARIABLE natur-aux-anterior AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hbtConf         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtConf_esp     AS HANDLE      NO-UNDO.


DEFINE VARIABLE hbtDelete         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtDelete_esp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE hbtRateio         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtRateio_esp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE l-retorno AS LOGICAL     NO-UNDO.

DEFINE VARIABLE i-qtd-item AS INTEGER     NO-UNDO.

DEFINE VARIABLE d-base AS DECIMAL.

DEFINE VARIABLE hm_Atualiza     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcod-emitente   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hserie-docto    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hnro-docto      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hnat-operacao   AS HANDLE      NO-UNDO.

DEFINE VARIABLE hRe1001       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hRe1001a1     AS HANDLE      NO-UNDO.
DEFINE VARIABLE rw-docum-est  AS ROWID       NO-UNDO.
DEFINE VARIABLE hBoin090      AS HANDLE      NO-UNDO.
DEFINE VARIABLE ca_nat-oper   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ldif-nat-oper AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-msg-nat     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lerro         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE p-cfop        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-model       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-crt         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-csosn       AS INTEGER     NO-UNDO.
DEFINE VARIABLE d-pcredsn     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-vcredicmssn AS DECIMAL     NO-UNDO.
DEFINE VARIABLE p-crt         AS INTEGER     NO-UNDO.
DEFINE VARIABLE d-ICMS        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-ipi         AS DECIMAL     NO-UNDO.

DEFINE VARIABLE i-cont         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont-de-para AS INTEGER     NO-UNDO.

DEFINE VARIABLE p-rateio AS LOGICAL     NO-UNDO.

DEFINE VARIABLE d-aliq-icm AS DECIMAL     NO-UNDO INITIAL 0.

DEFINE VARIABLE d-icms-BC     LIKE nfe-it-nota-fisc-rec.imp-vBC.     
DEFINE VARIABLE d-Vicms       LIKE nfe-it-nota-fisc-rec.imp-vICMS.   
DEFINE VARIABLE d-VIPI        LIKE nfe-it-nota-fisc-rec.imp-ipi-vIPI.
DEFINE VARIABLE d-ipi-bc      LIKE nfe-it-nota-fisc-rec.imp-ipi-vBC. 
DEFINE VARIABLE d-RedBc       LIKE nfe-it-nota-fisc-rec.imp-pRedBC.
DEFINE VARIABLE imp-pICMSST   LIKE nfe-it-nota-fisc-rec.imp-pICMSST.


DEFINE VARIABLE ca-oper       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-primeira    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-semaliquota AS LOGICAL     NO-UNDO.

DEFINE VARIABLE l-autom-retenc AS LOGICAL    NO-UNDO.
DEFINE VARIABLE i-cst-icms     AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-cst-piscof   AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-sequencia-item AS INTEGER     NO-UNDO.
DEFINE VARIABLE seq-ant          AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-sequencia-aux  AS INTEGER     NO-UNDO.

DEFINE VARIABLE l-ra           AS LOGICAL     NO-UNDO.

DEFINE VARIABLE hshowmsg AS HANDLE      NO-UNDO.
/* Begins REV003  */
DEFINE VARIABLE h-bodi515  AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-bodi516  AS HANDLE      NO-UNDO.
/* End REV003 */
/* Beins Rev004 */
DEFINE VARIABLE h_boin176 AS HANDLE      NO-UNDO.
DEFINE VARIABLE h_boin090 AS HANDLE      NO-UNDO.
/* End REV004 */


DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/* Temp-table de regras */

DEFINE TEMP-TABLE tt-natoper NO-UNDO
    FIELD nat-operacao  AS CHARACTER
    FIELD l-achou       AS LOGICAL INITIAL YES
    FIELD regras        AS INTEGER
    FIELD motivo        AS CHARACTER FORMAT "x(20)"
    INDEX atende IS PRIMARY l-achou regras DESC.

DEFINE TEMP-TABLE tt-itens NO-UNDO 
    LIKE es-item-doc-est-natoper
	FIELD nat-operacao2	LIKE es-item-doc-est-natoper.nat-operacao
    FIELD l-buscaicms   AS LOGICAL INITIAL TRUE
    FIELD ALIquota-ipi AS DEC
    FIELD d-icms-BC    LIKE nfe-it-nota-fisc-rec.imp-vBC     
    FIELD d-Vicms      LIKE nfe-it-nota-fisc-rec.imp-vICMS   
    FIELD d-VIPI       LIKE nfe-it-nota-fisc-rec.imp-ipi-vIPI
    FIELD d-ipi-bc     LIKE nfe-it-nota-fisc-rec.imp-ipi-vBC
    FIELD d-RedBc      LIKE nfe-it-nota-fisc-rec.imp-pRedBC
    FIELD log-2        AS LOG
    FIELD imp-vBCST    LIKE nfe-it-nota-fisc-rec.imp-vBCST  
    FIELD imp-vICMSST  LIKE nfe-it-nota-fisc-rec.imp-vICMSST
    FIELD d-aliq-icm   AS DEC.

/* Begins Rev004 - 31/08/2018 - Willians Ambrosio / Grupo DKP */
def temp-table tt-total-item NO-UNDO
    field tot-peso            as decimal format ">>>>,>>>,>>9.9999"
    field peso-bruto-tot      as decimal format ">>>,>>>,>>9.999"
    field tot-desconto        as decimal format ">>>>>,>>>,>>9.99"
    field despesa-nota        as decimal format ">>>>>,>>>,>>9.99"
    field valor-mercad        as decimal format ">>>>>,>>>,>>9.99"
    field base-ipi            as decimal format ">>>>>,>>>,>>9.99"
    field valor-ipi           as decimal format ">>>>>,>>>,>>9.99"
    field base-icm            as decimal format ">>>>>,>>>,>>9.99"
    field valor-icm           as decimal format ">>>>>,>>>,>>9.99"
    field base-iss            as decimal format ">>>>>,>>>,>>9.99"
    field valor-iss           as decimal format ">>>>>,>>>,>>9.99"
    field base-subs           as decimal format ">>>>>,>>>,>>9.99"
    field valor-subs          as decimal format ">>>>>,>>>,>>9.99"
    field base-icm-complem    as decimal format ">>>>>,>>>,>>9.99"
    field icm-complem         as decimal format ">>>>>,>>>,>>9.99"
    field fundo-pobreza       as decimal format ">>>>>,>>>,>>9.99"
    field ipi-outras          as decimal format ">>>>>,>>>,>>9.99"
    field valor-pis           as decimal format ">>>>>,>>>,>>9.99"
    field valor-cofins        as decimal format ">>>>>,>>>,>>9.99"
    field total-pis-subst     as decimal format ">>>>>,>>>,>>9.99"
    field total-cofins-subs   as decimal format ">>>>>,>>>,>>9.99"
    field total-icms-diferim  as decimal format ">>>>>,>>>,>>9.99"
    field valor-frete         as decimal format ">>>>>,>>>,>>9.99"
    field valor-pedagio       as decimal format ">>>>>,>>>,>>9.99"
    field valor-icm-trib      as decimal format ">>>>>,>>>,>>9.99"
    field de-tot-valor-calc   as decimal format ">>>>,>>>,>>>,>>9.99".
/* End Rev004 - 31/08/2018 - Willians Ambrosio / Grupo DKP */

/* Definiá‰es para Recriar Documento */
DEFINE VARIABLE i-erro AS INTEGER     NO-UNDO.
def temp-table tt-docum-est no-undo like docum-est
    field r-rowid as rowid.
def temp-table tt-rat-docum no-undo like rat-docum
    field r-rowid as rowid.
def temp-table tt-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.
def temp-table tt-dupli-apagar no-undo like dupli-apagar
    field r-rowid as rowid.
def temp-table tt-dupli-imp no-undo like dupli-imp
    field r-rowid as rowid.
def temp-table tt-rat-ordem no-undo like rat-ordem
    field r-rowid as rowid.

def temp-table tt-erro no-undo
    field identif-segment as char
    field cd-erro         as integer
    field desc-erro       as char format "x(80)".

DEF TEMP-TABLE tt-mensagem-erro NO-UNDO
    FIELD i-sequen              AS   INTE
    FIELD cd-erro               AS   INTE
    FIELD mensagem              AS   CHAR FORMAT "x(255)".

DEFINE TEMP-TABLE tt-docum-est2    NO-UNDO LIKE tt-docum-est.
DEFINE TEMP-TABLE tt-rat-docum2    NO-UNDO LIKE tt-rat-docum.

DEF TEMP-TABLE btt-item-doc-est LIKE tt-item-doc-est.

/** Global **/
DEFINE NEW GLOBAL SHARED VAR grDocumEst        AS ROWID           NO-UNDO.

/*                                                            */
/* /* /**** Main Block ****/                                     */ */
/* message "p-ind-event..:" p-ind-event                  skip       */
/*         "p-ind-object.:" p-ind-object                 skip       */
/*         "p-cod-table..:" STRING(p-cod-table)          skip       */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip       */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip       */
/*         "p-row-table..:" string(p-row-table)          skip       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */


IF p-ind-event  = "AFTER-INITIALIZE" AND
   p-ind-object = "CONTAINER"        AND
   p-cod-table  = "docum-est"        THEN DO:


    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

       IF VALID-HANDLE(hbtDelete) AND NOT VALID-HANDLE(hbtDelete_esp)  THEN DO:
        CREATE BUTTON hbtDelete_esp
            ASSIGN  ROW         = hbtDelete:ROW  
                    COLUMN      = hbtDelete:COLUMN - 4
                    WIDTH       = hbtDelete:WIDTH
                    HEIGHT      = hbtDelete:HEIGHT
                    LABEL       = hbtDelete:LABEL
                    FRAME       = hbtDelete:FRAME
                    FLAT-BUTTON = hbtDelete:FLAT-BUTTON
                    TOOLTIP     = "*" + hbtDelete:TOOLTIP
                    HELP        = hbtDelete:HELP
                    NAME        = "btDelete_esp"
                    SENSITIVE   = hbtDelete:SENSITIVE  
                    VISIBLE     = hbtDelete:VISIBLE
                    CONVERT-3D-COLOR = hbtDelete:convert-3D-COLOR
    /*                 NO-FOCUS    = hbtConf:NO-FOCUS */
                    .
        ON "CHOOSE" OF hbtDelete_esp PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btDelete_esp",
                                                                         INPUT p-ind-object,
                                                                         INPUT p-wgh-object,
                                                                         INPUT p-wgh-frame,
                                                                         INPUT p-cod-table,
                                                                         INPUT p-row-table).

        ON "CTRL-ENTER" OF p-wgh-frame PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btDelete_esp",
                                                                        INPUT p-ind-object,
                                                                        INPUT p-wgh-object,
                                                                        INPUT p-wgh-frame,
                                                                        INPUT p-cod-table,
                                                                        INPUT p-row-table).
        hbtDelete_esp:LOAD-IMAGE-UP(hbtDelete:IMAGE-UP).
        hbtDelete_esp:LOAD-IMAGE-INSENSITIVE(hbtDelete:IMAGE-INSENSITIVE).
        hbtDelete_esp:MOVE-TO-TOP().
/*         hbtConf_esp:MOVE-AFTER-TAB-ITEM(hbtConf). */
        ASSIGN hbtDelete:TAB-STOP  = NO
               hbtDelete:SENSITIVE = NO
               .

        /*Tratar Menu*/
        RUN pi-LocalizaCampo(INPUT p-wgh-frame:WINDOW:MENUBAR).
        ON "CHOOSE" OF hm_atualiza PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btDelete_esp",
                                                                       INPUT p-ind-object,
                                                                       INPUT p-wgh-object,
                                                                       INPUT p-wgh-frame,
                                                                       INPUT p-cod-table,
                                                                       INPUT p-row-table).

    END.

    /* Cria bot∆o especifico para validar */
    IF VALID-HANDLE(hbtConf) AND NOT VALID-HANDLE(hbtConf_esp)  THEN DO:
        CREATE BUTTON hbtConf_esp
            ASSIGN  ROW         = hbtConf:ROW  
                    COLUMN      = hbtConf:COLUMN
                    WIDTH       = hbtConf:WIDTH
                    HEIGHT      = hbtConf:HEIGHT
                    LABEL       = hbtConf:LABEL
                    FRAME       = hbtConf:FRAME
                    FLAT-BUTTON = hbtConf:FLAT-BUTTON
                    TOOLTIP     = "*" + hbtConf:TOOLTIP
                    HELP        = hbtConf:HELP
                    NAME        = "btConf_esp"
                    SENSITIVE   = hbtConf:SENSITIVE  
                    VISIBLE     = hbtConf:VISIBLE
                    CONVERT-3D-COLOR = hbtConf:convert-3D-COLOR
/*                     NO-FOCUS    = hbtConf:NO-FOCUS */
                    .
        ON "CHOOSE" OF hbtConf_esp PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btConf_esp",
                                                                       INPUT p-ind-object,
                                                                       INPUT p-wgh-object,
                                                                       INPUT p-wgh-frame,
                                                                       INPUT p-cod-table,
                                                                       INPUT p-row-table).

        ON "CTRL-ENTER" OF p-wgh-frame PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btConf_esp",
                                                                        INPUT p-ind-object,
                                                                        INPUT p-wgh-object,
                                                                        INPUT p-wgh-frame,
                                                                        INPUT p-cod-table,
                                                                        INPUT p-row-table).
        hbtConf_esp:LOAD-IMAGE-UP(hbtconf:IMAGE-UP).
        hbtConf_esp:LOAD-IMAGE-INSENSITIVE(hbtconf:IMAGE-INSENSITIVE).
        hbtConf_esp:MOVE-TO-TOP().
/*         hbtConf_esp:MOVE-AFTER-TAB-ITEM(hbtConf). */
        ASSIGN hbtConf:TAB-STOP  = NO
               hbtConf:SENSITIVE = NO
               .

        /*Tratar Menu*/
        RUN pi-LocalizaCampo(INPUT p-wgh-frame:WINDOW:MENUBAR).
        ON "CHOOSE" OF hm_atualiza PERSISTENT RUN upc/upc-re1001-u01.p(INPUT "UPCRE1001-btConf_esp",
                                                                       INPUT p-ind-object,
                                                                       INPUT p-wgh-object,
                                                                       INPUT p-wgh-frame,
                                                                       INPUT p-cod-table,
                                                                       INPUT p-row-table).

    END.
END. /* p-ind-event  = "BEFORE-INITIALIZE" */

IF p-ind-event = "UPCRE1001-btDelete_esp" THEN 
DO:
    /* Identificar os campos */
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    ASSIGN hRe1001 = p-wgh-frame:WINDOW:INSTANTIATING-PROCEDURE.
    /* Pegar handle do BOIN090 (DOCUM-EST) */
    RUN getDBOParentHandle IN hRe1001 (OUTPUT hBoin090).

 deleta: DO TRANSACTION ON ERROR UNDO deleta, RETURN "NOK":
    FIND FIRST b-del-es-item-doc-est-natoper 
        WHERE b-del-es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
          AND b-del-es-item-doc-est-natoper.serie-docto  = hserie-docto:SCREEN-VALUE
          AND b-del-es-item-doc-est-natoper.nro-docto    = hnro-docto:SCREEN-VALUE
          AND b-del-es-item-doc-est-natoper.cod-emitente = INTEGER(hcod-emitente:SCREEN-VALUE)  
          AND b-del-es-item-doc-est-natoper.nat-operacao = hnat-operacao:SCREEN-VALUE NO-ERROR.
    IF AVAIL(b-del-es-item-doc-est-natoper) THEN DO:
        DELETE b-del-es-item-doc-est-natoper.
    END.
    ELSE
        UNDO deleta, LEAVE deleta.


 END.

 APPLY "CHOOSE" TO hbtDelete.
 RETURN RETURN-VALUE.

END.

IF p-ind-event = "UPCRE1001-btConf_esp" THEN 
DO:
    /* Identificar os campos */
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    /* Pegar handle do programa */
    ASSIGN hRe1001 = p-wgh-frame:WINDOW:INSTANTIATING-PROCEDURE.
    /* Pegar handle do BOIN090 (DOCUM-EST) */
    RUN getDBOParentHandle IN hRe1001 (OUTPUT hBoin090).

    /* Verificar se j† foi substitu°do a natureza de operaá∆o ou 
       Ç natureza de exceá∆o */
    FOR FIRST bnatur-oper
        WHERE bnatur-oper.nat-operacao = hnat-operacao:SCREEN-VALUE NO-LOCK: END.
    IF NOT AVAIL bnatur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o~~" + QUOTER(hnat-operacao:SCREEN-VALUE)).
        RETURN "NOK".
    END.
    FIND FIRST es-natoper-rec 
        WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL es-natoper-rec THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o recebimento(placebo)~~" + "Empresa: " + QUOTER(i-ep-codigo-usuario)).
        RETURN "NOK".
    END.

    /* Bisneto */
    ASSIGN
      c-serie-docto  = hserie-docto:SCREEN-VALUE              
      c-nro-docto    = hnro-docto:SCREEN-VALUE                
      i-cod-emitente = INTEGER(hcod-emitente:SCREEN-VALUE)    
      c-nat-operacao = hnat-operacao:SCREEN-VALUE. 

    /* fim bisneto */
    /* Verifica se a nota Ç  CTE */     
    FIND FIRST cte-docum-est NO-LOCK
         WHERE cte-docum-est.serie-docto  = hserie-docto:SCREEN-VALUE          
           AND cte-docum-est.nro-docto    = hnro-docto:SCREEN-VALUE            
           AND cte-docum-est.cod-emitente = INTEGER(hcod-emitente:SCREEN-VALUE)
           AND cte-docum-est.nat-operacao = hnat-operacao:SCREEN-VALUE NO-ERROR. 
    IF AVAIL(cte-docum-est) THEN DO:
        
        FIND FIRST docto-orig-cte NO-LOCK      
             WHERE docto-orig-cte.serie-docto  = cte-docum-est.serie-docto
               AND docto-orig-cte.nro-docto    = cte-docum-est.nro-docto
               AND docto-orig-cte.cod-emitente = cte-docum-est.cod-emitente
               AND docto-orig-cte.nat-operacao = cte-docum-est.nat-operacao NO-ERROR.
        IF AVAIL(docto-orig-cte) THEN DO: /* Verifica se a nota Ç de origem do NEOGRID*/

            FOR EACH cte-item-doc-est OF cte-docum-est NO-LOCK:

                FIND FIRST es-item-doc-est-natoper
                     WHERE es-item-doc-est-natoper.ep-codigo         = i-ep-codigo-usuario
                       AND es-item-doc-est-natoper.serie-docto       = cte-item-doc-est.serie-docto
                       AND es-item-doc-est-natoper.nro-docto         = cte-item-doc-est.nro-docto
                       AND es-item-doc-est-natoper.cod-emitente      = cte-item-doc-est.cod-emitente
                       AND es-item-doc-est-natoper.nat-operacao      = cte-item-doc-est.nat-operacao
                       AND es-item-doc-est-natoper.sequencia         = cte-item-doc-est.sequencia 
                       /*AND es-item-doc-est-natoper.nat-operacao-orig = ""*/ NO-ERROR.
                IF NOT AVAIL(es-item-doc-est-natoper) THEN DO:
                    CREATE es-item-doc-est-natoper.
                    ASSIGN es-item-doc-est-natoper.ep-codigo          = i-ep-codigo-usuario       
                           es-item-doc-est-natoper.serie-docto        = cte-item-doc-est.serie-docto 
                           es-item-doc-est-natoper.nro-docto          = cte-item-doc-est.nro-docto   
                           es-item-doc-est-natoper.cod-emitente       = cte-item-doc-est.cod-emitente
                           es-item-doc-est-natoper.sequencia          = cte-item-doc-est.sequencia
                           es-item-doc-est-natoper.it-codigo          = cte-item-doc-est.it-codigo
                           es-item-doc-est-natoper.cod-cfop-saida     = docto-orig-cte.cod-cfop
                           es-item-doc-est-natoper.nat-operacao       = cte-item-doc-est.nat-operacao 
                           /*es-item-doc-est-natoper.nat-operacao-orig  = cte-item-doc-est.nat-operacao*/ .
                            
                END.
            END.
        END.
        ELSE DO: /*Verifica se veio do RA */

            FIND FIRST nfe-cte-inf NO-LOCK
                 WHERE nfe-cte-inf.chave-acesso = cte-docum-est.cod-chave-aces-nf-eletro NO-ERROR.
            IF AVAIL(nfe-cte-inf) THEN 
            DO:


                FOR EACH cte-item-doc-est OF cte-docum-est NO-LOCK:

                    FIND FIRST es-item-doc-est-natoper
                         WHERE es-item-doc-est-natoper.ep-codigo         = i-ep-codigo-usuario
                           AND es-item-doc-est-natoper.serie-docto       = cte-item-doc-est.serie-docto
                           AND es-item-doc-est-natoper.nro-docto         = cte-item-doc-est.nro-docto
                           AND es-item-doc-est-natoper.cod-emitente      = cte-item-doc-est.cod-emitente
                           AND es-item-doc-est-natoper.nat-operacao      = cte-item-doc-est.nat-operacao
                           AND es-item-doc-est-natoper.sequencia         = cte-item-doc-est.sequencia 
                           /*AND es-item-doc-est-natoper.nat-operacao-orig = ""*/ NO-ERROR.
                    IF NOT AVAIL(es-item-doc-est-natoper) THEN DO:
                        CREATE es-item-doc-est-natoper.
                        ASSIGN es-item-doc-est-natoper.ep-codigo          = i-ep-codigo-usuario       
                               es-item-doc-est-natoper.serie-docto        = cte-item-doc-est.serie-docto 
                               es-item-doc-est-natoper.nro-docto          = cte-item-doc-est.nro-docto   
                               es-item-doc-est-natoper.cod-emitente       = cte-item-doc-est.cod-emitente
                               es-item-doc-est-natoper.sequencia          = cte-item-doc-est.sequencia
                               es-item-doc-est-natoper.it-codigo          = cte-item-doc-est.it-codigo
                               es-item-doc-est-natoper.cod-cfop-saida     = nfe-cte-inf.ide-CFOP
                               es-item-doc-est-natoper.nat-operacao       = cte-item-doc-est.nat-operacao
                               /*es-item-doc-est-natoper.nat-operacao-orig  = cte-item-doc-est.nat-operacao*/ .
                    END.
                END.

            END.

        END.
    END.
    /* Verifica se a nota Ç  CTE */

    /* Verificar se j† foi feito a Atribuiá∆o de Natureza conforme CFA */
    FIND FIRST es-item-doc-est-natoper
        WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
          AND es-item-doc-est-natoper.serie-docto  = hserie-docto:SCREEN-VALUE
          AND es-item-doc-est-natoper.nro-docto    = hnro-docto:SCREEN-VALUE
          AND es-item-doc-est-natoper.cod-emitente = INTEGER(hcod-emitente:SCREEN-VALUE)  
          AND es-item-doc-est-natoper.nat-operacao = hnat-operacao:SCREEN-VALUE NO-LOCK NO-ERROR.
    FIND FIRST es-param-re
        WHERE es-param-re.ep-codigo = i-ep-codigo-usuario
          AND es-param-re.usuario   = c-seg-usuario NO-LOCK NO-ERROR.

    FIND FIRST b-cfop-rat-docum
         WHERE b-cfop-rat-docum.serie-docto       = hserie-docto:SCREEN-VALUE      
           AND b-cfop-rat-docum.nro-docto         = hnro-docto:SCREEN-VALUE        
           AND b-cfop-rat-docum.cod-emitente      = INTEGER(hcod-emitente:SCREEN-VALUE)  
           AND b-cfop-rat-docum.nat-operacao      = hnat-operacao:SCREEN-VALUE NO-ERROR.
    IF AVAIL(b-cfop-rat-docum) THEN 
    DO: /* Incluir CFOP Origem */
        IF AVAIL(es-item-doc-est-natoper) THEN DO:
            IF es-item-doc-est-natoper.cod-cfop-saida = "" THEN DO:
                run utp/ut-msgs.p (input "show":U, 
                                   input 27702, 
                                   input "CFOP n∆o Cadastrado!~~" + 
                                         "Deseja cadastrar o CFOP De Origem?" ).
                IF LOGICAL(RETURN-VALUE) THEN DO:
    
                    RUN esp/ymre1001.w(INPUT  b-cfop-rat-docum.serie-docto,
                                       INPUT  b-cfop-rat-docum.nro-docto,
                                       INPUT  b-cfop-rat-docum.cod-emitente,
                                       INPUT  b-cfop-rat-docum.nat-operacao).
                    
                END.
            END.
        END.
        ELSE DO:
            
            run utp/ut-msgs.p (input "show":U, 
                               input 27702, 
                               input "CFOP n∆o Cadastrado!~~" + 
                                     "Deseja cadastrar o CFOP De Origem?" ).
            IF LOGICAL(RETURN-VALUE) THEN DO:

                RUN esp/ymre1001.w(INPUT b-cfop-rat-docum.serie-docto,
                                   INPUT b-cfop-rat-docum.nro-docto,
                                   INPUT b-cfop-rat-docum.cod-emitente,
                                   INPUT b-cfop-rat-docum.nat-operacao).
            
            END.
        END.
    END.

    IF bnatur-oper.int-1 = 1 THEN
        ASSIGN l-natur-nota-propria = TRUE.
    ELSE
        ASSIGN l-natur-nota-propria = FALSE.

    IF bnatur-oper.transf      = es-natoper-rec.exc-transf      OR
       bnatur-oper.especie-doc = es-natoper-rec.exc-especie-doc OR 
       bnatur-oper.mercado     = es-natoper-rec.exc-mercado     OR
       bnatur-oper.terceiros   = es-natoper-rec.l-op-terceiros  OR
       l-natur-nota-propria    = es-natoper-rec.l-nota-propria  OR
       (AVAIL es-item-doc-est-natoper AND
        es-item-doc-est-natoper.nat-operacao-orig <> "")        OR 
       (AVAIL es-param-re 
          AND es-param-re.rec-nat-exc = TRUE
          AND (es-natoper-rec.nat-operacao-est        <> hnat-operacao:SCREEN-VALUE
          AND  es-natoper-rec.nat-operacao-int        <> hnat-operacao:SCREEN-VALUE
          AND  es-natoper-rec.nat-operacao-rateio     <> hnat-operacao:SCREEN-VALUE
          AND  es-natoper-rec.nat-operacao-rateio-int <> hnat-operacao:SCREEN-VALUE)) THEN DO:

        /* Cria es-item-doc-est-natoper */
        
        FOR EACH bitem-doc-est NO-LOCK
           WHERE bitem-doc-est.serie-docto  = hserie-docto:SCREEN-VALUE
             AND bitem-doc-est.nro-docto    = hnro-docto:SCREEN-VALUE
             AND bitem-doc-est.cod-emitente = INTEGER(hcod-emitente:SCREEN-VALUE)
             AND bitem-doc-est.nat-operacao = hnat-operacao:SCREEN-VALUE:
                                            

            FIND FIRST es-item-doc-est-natoper
                 WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
                   AND es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto
                   AND es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto
                   AND es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
                   AND es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
                   AND es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia NO-ERROR.
            IF NOT AVAIL es-item-doc-est-natoper THEN DO:
                CREATE es-item-doc-est-natoper.
                ASSIGN es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario       
                       es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto 
                       es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto   
                       es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
                       es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
                       es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia
                       es-item-doc-est-natoper.it-codigo    = bitem-doc-est.it-codigo.
            END.

        END.

        /* Cria es-item-doc-est-natoper */
        
        /* Alteraá∆o para Re-Sequenciamento 23/07/2015 */        
        FOR EACH es-item-doc-est-natoper
           WHERE es-item-doc-est-natoper.ep-codigo         = i-ep-codigo-usuario    
             AND es-item-doc-est-natoper.serie-docto       = hserie-docto:SCREEN-VALUE
             AND es-item-doc-est-natoper.nro-docto         = hnro-docto:SCREEN-VALUE
             AND es-item-doc-est-natoper.cod-emitente      = INTEGER(hcod-emitente:SCREEN-VALUE)
             AND es-item-doc-est-natoper.nat-operacao      = hnat-operacao:SCREEN-VALUE
             /*AND es-item-doc-est-natoper.sequencia         = tt-itens.sequencia*/
             BREAK BY es-item-doc-est-natoper.nat-operacao:

            i-sequencia-item = i-sequencia-item + 10.

            ASSIGN es-item-doc-est-natoper.sequencia-it-doc-fisc = i-sequencia-item.

            IF LAST-OF(es-item-doc-est-natoper.nat-operacao) THEN 
                ASSIGN i-sequencia-item = 0.


        END.
        /* Alteraá∆o para Re-Sequenciamento 23/07/2015 */

        /* Bisneto */
        /*----------------------------------------------------------------*/
        FIND docum-est
          NO-LOCK
          WHERE docum-est.serie-docto  = c-serie-docto 
          AND   docum-est.nro-docto    = c-nro-docto   
          AND   docum-est.cod-emitente = i-cod-emitente
          AND   docum-est.nat-operacao = c-nat-operacao
          NO-ERROR.
        /*----------------------------------------------------------------*/
        /* Fim Bisneto */
        /* Continuar processo de atualizaá∆o padr∆o */
        APPLY "CHOOSE" TO hbtConf.
        /* Bisneto */
        /*-----------------------------------------------------------------*/
        /* Fim Bisneto */
        RETURN RETURN-VALUE.

    END.

    /* Caso n∆o confirmar */
    run utp/ut-msgs.p (input "show":U, 
                       input 27702, 
                       input "Confirma a Atribuiá∆o da Natureza de operaá∆o?~~" + 
                             "Documento ainda n∆o consistido. Deseja confirmar a Atribuiá∆o da Natureza de Operaá∆o?" ).
    IF NOT LOGICAL(RETURN-VALUE) THEN
        RETURN "NOK".

    /* Identificar documento */
    RUN getRowid IN hboin090 ( OUTPUT rw-docum-est ).

    FOR EACH bdocum-est WHERE 
             ROWID(bdocum-est) = rw-docum-est NO-LOCK:
        FOR EACH bitem-doc-est OF bdocum-est:
            RUN pi-atualiza-sd-medicao.
        END.
    END.

    ASSIGN lerro      = YES
           l-primeira = YES.

    
    Gravar: DO TRANSACTION ON ERROR UNDO Gravar, RETURN "NOK":

        /* Fase 01 - Verificar casos de diferencial de aliquota */
        EMPTY TEMP-TABLE tt-itens.
        FIND FIRST bdocum-est 
            WHERE ROWID(bdocum-est) = rw-docum-est NO-ERROR.
        IF NOT AVAIL bdocum-est THEN
            RETURN "NOK":U.
        

        ASSIGN i-qtd-item = 0.

        /* Validar mudanáa de natureza */        
        FOR EACH bitem-doc-est NO-LOCK
            OF bdocum-est:
                             
            ASSIGN i-qtd-item = i-qtd-item + 1.

            CREATE tt-itens.
            BUFFER-COPY bitem-doc-est EXCEPT nat-operacao TO tt-itens
                ASSIGN tt-itens.nat-operacao-orig = bitem-doc-est.nat-operacao.     

            ASSIGN d-icms        = 0
                   l-semaliquota = FALSE.

            FOR FIRST es-item-doc-est-natoper
                WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
                  AND es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto
                  AND es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto
                  AND es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
                  AND es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
                  AND es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia NO-LOCK:
                ASSIGN tt-itens.cod-cfop             = es-item-doc-est-natoper.cod-cfop
                       tt-itens.cod-model-nf-eletro  = es-item-doc-est-natoper.cod-model-nf-eletro
                       p-crt                         = es-item-doc-est-natoper.emit-crt
                       d-icms                        = es-item-doc-est-natoper.aliquota-icm
                       tt-itens.cod-beneficio-icms   = es-item-doc-est-natoper.cod-beneficio-icms
                       tt-itens.cod-beneficio-piscof = es-item-doc-est-natoper.cod-beneficio-piscof.
                IF d-icms > 0 THEN DO:
                    ASSIGN l-primeira            = NO
                           tt-itens.aliquota-icm = es-item-doc-est-natoper.aliquota-icm.
                END.
                    
                IF es-item-doc-est-natoper.aliquota-icm > 0 THEN
                    ASSIGN tt-itens.l-buscaicms = NO.
            END.

            /* Pegar CFOP do XML quando originado do RA */
            ASSIGN l-ra = FALSE.

            IF CONNECTED("RA") THEN DO:
                ASSIGN l-ra        = TRUE
                       imp-pICMSST = 0.

                FIND FIRST nfe-cte-inf NO-LOCK
                     WHERE nfe-cte-inf.chave-acesso = bdocum-est.cod-chave-aces-nf-eletro NO-ERROR.

                FIND FIRST nfe-relac-ordem-rec NO-LOCK
                     WHERE nfe-relac-ordem-rec.chave-acesso = bdocum-est.cod-chave-aces-nf-eletro
                       AND nfe-relac-ordem-rec.numero-ordem = bitem-doc-est.numero-ordem NO-ERROR.
                IF AVAIL(nfe-relac-ordem-rec) THEN DO:
                     IF nfe-relac-ordem-rec.seq-item <> seq-ant THEN
                        ASSIGN i-sequencia-aux = i-sequencia-aux + 1.

                     ASSIGN seq-ant = nfe-relac-ordem-rec.seq-item.
                END.
                ELSE DO:
                    ASSIGN i-sequencia-aux = i-sequencia-aux + 1.
                END.
                
                RUN upc/upc-re1001-u01ra.p (INPUT  bdocum-est.cod-chave-aces-nf-eletro,
                                            INPUT  bitem-doc-est.sequencia / 10,
                                            INPUT  i-sequencia-aux,
                                            INPUT  bitem-doc-est.numero-ordem,
                                            OUTPUT p-cfop,
                                            OUTPUT p-model,
                                            OUTPUT i-crt,        
                                            OUTPUT i-csosn,      
                                            OUTPUT d-pcredsn,    
                                            OUTPUT d-vcredicmssn,
                                            OUTPUT d-icms,
                                            OUTPUT d-ipi,
                                            OUTPUT i-cst-icms,
                                            OUTPUT i-cst-piscof,
                                            OUTPUT d-icms-BC,
                                            OUTPUT d-Vicms,  
                                            OUTPUT d-VIPI,   
                                            OUTPUT d-ipi-bc,
                                            OUTPUT d-RedBc,
                                            OUTPUT imp-pICMSST,
                                            OUTPUT imp-vBCST,
                                            OUTPUT imp-vICMSST ).

                IF imp-pICMSST <> 0 THEN DO:
                    ASSIGN tt-itens.log-2 = TRUE.
                END.
                ELSE 
                    ASSIGN tt-itens.log-2 = FALSE.

                IF p-cfop <> "" THEN
                    ASSIGN tt-itens.cod-cfop = p-cfop.

                IF p-model <> "" THEN
                    ASSIGN tt-itens.cod-model-nf-eletro = p-model.

                IF i-crt <> 0 THEN DO:   

                    ASSIGN tt-itens.emit-crt             = IF LOOKUP(STRING(i-cst-icms,"99"),es-natoper-rec.red-cst-icms,";") > 0 THEN 4 ELSE i-crt
                           tt-itens.imp-csosn            = i-csosn
                           tt-itens.imp-pcredsn          = d-pcredsn
                           tt-itens.imp-vcredicmssn      = d-vcredicmssn
                           p-crt                         = i-crt /* Trib Simples,1,Trib Normal,3,Red. ICMS,4 */
                           tt-itens.aliquota-icm         = d-icms
                           tt-itens.aliquota-ipi         = d-ipi

                           tt-itens.d-icms-BC            = d-icms-BC
                           tt-itens.d-Vicms              = d-Vicms  
                           tt-itens.d-VIPI               = d-VIPI   
                           tt-itens.d-ipi-bc             = d-ipi-bc 
                           tt-itens.d-RedBc              = d-RedBc
                                                         
                           tt-itens.l-buscaicms          = NO
                           tt-itens.imp-vBCST            = imp-vBCST    
                           tt-itens.imp-vICMSST          = imp-vICMSST  
                           l-primeira                    = NO.

                    IF LOOKUP(STRING(i-cst-icms,"99"),es-natoper-rec.benef-cst-icms,";") > 0 THEN
                        ASSIGN tt-itens.cod-beneficio-icms = es-natoper-rec.cod-benef-icms-rec.
                    ELSE
                        ASSIGN tt-itens.cod-beneficio-icms = 0.
                    IF LOOKUP(STRING(i-cst-piscof,"99"),es-natoper-rec.benef-cst-pis-cofins,";") > 0 THEN
                        ASSIGN tt-itens.cod-beneficio-piscof = es-natoper-rec.cod-benef-pis-cofins-rec.
                    ELSE
                        ASSIGN tt-itens.cod-beneficio-piscof = 0.                           
                END.
                IF AVAIL(nfe-cte-inf) THEN DO:
                    
                    ASSIGN tt-itens.emit-crt             = IF LOOKUP(STRING(i-cst-icms,"99"),es-natoper-rec.red-cst-icms,";") > 0 THEN 4 ELSE i-crt
                           tt-itens.aliquota-icm         = d-icms
                           tt-itens.d-icms-BC            = d-icms-BC
                           tt-itens.d-Vicms              = d-Vicms
                           tt-itens.d-Vicms              = d-Vicms
                           tt-itens.d-RedBc              = d-RedBc.

                END.
            END. /* EACH bitem-doc-est */

            IF tt-itens.nat-operacao-orig = es-natoper-rec.nat-operacao-rateio
            OR tt-itens.nat-operacao-orig = es-natoper-rec.nat-operacao-rateio-int THEN
                ASSIGN p-rateio = TRUE.
            ELSE DO:

                FIND FIRST nfe-cte-inf NO-LOCK
                     WHERE nfe-cte-inf.chave-acesso = bdocum-est.cod-chave-aces-nf-eletro 
                       AND nfe-cte-inf.tipo-entrada = 1 NO-ERROR.
                IF AVAIL(nfe-cte-inf) THEN 
                    ASSIGN p-rateio = TRUE.
                ELSE
                    ASSIGN p-rateio = FALSE.

            END.


            RUN pi-atrib-natoper (INPUT  bdocum-est.cod-estabel,
                                  INPUT  bitem-doc-est.it-codigo,
                                  INPUT  bitem-doc-est.cod-emitente,
                                  INPUT  tt-itens.cod-model-nf-eletro, /* Modelo*/
                                  INPUT  tt-itens.cod-cfop, /*CFOP*/
                                  INPUT  IF l-primeira THEN 0 ELSE tt-itens.aliquota-icm,
                                  INPUT  tt-itens.cod-beneficio-icms,
                                  INPUT  tt-itens.cod-beneficio-piscof,
                                  INPUT  p-rateio,
                                  OUTPUT tt-itens.classe,
                                  OUTPUT tt-itens.nat-operacao,
                                  OUTPUT ca-oper).

            ASSIGN tt-itens.d-aliq-icm = d-aliq-icm.

            /* Verificar este ponto */

            IF ca-oper <> "I"                  OR 
               (LOOKUP(tt-itens.classe,es-natoper-rec.cod-cfa-aplic,";") > 0 ) THEN
                ASSIGN l-primeira           = NO
                       tt-itens.l-buscaicms = NO.

            IF TRIM(tt-itens.classe) = "" THEN DO:
                run utp/ut-msgs.p (input "show":U, 
                                   input 17006, 
                                   input "Item sem CFA~~" + 
                                         "N∆o foi encontrado CFA para o item " + QUOTER(bitem-doc-est.it-codigo) +
                                         " no estabelecimento " + QUOTER(bdocum-est.cod-estabel) + 
                                         ", solicite a correá∆o com os respons†veis antes de continuar.").
                RETURN "NOK":U.
            END.
        END. /* EACH bitem-doc-est */

        FIND FIRST tt-itens
            WHERE tt-itens.l-buscaicms = TRUE NO-ERROR.
        /* Aplicar primeira atribuiá∆o */
        IF AVAIL tt-itens THEN 
        DO:

            ASSIGN ca_nat-oper   = "VAZIO"
                   ldif-nat-oper = NO
                   c-msg-nat     = "".
            FOR EACH tt-itens
                WHERE tt-itens.l-buscaicms = TRUE:
                IF ca_nat-oper = "VAZIO" THEN
                    ASSIGN ca_nat-oper = tt-itens.nat-operacao.

                IF tt-itens.nat-operacao = "" THEN
                    ASSIGN ldif-nat-oper = YES.
    
                ASSIGN c-msg-nat = c-msg-nat +
    			       CHR(13) +
                                   "Seq: "           + STRING(tt-itens.sequencia) +
                                   ", Item: "        + QUOTER(tt-itens.it-codigo) +  
                                   ", CFA: "         + TRIM(tt-itens.classe) +
                                   ", Benef. ICMS "  + string(tt-itens.cod-beneficio-icms)  +
                                   ", Benef. PIS "   + string(tt-itens.cod-beneficio-piscof) +
                                   ", CFOP Fornec. " + string(tt-itens.cod-cfop) +
                                   ", ICMS Difal "   + string(tt-itens.d-aliq-icm) +        
                                   " = Nat Oper: "   + QUOTER(tt-itens.nat-operacao) .
            END.

            IF ldif-nat-oper THEN DO:                                                                                                                                   
                run utp/ut-msgs.p (input "show":U,                                                                                                                         
                                   input 27702,                                                                                                                            
                                   input "Continua implantaá∆o com natureza Tempor†ria?~~" +                                                                               
                                         "N∆o foi encontrado natureza de operaá∆o para os itens da nota fiscal, deseja continuar com a natureza de operaá∆o tempor†ria?" + 
										 c-msg-nat).
                IF NOT LOGICAL(RETURN-VALUE) THEN                                                                                                                          
                    RETURN "NOK".                                                                                                                                          
                /* Continuar processo de atualizaá∆o padr∆o */   
                FOR EACH tt-itens
					WHERE tt-itens.nat-operacao = "" :
						ASSIGN tt-itens.nat-operacao = tt-itens.nat-operacao-orig 
						       tt-itens.l-buscaicms  = NO.
                END.     
            END.                                                                                                                                                           

           FOR EACH tt-itens
               WHERE tt-itens.l-buscaicms:

                FIND FIRST tt-docum-est
                    WHERE tt-docum-est.serie-docto       = tt-itens.serie-docto      
                      AND tt-docum-est.nro-docto         = tt-itens.nro-docto        
                      AND tt-docum-est.cod-emitente      = tt-itens.cod-emitente     
                      AND tt-docum-est.nat-operacao      = tt-itens.nat-operacao NO-ERROR.
                IF NOT AVAIL tt-docum-est THEN 
                DO:
                    BUFFER-COPY bdocum-est     TO tt-docum-est
                                           ASSIGN tt-docum-est.nat-operacao            = tt-itens.nat-operacao
                                                  SUBSTRING(tt-docum-est.char-2,256,1) = SUBSTRING(bdocum-est.char-2,256,1).
                END.

                FOR EACH bitem-doc-est NO-LOCK
                    OF bdocum-est
                    WHERE bitem-doc-est.sequencia = tt-itens.sequencia:

                    CREATE tt-item-doc-est.
                    BUFFER-COPY bitem-doc-est      TO tt-item-doc-est
                                               ASSIGN tt-item-doc-est.cd-trib-icm  = 0 
                                                      tt-item-doc-est.cd-trib-ipi  = 0
                                                      tt-item-doc-est.nat-operacao = tt-itens.nat-operacao                                                                                     
                                                      tt-item-doc-est.aliquota-ipi          = tt-itens.ALIquota-ipi
                                                      tt-item-doc-est.valor-ipi             = tt-itens.d-VIPI   
                                                      tt-item-doc-est.base-ipi              = tt-itens.d-ipi-bc     
                                                      tt-item-doc-est.base-icm              = bitem-doc-est.base-icm
                                                      tt-item-doc-est.valor-icm[1]          = bitem-doc-est.valor-icm[1]                             
                                                      tt-item-doc-est.valor-icm[2]          = bitem-doc-est.valor-icm[2]  
                                                      tt-itens.d-Vicms                      = tt-item-doc-est.valor-icm[1]
                                                      tt-item-doc-est.valor-icm             = bitem-doc-est.valor-icm
                                                      OVERLAY(tt-item-doc-est.char-2,7,6)   = STRING(tt-itens.d-RedBc * 10000,"999999")
                                                      tt-item-doc-est.val-perc-red-icms     = tt-itens.d-RedBc
                                                      tt-item-doc-est.log-2                 = tt-itens.log-2
                                                      tt-item-doc-est.base-subs[1]          = tt-itens.imp-vBCST
                                                      tt-item-doc-est.vl-subs[1]            = tt-itens.imp-vICMSST
                                                      OVERLAY(tt-item-doc-est.char-2,941,3) = SUBSTRING(bitem-doc-est.char-2,941,3)
                                                      /* Begins - Willians Ambrosio - DKP - 05/02/2019 - Ajustada regra para receber a natureza atualizada */
                                                      tt-item-doc-est.nat-of                = tt-itens.nat-operacao.   
                                                      /* End 05/02/2019 */

                    /* Begins - Willians Ambrosio - DKP - 16/11/2018 - Quando for tipo serviáo, n∆o dever† atribuir o valor de icms */
                    IF bdocum-est.cod-observa = 4 /* Serviáo */ THEN
                       ASSIGN tt-item-doc-est.base-icm      = 0
                              tt-item-doc-est.valor-icm[1]  = 0
                              tt-item-doc-est.valor-icm[2]  = 0
                              tt-itens.d-Vicms              = 0
                              tt-item-doc-est.valor-icm     = 0.
                    /* End 16/11/2018 */

                    FIND FIRST rat-ordem NO-LOCK
                         WHERE rat-ordem.cod-emitente = tt-itens.cod-emitente
                           AND rat-ordem.serie-docto  = tt-itens.serie-docto 
                           AND rat-ordem.nro-docto    = tt-itens.nro-docto   
                           AND rat-ordem.nat-operacao = tt-itens.nat-operacao-orig
                           AND rat-ordem.sequencia    = tt-itens.sequencia NO-ERROR.
                    IF AVAIL(rat-ordem) THEN DO:
                        /*alteraá∆o para entrada de contrato com mediá∆o*/
                        ASSIGN tt-item-doc-est.ct-codigo = "11505002"
                               tt-item-doc-est.log-1     = TRUE.
                    END.

                    IF i-crt = 1 /* Simples Nacional */ THEN DO:        
                        ASSIGN tt-item-doc-est.aliquota-icm   = tt-itens.imp-pcredsn
                               tt-item-doc-est.cd-trib-icm    = tt-itens.imp-csosn
                               tt-item-doc-est.log-icm-retido = tt-itens.imp-vcredicmssn > 0
                               tt-item-doc-est.valor-icm      = tt-itens.imp-vcredicmssn.
                    END.
                    ELSE IF p-crt = 1 THEN DO:
                        ASSIGN tt-item-doc-est.aliquota-icm   = bitem-doc-est.aliquota-icm
                               tt-item-doc-est.cd-trib-icm    = bitem-doc-est.cd-trib-icm  
                               tt-item-doc-est.log-icm-retido = bitem-doc-est.log-icm-retido.
                    END.
                    ELSE 
                        ASSIGN tt-item-doc-est.aliquota-icm = tt-itens.aliquota-icm.    

                    IF tt-itens.emit-crt = 4 THEN DO:

                        FIND FIRST bnatur-oper NO-LOCK
                             WHERE bnatur-oper.nat-operacao = tt-itens.nat-operacao NO-ERROR.
                        IF bnatur-oper.cd-trib-icm = 1  THEN DO:

                            ASSIGN tt-item-doc-est.cd-trib-icm = tt-itens.emit-crt.
                        END.

                    END.

                END.



            END. /* EACH tt-itens */

            /* Recriar */
            FIND FIRST tt-itens
                WHERE tt-itens.l-buscaicms NO-ERROR.
            IF AVAIL tt-itens THEN DO:

                EMPTY TEMP-TABLE tt-mensagem-erro.
                EMPTY TEMP-TABLE tt-erro.

                RUN rep\reapi316b.p (INPUT "ADD",
                                     INPUT  TABLE tt-docum-est,
                                     INPUT  TABLE tt-rat-docum,
                                     INPUT  TABLE tt-item-doc-est,
                                     INPUT  TABLE tt-dupli-apagar,
                                     INPUT  TABLE tt-dupli-imp,
                                     OUTPUT TABLE tt-erro).
                    
                IF CAN-FIND(FIRST tt-erro) THEN DO:
                    
                    ASSIGN i-erro = 0.
                        
                    FOR EACH tt-erro NO-LOCK:
                        ASSIGN i-erro = i-erro + 1.
                            
                        CREATE tt-mensagem-erro.
                            
                        ASSIGN tt-mensagem-erro.i-sequen = i-erro
                               tt-mensagem-erro.cd-erro  = tt-erro.cd-erro 
                               tt-mensagem-erro.mensagem = tt-erro.desc-erro.
            
                    END.

                    RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
                    UNDO Gravar, LEAVE Gravar.
                END.
    
                FOR EACH tt-itens
                    WHERE tt-itens.l-buscaicms = TRUE:
                    FOR FIRST bitem-doc-est
                        WHERE bitem-doc-est.serie-docto  = tt-itens.serie-docto   
                          AND bitem-doc-est.nro-docto    = tt-itens.nro-docto     
                          AND bitem-doc-est.cod-emitente = tt-itens.cod-emitente  
                          AND bitem-doc-est.nat-operacao = tt-itens.nat-operacao  
                          AND bitem-doc-est.sequencia    = tt-itens.sequencia   NO-LOCK:
                        
                        ASSIGN tt-itens.aliquota-icm = bitem-doc-est.aliquota-icm.
                    END.
                END.
    
                /* Deletar antigo */
                EMPTY TEMP-TABLE tt-mensagem-erro.
                EMPTY TEMP-TABLE tt-erro.

                RUN rep\reapi316b.p (INPUT "DEL",
                                     INPUT  TABLE tt-docum-est,
                                     INPUT  TABLE tt-rat-docum,
                                     INPUT  TABLE tt-item-doc-est,
                                     INPUT  TABLE tt-dupli-apagar,
                                     INPUT  TABLE tt-dupli-imp,
                                     OUTPUT TABLE tt-erro).
                    
                IF CAN-FIND(FIRST tt-erro) THEN DO:
                    
                    ASSIGN i-erro = 0.
                        
                    FOR EACH tt-erro NO-LOCK:
                        ASSIGN i-erro = i-erro + 1.
                            
                        CREATE tt-mensagem-erro.
                            
                        ASSIGN tt-mensagem-erro.i-sequen = i-erro
                               tt-mensagem-erro.cd-erro  = tt-erro.cd-erro 
                               tt-mensagem-erro.mensagem = tt-erro.desc-erro.
            
                    END.

                    RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
                    UNDO Gravar, LEAVE Gravar.
                END.
            END. /* AVAIL tt-itens com icms */ 
        END. /* AVAIL tt-itens*/
        /* Fim primeira atribuiá∆o */
        FIND FIRST bdocum-est 
            WHERE ROWID(bdocum-est) = rw-docum-est NO-ERROR.
        IF NOT AVAIL bdocum-est THEN
            RETURN "NOK":U.


        /* Validar mudanáa de natureza */        
        FOR EACH tt-itens
            WHERE tt-itens.l-buscaicms = TRUE:

            RUN pi-atrib-natoper (INPUT  bdocum-est.cod-estabel,
                                  INPUT  tt-itens.it-codigo,
                                  INPUT  tt-itens.cod-emitente,
                                  INPUT  tt-itens.cod-model-nf-eletro, /* Modelo*/
                                  INPUT  tt-itens.cod-cfop, /*CFOP*/
                                  INPUT  tt-itens.aliquota-icm,
                                  INPUT  tt-itens.cod-beneficio-icms,
                                  INPUT  tt-itens.cod-beneficio-piscof,
                                  INPUT  p-rateio,
                                  OUTPUT tt-itens.classe,
                                  OUTPUT tt-itens.nat-operacao2,
                                  OUTPUT ca-oper).
            
            ASSIGN tt-itens.d-aliq-icm = d-aliq-icm.

			IF tt-itens.nat-operacao2 <> "" THEN
				ASSIGN tt-itens.nat-operacao = tt-itens.nat-operacao2.

            IF TRIM(tt-itens.classe) = "" THEN DO:
                run utp/ut-msgs.p (input "show":U, 
                                   input 17006, 
                                   input "Item sem CFA~~" + 
                                         "N∆o foi encontrado CFA para o item " + QUOTER(tt-itens.it-codigo) +
                                         " no estabelecimento " + QUOTER(bdocum-est.cod-estabel) + 
                                         ", solicite a correá∆o com os respons†veis antes de continuar.").
                RETURN "NOK":U.
            END.
        END.

        ASSIGN ca_nat-oper   = "VAZIO"
               ldif-nat-oper = NO
               c-msg-nat     = "".

        IF i-qtd-item < 30 THEN DO:
            FOR EACH tt-itens:
                IF ca_nat-oper = "VAZIO" THEN
                    ASSIGN ca_nat-oper = tt-itens.nat-operacao.
    
                IF tt-itens.nat-operacao = "" THEN
                    ASSIGN ldif-nat-oper = YES.
    
                
                ASSIGN c-msg-nat = c-msg-nat +
                    CHR(13) +
                                "Seq: "           + STRING(tt-itens.sequencia) +
                                ", Item: "        + QUOTER(tt-itens.it-codigo) +  
                                ", CFA: "         + TRIM(tt-itens.classe) +
                                ", Benef. ICMS "  + string(tt-itens.cod-beneficio-icms)  +
                                ", Benef. PIS "   + string(tt-itens.cod-beneficio-piscof) +
                                ", CFOP Fornec. " + string(tt-itens.cod-cfop) +
                                ", ICMS Difal "   + string(tt-itens.d-aliq-icm) +        
                                " = Nat Oper: "   + QUOTER(tt-itens.nat-operacao) .
                                
            END.
        END.
        ELSE DO:
             ASSIGN c-msg-nat = c-msg-nat +
                    CHR(13) + "Favor verificar o arquivo em: "
                            + SESSION:TEMP-DIRECTORY + "Log_mensagem_re1001.csv".

             OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "Log_mensagem_re1001.csv").
             PUT "Seq: ;"          
                 "Item: ;"       
                 "CFA: ;"        
                 "Benef. ICMS ;" 
                 "Benef. PIS ;"  
                 "CFOP Fornec. ;"
                 "ICMS Difal ;"  
                 "Nat Oper: ;" SKIP.

             FOR EACH tt-itens:
                 IF ca_nat-oper = "VAZIO" THEN
                     ASSIGN ca_nat-oper = tt-itens.nat-operacao.

                 IF tt-itens.nat-operacao = "" THEN
                     ASSIGN ldif-nat-oper = YES.

                 EXPORT DELIMITER ';' tt-itens.sequencia
                     tt-itens.it-codigo
                     tt-itens.classe
                     tt-itens.cod-beneficio-icms
                     tt-itens.cod-beneficio-piscof
                     tt-itens.cod-cfop
                     tt-itens.d-aliq-icm
                     tt-itens.nat-operacao.
             END.
             OUTPUT CLOSE.


        END.

        IF ldif-nat-oper THEN DO:                                                                                                                                   
            run utp/ut-msgs.p (input "show":U,                                                                                                                         
                               input 27702,                                                                                                                            
                               input "Continua implantaá∆o com natureza Tempor†ria?~~" +                                                                               
                                     "N∆o foi encontrado natureza de operaá∆o para os itens da nota fiscal, deseja continuar com a natureza de operaá∆o tempor†ria?" +
                                     c-msg-nat ).
            IF NOT LOGICAL(RETURN-VALUE) THEN                                                                                                                          
                RETURN "NOK".                                                                                                                                          
            /* Continuar processo de atualizaá∆o padr∆o */                                                                                                             
            FOR EACH tt-itens
				WHERE tt-itens.nat-operacao = "":
		        ASSIGN tt-itens.nat-operacao = tt-itens.nat-operacao-orig.                                                                     
            END.                                                                                                                                                       
        END.   

        EMPTY TEMP-TABLE tt-docum-est.
        EMPTY TEMP-TABLE tt-rat-docum.
        EMPTY TEMP-TABLE tt-item-doc-est.
        EMPTY TEMP-TABLE tt-dupli-apagar.
        EMPTY TEMP-TABLE tt-dupli-imp.

        CREATE tt-docum-est.
        BUFFER-COPY bdocum-est      TO tt-docum-est
                                ASSIGN tt-docum-est.r-rowid                 = ROWID(bdocum-est)
                                       SUBSTRING(tt-docum-est.char-2,256,1) = SUBSTRING(bdocum-est.char-2,256,1)
                                       tt-docum-est.base-icm                = bdocum-est.base-icm. 
        
        FOR EACH brat-docum NO-LOCK
            OF bdocum-est:
            CREATE tt-rat-docum.
            BUFFER-COPY brat-docum TO tt-rat-docum.
        END.

        FOR EACH bitem-doc-est NO-LOCK
            OF bdocum-est:

            CREATE tt-item-doc-est.
            BUFFER-COPY bitem-doc-est     TO tt-item-doc-est
                                      ASSIGN tt-item-doc-est.cd-trib-icm = 0 
                                             tt-item-doc-est.cd-trib-ipi = 0
                                             OVERLAY(tt-item-doc-est.char-2,941,3) = SUBSTRING(bitem-doc-est.char-2,941,3)
                                             tt-item-doc-est.valor-icm             = bitem-doc-est.valor-icm.    

                FOR FIRST tt-itens
                    WHERE tt-itens.serie-docto       = bitem-doc-est.serie-docto   
                      AND tt-itens.nro-docto         = bitem-doc-est.nro-docto     
                      AND tt-itens.cod-emitente      = bitem-doc-est.cod-emitente  
                      AND tt-itens.nat-operacao-orig = bitem-doc-est.nat-operacao  
                      AND tt-itens.sequencia         = bitem-doc-est.sequencia   NO-LOCK:  END.

                     ASSIGN tt-item-doc-est.aliquota-ipi        = tt-itens.ALIquota-ipi
                            tt-item-doc-est.valor-ipi           = tt-itens.d-VIPI   
                            tt-item-doc-est.base-ipi            = tt-itens.d-ipi-bc  .

                     IF tt-itens.d-icms-BC = 0 THEN 
                        ASSIGN tt-item-doc-est.base-icm = bitem-doc-est.base-icm.
                     ELSE
                        ASSIGN tt-item-doc-est.base-icm[1] = tt-itens.d-icms-BC
                               tt-item-doc-est.base-icm[2] = tt-itens.d-icms-BC. 

                    ASSIGN  tt-item-doc-est.valor-icm[1]        = bitem-doc-est.valor-icm[1]                             
                            tt-item-doc-est.valor-icm[2]        = bitem-doc-est.valor-icm[2]  
                            tt-itens.d-Vicms                    = tt-item-doc-est.valor-icm[1] 
                            OVERLAY(tt-item-doc-est.char-2,7,6) = STRING(tt-itens.d-RedBc * 10000,"999999")
                            tt-item-doc-est.val-perc-red-icms   = tt-itens.d-RedBc
                            tt-item-doc-est.log-2               = tt-itens.log-2
                            tt-item-doc-est.base-subs[1]        = tt-itens.imp-vBCST
                            tt-item-doc-est.vl-subs[1]          = tt-itens.imp-vICMSST
                            /* Begins - Willians Ambrosio - DKP - 05/02/2019 - Ajustada regra para receber a natureza atualizada */
                            tt-item-doc-est.nat-of              = bitem-doc-est.nat-operacao.   
                            /* End 05/02/2019 */
                            

                 /* Begins - Willians Ambrosio - DKP - 16/11/2018 - Quando for tipo serviáo, n∆o dever† atribuir o valor de icms */
                 IF bdocum-est.cod-observa = 4 /* Serviáo */ THEN
                    ASSIGN tt-item-doc-est.base-icm      = 0
                           tt-item-doc-est.valor-icm[1]  = 0
                           tt-item-doc-est.valor-icm[2]  = 0
                           tt-itens.d-Vicms              = 0
                           tt-item-doc-est.valor-icm     = 0.
                 /* End 16/11/2018 */                      
                                
                IF i-crt = 1 /* Simples Nacional */ THEN DO:
                    ASSIGN tt-item-doc-est.aliquota-icm   = tt-itens.imp-pcredsn
                           tt-item-doc-est.cd-trib-icm    = tt-itens.imp-csosn
                           tt-item-doc-est.log-icm-retido = tt-itens.imp-vcredicmssn > 0
                           tt-item-doc-est.valor-icm      = tt-itens.imp-vcredicmssn.
                END.
                ELSE IF p-crt = 1 THEN DO: /*aqui*/
                    ASSIGN tt-item-doc-est.aliquota-icm   = bitem-doc-est.aliquota-icm
                           tt-item-doc-est.cd-trib-icm    = bitem-doc-est.cd-trib-icm  
                           tt-item-doc-est.log-icm-retido = bitem-doc-est.log-icm-retido.
                END.
                ELSE DO:
                    ASSIGN tt-item-doc-est.aliquota-icm = tt-itens.aliquota-icm.
                END.
                
                FIND FIRST es-item-doc-est-natoper NO-LOCK
                    WHERE es-item-doc-est-natoper.ep-codigo         = i-ep-codigo-usuario    
                      AND es-item-doc-est-natoper.serie-docto       = tt-itens.serie-docto
                      AND es-item-doc-est-natoper.nro-docto         = tt-itens.nro-docto
                      AND es-item-doc-est-natoper.cod-emitente      = tt-itens.cod-emitente
                      AND es-item-doc-est-natoper.nat-operacao      = tt-itens.nat-operacao-orig
                      AND es-item-doc-est-natoper.sequencia         = tt-itens.sequencia NO-ERROR.
                IF AVAIL(es-item-doc-est-natoper) THEN 
                DO:
                    FIND FIRST bnatur-oper NO-LOCK
                         WHERE bnatur-oper.nat-operacao = tt-itens.nat-operacao NO-ERROR.

                        IF es-item-doc-est-natoper.emit-crt = 4 THEN DO:
                            IF bnatur-oper.cd-trib-icm = 1  THEN
                                ASSIGN tt-item-doc-est.cd-trib-icm = es-item-doc-est-natoper.emit-crt
                                       tt-itens.emit-crt           = es-item-doc-est-natoper.emit-crt.
                            ELSE 
                                ASSIGN tt-item-doc-est.cd-trib-icm = bnatur-oper.cd-trib-icm.
    
                        END.
                        ELSE
                            ASSIGN tt-item-doc-est.cd-trib-icm = bnatur-oper.cd-trib-icm.

                    
                END.
                ELSE 
                DO:
                    FIND FIRST bnatur-oper NO-LOCK
                         WHERE bnatur-oper.nat-operacao = tt-itens.nat-operacao NO-ERROR.
                    IF tt-itens.emit-crt = 4 THEN DO:
                        IF bnatur-oper.cd-trib-icm = 1  THEN
                            ASSIGN tt-item-doc-est.cd-trib-icm = tt-itens.emit-crt.
                        ELSE 
                            ASSIGN tt-item-doc-est.cd-trib-icm = bnatur-oper.cd-trib-icm.
                    END.
                    ELSE 
                        ASSIGN tt-item-doc-est.cd-trib-icm =  bnatur-oper.cd-trib-icm.

                END.                
        END.        

        FOR EACH bdupli-apagar NO-LOCK
            OF bdocum-est:
            CREATE tt-dupli-apagar.
            BUFFER-COPY bdupli-apagar TO tt-dupli-apagar.
        END.
        FOR EACH bdupli-imp NO-LOCK
            OF bdocum-est:
            CREATE tt-dupli-imp.
            BUFFER-COPY bdupli-imp TO tt-dupli-imp.
        END.

        FOR EACH brat-ordem NO-LOCK
            OF bdocum-est:
            CREATE tt-rat-ordem.
            BUFFER-COPY brat-ordem TO tt-rat-ordem.
        END.

        /* Deletar antigo */
        EMPTY TEMP-TABLE tt-mensagem-erro.
        EMPTY TEMP-TABLE tt-erro.
        
        RUN rep\reapi316b.p (INPUT "DEL",
                             INPUT  TABLE tt-docum-est,
                             INPUT  TABLE tt-rat-docum,
                             INPUT  TABLE tt-item-doc-est,
                             INPUT  TABLE tt-dupli-apagar,
                             INPUT  TABLE tt-dupli-imp,
                             OUTPUT TABLE tt-erro).
            
        IF CAN-FIND(FIRST tt-erro) THEN DO:
            
            ASSIGN i-erro = 0.
                
            FOR EACH tt-erro NO-LOCK:
                ASSIGN i-erro = i-erro + 1.
                    
                CREATE tt-mensagem-erro.
                    
                ASSIGN tt-mensagem-erro.i-sequen = i-erro
                       tt-mensagem-erro.cd-erro  = tt-erro.cd-erro 
                       tt-mensagem-erro.mensagem = tt-erro.desc-erro.
            END.

            RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
            UNDO Gravar, LEAVE Gravar.
        END.

        /* Recriar */
        EMPTY TEMP-TABLE tt-docum-est2.
        FOR EACH tt-docum-est:
            CREATE tt-docum-est2.
            BUFFER-COPY tt-docum-est TO tt-docum-est2.
        END.
        EMPTY TEMP-TABLE tt-docum-est.
        EMPTY TEMP-TABLE tt-rat-docum2.
        FOR EACH tt-rat-docum:
            CREATE tt-rat-docum2.
            BUFFER-COPY tt-rat-docum TO tt-rat-docum2.
        END.
        EMPTY TEMP-TABLE tt-rat-docum.

        EMPTY TEMP-TABLE tt-dupli-apagar. /* Zerar para recriar pela natureza de operaá∆o */
        FOR EACH tt-itens:            
            FIND FIRST tt-docum-est
                WHERE tt-docum-est.serie-docto       = tt-itens.serie-docto      
                  AND tt-docum-est.nro-docto         = tt-itens.nro-docto        
                  AND tt-docum-est.cod-emitente      = tt-itens.cod-emitente     
                  AND tt-docum-est.nat-operacao      = tt-itens.nat-operacao NO-ERROR.
            IF NOT AVAIL tt-docum-est THEN DO:                
                FIND FIRST tt-docum-est2
                    WHERE tt-docum-est2.serie-docto       = tt-itens.serie-docto      
                      AND tt-docum-est2.nro-docto         = tt-itens.nro-docto        
                      AND tt-docum-est2.cod-emitente      = tt-itens.cod-emitente     
                      AND tt-docum-est2.nat-operacao      = tt-itens.nat-operacao-orig NO-ERROR.
                BUFFER-COPY tt-docum-est2    TO tt-docum-est
                                         ASSIGN tt-docum-est.nat-operacao = tt-itens.nat-operacao
                                                SUBSTRING(tt-docum-est.char-2,256,1) = SUBSTRING(tt-docum-est2.char-2,256,1).
        
                    /* Begins: 09/08/2018  - Quando n∆o houver imposto atrelado a duplicata dever† desmarcar o flag de reinf */ 
                    IF CAN-FIND(FIRST tt-dupli-imp WHERE 
                                      tt-dupli-imp.cod-retencao = 2631) THEN
                       ASSIGN SUBSTRING(tt-docum-est.char-2,256,1) = "S".
                    ELSE
                       ASSIGN SUBSTRING(tt-docum-est.char-2,256,1) = "N".
                    /* End 09/08/2018 */                             
            END.            

            FIND FIRST tt-rat-docum
                WHERE tt-rat-docum.serie-docto       = tt-itens.serie-docto      
                  AND tt-rat-docum.nro-docto         = tt-itens.nro-docto        
                  AND tt-rat-docum.cod-emitente      = tt-itens.cod-emitente     
                  AND tt-rat-docum.nat-operacao      = tt-itens.nat-operacao NO-ERROR.
            IF NOT AVAIL tt-rat-docum THEN DO:
                FOR EACH tt-rat-docum2:
                    CREATE tt-rat-docum.
                    BUFFER-COPY tt-rat-docum2 EXCEPT nat-operacao TO tt-rat-docum
                        ASSIGN tt-rat-docum.nat-operacao = tt-itens.nat-operacao.
                END.
            END.            

            FIND FIRST tt-item-doc-est
                 WHERE  tt-item-doc-est.serie-docto       = tt-itens.serie-docto
                   AND  tt-item-doc-est.nro-docto         = tt-itens.nro-docto
                   AND  tt-item-doc-est.cod-emitente      = tt-itens.cod-emitente
                   AND  tt-item-doc-est.nat-operacao      = tt-itens.nat-operacao-orig
                   AND  tt-item-doc-est.sequencia         = tt-itens.sequencia NO-ERROR.
                ASSIGN tt-item-doc-est.nat-operacao = tt-itens.nat-operacao.


                


             /*implant. Manual Rateio*/
            IF p-rateio = TRUE THEN DO:
                IF AVAIL(tt-docum-est) THEN DO:

                    IF natur-aux-anterior = tt-itens.nat-operacao THEN
                        ASSIGN tt-docum-est.valor-mercad = tt-docum-est.valor-mercad + tt-item-doc-est.preco-total[1].
                    ELSE
                        ASSIGN tt-docum-est.valor-mercad = tt-item-doc-est.preco-total[1].


                        ASSIGN natur-aux-anterior = tt-itens.nat-operacao.

                END.
                    
            END.            

            FIND FIRST tt-rat-ordem NO-LOCK
                 WHERE tt-rat-ordem.cod-emitente = tt-itens.cod-emitente
                   AND tt-rat-ordem.serie-docto  = tt-itens.serie-docto 
                   AND tt-rat-ordem.nro-docto    = tt-itens.nro-docto   
                   AND tt-rat-ordem.nat-operacao = tt-itens.nat-operacao-orig
                   AND tt-rat-ordem.sequencia    = tt-itens.sequencia NO-ERROR.
            IF AVAIL(tt-rat-ordem) THEN DO:

                /*alteraá∆o para entrada de contrato com mediá∆o*/
                ASSIGN tt-item-doc-est.ct-codigo = "11505002"
                       tt-item-doc-est.log-1 = TRUE.

                FOR EACH tt-rat-ordem NO-LOCK
                   WHERE tt-rat-ordem.cod-emitente = tt-itens.cod-emitente
                     AND tt-rat-ordem.serie-docto  = tt-itens.serie-docto 
                     AND tt-rat-ordem.nro-docto    = tt-itens.nro-docto   
                     AND tt-rat-ordem.nat-operacao = tt-itens.nat-operacao-orig
                     AND tt-rat-ordem.sequencia    = tt-itens.sequencia :
                    CREATE rat-ordem.
                    BUFFER-COPY tt-rat-ordem EXCEPT nat-operacao TO rat-ordem.
                        ASSIGN rat-ordem.nat-operacao = tt-itens.nat-operacao.

                        ASSIGN tt-rat-ordem.nat-operacao = tt-itens.nat-operacao.
                END.
                
            END.            

            FOR EACH tt-dupli-imp:
                ASSIGN tt-dupli-imp.nat-operacao = tt-itens.nat-operacao.
            END.
        END. /* tt-itens */


        EMPTY TEMP-TABLE tt-mensagem-erro.
        EMPTY TEMP-TABLE tt-erro.

        /*Implant. Manual de Rateio*/
        IF p-rateio THEN DO:

            FOR EACH tt-item-doc-est:
                CREATE btt-item-doc-est.
                BUFFER-COPY tt-item-doc-est TO btt-item-doc-est.
            END.

            EMPTY TEMP-TABLE tt-item-doc-est.
            
        END.            

        /*Begins 14/05/2018 - Para evitar o bloqueio de saldo nos contratos de mediá∆o dever† liberar o saldo do contrato de mediá∆o neste ponto */   
        FIND FIRST tt-docum-est NO-LOCK NO-ERROR.
        IF AVAIL tt-docum-est THEN
        DO:      
            FOR FIRST tt-item-doc-est OF tt-docum-est:
                
                FIND FIRST ordem-compra WHERE
                           ordem-compra.numero-ordem = tt-item-doc-est.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN
                   FIND FIRST contrato-for where
                              contrato-for.nr-contrato = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN
                DO:            
                    if contrato-for.ind-control-rec = 2 then
                    do:          
                        find item-contrat
                            where item-contrat.num-seq-item = ordem-compra.num-seq-item
                              and item-contrat.nr-contrato = ordem-compra.nr-contrato
                            no-lock no-error.
                    end.
            

                    if contrato-for.ind-control-rec = 2 and avail item-contrat then
                    do:
                        for each medicao-contrat use-index dat-prev
                           where medicao-contrat.numero-ordem  = ordem-compra.numero-ordem
                             and medicao-contrat.ind-sit-medicao = 2
                             and medicao-contrat.dat-prev-medicao <= bdocum-est.dt-emissao
                             and medicao-contrat.log-rec-medicao = NO EXCLUSIVE-LOCK:
            
                            ASSIGN medicao-contrat.val-sdo-aloc-med = medicao-contrat.val-sdo-aloc-med - tt-item-doc-est.preco-total[1].
                        END.
                    END.
                END.
            END.
        END.
        /* end 14/05/2018 */  

        /* Begins: 08/02/2019 - Ajuste na regra de atualizaá∆o das obrigaá‰es fiscais */
        FOR EACH tt-item-doc-est:
            ASSIGN tt-item-doc-est.nat-of = tt-item-doc-est.nat-operacao.
        END.
        /* End 08/02/2019 */

        RUN rep\reapi316b.p (INPUT "ADD",
                             INPUT  TABLE tt-docum-est,
                             INPUT  TABLE tt-rat-docum,
                             INPUT  TABLE tt-item-doc-est,
                             INPUT  TABLE tt-dupli-apagar,
                             INPUT  TABLE tt-dupli-imp,
                             OUTPUT TABLE tt-erro).

        IF CAN-FIND(FIRST tt-erro) THEN DO:
            
            ASSIGN i-erro = 0.
                
            FOR EACH tt-erro NO-LOCK:
                ASSIGN i-erro = i-erro + 1.
                    
                CREATE tt-mensagem-erro.
                    
                ASSIGN tt-mensagem-erro.i-sequen = i-erro
                       tt-mensagem-erro.cd-erro  = tt-erro.cd-erro 
                       tt-mensagem-erro.mensagem = tt-erro.desc-erro.
            END.

            RUN cdp\cd0666.w (INPUT TABLE tt-mensagem-erro).
            UNDO Gravar, LEAVE Gravar.
        END.

        FOR EACH tt-itens: /*ponto para alteracao*/
        /* Criar controle de atribuiá∆o de natureza */
            FOR EACH ITEM-doc-est NO-LOCK
               WHERE item-doc-est.nro-docto    = tt-itens.nro-docto
                 AND item-doc-est.serie-docto  = tt-itens.serie-docto
                 AND item-doc-est.cod-emitente = tt-itens.cod-emitente
                 AND item-doc-est.nat-operacao = tt-itens.nat-operacao:

                    FIND FIRST es-item-doc-est-natoper
                         WHERE es-item-doc-est-natoper.ep-codigo         = i-ep-codigo-usuario    
                           AND es-item-doc-est-natoper.serie-docto       = tt-itens.serie-docto
                           AND es-item-doc-est-natoper.nro-docto         = tt-itens.nro-docto
                           AND es-item-doc-est-natoper.cod-emitente      = tt-itens.cod-emitente
                           AND es-item-doc-est-natoper.nat-operacao      = item-doc-est.nat-operacao
                           AND es-item-doc-est-natoper.sequencia         = item-doc-est.sequencia NO-ERROR.
                    IF NOT AVAIL es-item-doc-est-natoper THEN DO:
                        CREATE es-item-doc-est-natoper.
                        /*BUFFER-COPY tt-itens TO es-item-doc-est-natoper.*/
                        ASSIGN es-item-doc-est-natoper.serie-docto            = tt-itens.serie-docto
                               es-item-doc-est-natoper.nro-docto              = tt-itens.nro-docto
                               es-item-doc-est-natoper.cod-emitente           = tt-itens.cod-emitente
                               es-item-doc-est-natoper.nat-operacao           = item-doc-est.nat-operacao
                               es-item-doc-est-natoper.nat-operacao-orig      = tt-itens.nat-operacao-orig
                               es-item-doc-est-natoper.cod-cfop-saida         = tt-itens.cod-cfop-saida    
                               es-item-doc-est-natoper.cod-beneficio-icms     = tt-itens.cod-beneficio-icms
                               es-item-doc-est-natoper.classe                 = tt-itens.classe            
                               es-item-doc-est-natoper.sequencia              = item-doc-est.sequencia
                               es-item-doc-est-natoper.it-codigo              = item-doc-est.it-codigo
                               es-item-doc-est-natoper.cod-model-nf-eletro    = tt-itens.cod-model-nf-eletro 
                               es-item-doc-est-natoper.cod-beneficio-piscof   = tt-itens.cod-beneficio-piscof
                               es-item-doc-est-natoper.emit-crt               = tt-itens.emit-crt            
                               es-item-doc-est-natoper.imp-CSOSN              = tt-itens.imp-CSOSN           
                               es-item-doc-est-natoper.imp-pCredSN            = tt-itens.imp-pCredSN         
                               es-item-doc-est-natoper.imp-vCredICMSSN        = tt-itens.imp-vCredICMSSN     
                               es-item-doc-est-natoper.aliquota-icm           = tt-itens.aliquota-icm        
                               es-item-doc-est-natoper.ep-codigo              = i-ep-codigo-usuario.
                    END.

                        
                        

            END.
        END.

        /* Alteraá∆o para Re-Sequenciamento 23/07/2015 */
        FOR EACH tt-itens:

            FOR EACH es-item-doc-est-natoper
               WHERE es-item-doc-est-natoper.ep-codigo         =  i-ep-codigo-usuario       
                 AND es-item-doc-est-natoper.serie-docto       =  tt-itens.serie-docto      
                 AND es-item-doc-est-natoper.nro-docto         =  tt-itens.nro-docto        
                 AND es-item-doc-est-natoper.cod-emitente      =  tt-itens.cod-emitente     
                 AND es-item-doc-est-natoper.nat-operacao      =  tt-itens.nat-operacao-orig
                 BREAK BY es-item-doc-est-natoper.nat-operacao:
    
                i-sequencia-item = i-sequencia-item + 10.
    
                ASSIGN es-item-doc-est-natoper.sequencia-it-doc-fisc = i-sequencia-item.
    
                IF LAST-OF(es-item-doc-est-natoper.nat-operacao) THEN 
                    ASSIGN i-sequencia-item = 0.
    
    
            END.

        END.
        /* Alteraá∆o para Re-Sequenciamento 23/07/2015 */

        /* Altera status RA */
    
        FIND FIRST tt-docum-est NO-LOCK NO-ERROR.

        FOR EACH rat-ordem EXCLUSIVE-LOCK
             WHERE rat-ordem.cod-emitente = tt-docum-est.cod-emitente
               AND rat-ordem.serie-docto  = tt-docum-est.serie-docto 
               AND rat-ordem.nro-docto    = tt-docum-est.nro-docto   
               AND rat-ordem.nat-operacao = tt-docum-est.nat-operacao:

            FIND FIRST tt-rat-ordem NO-LOCK
                 WHERE tt-rat-ordem.cod-emitente = rat-ordem.cod-emitente
                   AND tt-rat-ordem.serie-docto  = rat-ordem.serie-docto 
                   AND tt-rat-ordem.nro-docto    = rat-ordem.nro-docto   
                   AND tt-rat-ordem.nat-operacao = rat-ordem.nat-operacao
                   AND tt-rat-ordem.sequencia    = rat-ordem.sequencia 
                   AND tt-rat-ordem.numero-ordem = rat-ordem.numero-ordem
                   AND tt-rat-ordem.num-pedido   = rat-ordem.num-pedido   NO-ERROR.
            IF NOT AVAIL(tt-rat-ordem) THEN DO:
              DELETE rat-ordem.
              NEXT.
            END.
        
            /*teste apagar conta */
            FOR EACH item-doc-est 
                WHERE  item-doc-est.serie-docto       = rat-ordem.serie-docto
                  AND  item-doc-est.nro-docto         = rat-ordem.nro-docto
                  AND  item-doc-est.cod-emitente      = rat-ordem.cod-emitente
                  AND  item-doc-est.nat-operacao      = rat-ordem.nat-operacao
                  AND  item-doc-est.sequencia         = rat-ordem.sequencia: 

                ASSIGN item-doc-est.ct-codigo      = ""
                       item-doc-est.cod-unid-negoc = ""
                       item-doc-est.log-1          = FALSE.
            END.

        END.


        FIND FIRST nfe-dfe
             WHERE nfe-dfe.chave-acesso = tt-docum-est.cod-chave-aces-nf-eletro NO-ERROR.
        IF AVAIL(nfe-dfe) THEN
            ASSIGN nfe-dfe.sit-erp = 2.

        
        /* Altera status RA */

        IF NOT l-ra THEN
            RUN piGeraDuplicata.

        FIND FIRST tt-docum-est NO-ERROR.
        RUN setConstraintChave IN hBoin090 (INPUT tt-docum-est.serie-docto,
                                            INPUT tt-docum-est.nro-docto,
                                            INPUT tt-docum-est.cod-emitente,
                                            INPUT tt-docum-est.nat-operacao).
        RUN openQueryStatic IN hBoin090(INPUT "Chave":U).
    
        RUN findFirstChave in hBoin090.
    
        /* Retorna rowid do registro corrente do DBO */
        RUN getRowid IN hBoin090 (OUTPUT rw-docum-est).
        IF rw-docum-est = ? THEN DO:
            UNDO Gravar, LEAVE Gravar.
        END.

        ASSIGN lerro = NO.
        IF NOT l-primeira THEN
            LEAVE Gravar.
        ASSIGN l-primeira = NO.
        /* Begins REV003 */
        FIND FIRST docum-est WHERE 
             ROWID(docum-est) = rw-docum-est EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE(docum-est) THEN 
        DO:  
            RUN piInitializaDBO.
            FOR EACH item-doc-est OF docum-est:  
                RUN piAtualizaNfAdc.                
            END.
            RUN piFinalizaDBO.
        END.
        /* End REV 003 */
    END. /* Gravar */

    IF lerro THEN DO:
        /* Atualizar documento na tela */
        RUN repositionRecord IN hRe1001 (INPUT rw-docum-est).
        RETURN "NOK".
    END.

    FIND tt-docum-est NO-LOCK NO-ERROR.

    FIND FIRST bdocum-est 
        WHERE ROWID(bdocum-est) = rw-docum-est NO-ERROR.


    ASSIGN i-cont = 0
           i-cont-de-para = 1.

    IF NOT p-rateio THEN 
    DO:

        FOR EACH tt-docum-est:
            FOR EACH tt-item-doc-est OF tt-docum-est NO-LOCK 
                  BY tt-item-doc-est.sequencia:
 
                    FIND FIRST natur-item-doc-est
                         WHERE natur-item-doc-est.serie-docto  = tt-item-doc-est.serie-docto 
                           AND natur-item-doc-est.nro-docto    = tt-item-doc-est.nro-docto   
                           AND natur-item-doc-est.cod-emitente = tt-item-doc-est.cod-emitente
                           AND natur-item-doc-est.nat-operacao = tt-item-doc-est.nat-operacao
                           AND natur-item-doc-est.sequencia    = tt-item-doc-est.sequencia       NO-ERROR.   
                    
                    RUN esp/pi-recalcula-re1001-ra.p (INPUT tt-item-doc-est.serie-docto, 
                                                      INPUT tt-item-doc-est.nro-docto, 
                                                      INPUT tt-item-doc-est.cod-emitente, 
                                                      INPUT tt-item-doc-est.nat-operacao,  
                                                      INPUT tt-item-doc-est.sequencia,
                                                      INPUT tt-item-doc-est.it-codigo,
                                                      INPUT tt-item-doc-est.qt-do-forn,
                                                      INPUT tt-item-doc-est.preco-unit[1],
                                                      INPUT tt-item-doc-est.preco-total[1],
                                                      INPUT tt-item-doc-est.desconto[1],  
                                                      INPUT tt-item-doc-est.despesas[1],  
                                                      INPUT tt-item-doc-est.pr-total-cmi, 
                                                      INPUT tt-item-doc-est.peso-liquido, 
                                                      INPUT tt-item-doc-est.aliquota-ipi,
                                                      INPUT natur-item-doc-est.cd-trib-ipi,  /*enviar o tipo da natureza atribuida depois da verificaá∆o */
                                                      INPUT tt-item-doc-est.aliquota-iss,
                                                      INPUT tt-item-doc-est.cd-trib-iss, 
                                                      INPUT tt-item-doc-est.aliquota-icm,
                                                      INPUT tt-item-doc-est.cd-trib-icm,
                                                      INPUT DEC(SUBSTR(tt-item-doc-est.char-2,1,6)),
                                                      INPUT tt-item-doc-est.log-2,
                                                      INPUT DEC(trim(SUBSTRING(tt-item-doc-est.char-2,7,6))) / 10000,
                                                      INPUT tt-item-doc-est.base-subs[1],
                                                      INPUT tt-item-doc-est.vl-subs[1]).
            END.
        END.
    END.
    ELSE DO:

        FOR EACH tt-docum-est:
        
            FOR EACH item-doc-est OF tt-docum-est NO-LOCK 
                  BY item-doc-est.sequencia:

                FIND FIRST btt-item-doc-est
                     WHERE btt-item-doc-est.serie-docto  = item-doc-est.serie-docto 
                       AND btt-item-doc-est.nro-docto    = item-doc-est.nro-docto   
                       AND btt-item-doc-est.cod-emitente = item-doc-est.cod-emitente
                       AND btt-item-doc-est.nat-operacao = item-doc-est.nat-operacao NO-ERROR.


                FIND FIRST natur-item-doc-est
                     WHERE natur-item-doc-est.serie-docto  = btt-item-doc-est.serie-docto 
                       AND natur-item-doc-est.nro-docto    = btt-item-doc-est.nro-docto   
                       AND natur-item-doc-est.cod-emitente = btt-item-doc-est.cod-emitente
                       AND natur-item-doc-est.nat-operacao = btt-item-doc-est.nat-operacao
                       AND natur-item-doc-est.sequencia    = btt-item-doc-est.sequencia NO-ERROR.   
                    
                    RUN esp/pi-recalcula-re1001-ra.p (INPUT item-doc-est.serie-docto, 
                                                      INPUT item-doc-est.nro-docto, 
                                                      INPUT item-doc-est.cod-emitente, 
                                                      INPUT item-doc-est.nat-operacao,  
                                                      INPUT item-doc-est.sequencia,
                                                      INPUT item-doc-est.it-codigo,
                                                      INPUT item-doc-est.qt-do-forn,
                                                      INPUT item-doc-est.preco-unit[1],
                                                      INPUT item-doc-est.preco-total[1],
                                                      INPUT item-doc-est.desconto[1],  
                                                      INPUT item-doc-est.despesas[1],  
                                                      INPUT btt-item-doc-est.pr-total-cmi, 
                                                      INPUT item-doc-est.peso-liquido, 
                                                      INPUT btt-item-doc-est.aliquota-ipi,
                                                      INPUT natur-item-doc-est.cd-trib-ipi,  /*enviar o tipo da natureza atribuida depois da verificaá∆o */
                                                      INPUT item-doc-est.aliquota-iss,
                                                      INPUT item-doc-est.cd-trib-iss, 
                                                      INPUT btt-item-doc-est.aliquota-icm,
                                                      INPUT btt-item-doc-est.cd-trib-icm,
                                                      INPUT DEC(SUBSTR(btt-item-doc-est.char-2,1,6)),
                                                      INPUT btt-item-doc-est.log-2,
                                                      INPUT DEC(trim(SUBSTRING(btt-item-doc-est.char-2,7,6))) / 10000,
                                                      INPUT item-doc-est.base-subs[1],
                                                      INPUT item-doc-est.vl-subs[1]).
            END.
        END.
    END.  
    
    
    FOR EACH tt-docum-est NO-LOCK:

       FIND FIRST docum-est OF tt-docum-est EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE(docum-est) THEN 
       DO:
           ASSIGN d-base = 0.
           /* Begins REV003 */
           RUN piInitializaDBO.
           FOR EACH item-doc-est OF docum-est:

               ASSIGN d-base = DECIMAL(SUBSTRING(item-doc-est.char-2,860,20)).

               RUN piAtualizaNfAdc.
               
           END.
           RUN piFinalizaDBO.
           /* End REV 003 */

           OVERLAY(docum-est.char-2,191,20,'character') = STRING(d-base,'>>>>>>9.99').
       END.
    END.

    FIND FIRST natur-oper  NO-LOCK
         WHERE natur-oper.nat-operacao = natur-item-doc-est.nat-operacao
           AND natur-oper.emite-duplic = TRUE NO-ERROR.    
    IF AVAIL(natur-oper) THEN
       RUN piGeraDuplicata.      
    /*Begins Rev004 : 31/08/2018 */
    RUN piAtualizaTotais.
    /* End Rev004: 31/08/2018 */

    /* Mostrar tela de totais */
    RUN pi-MostraTotais.   
/*     /* Continuar processo de atualizaá∆o padr∆o */ */
/*     APPLY "CHOOSE" TO hbtConf.                     */
END. /* p-ind-event = "UPCRE1001-btConf_esp" */

/* Controlar enable/disable do bot∆o */
IF p-ind-event  = "AFTER-CONTROL-TOOL-BAR" AND
   p-ind-object = "CONTAINER"              AND
   p-cod-table  = "docum-est"              THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF VALID-HANDLE(hbtConf_esp) THEN DO:
        ASSIGN hbtConf_esp:SENSITIVE = hbtConf:SENSITIVE
               hbtConf:TAB-STOP      = NO
               hbtConf:SENSITIVE     = NO
               .
        hbtConf_esp:MOVE-TO-TOP().
    END.
END. /*p-ind-event  = "AFTER-CONTROL-TOOL-BAR" */
/* Deletar relacionamento especifico */
IF p-ind-event  = "EventDelete" AND
   p-cod-table  = "docum-est"   AND
   p-row-table <> ?             THEN DO:

    FIND FIRST bdocum-est NO-LOCK
        WHERE ROWID(bdocum-est) = p-row-table NO-ERROR.
    IF AVAIL bdocum-est THEN DO:
        FOR EACH es-item-doc-est-natoper
            WHERE es-item-doc-est-natoper.ep-codigo = i-ep-codigo-usuario
              AND es-item-doc-est-natoper.serie-docto  = bdocum-est.serie-docto
              AND es-item-doc-est-natoper.nro-docto    = bdocum-est.nro-docto
              AND es-item-doc-est-natoper.cod-emitente = bdocum-est.cod-emitente
              AND es-item-doc-est-natoper.nat-operacao = bdocum-est.nat-operacao:
            DELETE es-item-doc-est-natoper.
        END.
    END.
END. /* p-ind-event  = "EventDelete" */

/* Tratar importaá∆o com sucesso do RA */
IF p-ind-event  = "SUCESSO-DANFE-RA" AND
   p-ind-object = "BT-REPOSICIONA"   AND
   p-cod-table  = "docum-est"        THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF VALID-HANDLE(hbtConf_esp) THEN DO:
        APPLY "CHOOSE" TO hbtConf_esp.
    END.
END. /*p-ind-event  = "AFTER-CONTROL-TOOL-BAR" */

RETURN "OK".
/**** END Main Block ****/
PROCEDURE pi-LocalizaCampo:
    DEFINE INPUT  PARAMETER ph_frame AS HANDLE     NO-UNDO.

    ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    blk_frame:
    DO WHILE ph_frame <> ?:
       IF ph_frame:TYPE <> "field-group" THEN DO:
          CASE ph_frame:TYPE:
             WHEN "frame" THEN DO:
                 IF  ph_frame:NAME = "fPage0" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).      

                 IF  ph_frame:NAME = "fPage6" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
/*                  IF ph_frame:NAME = "fPage2" THEN LEAVE blk_frame. */
             END.
             WHEN "FILL-IN" THEN DO:
                 IF ph_frame:NAME = "cod-emitente"  THEN ASSIGN hcod-emitente = ph_frame.
                 IF ph_frame:NAME = "serie-docto"   THEN ASSIGN hserie-docto  = ph_frame.
                 IF ph_frame:NAME = "nro-docto"     THEN ASSIGN hnro-docto    = ph_frame.
                 IF ph_frame:NAME = "nat-operacao"  THEN ASSIGN hnat-operacao = ph_frame.
             END.
             WHEN "BUTTON" THEN DO:
                 IF ph_frame:NAME = "btConf"      THEN ASSIGN hbtConf     = ph_frame.
                 IF ph_frame:NAME = "btConf_esp"  THEN ASSIGN hbtConf_esp = ph_frame.
                 IF ph_frame:NAME = "btDelete"    THEN ASSIGN hbtDelete   = ph_frame.
                 IF ph_frame:NAME = "btRateio"    THEN ASSIGN hbtRateio   = ph_frame.
             END.
             WHEN "SUB-MENU" THEN DO:
                 RUN pi-LocalizaCampo(INPUT ph_frame).
             END.
             WHEN "MENU-ITEM" THEN DO:
                 IF ph_frame:NAME = "m_Atualiza"  THEN ASSIGN hm_Atualiza = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */

PROCEDURE pi-MostraTotais:
        run rep/re1001a1.w persistent set hRe1001a1 (  input rw-docum-est,
                                                       input "UPDATE":U,
                                                       input hRe1001 ) no-error.

        run initializeInterface in hRe1001a1 no-error.
        &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN
        run setFolder in hRe1001a1(7) no-error.
        &else
        run setFolder in hRe1001a1(6) no-error.
        &endif
        wait-for close of hRe1001a1 focus hRe1001a1.
END PROCEDURE. /* pi-MostraTotais */


PROCEDURE pi-atrib-natoper:
    DEFINE INPUT  PARAMETER p-estab        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-it-codigo    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-cod-emitente AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER p-modelo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-cfop         AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-aliq-icm     AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER p-ben-icms     AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER p-ben-pisc     AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER l-rateio       AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER p-cfa          AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER p-natureza     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER p-oper         AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE i-cfop     AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE i-exc      AS INTEGER     NO-UNDO.

    FOR FIRST bestabelec FIELDS (estado pais ep-codigo)
        WHERE bestabelec.cod-estabel = p-estab NO-LOCK: END.
    FOR FIRST bunid-feder FIELDS(per-icms-int estado)
        WHERE bunid-feder.pais   = bestabelec.pais
          AND bunid-feder.estado = bestabelec.estado NO-LOCK:
    END.

    /* Determinar CFA */
/*     es-movto-ext-item-cfa:                                           */
/*     FOR EACH es-movto-ext-item-cfa                                   */
/*         WHERE es-movto-ext-item-cfa.it-codigo = p-it-codigo          */
/*           AND es-movto-ext-item-cfa.ep-codigo = bestabelec.ep-codigo */
/*         BY  es-movto-ext-item-cfa.identificador DESC                 */
/*         BY es-movto-ext-item-cfa.nr-seq :                            */
/*                                                                      */
/*         ASSIGN p-cfa = es-movto-ext-item-cfa.classe.                 */
/*         LEAVE es-movto-ext-item-cfa.                                 */
/*     END.                                                             */
    FOR FIRST ext-item-cfa FIELDS(classe)
        WHERE ext-item-cfa.it-codigo = p-it-codigo
          AND ext-item-cfa.ep-codigo = bestabelec.ep-codigo NO-LOCK:
        ASSIGN p-cfa = ext-item-cfa.classe.
    END.
    
    /* Operaá∆o */
    
    FOR FIRST bemitente FIELDS (estado pais)
        WHERE bemitente.cod-emitente = p-cod-emitente NO-LOCK: END.
    
    IF bemitente.pais     = "Brasil"         AND
       bestabelec.estado <> bemitente.estado THEN
        ASSIGN p-oper = "I".
    ELSE
        ASSIGN p-oper = "E".

/*es-natoper-rec.cod-cfa-aplic*/


    IF  p-oper = "I" AND 
       (LOOKUP(p-cfa,es-natoper-rec.cod-cfa-aplic,";") > 0 ) AND
        /* bno - 30/07/2015 */
        NOT LOGICAL (INDEX (es-natoper-rec.excec-difal-cfop,p-cfop))
        /* bno - 30/07/2015 */
        THEN DO:

        IF p-crt = 1 /* Simples Nacional*/ THEN DO:
            
            FOR FIRST beunid-feder FIELDS(per-icms-int est-exc perc-exc per-icms-ext)
                WHERE beunid-feder.pais   = bemitente.pais
                  AND beunid-feder.estado = bemitente.estado NO-LOCK:

                /*ASSIGN p-aliq-icm = beunid-feder.per-icms-int.*/

                DO i-exc = 1 TO 25:
                    IF beunid-feder.est-exc[i-exc] = bestabelec.estado /*bunid-feder.estado*/ THEN
                        ASSIGN p-aliq-icm = beunid-feder.perc-exc[i-exc].
                END.

                IF p-aliq-icm = 0 THEN
                    ASSIGN p-aliq-icm = beunid-feder.per-icms-ext.

            END.



        END.
        ELSE DO:
            IF p-aliq-icm = 0 THEN DO:
                FOR FIRST beunid-feder FIELDS(per-icms-int est-exc perc-exc per-icms-ext)
                WHERE beunid-feder.pais   = bemitente.pais
                  AND beunid-feder.estado = bemitente.estado NO-LOCK:

                /*ASSIGN p-aliq-icm = beunid-feder.per-icms-int.*/

                DO i-exc = 1 TO 25:

                    IF beunid-feder.est-exc[i-exc] = bestabelec.estado /*bunid-feder.estado*/ THEN
                        ASSIGN p-aliq-icm = beunid-feder.perc-exc[i-exc].
                END.

                IF p-aliq-icm = 0 THEN
                    ASSIGN p-aliq-icm = beunid-feder.per-icms-ext.

                END.
            END.
        END.
        
        IF p-aliq-icm = 0 THEN
            ASSIGN d-aliq-icm = 0.
        ELSE
            ASSIGN d-aliq-icm = ABS(bunid-feder.per-icms-int - p-aliq-icm).

    END.
    ELSE 
        ASSIGN d-aliq-icm = 0.

    EMPTY TEMP-TABLE tt-natoper.
    es-rec-atrib-natoper:
    FOR EACH es-rec-atrib-natoper
        WHERE es-rec-atrib-natoper.ep-codigo = i-ep-codigo-usuario
        NO-LOCK:
        CREATE tt-natoper.
        ASSIGN tt-natoper.nat-operacao = es-rec-atrib-natoper.nat-operacao.

        /* Estabelecimento */
        IF NOT es-rec-atrib-natoper.estab-todos THEN DO: 
           IF es-rec-atrib-natoper.cod-estabel <> p-estab  THEN DO:
                ASSIGN tt-natoper.l-achou = NO
                       tt-natoper.motivo  = "Estab".
                NEXT es-rec-atrib-natoper.
            END.
            ELSE
                ASSIGN tt-natoper.regras = tt-natoper.regras + 1.
        END.

        /* CFA */
        IF NOT es-rec-atrib-natoper.classe-todos THEN DO:
/*            IF es-rec-atrib-natoper.classe <> p-cfa  THEN DO: */
           IF LOOKUP(p-cfa,es-rec-atrib-natoper.classe,";") = 0  THEN DO:
                ASSIGN tt-natoper.l-achou = NO
                       tt-natoper.motivo  = "CFA" + p-cfa + " not in " + es-rec-atrib-natoper.classe.
                NEXT es-rec-atrib-natoper.
            END.
            ELSE
                ASSIGN tt-natoper.regras = tt-natoper.regras + 1.
        END.

        /* Operaá∆o */
        IF es-rec-atrib-natoper.operacao <> p-oper  THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "OPERACAO".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.

        /* Benef°cio ICMS */
        IF es-rec-atrib-natoper.nenhum-ben-icms      AND
           p-ben-icms                           <> 0 THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "Ben ICMS Nen".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE IF NOT es-rec-atrib-natoper.nenhum-ben-icms              AND
                es-rec-atrib-natoper.cod-beneficio-icms <> p-ben-icms THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "Ben ICMS <>".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.

        /* Benef°cio PIS/COFINS */
        IF es-rec-atrib-natoper.nenhum-ben-piscof    AND
           p-ben-pisc                           <> 0 THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "Ben PIS Nen".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE IF NOT es-rec-atrib-natoper.nenhum-ben-piscof              AND
                es-rec-atrib-natoper.cod-beneficio-piscof <> p-ben-pisc THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "Ben PIS <>".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.

        /* Modelo */
        IF NOT es-rec-atrib-natoper.model-todos THEN DO:
           IF es-rec-atrib-natoper.cod-model-nf-eletro <> p-modelo  THEN DO:
                ASSIGN tt-natoper.l-achou = NO
                       tt-natoper.motivo  = "Model".
                NEXT es-rec-atrib-natoper.
            END.
            ELSE
                ASSIGN tt-natoper.regras = tt-natoper.regras + 1.
        END.

        /* CFOP Sa°da */
        IF es-rec-atrib-natoper.nenhum-cfop       AND
           TRIM(p-cfop)                     <> "" THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "CFOP Nen".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE IF NOT es-rec-atrib-natoper.nenhum-cfop             AND
                LOOKUP(p-cfop,es-rec-atrib-natoper.cod-cfop,";") = 0 THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "CFOP Dif" + STRING(LOOKUP(p-cfop,es-rec-atrib-natoper.cod-cfop)) + "-" + p-cfop + "," + es-rec-atrib-natoper.cod-cfop.
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.

        /* Diferencial de Aliquota ICMS  */
        IF es-rec-atrib-natoper.dif-aliq-icm <> d-aliq-icm  THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "DIF ICM".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.


        IF es-rec-atrib-natoper.l-rateio <> l-rateio THEN DO:
            ASSIGN tt-natoper.l-achou = NO
                   tt-natoper.motivo  = "Rateio False".
            NEXT es-rec-atrib-natoper.
        END.
        ELSE
            ASSIGN tt-natoper.regras = tt-natoper.regras + 1.


    END.

    FOR FIRST tt-natoper USE-INDEX atende
        WHERE tt-natoper.l-achou = YES:
        ASSIGN p-natureza = tt-natoper.nat-operacao.
    END.

END PROCEDURE. /* pi-atrib-natoper */


PROCEDURE piGeraDuplicata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def var rw-docum-est as rowid no-undo.
    DEFINE VARIABLE l-duplicatas AS LOGICAL     NO-UNDO.

    DEFINE BUFFER bnatur-oper FOR natur-oper.

    ASSIGN l-autom-retenc = CAN-FIND(FIRST funcao WHERE 
                                           funcao.cd-funcao = "spp-aut-impto":U AND
                                           funcao.ativo).

    /*--- seta o cursor no mouse para espera ---*/
    session:set-wait-state("general":U).

    /*--- recupera o rowid do docum-est corrente ---*/
    FOR EACH tt-docum-est:

        FOR FIRST bdocum-est FIELDS(nat-operacao)
            WHERE bdocum-est.serie-docto  = tt-docum-est.serie-docto
              AND bdocum-est.nro-docto    = tt-docum-est.nro-docto  
              AND bdocum-est.cod-emitente = tt-docum-est.cod-emitente
              AND bdocum-est.nat-operacao = tt-docum-est.nat-operacao NO-LOCK,
            FIRST bnatur-oper 
            WHERE bnatur-oper.nat-operacao = bdocum-est.nat-operacao
              AND bnatur-oper.emite-duplic = TRUE NO-LOCK:
        END.
        IF NOT AVAIL bdocum-est THEN
            NEXT.
        ASSIGN rw-docum-est = ROWID(bdocum-est).

        /*--- cria duplicata a partir do documento corrente ---*/
        
        run rep/re9341.p (input rw-docum-est, 
                          input no).
        
        /*--- seta o cursor do mouse para normal ---*/
        session:set-wait-state("":U).
    
        /* Automaá∆o das retená‰es no recebimento */
        EMPTY TEMP-TABLE tt-dupli-apagar.
        ASSIGN l-duplicatas = NO.
        FOR EACH bdupli-apagar OF 
            tt-docum-est NO-LOCK:
            CREATE tt-dupli-apagar.
            BUFFER-COPY bdupli-apagar TO tt-dupli-apagar
                ASSIGN tt-dupli-apagar.r-rowid = ROWID(bdupli-apagar).
            ASSIGN l-duplicatas = TRUE.
        END.

        if l-duplicatas   and 
           l-autom-retenc then do:
           run piGeraImpto.
        end.
    END. /* EACH tt-docum-es */
    return "OK":U.
END PROCEDURE.

PROCEDURE piGeraImpto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    session:set-wait-state("general":U).

    empty temp-table RowErrors.

    run rep/re9343.p (input table tt-dupli-apagar, 
                      output table RowErrors).

    session:set-wait-state("":U).

    if can-find(first RowErrors
                where RowErrors.ErrorType <> "INTERNAL":U) then do:
       {method/showmessage.i1}
       {method/showmessage.i2 &Modal=YES}
    end.
    ELSE
    DO:

        

        FIND FIRST bdocum-estImpto WHERE 
                   ROWID(bdocum-estImpto) = rw-docum-est EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL bdocum-estImpto THEN
           return "OK":U.


        

        ASSIGN SUBSTRING(bdocum-estImpto.char-2,256,1) = "N".
        
        FOR EACH tt-dupli-apagar:

            /* Begins: 09/08/2018  - Quando n∆o houver imposto atrelado a duplicata dever† desmarcar o flag de reinf */ 
            IF CAN-FIND(FIRST dupli-imp OF tt-dupli-apagar WHERE 
                              dupli-imp.cod-retencao = 2631) THEN
               ASSIGN SUBSTRING(bdocum-estImpto.char-2,256,1) = "S".               
            /* End 09/08/2018 */   

        END.

        

        RELEASE bdocum-estImpto.
    END.


return "OK":U.

END PROCEDURE.

/*Begins 10/05/2018 - Para evitar o bloqueio de saldo nos contratos de mediá∆o dever† liberar o saldo do contrato de mediá∆o neste ponto */         
PROCEDURE pi-atualiza-sd-medicao:
    
    FIND FIRST ordem-compra WHERE
               ordem-compra.numero-ordem = bitem-doc-est.numero-ordem NO-LOCK NO-ERROR.
    IF AVAIL ordem-compra THEN
       FIND FIRST contrato-for where
                  contrato-for.nr-contrato = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
    IF AVAIL contrato-for THEN
    DO:

        if contrato-for.ind-control-rec = 2 then
        do:            
            find item-contrat
                where item-contrat.num-seq-item = ordem-compra.num-seq-item
                  and item-contrat.nr-contrato = ordem-compra.nr-contrato
                no-lock no-error.
        end.

        if contrato-for.ind-control-rec = 2 and avail item-contrat then
        do:
            for each medicao-contrat use-index dat-prev
               where medicao-contrat.numero-ordem  = ordem-compra.numero-ordem
                 and medicao-contrat.ind-sit-medicao = 2
                 and medicao-contrat.dat-prev-medicao <= bdocum-est.dt-emissao
                 and medicao-contrat.log-rec-medicao = NO EXCLUSIVE-LOCK:

                ASSIGN medicao-contrat.val-sdo-aloc-med = medicao-contrat.val-sdo-aloc-med - bitem-doc-est.preco-total[1].

            END.
        END.
    END.
END PROCEDURE.
/* end 10/05/2018 */

PROCEDURE piAtualizaNfAdc:    
    FIND FIRST nota-fisc-adc WHERE
               nota-fisc-adc.cod-estab        = docum-est.cod-estabel  AND
               nota-fisc-adc.cod-serie        = docum-est.serie-docto  AND
               nota-fisc-adc.cod-nota-fisc    = docum-est.nro-docto    AND
               nota-fisc-adc.cdn-emitente     = docum-est.cod-emitente AND 
               nota-fisc-adc.cod-natur-operac = docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fisc-adc THEN NEXT.

    RUN goToKeyDoctoRef IN h-bodi515 (INPUT docum-est.cod-estabel , 
                                      INPUT docum-est.serie-docto , 
                                      INPUT docum-est.nro-docto   ,
                                      INPUT docum-est.cod-emitente,
                                      INPUT docum-est.nat-operacao,
                                      INPUT 27,
                                      INPUT "",
                                      INPUT "",
                                      INPUT 0).

    IF RETURN-VALUE = "OK":U THEN 
    DO:
       RUN goToKey IN h-bodi516 (INPUT docum-est.cod-estabel        ,  /* cod-estab           */
                                 input docum-est.serie-docto        ,  /* cod-serie           */
                                 input docum-est.nro-docto          ,  /* cod-nota-fisc       */
                                 input docum-est.cod-emitente       ,  /* cdn-emitente        */
                                 input item-doc-est.nat-operacao    ,  /* cod-natur-operac    */
                                 input 27                           ,  /* idi-tip-dado        */
                                 input item-doc-est.sequencia       ,  /* num-seq             */
                                 input 0                            ,  /* num-seq-item-nf     */
                                 input item-doc-est.it-codigo       ). /* cod-item            */
       IF RETURN-VALUE = "NOK":U THEN
       DO:
          FIND FIRST item-nf-adc WHERE
                     item-nf-adc.cod-serie      = item-doc-est.serie-docto  AND
                     item-nf-adc.cod-nota-fisc  = item-doc-est.nro-docto    AND
                     item-nf-adc.cod-item       = item-doc-est.it-codigo    AND
                     item-nf-adc.cdn-emitente   = item-doc-est.cod-emitente AND
                     item-nf-adc.num-seq        = item-doc-est.sequencia    EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL item-nf-adc                                              AND
             item-nf-adc.cod-natur-operac <> nota-fisc-adc.cod-natur-operac THEN
          DO:
             ASSIGN item-nf-adc.cod-natur-operac = nota-fisc-adc.cod-natur-operac.
             RELEASE item-nf-adc.
          END.
       END.
    END.    
END PROCEDURE.


PROCEDURE piInitializaDBO:

    RUN dibo/bodi515.p PERSISTENT SET h-bodi515.
    RUN openQueryStatic IN h-bodi515 (INPUT "Main":U).

    RUN dibo/bodi516.p PERSISTENT SET h-bodi516.
    RUN openQueryStatic IN h-bodi516 (INPUT "Main":U).
END.

PROCEDURE piFinalizaDBO:
    if  valid-handle( h-bodi515 ) then do:
        delete procedure h-bodi515.
        assign h-bodi515 = ?.
    end.

    if  valid-handle( h-bodi516 ) then do:
        delete procedure h-bodi516.
        assign h-bodi516 = ?.
    end.   
END PROCEDURE.


PROCEDURE piAtualizaTotais:    

    FIND FIRST docum-est WHERE 
         ROWID(docum-est) = rw-docum-est NO-LOCK NO-ERROR.
    IF NOT AVAIL docum-est THEN NEXT.


    RUN inbo/boin176.p PERSISTENT SET h_boin176.
    RUN inbo/boin090.p PERSISTENT SET h_boin090.

    RUN openQueryStatic IN h_boin176 ("Main":U).
    RUN openQueryStatic IN h_boin090 ("Main":U).

    FOR EACH bf-item-doc-est OF docum-est EXCLUSIVE-LOCK:   

        FOR FIRST tt-itens
            WHERE tt-itens.serie-docto  = bf-item-doc-est.serie-docto   
              AND tt-itens.nro-docto    = bf-item-doc-est.nro-docto     
              AND tt-itens.cod-emitente = bf-item-doc-est.cod-emitente  
              AND tt-itens.nat-operacao = bf-item-doc-est.nat-operacao  
              AND tt-itens.sequencia    = bf-item-doc-est.sequencia   NO-LOCK:
                           
            IF bf-item-doc-est.valor-icm[1] = 0 THEN
               ASSIGN bf-item-doc-est.valor-icm[1] = tt-itens.d-Vicms 
                      bf-item-doc-est.valor-icm[2] = tt-itens.d-Vicms. 
        END.

        RELEASE bf-item-doc-est.
    END.

    FOR EACH bf-item-doc-est OF docum-est NO-LOCK:  

        RUN setHandleDocumEst      IN h_boin176 (INPUT h_boin090).
        RUN findDocumento          IN h_boin176 (bf-item-doc-est.cod-emitente,
                                                 bf-item-doc-est.serie-docto,
                                                 bf-item-doc-est.nro-docto,
                                                 bf-item-doc-est.nat-operacao).   

        RUN getTotalizaNota        IN h_boin176 (INPUT 1).
        RUN transferTotalItensNota IN h_boin176 (INPUT bf-item-doc-est.cod-emitente,
                                                 INPUT bf-item-doc-est.serie-docto,
                                                 INPUT bf-item-doc-est.nro-docto,
                                                 INPUT bf-item-doc-est.nat-operacao).

        RUN validateValues IN h_boin090.

        RUN calculateTotalItem IN h_boin176(INPUT bf-item-doc-est.cod-emitente,
                                            INPUT bf-item-doc-est.serie-docto,
                                            INPUT bf-item-doc-est.nro-docto,
                                            INPUT bf-item-doc-est.nat-operacao,
                                            OUTPUT TABLE tt-total-item).

        find first tt-total-item no-lock no-error.

        run setTotaisNota in hBoin090  ( input table tt-total-item,
                                         input 0 ) /*piTotaliza = 0 or (piTotaliza = 1 and RowObject.tot-valor = 0)) */.

    END.

    IF VALID-HANDLE(h_boin176) THEN DELETE PROCEDURE h_boin176.
    IF VALID-HANDLE(h_boin090) THEN DELETE PROCEDURE h_boin090.

END PROCEDURE.
