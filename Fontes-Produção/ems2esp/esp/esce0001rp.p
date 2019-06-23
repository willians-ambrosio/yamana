&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** Descricao: Relatorio de Atendimento de Requisicao
** Autor: Gilberto Rissati - Datasul - GWA
** Data: 08/10/2003
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESCE0001RP 2.06.00.004}

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)".
def var c-liter-sel                  as character format "x(10)".
def var c-liter-imp                  as character format "x(12)".    
def var c-destino                    as character format "x(15)".

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as integer
    FIELD cod-estabel-ini   AS CHAR
    FIELD cod-estabel-fim   AS CHAR
    FIELD nr-requisicao-ini AS INTEGER
    FIELD nr-requisicao-fim AS INTEGER
    FIELD dt-requisicao-ini AS DATE
    FIELD dt-requisicao-fim AS DATE
    FIELD nome-abrev-ini    AS CHAR
    FIELD nome-abrev-fim    AS CHAR
    FIELD tb-aprovadas      AS LOGICAL
    FIELD tb-naoaprovadas   AS LOGICAL
    FIELD tb-abertas        AS LOGICAL
    FIELD tb-fechadas       AS LOGICAL
    field classifica        as integer
    FIELD desc-classifica   AS CHARACTER
    FIELD imp-param         AS LOGICAL.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
          ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.


DEFINE TEMP-TABLE tt_requisicao 
   FIELD nr-requisicao  LIKE requisicao.nr-requisicao           
   FIELD nome-abrev     LIKE requisicao.nome-abrev                
   FIELD dt-requisicao  LIKE requisicao.dt-requisicao           
   FIELD nome-usuar     LIKE usuar-mater.nome-usuar             
   FIELD loc-entrega    LIKE requisicao.loc-entrega             
   FIELD estado         LIKE requisicao.estado             
   FIELD cod-estabel    LIKE requisicao.cod-estabel
   INDEX idx-requisicao AS PRIMARY UNIQUE nr-requisicao
   INDEX idx-requisitante nome-abrev
                          nr-requisicao.


DEFINE TEMP-TABLE tt_it-requisicao
   FIELD sequencia      LIKE it-requisicao.sequencia 
   FIELD it-codigo      LIKE it-requisicao.it-codigo   
   FIELD qt-a-atender   LIKE it-requisicao.qt-a-atender
   FIELD preco-unit     LIKE it-requisicao.preco-unit  
   FIELD dt-entrega     LIKE it-requisicao.dt-entrega  
   FIELD cod-depos      LIKE it-requisicao.cod-depos
   FIELD cod-localiz    LIKE it-requisicao.cod-localiz 
   FIELD sc-codigo      LIKE it-requisicao.sc-codigo   
   FIELD ct-codigo      LIKE it-requisicao.ct-codigo
   FIELD conta-contabil LIKE it-requisicao.conta-contabil
   FIELD un             LIKE ITEM.un                   
   FIELD descricao      AS CHAR FORMAT "x(36)"           
   FIELD qtidade-atu    LIKE saldo-estoq.qtidade-atu
   FIELD nr-requisicao  LIKE requisicao.nr-requisicao
   INDEX idx-it-requisicao AS PRIMARY UNIQUE nr-requisicao
                                             sequencia
                                             it-codigo
                                             cod-localiz
                                             cod-depos
                                             conta-contabil.

FORM tt_it-requisicao.sequencia      COLUMN-LABEL "Seq" FORMAT ">>9"
     tt_it-requisicao.it-codigo      COLUMN-LABEL "Item Supr."      FORMAT "x(10)"
     tt_it-requisicao.descricao      COLUMN-LABEL "Descri‡Æo"       FORMAT "x(32)"
     tt_it-requisicao.un             COLUMN-LABEL "Un"
     tt_it-requisicao.qt-a-atender   COLUMN-LABEL "Qtde a Atender"  FORMAT ">,>>>,>>9.9999" 
     tt_it-requisicao.preco-unit     COLUMN-LABEL "Vlr. a Atender"  FORMAT ">,>>>,>>9.9999"
     tt_it-requisicao.dt-entrega     COLUMN-LABEL "Dt.Entrega"
     tt_it-requisicao.cod-depos      COLUMN-LABEL "Dep"
     tt_it-requisicao.cod-localiz    COLUMN-LABEL "Localiz."
     tt_it-requisicao.conta-contabil COLUMN-LABEL "Conta Contabil"
     tt_it-requisicao.qtidade-atu    COLUMN-LABEL "Qtde em Estoque" FORMAT ">>>,>>>,>>9.99"
     WITH FRAME f-it-requisicao STREAM-IO NO-BOX WIDTH 220 DOWN.
/* unica que estava 180 */

DEFINE BUFFER b_tt_it-requisicao FOR tt_it-requisicao.

DEFINE VARIABLE de-qtidade-atu LIKE saldo-estoq.qtidade-atu  NO-UNDO.

DEFINE VARIABLE ch-estado AS CHARACTER INIT "Aprovada,NÆo Aprovada" NO-UNDO.


FORM tt_requisicao.nr-requisicao LABEL "N£mero"
     tt_requisicao.dt-requisicao LABEL "Data de EmissÆo"  AT 30
     ch-estado FORMAT "x(10)"    LABEL "Estado"           AT 80 SKIP(1)
     tt_requisicao.loc-entrega   LABEL "Local de Entrega" SKIP(1)
     tt_requisicao.nome-abrev    LABEL "Requisitante" 
     tt_requisicao.nome-usuar    NO-LABEL SKIP(2)
     WITH FRAME f-requisitante  STREAM-IO NO-BOX SIDE-LABEL WIDTH 220.

FORM SKIP(1)
     "SELE€ÇO: " SKIP(1)
     tt-param.cod-estabel-ini   LABEL "Estabelecimento"
     tt-param.cod-estabel-fim   LABEL "at‚"              AT 32 SKIP

     tt-param.nr-requisicao-ini LABEL "Requisi‡Æo"       AT 06
     tt-param.nr-requisicao-fim LABEL "at‚"              AT 32 SKIP
     tt-param.dt-requisicao-ini LABEL "Data Requisi‡Æo"  AT 01 FORMAT "99/99/9999"
     tt-param.dt-requisicao-fim LABEL "at‚"              AT 32 FORMAT "99/99/9999" SKIP
     tt-param.nome-abrev-ini    LABEL "Requisitante"     AT 04
     tt-param.nome-abrev-fim    LABEL "ate"              AT 32 SKIP(1)
     "PAR¶METROS:"                                       SKIP(1)
     tt-param.tb-aprovadas      LABEL "Aprovadas"        AT 07 FORMAT "Sim/NÆo" SKIP
     tt-param.tb-naoaprovadas   LABEL "NÆo Aprovadas"    AT 03 FORMAT "Sim/NÆo" SKIP
     tt-param.tb-abertas        LABEL "Abertas"          AT 09 FORMAT "Sim/NÆo" SKIP
     tt-param.tb-fechadas       LABEL "Fechadas"         AT 08 FORMAT "Sim/NÆo" SKIP(1)
     "CLASSIFICA€ÇO:"                                    SKIP(1)
     tt-param.desc-classifica   LABEL "Classif. por"     AT 04 FORMAT "x(20)" SKIP
     WITH FRAME f-parametros STREAM-IO NO-BOX SIDE-LABEL WIDTH 220.

def var h-acomp         as handle no-undo.

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 220 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 220 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.


for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*FIND FIRST tt-param NO-LOCK NO-ERROR.*/

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR¶METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELE€ÇO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÇO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa NO-LOCK
     WHERE empresa.ep-codigo = param-global.empresa-prin
     NO-ERROR.

{utp/ut-liter.i Pre‡o Venda * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = "Atendimento de Requisi‡Æo - V04".
assign c-empresa     = "Empresa - " + empresa.razao
       c-programa    = "ESCE0001"
       c-versao      = "2.00"
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}
    .
{include/i-rpcab.i &WIDTH=150} /* 256 */

FORM HEADER
    FILL("-", 154) FORMAT "x(154)" SKIP
    c-empresa c-titulo-relat FORMAT "x(60)" AT 60 "Pagina:" AT 143 PAGE-NUMBER AT 151 FORMAT ">>>9" SKIP
    FILL("-", 132) FORMAT "x(132)" TODAY FORMAT "99/99/9999" "-" STRING(time,"HH:MM:SS") SKIP
    WITH WIDTH 220 NO-LABELS NO-BOX PAGE-TOP FRAME f-cabec-255 STREAM-IO.
    
ASSIGN c-rodape = "DATASUL - " + c-programa + " - V:" + c-versao + c-revisao
       c-rodape = FILL("-", 141 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER

     FILL("-", 141) FORMAT "x(141)" SKIP(2)
     /**
     c-rodape FORMAT "x(150)" SKIP(2)     
     FILL("-", 128) FORMAT "x(128)" TODAY FORMAT "99/99/9999" "-" STRING(time,"HH:MM:SS") SKIP(2)
     **/
     WITH WIDTH 220 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape STREAM-IO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 5.17
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/* {include/i-rpcab.i &WIDTH=256} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


do on stop undo, LEAVE:
    {include/i-rpout.i}
    view frame f-cabec-255.
    view frame f-rodape.
    run utp/ut-acomp.p persistent set h-acomp.  
    
    run pi-inicializar in h-acomp (input "Verificando"). 
    
    /* --- Colocar aqui o codigo de impressão --- */

    FOR EACH requisicao NO-LOCK
       WHERE requisicao.nr-requisicao >= tt-param.nr-requisicao-ini
         AND requisicao.nr-requisicao <= tt-param.nr-requisicao-fim
         AND requisicao.cod-estabel   >= tt-param.cod-estabel-ini
         AND requisicao.cod-estabel   <= tt-param.cod-estabel-fim
         AND requisicao.dt-requisicao >= tt-param.dt-requisicao-ini
         AND requisicao.dt-requisicao <= tt-param.dt-requisicao-fim
         AND requisicao.nome-abrev    >= tt-param.nome-abrev-ini
         AND requisicao.nome-abrev    <= tt-param.nome-abrev-fim
         AND requisicao.tp-requis      = 1: /* requisicao de estoque */

       IF (requisicao.estado = 1   AND tt-param.tb-aprovadas    = NO) OR 
          (requisicao.estado = 2   AND tt-param.tb-naoaprovadas = NO) OR
          (requisicao.situacao = 1 AND tt-param.tb-abertas      = NO) OR
          (requisicao.situacao = 2 AND tt-param.tb-fechadas     = NO) THEN NEXT.

       FIND FIRST usuar-mater NO-LOCK
            WHERE usuar-mater.cod-usuario = requisicao.nome-abrev
              NO-ERROR.

       CREATE tt_requisicao.
       ASSIGN tt_requisicao.nr-requisicao = requisicao.nr-requisicao
              tt_requisicao.cod-estabel   = requisicao.cod-estabel  
              tt_requisicao.dt-requisicao = requisicao.dt-requisicao
              tt_requisicao.nome-abrev    = requisicao.nome-abrev
              tt_requisicao.estado        = requisicao.estado
              tt_requisicao.loc-entrega   = requisicao.loc-entrega
              tt_requisicao.nome-usuar    = usuar-mater.nome-usuar.

       RUN pi-acompanhar in h-acomp (input "Requisi‡Æo: " + STRING(requisicao.nr-requisicao)).

       /*
       MESSAGE "requisicao.estado   " requisicao.estado SKIP
               "requisicao.situacao " requisicao.situacao SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
       */

       FOR EACH it-requisicao NO-LOCK
          WHERE it-requisicao.nr-requisicao = requisicao.nr-requisicao
           AND  it-requisicao.qt-a-atender  > 0:

          FIND FIRST ITEM NO-LOCK
               WHERE ITEM.it-codigo = it-requisicao.it-codigo
               NO-ERROR.

          FOR EACH saldo-estoq NO-LOCK
             WHERE saldo-estoq.cod-estabel = it-requisicao.cod-estabel
               AND saldo-estoq.it-codigo   = it-requisicao.it-codigo
               AND saldo-estoq.qtidade-atu > 0:

             FIND FIRST tt_it-requisicao
                  WHERE tt_it-requisicao.sequencia      = it-requisicao.sequencia
                    AND tt_it-requisicao.nr-requisicao  = requisicao.nr-requisicao
                    AND tt_it-requisicao.cod-depos      = saldo-estoq.cod-depos
                    AND tt_it-requisicao.cod-localiz    = saldo-estoq.cod-localiz
                    AND tt_it-requisicao.conta-contabil = TRIM(it-requisicao.ct-codigo) + "." + TRIM(it-requisicao.sc-codigo)
                    AND tt_it-requisicao.it-codigo      = saldo-estoq.it-codigo
                    NO-ERROR.
             IF NOT AVAIL tt_it-requisicao THEN DO:
                CREATE tt_it-requisicao.
                ASSIGN tt_it-requisicao.nr-requisicao  = requisicao.nr-requisicao
                       tt_it-requisicao.sequencia      = it-requisicao.sequencia
                       tt_it-requisicao.cod-depos      = saldo-estoq.cod-depos
                       tt_it-requisicao.cod-localiz    = saldo-estoq.cod-localiz
                       tt_it-requisicao.it-codigo      = it-requisicao.it-codigo   
                       tt_it-requisicao.qt-a-atender   = it-requisicao.qt-a-atender
                       tt_it-requisicao.preco-unit     = (it-requisicao.preco-unit * it-requisicao.qt-a-atender)
                       tt_it-requisicao.dt-entrega     = it-requisicao.dt-entrega  
                       tt_it-requisicao.sc-codigo      = it-requisicao.sc-codigo
                       tt_it-requisicao.ct-codigo      = it-requisicao.ct-codigo
                       tt_it-requisicao.conta-contabil = TRIM(it-requisicao.ct-codigo) + "." + TRIM(it-requisicao.sc-codigo)
                       tt_it-requisicao.un             = ITEM.un
                       tt_it-requisicao.descricao      = ITEM.descricao-1 + ITEM.descricao-2.
                ASSIGN tt_it-requisicao.qtidade-atu    = saldo-estoq.qtidade-atu.
             END.
             
          END.
       END.

       FIND FIRST tt_it-requisicao NO-LOCK
            WHERE tt_it-requisicao.nr-requisicao = tt_requisicao.nr-requisicao
            NO-ERROR.
       IF NOT AVAIL tt_it-requisicao THEN DO:
          DELETE tt_requisicao.
       END.
    END.

    FIND FIRST tt_requisicao NO-LOCK NO-ERROR.
    IF AVAIL tt_requisicao THEN
       RUN MostraDados.

    RUN pi-finalizar in h-acomp.
    {include/i-rpclo.i}
end.

PROCEDURE MostraDados:
   IF tt-param.classifica = 1 THEN DO:   
      FOR EACH tt_requisicao USE-INDEX idx-requisicao NO-LOCK
         BREAK
            BY tt_requisicao.nr-requisicao:
         IF FIRST-OF(tt_requisicao.nr-requisicao) THEN
            PAGE.

         RUN Mostra_Item.
      END.
   END.
   ELSE
      FOR EACH tt_requisicao USE-INDEX idx-requisitante NO-LOCK
         BREAK
            BY tt_requisicao.nome-abrev
            BY tt_requisicao.nr-requisicao:
         IF FIRST-OF(tt_requisicao.nr-requisicao) THEN
            PAGE.

         RUN Mostra_Item.
      END.
   IF tt-param.imp-param THEN
      RUN pi_Parametros.

END PROCEDURE.

PROCEDURE Mostra_Item:

   DEFINE VARIABLE de-qt-a-atender LIKE it-requisicao.qt-a-atender    NO-UNDO.
   DEFINE VARIABLE lc-c-conta      AS CHARACTER   NO-UNDO.

   DISP tt_requisicao.nr-requisicao
        tt_requisicao.dt-requisicao
        WITH FRAME f-requisitante.
  
   DISP ENTRY(tt_requisicao.estado,ch-estado) @ ch-estado
        WITH FRAME f-requisitante.
  
   DISP tt_requisicao.loc-entrega
        tt_requisicao.nome-abrev 
        tt_requisicao.nome-usuar 
        WITH FRAME f-requisitante.
  
   ASSIGN lc-c-conta = "".

   FOR EACH tt_it-requisicao NO-LOCK
      WHERE tt_it-requisicao.nr-requisicao = tt_requisicao.nr-requisicao
      BREAK         
         BY tt_it-requisicao.sequencia
         BY tt_it-requisicao.it-codigo
         BY tt_it-requisicao.cod-depos
         BY tt_it-requisicao.cod-localiz
         BY tt_it-requisicao.conta-contabil
         BY tt_it-requisicao.dt-entrega:
  
      IF FIRST-OF(tt_it-requisicao.it-codigo) THEN DO:
          DISP tt_it-requisicao.sequencia
               tt_it-requisicao.it-codigo
               tt_it-requisicao.un   
               tt_it-requisicao.qt-a-atender
               tt_it-requisicao.preco-unit 
               tt_it-requisicao.dt-entrega
               tt_it-requisicao.descricao
               tt_it-requisicao.conta-contabil
               WITH FRAME f-it-requisicao.
          ASSIGN lc-c-conta = tt_it-requisicao.conta-contabil.
      END.
      ELSE DO:
          IF lc-c-conta <> tt_it-requisicao.conta-contabil THEN DO:
              DISP tt_it-requisicao.conta-contabil
                  WITH FRAME f-it-requisicao.
          END.
          ELSE.
      END.

      IF FIRST-OF(tt_it-requisicao.cod-depos) THEN
          DISP tt_it-requisicao.cod-depos
               WITH FRAME f-it-requisicao.

      IF FIRST-OF(tt_it-requisicao.cod-localiz) THEN
         DISP tt_it-requisicao.cod-localiz
              tt_it-requisicao.qtidade-atu
              WITH FRAME f-it-requisicao.
  
      ASSIGN de-qt-a-atender = de-qt-a-atender + tt_it-requisicao.qt-a-atender.
      IF LAST-OF(tt_it-requisicao.it-codigo) THEN DO:
/*          PUT "TOTAL............." AT 6 de-qt-a-atender TO 39 SKIP. */
         ASSIGN de-qt-a-atender = 0.
         ASSIGN lc-c-conta = "".
      END.
      DOWN WITH FRAME f-it-requisicao.
   END.

END PROCEDURE.

PROCEDURE Pi_Parametros:

   PAGE.
   DISP tt-param.cod-estabel-ini  
        tt-param.cod-estabel-fim  
        tt-param.nr-requisicao-ini
        tt-param.nr-requisicao-fim
        tt-param.dt-requisicao-ini
        tt-param.dt-requisicao-fim
        tt-param.nome-abrev-ini   
        tt-param.nome-abrev-fim   
        tt-param.tb-aprovadas     
        tt-param.tb-naoaprovadas  
        tt-param.tb-abertas       
        tt-param.tb-fechadas      
        tt-param.desc-classifica  
        WITH FRAME f-parametros.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


