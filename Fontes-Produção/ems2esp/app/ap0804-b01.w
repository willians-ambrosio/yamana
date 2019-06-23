&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda for ems2cadme.moeda.

{include/i-prgvrs.i AP0804-B01 2.00.00.014}  /*** 010014 ***/

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*Tratamento para independˆncia de objetos*/
&glob ORIGINALNAME 'adbrw~\b09ad260.w'

{app/apapi007.i}
{app/apapi007.i1}

/* Miniflexibiliza‡Æo */
{include/i_dbvers.i}
{cdp/cdcfgfin.i}

def new global shared var rw-tit as rowid no-undo.

def buffer b-tt-tit-ap for tt-tit-ap.

def buffer b-emitente  for emitente.

def temp-table tt-tit-ap-aux like tt-tit-ap.

def var l-aberto       as logical   no-undo.
def var l-matriz       as logical   no-undo.
def var l-faixa        as logical   no-undo.
def var i-mo-codigo    as integer   no-undo.
def var c-cod-est      as character no-undo.
def var c-cod-est-ini  as character no-undo.
def var c-cod-est-fim  as character no-undo.
def var c-ini-esp      as character no-undo.
def var c-fim-esp      as character no-undo.
def var port-ini       as integer   no-undo.
def var port-fim       as integer   no-undo.
def var dt-ini         as date      no-undo.
def var dt-fim         as date      no-undo.
def var dt-conversao   as date      no-undo.

def var c-acomp       as character no-undo.
def var h-acomp       as handle    no-undo.
DEF VAR cName          AS CHAR NO-UNDO.
DEF VAR lDesc          AS LOGICAL NO-UNDO.

def temp-table tt-documento no-undo like titulo
     field row-documento  as rowid
     field c-modalidade   like titulo.modalidade
     field c-mo-codigo    as char format "x(03)"
     field de-saldo       like titulo.vl-saldo
     field de-saldo-me    like titulo.vl-saldo-me
     field i-dias         as int init 0
     field de-juros       as dec 
     field de-total-juros as dec.

DEFINE TEMP-TABLE tt-doc NO-UNDO
       FIELD c-barra                      AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2                     AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-estabel                  LIKE titulo.cod-estabel
       FIELD cod-esp                      LIKE titulo.cod-esp
       FIELD serie                        LIKE titulo.serie
       FIELD nr-docto                     LIKE titulo.nr-docto
       FIELD parcela                      LIKE titulo.parcela
       FIELD portador                     LIKE tit-ap.portador
       FIELD modalidade                   LIKE tit-ap.modalidade
       FIELD vl-saldo                     LIKE titulo.vl-saldo
       FIELD vl-original                  LIKE titulo.vl-original
       FIELD dt-emissao                   LIKE tit-ap.dt-emissao
       FIELD dt-vencimen                  LIKE tit-ap.dt-vencimen
       FIELD cod-emitente                 LIKE titulo.cod-emitente
       FIELD nome-abrev                   LIKE titulo.nome-abrev
       FIELD dias-atraso                  LIKE tit-ap.dias-atraso.

DEFINE TEMP-TABLE tt-pag NO-UNDO
       FIELD c-barra        AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2       AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-emitente   LIKE emitente.cod-emitente
       FIELD nome-abrev     LIKE mov-ap.nome-abrev
       FIELD ep-codigo      LIKE mov-ap.ep-codigo
       FIELD cod-estabel    LIKE mov-ap.cod-estabel
       FIELD cod-esp        LIKE mov-ap.cod-esp
       FIELD nr-docto       LIKE mov-ap.nr-docto
       FIELD parcela        LIKE mov-ap.parcela
       FIELD portador       LIKE mov-ap.portador
       FIELD modalidade     LIKE mov-ap.modalidade 
       FIELD dt-vencimen    LIKE mov-ap.dt-vencimen
       FIELD dt-transacao   LIKE mov-ap.dt-transacao
       FIELD valor-baixa    LIKE mov-ap.valor-mov
       FIELD de-dias-atraso AS INT.

def VAR r-emitente       as rowid no-undo.
def VAR fi-total-titulos as dec no-undo.
/*def VAR de-tot-aberto    as dec no-undo.
def VAR de-tot-anterior  as dec no-undo.*/

DEFINE BUFFER b-tt-documento FOR tt-documento.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-tit-ap

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tit-ap

/* Definitions for BROWSE br-tit-ap                                     */
&Scoped-define FIELDS-IN-QUERY-br-tit-ap tt-tit-ap.cod-estabel tt-tit-ap.cod-fornec tt-tit-ap.cod-esp tt-tit-ap.serie tt-tit-ap.nr-docto tt-tit-ap.parcela tt-tit-ap.moeda tt-tit-ap.vl-original tt-tit-ap.valor-saldo tt-tit-ap.vl-orig-me tt-tit-ap.vl-saldo-me tt-tit-ap.i-atraso tt-tit-ap.dt-transacao tt-tit-ap.dt-emissao tt-tit-ap.dt-vencimen tt-tit-ap.nr-bordero tt-tit-ap.portador tt-tit-ap.c-modalidade tt-tit-ap.c-tipo-pagto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tit-ap   
&Scoped-define SELF-NAME br-tit-ap
&Scoped-define QUERY-STRING-br-tit-ap FOR EACH tt-tit-ap NO-LOCK
&Scoped-define OPEN-QUERY-br-tit-ap OPEN QUERY {&self-name} FOR EACH tt-tit-ap NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-tit-ap tt-tit-ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-tit-ap tt-tit-ap


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-tit-ap bt-anterior bt-filtro bt-detalhar ~
BT-IMP bt-aberto 
&Scoped-Define DISPLAYED-OBJECTS c-sigla1 de-tot-anterior c-sigla2 ~
DE-TOT-ABERTO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-aberto 
     IMAGE-UP FILE "image~\im-sav":U
     LABEL "bt confirma 2" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-anterior 
     IMAGE-UP FILE "image~\im-sav":U
     LABEL "Button 1" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-detalhar 
     LABEL "" 
     SIZE 13 BY 1.

DEFINE BUTTON bt-filtro 
     LABEL "" 
     SIZE 13 BY 1.

DEFINE BUTTON BT-IMP 
     LABEL "Imprimir" 
     SIZE 13 BY 1.

DEFINE VARIABLE c-sigla1 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla2 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE DE-TOT-ABERTO AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE de-tot-anterior AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-tit-ap FOR 
      tt-tit-ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-tit-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tit-ap B-table-Win _FREEFORM
  QUERY br-tit-ap NO-LOCK DISPLAY
      tt-tit-ap.cod-estabel
      tt-tit-ap.cod-fornec
      tt-tit-ap.cod-esp
      tt-tit-ap.serie
      tt-tit-ap.nr-docto
      tt-tit-ap.parcela
      tt-tit-ap.moeda
      tt-tit-ap.vl-original
      tt-tit-ap.valor-saldo
      tt-tit-ap.vl-orig-me      
      tt-tit-ap.vl-saldo-me
      tt-tit-ap.i-atraso
      tt-tit-ap.dt-transacao
      tt-tit-ap.dt-emissao
      tt-tit-ap.dt-vencimen
      tt-tit-ap.nr-bordero
      tt-tit-ap.portador
      tt-tit-ap.c-modalidade
      tt-tit-ap.c-tipo-pagto
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 86.29 BY 6.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-tit-ap AT ROW 1 COL 1
     bt-anterior AT ROW 7.96 COL 83.43
     c-sigla1 AT ROW 8.04 COL 60.57 COLON-ALIGNED
     de-tot-anterior AT ROW 8.04 COL 65 COLON-ALIGNED NO-LABEL
     bt-filtro AT ROW 8.88 COL 1
     bt-detalhar AT ROW 8.88 COL 14
     BT-IMP AT ROW 8.88 COL 27
     bt-aberto AT ROW 9 COL 83.43
     c-sigla2 AT ROW 9.04 COL 60.57 COLON-ALIGNED
     DE-TOT-ABERTO AT ROW 9.04 COL 65 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   External Tables: mgadm.emitente
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.04
         WIDTH              = 86.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-tit-ap 1 F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 9.04
       FRAME F-Main:WIDTH            = 86.72.

/* SETTINGS FOR FILL-IN c-sigla1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DE-TOT-ABERTO IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-tot-anterior IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tit-ap
/* Query rebuild information for BROWSE br-tit-ap
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-tit-ap NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-tit-ap */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-tit-ap
&Scoped-define SELF-NAME br-tit-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON DEL OF br-tit-ap IN FRAME F-Main
DO:
  run pi-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON INS OF br-tit-ap IN FRAME F-Main
DO:
  run pi-save-record. 
  run insere-registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON OFF-END OF br-tit-ap IN FRAME F-Main
DO:
  run pi-off-end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON ROW-DISPLAY OF br-tit-ap IN FRAME F-Main
DO:

    if  avail tt-tit-ap then do:
        if  tt-tit-ap.vl-original = tt-tit-ap.valor-saldo then 
            assign tt-tit-ap.cod-estabel:fgcolor   in browse {&browse-name} = 0
                   tt-tit-ap.cod-fornec:fgcolor    in browse {&browse-name} = 0
                   tt-tit-ap.cod-esp:fgcolor       in browse {&browse-name} = 0
                   tt-tit-ap.serie:fgcolor         in browse {&browse-name} = 0
                   tt-tit-ap.nr-docto:fgcolor      in browse {&browse-name} = 0
                   tt-tit-ap.parcela:fgcolor       in browse {&browse-name} = 0
                   tt-tit-ap.moeda:fgcolor         in browse {&browse-name} = 0
                   tt-tit-ap.vl-original:fgcolor   in browse {&browse-name} = 0
                   tt-tit-ap.vl-orig-me:fgcolor    in browse {&browse-name} = 0
                   tt-tit-ap.valor-saldo:fgcolor   in browse {&browse-name} = 0
                   tt-tit-ap.vl-saldo-me:fgcolor   in browse {&browse-name} = 0
                   tt-tit-ap.i-atraso:fgcolor      in browse {&browse-name} = 0
                   tt-tit-ap.dt-transacao:fgcolor  in browse {&browse-name} = 0
                   tt-tit-ap.dt-emissao:fgcolor    in browse {&browse-name} = 0
                   tt-tit-ap.portador:fgcolor      in browse {&browse-name} = 0
                   tt-tit-ap.c-modalidade:fgcolor  in browse {&browse-name} = 0
                   tt-tit-ap.c-tipo-pagto:fgcolor  in browse {&browse-name} = 0
                   tt-tit-ap.dt-vencimen:fgcolor   in browse {&browse-name} = 0
                   tt-tit-ap.nr-bordero:fgcolor    in browse {&browse-name} = 0.

        else 
            assign tt-tit-ap.cod-estabel:fgcolor   in browse {&browse-name} = 9
                   tt-tit-ap.cod-fornec:fgcolor    in browse {&browse-name} = 9
                   tt-tit-ap.cod-esp:fgcolor       in browse {&browse-name} = 9
                   tt-tit-ap.serie:fgcolor         in browse {&browse-name} = 9
                   tt-tit-ap.nr-docto:fgcolor      in browse {&browse-name} = 9
                   tt-tit-ap.parcela:fgcolor       in browse {&browse-name} = 9
                   tt-tit-ap.moeda:fgcolor         in browse {&browse-name} = 9
                   tt-tit-ap.vl-original:fgcolor   in browse {&browse-name} = 9
                   tt-tit-ap.vl-orig-me:fgcolor    in browse {&browse-name} = 9
                   tt-tit-ap.valor-saldo:fgcolor   in browse {&browse-name} = 9
                   tt-tit-ap.vl-saldo-me:fgcolor   in browse {&browse-name} = 9
                   tt-tit-ap.i-atraso:fgcolor      in browse {&browse-name} = 9
                   tt-tit-ap.dt-transacao:fgcolor  in browse {&browse-name} = 9
                   tt-tit-ap.dt-emissao:fgcolor    in browse {&browse-name} = 9
                   tt-tit-ap.portador:fgcolor      in browse {&browse-name} = 9
                   tt-tit-ap.c-modalidade:fgcolor  in browse {&browse-name} = 9
                   tt-tit-ap.c-tipo-pagto:fgcolor  in browse {&browse-name} = 9
                   tt-tit-ap.dt-vencimen:fgcolor   in browse {&browse-name} = 9
                   tt-tit-ap.nr-bordero:fgcolor    in browse {&browse-name} = 9.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON ROW-ENTRY OF br-tit-ap IN FRAME F-Main
DO:
    /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON ROW-LEAVE OF br-tit-ap IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
       by pressing the Save button on an Update SmartPanel. */
    {src/adm/template/brsleave.i}

    run pi-row-leave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON START-SEARCH OF br-tit-ap IN FRAME F-Main
DO:
    
    &IF INTEGER(ENTRY(1,PROVERSION,'.')) >= 9 &THEN
        DEF VAR hTemp1 AS WIDGET-HANDLE NO-UNDO.
        DEF VAR hTemp2 AS WIDGET-HANDLE NO-UNDO.
        ASSIGN hTemp1 = br-tit-ap:CURRENT-COLUMN
             hTemp2 = br-tit-ap:QUERY.
        IF cName = hTemp1:NAME THEN
          ASSIGN lDesc = NOT lDesc.
        ELSE
          ASSIGN lDesc = YES.
        IF lDesc THEN DO:
          hTemp2:QUERY-PREPARE("For each tt-tit-ap " +
                               " OUTER-JOIN by " + hTemp1:NAME).
        END.
        ELSE DO:
          hTemp2:QUERY-PREPARE("For each tt-tit-ap " +
                               " OUTER-JOIN by " + hTemp1:NAME + " desc").
        END.
        ASSIGN cName = hTemp1:NAME.
        hTemp2:QUERY-OPEN().        
    &ENDIF
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tit-ap B-table-Win
ON VALUE-CHANGED OF br-tit-ap IN FRAME F-Main
DO:
    /* This ADM trigger code must be preserved in order to notify other
       objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-aberto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-aberto B-table-Win
ON CHOOSE OF bt-aberto IN FRAME F-Main /* bt confirma 2 */
DO:

    for each tt-param-filtro:
        delete tt-param-filtro.
    end.

    create tt-param-filtro.
    assign tt-param-filtro.i-empresa        = i-ep-codigo-usuario
           tt-param-filtro.i-cod-fornec     = emitente.cod-emit
           tt-param-filtro.l-faixa          = l-faixa
           tt-param-filtro.c-cod-est        = c-cod-est
           tt-param-filtro.c-cod-est-ini    = c-cod-est-ini
           tt-param-filtro.c-cod-est-fim    = c-cod-est-fim
           tt-param-filtro.c-esp-ini        = c-ini-esp
           tt-param-filtro.c-esp-fim        = c-fim-esp
           tt-param-filtro.dt-trans-ini     = dt-ini
           tt-param-filtro.dt-trans-fim     = dt-fim
           tt-param-filtro.i-port-ini       = port-ini    
           tt-param-filtro.i-port-fim       = port-fim
           tt-param-filtro.i-moeda          = i-mo-codigo
           tt-param-filtro.da-conversao     = dt-conversao
           tt-param-filtro.l-matriz         = l-matriz
           tt-param-filtro.l-saldo-aberto   = l-aberto
           tt-param-filtro.l-matriz         = l-matriz
           tt-param-filtro.l-ant-aber       = no
           tt-param-filtro.cod-versao-integ = 001
           tt-param-filtro.l-acompanha      = yes
           tt-param-filtro.l-vid-rel        = yes.    

    run app/apapi007.p (input-output table tt-param-filtro,
                        input-output table tt-tit-ap-aux,
                        output       table tt-erro).

    find first tt-param-filtro exclusive-lock.

    assign de-tot-aberto = tt-param-filtro.saldo-aberto.

    disp de-tot-aberto with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anterior B-table-Win
ON CHOOSE OF bt-anterior IN FRAME F-Main /* Button 1 */
DO:

    for each tt-param-filtro:
        delete tt-param-filtro.
    end.

    create tt-param-filtro.
    assign tt-param-filtro.i-empresa        = i-ep-codigo-usuario
           tt-param-filtro.i-cod-fornec     = emitente.cod-emit
           tt-param-filtro.c-cod-est        = c-cod-est
           tt-param-filtro.c-cod-est-ini    = c-cod-est-ini
           tt-param-filtro.c-cod-est-fim    = c-cod-est-fim
           tt-param-filtro.c-esp-ini        = c-ini-esp
           tt-param-filtro.c-esp-fim        = c-fim-esp
           tt-param-filtro.dt-trans-ini     = dt-ini
           tt-param-filtro.dt-trans-fim     = dt-fim
           tt-param-filtro.i-port-ini       = port-ini    
           tt-param-filtro.i-port-fim       = port-fim
           tt-param-filtro.i-moeda          = i-mo-codigo
           tt-param-filtro.da-conversao     = dt-conversao
           tt-param-filtro.l-matriz         = l-matriz
           tt-param-filtro.l-saldo-aberto   = l-aberto
           tt-param-filtro.l-faixa          = l-faixa
           tt-param-filtro.cod-versao-integ = 001
           tt-param-filtro.l-ant-aber       = yes
           tt-param-filtro.l-acompanha      = yes
           tt-param-filtro.l-vid-rel        = yes.    

    run app/apapi007.p (input-output table tt-param-filtro,
                        input-output table tt-tit-ap-aux,
                        output       table tt-erro).
    find first tt-param-filtro exclusive-lock.

    assign de-tot-anterior = tt-param-filtro.saldo-anterior.

    disp de-tot-anterior with frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhar B-table-Win
ON CHOOSE OF bt-detalhar IN FRAME F-Main
DO:

    if  avail tt-tit-ap then do:
         assign rw-tit = tt-tit-ap.row-tit-ap.
         run app/ap0804f.w.
    end.      
    else do:
        run utp/ut-msgs.p (input "show",
                           input 1880,
                           input "").            
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main
DO:

    run app/ap0804aa.w (input-output l-matriz,
                        input-output l-aberto,
                        input-output l-faixa,
                        input-output i-mo-codigo,
                        input-output c-cod-est,
                        input-output c-cod-est-ini,
                        input-output c-cod-est-fim,
                        input-output c-ini-esp,
                        input-output c-fim-esp,
                        input-output port-ini,
                        input-output port-fim,
                        input-output dt-ini,
                        input-output dt-fim,
                        input-output dt-conversao).

    find first moeda
         where moeda.mo-codigo = i-mo-codigo no-lock no-error.

    if  avail moeda then
        assign c-sigla1:screen-value in frame {&FRAME-NAME} = moeda.sigla
               c-sigla2:screen-value in frame {&FRAME-NAME} = moeda.sigla.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-IMP B-table-Win
ON CHOOSE OF BT-IMP IN FRAME F-Main /* Imprimir */
DO:
    DEF VAR l-cont AS INTEGER INITIAL 0 NO-UNDO.

    for each tt-doc:
        delete tt-doc.
    end.

    FOR EACH tt-tit-ap NO-LOCK.

        ASSIGN l-cont = l-cont + 1.

        CREATE tt-doc.
        ASSIGN tt-doc.cod-emitente   = emitente.cod-emitente
               tt-doc.nome-abrev     = emitente.nome-abrev
               tt-doc.cod-estabel    = tt-tit-ap.cod-estabel
               tt-doc.cod-esp        = tt-tit-ap.cod-esp
               tt-doc.serie          = tt-tit-ap.serie
               tt-doc.nr-docto       = tt-tit-ap.nr-docto
               tt-doc.parcela        = tt-tit-ap.parcela
               tt-doc.vl-original    = tt-tit-ap.vl-original
               tt-doc.vl-saldo       = tt-tit-ap.valor-saldo
               tt-doc.dias-atraso    = tt-tit-ap.dias-atraso
               tt-doc.dt-emissao     = tt-tit-ap.dt-emissao
               tt-doc.dt-vencimen    = tt-tit-ap.dt-vencimen
               tt-doc.portador       = tt-tit-ap.portador
               tt-doc.modalidade     = tt-tit-ap.modalidade.
    END.

    RUN app/ap0804n.w (INPUT TABLE tt-doc,
                       INPUT TABLE tt-pag,
                       INPUT TABLE tt-documento,
                       INPUT r-emitente,
                       INPUT fi-total-titulos,   
                       INPUT de-tot-aberto,   
                       input de-tot-anterior,
                       INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Filtro}
assign bt-filtro:label in frame {&frame-name} = trim(return-value).

{utp/ut-liter.i Detalhar}
assign bt-detalhar:label in frame {&frame-name} = trim(return-value).

{utp/ut-liter.i Saldo_Anterior}
assign c-sigla1:label in frame {&frame-name} = trim(return-value).

{utp/ut-liter.i Saldo_Aberto}
assign c-sigla2:label in frame {&frame-name} = trim(return-value).

{utp/ut-liter.i Modalidade}
assign tt-tit-ap.c-modalidade:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Tipo_Pagamento}
assign tt-tit-ap.c-tipo-pagto:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Valor_Original}
assign tt-tit-ap.vl-original:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Valor_Original_ME}
assign tt-tit-ap.vl-orig-me:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Valor_Saldo}
assign tt-tit-ap.valor-saldo:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Valor_Saldo_ME}
assign tt-tit-ap.vl-saldo-me:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Atraso}
assign tt-tit-ap.i-atraso:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i MO}
assign tt-tit-ap.moeda:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Imprimir}
assign BT-IMP:label in frame {&frame-name} = trim(return-value).


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  find first moeda
       where moeda.mo-codigo = i-mo-codigo no-lock no-error.

  if  avail moeda then
      assign c-sigla1:screen-value in frame {&FRAME-NAME} = moeda.sigla
             c-sigla2:screen-value in frame {&FRAME-NAME} = moeda.sigla.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

      ASSIGN bt-imp:VISIBLE IN FRAME {&FRAME-NAME} = YES.

      assign l-aberto       = no
           l-matriz       = no
           l-faixa        = yes
           i-mo-codigo    = 0
           c-cod-est      = ""
           c-cod-est-ini  = ""
           c-cod-est-fim  = "ZZZ"
           c-ini-esp      = "AA"
           c-fim-esp      = "ZZ"
           port-ini       = 0
           port-fim       = 99999
           dt-ini         = today
           dt-fim         = today
           dt-conversao   = today.

    &IF INTEGER(ENTRY(1,PROVERSION,'.')) >= 9 &THEN
        ASSIGN br-tit-ap:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.
    &ENDIF

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    for each tt-tit-ap:
        delete tt-tit-ap.
    end.

    assign DE-TOT-ABERTO   = 0
           de-tot-anterior = 0.

    run pi-criar-tt-tit-ap.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-tit-ap B-table-Win 
PROCEDURE pi-criar-tt-tit-ap :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Processando,_Aguarde...}
    run pi-inicializar in h-acomp (input trim(return-value)).

    {utp/ut-liter.i Docto/P:}
    assign c-acomp = trim(return-value).

    if  avail emitente then do on stop undo, leave:

        if  l-matriz = no then do:
            run pi-criar-tt-tit-ap-fornecedor.
        end.
        else do:
            run pi-criar-tt-tit-ap-matriz.
        end.

    end.

    run pi-finalizar in h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-tit-ap-fornecedor B-table-Win 
PROCEDURE pi-criar-tt-tit-ap-fornecedor :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    if  l-faixa = no then do:
        IF  dt-ini = dt-fim THEN DO:
            for each  tit-ap USE-INDEX dt-trans
                where tit-ap.ep-codigo     = i-ep-codigo-usuario 
                AND   tit-ap.dt-transacao  = dt-ini
                and   tit-ap.cod-fornec    = emitente.cod-emit
                and   tit-ap.cod-estabel   = c-cod-est
                and   tit-ap.cod-esp      >= c-ini-esp
                and   tit-ap.cod-esp      <= c-fim-esp
                and   tit-ap.portador     >= port-ini
                and   tit-ap.portador     <= port-fim no-lock:

                run pi-processamento.
                if return-value = 'NOK' then
                   next.

            end.    
        END.
        ELSE DO:
            FOR EACH espec-ap
                WHERE espec-ap.cod-esp >= c-ini-esp
                AND   espec-ap.cod-esp <= c-fim-esp NO-LOCK:
                
                for each  tit-ap USE-INDEX ch-tipo
                    where tit-ap.ep-codigo     = i-ep-codigo-usuario 
                    AND   tit-ap.tipo          = espec-ap.tipo
                    and   tit-ap.cod-fornec    = emitente.cod-emit
                    and   tit-ap.cod-estabel   = c-cod-est
                    and   tit-ap.cod-esp       = espec-ap.cod-esp
                    and   tit-ap.portador     >= port-ini
                    and   tit-ap.portador     <= port-fim
                    and   tit-ap.dt-transacao >= dt-ini
                    and   tit-ap.dt-transacao <= dt-fim no-lock:
        
                    run pi-processamento.
                    if return-value = 'NOK' then
                       next.
                end.    
            END.
        END.
    end.
    else do:
        IF  dt-ini = dt-fim THEN DO:
            for each  tit-ap USE-INDEX dt-trans
                where tit-ap.ep-codigo     = i-ep-codigo-usuario 
                AND   tit-ap.dt-transacao  = dt-ini
                and   tit-ap.cod-fornec    = emitente.cod-emit
                and   tit-ap.cod-estabel  >= c-cod-est-ini
                and   tit-ap.cod-estabel  <= c-cod-est-fim
                and   tit-ap.cod-esp      >= c-ini-esp
                and   tit-ap.cod-esp      <= c-fim-esp
                and   tit-ap.portador     >= port-ini
                and   tit-ap.portador     <= port-fim no-lock:

                run pi-processamento.
                if return-value = 'NOK' then
                   next.

            end.    
        END.
        ELSE DO:
            FOR EACH espec-ap
                WHERE espec-ap.cod-esp >= c-ini-esp
                AND   espec-ap.cod-esp <= c-fim-esp NO-LOCK:

                for each  tit-ap USE-INDEX ch-tipo
                    where tit-ap.ep-codigo     = i-ep-codigo-usuario 
                    AND   tit-ap.tipo          = espec-ap.tipo
                    and   tit-ap.cod-fornec    = emitente.cod-emit
                    and   tit-ap.cod-esp       = espec-ap.cod-esp
                    and   tit-ap.cod-estabel  >= c-cod-est-ini
                    and   tit-ap.cod-estabel  <= c-cod-est-fim
                    and   tit-ap.portador     >= port-ini
                    and   tit-ap.portador     <= port-fim
                    and   tit-ap.dt-transacao >= dt-ini
                    and   tit-ap.dt-transacao <= dt-fim no-lock:
        
                    run pi-processamento.
                    if return-value = 'NOK' then
                       next.
                end.    
            END.
        END.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-tit-ap-matriz B-table-Win 
PROCEDURE pi-criar-tt-tit-ap-matriz :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

find b-emitente
        where b-emitente.nome-abrev = emitente.nome-matriz
        no-lock no-error.

    if  l-faixa = no then do:

        for each  tit-ap
            where tit-ap.ep-codigo     = i-ep-codigo-usuario 
            and   tit-ap.cod-estabel   = c-cod-est
            and   tit-ap.matriz        = b-emitente.cod-emitente
            and   tit-ap.cod-esp      >= c-ini-esp
            and   tit-ap.cod-esp      <= c-fim-esp
            and   tit-ap.portador     >= port-ini
            and   tit-ap.portador     <= port-fim
            and   tit-ap.dt-transacao >= dt-ini
            and   tit-ap.dt-transacao <= dt-fim no-lock:

            run pi-processamento.
            if return-value = 'NOK' then
               next.

        end.    
    end.
    else do:
        for each  tit-ap
            where tit-ap.ep-codigo     = i-ep-codigo-usuario 
            and   tit-ap.cod-estabel  >= c-cod-est-ini
            and   tit-ap.cod-estabel  <= c-cod-est-fim
            and   tit-ap.matriz        = b-emitente.cod-emit
            and   tit-ap.cod-esp      >= c-ini-esp
            and   tit-ap.cod-esp      <= c-fim-esp
            and   tit-ap.portador     >= port-ini
            and   tit-ap.portador     <= port-fim
            and   tit-ap.dt-transacao >= dt-ini
            and   tit-ap.dt-transacao <= dt-fim no-lock:

            run pi-processamento.
            if return-value = 'NOK' then
               next.

        end.    
    end.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-off-end B-table-Win 
PROCEDURE pi-off-end :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processamento B-table-Win 
PROCEDURE pi-processamento :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    run pi-acompanhar in h-acomp (input trim(c-acomp)
                                  + "  "
                                  + caps(tit-ap.cod-esp)
                                  + "  "
                                  + tit-ap.nr-docto 
                                  + "/" 
                                  + tit-ap.parcela).

    if  tit-ap.tipo <> 1 
    and tit-ap.tipo <> 2 
    and tit-ap.tipo <> 3 
    and tit-ap.tipo <> 7 
    and tit-ap.tipo <> 8 
    and tit-ap.tipo <> 10 then
        return 'NOK'.

    if  l-aberto = no then do:
        if  tit-ap.vl-saldo-me = 0 then
            return 'NOK'.
    end.

    create tt-tit-ap.
    buffer-copy tit-ap to tt-tit-ap no-error.

    assign tt-tit-ap.row-tit-ap = rowid(tit-ap).

    if  tt-tit-ap.modalidade = 0 then
        assign tt-tit-ap.modalidade = 1.

    if  tt-tit-ap.tp-pagto = 0 then
        assign tt-tit-ap.tp-pagto = 1.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
 DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
 ASSIGN cAuxTraducao001 = {adinc/i03ad209.i 4 tt-tit-ap.modalidade}.
 run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                     INPUT "",
                     INPUT "").
 ASSIGN c-modalidade = RETURN-VALUE.
&else
 ASSIGN c-modalidade = {adinc/i03ad209.i 4 tt-tit-ap.modalidade}.
&endif
    &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
        DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
        ASSIGN cAuxTraducao002 = {adinc/i22ad098.i 4 tt-tit-ap.tp-pagto}.
        run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao002)," ","_"),
                            INPUT "",
                            INPUT "").
        ASSIGN c-tipo-pagto = RETURN-VALUE.
    &else
        ASSIGN c-tipo-pagto = {adinc/i22ad098.i 4 tt-tit-ap.tp-pagto}.
    &endif

    if  tit-ap.dt-ult-pagto <> ? and tit-ap.valor-saldo = 0 then do:
        assign tt-tit-ap.i-atraso = tit-ap.dt-ult-pagto - tit-ap.dt-vencimen.
    end.
    else do:
        if  tit-ap.tipo = 1 
        or  tit-ap.tipo = 6 
        or  tit-ap.tipo = 8 then
            assign tt-tit-ap.i-atraso = today - tit-ap.dt-vencimen.
    end.
    assign tt-tit-ap.i-atraso = tit-ap.dias-atraso.
    return 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "tt-tit-ap"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

