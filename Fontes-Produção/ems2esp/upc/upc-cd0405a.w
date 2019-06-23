&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i UPC-CD0405A 12.01.19.000 } /*** 010047 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i UPC-CD0405A MFT}
&ENDIF

/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.
{cdp/cdcfgman.i}
/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        UPC-CD0405A
&GLOBAL-DEFINE Version        12.01.19.000

&GLOBAL-DEFINE WindowType     Master

&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   <Folder1 ,Folder 2 ,... , Folder8>

&GLOBAL-DEFINE page0Widgets   btFiltrar btOK btMarca btTodos~
                              btNenhum btCancelar btHelp2 br-table

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable i-rows-returned            as integer no-undo.

DEFINE VARIABLE hDBODpProcesItem                 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBODpItem                       AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOItem                         AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE VARIABLE wh-pesquisa                      AS HANDLE NO-UNDO.

define variable c-desc-destino             as char    format "x(40)"  no-undo.
define variable c-item-ini                 as char    format "x(16)"  no-undo init "" .
define variable c-item-fim                 as char    format "x(16)"  no-undo init "ZZZZZZZZZZZZZZZZ":U.
define variable c-ok                       as LOG NO-UNDO.
define variable c-un                       as char    label "UN"      no-undo.
define variable c-un-destino               as char    label "UN"      no-undo.
define variable i-proces-ini               as integer initial       0 no-undo.
define variable i-proces-fim               as integer initial 9999999 no-undo.
define variable l-todos                    as logical initial yes     no-undo.
define variable l-ok                       as logical initial  no     no-undo.
define variable l-new-row                  as logical initial yes     no-undo.
define variable h-acomp                    as handle                  no-undo.
define variable h-dpapi001                 as handle                  no-undo.
define variable l-primeiro                 as logical initial  no     no-undo.
define variable l-saiu-row                 as logical initial  no     no-undo.
define variable l-novo-registro            as logical initial  no     no-undo.
define variable l-zoom                     as logical initial  no     no-undo.
define variable l-edit-brw                 as logical initial  no     no-undo.
define variable i-fat-conv                 as decimal decimals 8 
                                              format ">>,>>9.99999"   column-label "Fator Conv" no-undo.
define variable c-item-cot               like dp-item.it-codigo       no-undo.
define variable c-item-dp-antigo         like dp-item.item-dp         no-undo.
define variable i-num-proces-item-antigo like dp-proces-item.num-proces-item no-undo.

define variable l-elimina-dp-1             as logical format "Sim/Nao".
define variable tg-renum-estrut            as logical initial  no     no-undo.
define variable tg-renum-oper              as logical initial  no     no-undo.
define variable i-var-estrut               as integer initial  10     no-undo.
define variable i-var-oper                 as integer initial  10     no-undo.
define variable i-row                      as integer                 no-undo.

define variable iIndAprov                  as integer                 no-undo.
define variable v-cod-lista                as char                    no-undo.
define variable v-cod-roteiro              as char                    no-undo.
define variable c-descricao                as char                    no-undo.

DEFINE TEMP-TABLE ttNotasAdicionais NO-UNDO
    FIELD c-marca           AS CHARACTER FORMAT "X(1)"
    FIELD cod-estabel       LIKE it-doc-fisc.cod-estabel 
    FIELD cod-emitente      LIKE it-doc-fisc.cod-emitente
    FIELD nat-operacao      LIKE it-doc-fisc.nat-operacao 
    FIELD serie             LIKE it-doc-fisc.serie 
    FIELD nr-doc-fis        LIKE it-doc-fisc.nr-doc-fis 
    FIELD dt-emis-doc       LIKE it-doc-fisc.dt-emis-doc
    FIELD it-codigo         LIKE it-doc-fisc.it-codigo 
    FIELD quantidade        LIKE it-doc-fisc.quantidade    
    FIELD nr-seq-doc        LIKE it-doc-fisc.nr-seq-doc.

/***EAI ****/
DEFINE VARIABLE c-action AS CHARACTER  NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_log_eai_habilit AS LOGICAL NO-UNDO. 
DEFINE TEMP-TABLE tt-item-eai NO-UNDO LIKE ITEM.

&IF DEFINED (bf_man_206B) &THEN
      DEFINE VARIABLE l-unid-negoc AS LOGICAL    NO-UNDO.
      DEFINE VARIABLE h-campo AS HANDLE     NO-UNDO.      
      DEFINE VARIABLE c-cod-unid-negoc LIKE unid-negoc.cod-unid-negoc  NO-UNDO.
      DEFINE VARIABLE c-char-1 AS CHARACTER FORMAT "x(15)"  NO-UNDO.
&ENDIF

DEFINE VARIABLE pSerDoctoIni AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pSerDoctoFin AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pNroDoctoIni AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pNroDoctoFin AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pNatOperIni  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pNatOperFin  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pItCodigoIni AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pItCodigoFin AS CHARACTER NO-UNDO.  
DEFINE VARIABLE pProporcao   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hbofi258     AS HANDLE    NO-UNDO.
DEFINE VARIABLE deTotalBr    AS DECIMAL FORMAT ">>>,>>>,>>9.9999"  NO-UNDO.
DEFINE VARIABLE qtdTotalBr   AS DECIMAL FORMAT ">>>,>>>,>>9.99"    NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-emitente-re1001        AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-serie-re1001           AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nat-operacao-re1001    AS HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nro-docto-re1001       AS HANDLE  NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-esft4003-seq-wt-docto    AS WIDGET-HANDLE  NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttNotasAdicionais

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttNotasAdicionais.c-marca ttNotasAdicionais.nr-doc-fis ttNotasAdicionais.serie ttNotasAdicionais.nat-operacao ttNotasAdicionais.dt-emis-doc ttNotasAdicionais.it-codigo ttNotasAdicionais.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table ttNotasAdicionais.quantidade   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-table ttNotasAdicionais
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-table ttNotasAdicionais
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttNotasAdicionais BY ttNotasAdicionais.nr-doc-fis
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH ttNotasAdicionais BY ttNotasAdicionais.nr-doc-fis.
&Scoped-define TABLES-IN-QUERY-br-table ttNotasAdicionais
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttNotasAdicionais


/* Definitions for FRAME fpage0                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fpage0 ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar rtToolBar-2 btFiltrar br-table ~
fi-qtd-total fi-vl-total btOK btMarca btTodos btNenhum btCancelar btHelp2 
&Scoped-Define DISPLAYED-OBJECTS fi-qtd-total fi-vl-total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM m_Incluir      LABEL "&Incluir"       ACCELERATOR "CTRL-A"
       MENU-ITEM m_Modificar    LABEL "&Modificar"     ACCELERATOR "ALT-M"
       MENU-ITEM m_Delete       LABEL "&Eliminar"      ACCELERATOR "CTRL-D"
       MENU-ITEM m_OK           LABEL "&OK"            ACCELERATOR "CTRL-O"
       RULE
       MENU-ITEM m_Filtro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM m_Exporta      LABEL "&Exporta‡Æo"    ACCELERATOR "CTRL-E"
       MENU-ITEM m_Parametros   LABEL "&Parƒmetros"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "Aj&uda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancelar 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btFiltrar 
     IMAGE-UP FILE "image\im-param":U
     IMAGE-INSENSITIVE FILE "image\ii-param":U
     LABEL "Filtrar" 
     SIZE 4 BY 1.25 TOOLTIP "Filtrar".

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btMarca 
     LABEL "Marcar" 
     SIZE 10 BY 1.

DEFINE BUTTON btNenhum 
     LABEL "Nenhum" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btTodos 
     LABEL "Todos" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-qtd-total AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Quantidade Total" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .79 NO-UNDO.

DEFINE VARIABLE fi-vl-total AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .79 NO-UNDO.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 81 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE rtToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 81 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttNotasAdicionais SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table wWindow _FREEFORM
  QUERY br-table DISPLAY
      ttNotasAdicionais.c-marca      COLUMN-LABEL "*"
      ttNotasAdicionais.nr-doc-fis      
      ttNotasAdicionais.serie      
      ttNotasAdicionais.nat-operacao
      ttNotasAdicionais.dt-emis-doc  
      ttNotasAdicionais.it-codigo   
      ttNotasAdicionais.quantidade COLUMN-LABEL "Quantidade"
  ENABLE
      ttNotasAdicionais.quantidade
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 80.86 BY 13.42
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btFiltrar AT ROW 1.13 COL 1.43
     br-table AT ROW 2.58 COL 1.14
     fi-qtd-total AT ROW 16.5 COL 60.29 COLON-ALIGNED WIDGET-ID 6
     fi-vl-total AT ROW 17.42 COL 60.29 COLON-ALIGNED WIDGET-ID 8
     btOK AT ROW 18.58 COL 2
     btMarca AT ROW 18.58 COL 13
     btTodos AT ROW 18.58 COL 24
     btNenhum AT ROW 18.58 COL 34.86 WIDGET-ID 2
     btCancelar AT ROW 18.58 COL 45.72 WIDGET-ID 4
     btHelp2 AT ROW 18.58 COL 71.29
     rtToolBar AT ROW 18.38 COL 1
     rtToolBar-2 AT ROW 1 COL 1.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 18.96
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 18.96
         WIDTH              = 82
         MAX-HEIGHT         = 30
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 30
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{btb/btb008za.i0}
{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table btFiltrar fpage0 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY br-table FOR EACH ttNotasAdicionais BY ttNotasAdicionais.nr-doc-fis.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table wWindow
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME fpage0
DO:
  apply "choose" to btMarca in frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table wWindow
ON VALUE-CHANGED OF br-table IN FRAME fpage0
DO:
  if ttNotasAdicionais.c-marca:screen-value in browse br-table = ""
  THEN DO:
      {utp/ut-liter.i Marcar *}
      assign btMarca:label in frame {&frame-name} = return-value.
  END.
  ELSE DO:
     {utp/ut-liter.i Desmarcar *}
     assign btMarca:label in frame {&frame-name} = return-value.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancelar wWindow
ON CHOOSE OF btCancelar IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltrar wWindow
ON CHOOSE OF btFiltrar IN FRAME fpage0 /* Filtrar */
DO:
  EMPTY TEMP-TABLE ttNotasAdicionais.

  {&WINDOW-NAME}:SENSITIVE = NO.
  run upc/upc-cd0405aa.w(INPUT-OUTPUT pSerDoctoIni,
                         INPUT-OUTPUT pSerDoctoFin,
                         INPUT-OUTPUT pNroDoctoIni,
                         INPUT-OUTPUT pNroDoctoFin,
                         INPUT-OUTPUT pNatOperIni,
                         INPUT-OUTPUT pNatOperFin, 
                         INPUT-OUTPUT pItCodigoIni,
                         INPUT-OUTPUT pItCodigoFin, 
                         INPUT-OUTPUT pProporcao). 
  {&WINDOW-NAME}:SENSITIVE = YES.

  ASSIGN deTotalBr  = 0
         qtdTotalBr = 0.

  RUN piCarregaTT.


  ASSIGN fi-qtd-total:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(qtdTotalBr)
         fi-vl-total :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(deTotalBr).

  {&OPEN-QUERY-{&BROWSE-NAME}} 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMarca wWindow
ON CHOOSE OF btMarca IN FRAME fpage0 /* Marcar */
DO:
    if  br-table:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-table.
        if ttNotasAdicionais.c-marca:screen-value in browse br-table = ""
        then do:
           assign ttNotasAdicionais.c-marca = "*".
        end.
        else do:
           assign ttNotasAdicionais.c-marca = "".
        end.
        assign ttNotasAdicionais.c-marca:screen-value in browse br-table = ttNotasAdicionais.c-marca.

        apply "value-changed" to br-table in frame {&frame-name}.        
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNenhum wWindow
ON CHOOSE OF btNenhum IN FRAME fpage0 /* Nenhum */
DO:
    assign i-row = br-table:focused-row.

    IF i-row = 0 THEN
        ASSIGN i-row = 1.

    for each ttNotasAdicionais:
        assign ttNotasAdicionais.c-marca = "".
    end.   

    {&OPEN-QUERY-{&BROWSE-NAME}} 

  IF i-row <> ? THEN
  DO: 
    br-table:select-row(i-row).

    ENABLE  bttodos  WITH FRAME {&FRAME-NAME}.
    DISABLE btNenhum WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN pi-atualiza-nf-adc.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTodos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTodos wWindow
ON CHOOSE OF btTodos IN FRAME fpage0 /* Todos */
DO:
    assign i-row = br-table:focused-row.

    IF i-row = 0 THEN
        ASSIGN i-row = 1.

    for each ttNotasAdicionais:
        assign ttNotasAdicionais.c-marca = "*".
    end.  

    {&OPEN-QUERY-{&BROWSE-NAME}} 
  
    IF i-row <> ? THEN
    DO:   
       br-table:select-row(i-row).

       DISABLE bttodos  WITH FRAME {&FRAME-NAME}.
       ENABLE  btNenhum WITH FRAME {&FRAME-NAME}.
    END.                                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wWindow 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hbofi258) THEN
        DELETE PROCEDURE hbofi258.
    ASSIGN hbofi258 = ?.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AfterInitializeInterface wWindow 
PROCEDURE AfterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN pSerDoctoIni = ""
           pSerDoctoFin = "ZZZZZ"
           pNroDoctoIni = ""
           pNroDoctoFin = "ZZZZZZZZZZZZZZZZ"
           pNatOperIni  = ""
           pNatOperFin  = "ZZZZZZ"
           pItCodigoIni = ""
           pItCodigoFin = "ZZZZZZZZZZZZZZZZ"
           pProporcao   = 100.

    return "OK".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeDestroyInterface wWindow 
PROCEDURE BeforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hbofi258) THEN
        DELETE PROCEDURE hbofi258.
    ASSIGN hbofi258 = ?.

  return "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wWindow 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hbofi258) THEN
    RUN fibo/bofi258.p PERSISTENT SET hbofi258.

  return "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize wWindow 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  run pi-before-initialize.

  /* Dispatch standard ADM method.                             */
  {utp/ut9000.i "UPC-CD0405A" "12.01.19.000"}
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-nf-adc wWindow 
PROCEDURE pi-atualiza-nf-adc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iSeqNfAdc      AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-modelo-aux   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave-nfe    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-nat-oper-aux AS INTEGER     NO-UNDO.


FIND FIRST wt-docto WHERE 
           wt-docto.seq-wt-docto    = int(wh-esft4003-seq-wt-docto:SCREEN-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL wt-docto THEN
   RETURN "OK".

ASSIGN iSeqNfAdc = 0.

FOR EACH ttNotasAdicionais WHERE
         ttNotasAdicionais.c-marca = "*":

    FIND LAST nota-fisc-adc WHERE
              nota-fisc-adc.cod-estab          = wt-docto.cod-estabel           AND
              nota-fisc-adc.cod-serie          = wt-docto.serie                 AND
              nota-fisc-adc.cod-nota-fisc      = STRING(wt-docto.seq-wt-docto)  AND
              nota-fisc-adc.cdn-emitente       = wt-docto.cod-emitente          AND
              nota-fisc-adc.cod-natur-operac   = wt-docto.nat-operacao          AND
              nota-fisc-adc.idi-tip-dado       = 3                              NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fisc-adc THEN              
       ASSIGN iSeqNfAdc = 1.
    ELSE
       ASSIGN iSeqNfAdc = nota-fisc-adc.num-seq + 1.
    
    IF AVAIL nota-fisc-adc THEN
       RELEASE nota-fisc-adc.
    
    FIND FIRST doc-fiscal WHERE 
               doc-fiscal.cod-estabel  = wt-docto.cod-estabel                 AND
               doc-fiscal.serie        = ttNotasAdicionais.serie              AND   
               doc-fiscal.nr-doc-fis   = ttNotasAdicionais.nr-doc-fis         AND   
               doc-fiscal.cod-emitente = ttNotasAdicionais.cod-emitente       AND   
               doc-fiscal.nat-operacao = ttNotasAdicionais.nat-operacao       NO-LOCK NO-ERROR.

    FIND FIRST emitente WHERE 
               emitente.nome-abrev  = wt-docto.nome-abrev NO-LOCK NO-ERROR.
    
    FIND FIRST nota-fisc-adc WHERE
               nota-fisc-adc.cod-estab          = wt-docto.cod-estabel          AND
               nota-fisc-adc.cod-serie          = wt-docto.serie                AND
               nota-fisc-adc.cod-nota-fisc      = STRING(wt-docto.seq-wt-docto) AND
               nota-fisc-adc.cdn-emitente       = wt-docto.cod-emitente         AND
               nota-fisc-adc.cod-natur-operac   = wt-docto.nat-operacao         AND
               nota-fisc-adc.idi-tip-dado       = 3                             AND 
               nota-fisc-adc.num-seq            = iSeqNfAdc                     EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL nota-fisc-adc THEN
    DO:    
       CREATE nota-fisc-adc.
       ASSIGN nota-fisc-adc.cod-estab          = wt-docto.cod-estabel         
              nota-fisc-adc.cod-serie          = wt-docto.serie               
              nota-fisc-adc.cod-nota-fisc      = STRING(wt-docto.seq-wt-docto)
              nota-fisc-adc.cdn-emitente       = wt-docto.cod-emitente        
              nota-fisc-adc.cod-natur-operac   = wt-docto.nat-operacao        
              nota-fisc-adc.idi-tip-dado       = 3                                  
              nota-fisc-adc.num-seq            = iSeqNfAdc.                            
    END.  
    
    ASSIGN nota-fisc-adc.cod-ser-docto-referado   = ttNotasAdicionais.serie
           nota-fisc-adc.cod-docto-referado       = ttNotasAdicionais.nr-doc-fis
           nota-fisc-adc.cdn-emit-docto-referado  = ttNotasAdicionais.cod-emitente
           nota-fisc-adc.dat-docto-referado       = ttNotasAdicionais.dt-emis-doc
           nota-fisc-adc.idi-tip-docto-referado   = 1 /* Entrada/Aquisi‡Æo */
           nota-fisc-adc.idi-tip-emit-referado    = IF nota-fisc-adc.cdn-emitente = nota-fisc-adc.cdn-emit-docto-referado THEN 1 ELSE 2.    
    

   RUN pi-buscaModelo IN hbofi258 (INPUT  wt-docto.cod-estabel                 ,
                                   INPUT  nota-fisc-adc.cod-ser-docto-referado ,
                                   INPUT  nota-fisc-adc.cod-docto-referado     ,
                                   INPUT  nota-fisc-adc.cdn-emit-docto-referado,
                                   INPUT  doc-fiscal.nat-operacao              ,
                                   OUTPUT c-modelo-aux                         ,
                                   OUTPUT c-chave-nfe                          ).


    IF trim(SUBSTRING(nota-fisc-adc.cod-livre-2,1,60)) = "" THEN
        ASSIGN OVERLAY(nota-fisc-adc.cod-livre-2,1,60) = TRIM(c-chave-nfe).

    ASSIGN nota-fisc-adc.cod-model-docto-referado = c-modelo-aux
           i-nat-oper-aux                         = INT(SUBSTRING(nota-fisc-adc.cod-natur-operac,1,1)).

    IF i-nat-oper-aux >= 5 THEN
       ASSIGN nota-fisc-adc.idi-tip-docto-referado = 2
              nota-fisc-adc.idi-tip-emit-referado  = 1.      
    ELSE 
       ASSIGN nota-fisc-adc.idi-tip-docto-referado = 1
              nota-fisc-adc.idi-tip-emit-referado  = 2.

    ASSIGN OVERLAY(nota-fisc-adc.cod-livre-1,091,16) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,107,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,121,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,135,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,149,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,163,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,177,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,191,14) = "0,00"
           OVERLAY(nota-fisc-adc.cod-livre-1,89,01)  = "1".

    RELEASE nota-fisc-adc.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-consulta wWindow 
PROCEDURE pi-retorna-consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign l-new-row = br-table:new-row in frame fpage0.
  
  if num-results('br-table') > 0 then do:
    assign i-row = br-table:focused-row in frame {&frame-name}.
    if (l-new-row) or
       (l-primeiro = yes) then do:
       if avail ttNotasAdicionais THEN DO:
       END.
    end.
    else do:
       get current br-table.
       display ttNotasAdicionais.c-marca     
               ttNotasAdicionais.nr-doc-fis  
               ttNotasAdicionais.serie       
               ttNotasAdicionais.nat-operacao
               ttNotasAdicionais.dt-emis-doc 
               ttNotasAdicionais.it-codigo   
               ttNotasAdicionais.quantidade                 
          with browse br-table no-error.
    end.
  end.
  
  
  if num-results('br-table') > 0 then do:
      if (l-new-row) or
         (l-primeiro = yes) then do:
          if i-row >= 1 THEN
              br-table:select-row(i-row).
      end.
      else
          br-table:select-row(i-row).
  end.  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaTT wWindow 
PROCEDURE piCarregaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE h-acomp AS HANDLE     NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Filtrando, Aguarde...").



FOR EACH estabelec NO-LOCK,
    EACH serie where 
         serie.serie >= pSerDoctoIni AND
         serie.serie <= pSerDoctoFin NO-LOCK,
    EACH doc-fiscal use-index ch-docto WHERE 
         doc-fiscal.cod-estabel   = estabelec.cod-estabel AND
         doc-fiscal.serie         = serie.serie           AND   
         doc-fiscal.nr-doc-fis   >= pNroDoctoIni          AND   
         doc-fiscal.nr-doc-fis   <= pNroDoctoFin          AND 
         doc-fiscal.nat-operacao >= pNatOperIni           AND
         doc-fiscal.nat-operacao <= pNatOperFin           NO-LOCK,
    EACH it-doc-fisc OF doc-fiscal WHERE
         it-doc-fisc.it-codigo   >= pItCodigoIni AND
         it-doc-fisc.it-codigo   <= pItCodigoFin NO-LOCK
    BREAK BY doc-fiscal.cod-estabel
          BY doc-fiscal.serie      
          BY doc-fiscal.nr-doc-fis 
          BY doc-fiscal.cod-emitente
          BY doc-fiscal.nat-operacao:  

    RUN pi-acompanhar IN h-acomp (INPUT "NF: " + string(doc-fiscal.cod-estabel ) +
                                                 string(doc-fiscal.serie       ) +
                                                 string(doc-fiscal.nr-doc-fis  ) +
                                                 string(doc-fiscal.cod-emitente) +
                                                 string(doc-fiscal.nat-operacao)).

    IF FIRST-OF(doc-fiscal.cod-estabel ) AND 
       FIRST-OF(doc-fiscal.serie       ) AND  
       FIRST-OF(doc-fiscal.nr-doc-fis  ) AND 
       FIRST-OF(doc-fiscal.cod-emitente) AND 
       FIRST-OF(doc-fiscal.nat-operacao) THEN
       ASSIGN deTotalBr = deTotalBr + doc-fiscal.vl-cont-doc.


    FIND FIRST ttNotasAdicionais WHERE
               ttNotasAdicionais.cod-estabel     = it-doc-fisc.cod-estabel   AND
               ttNotasAdicionais.serie           = it-doc-fisc.serie         AND
               ttNotasAdicionais.nr-doc-fis      = it-doc-fisc.nr-doc-fis    AND
               ttNotasAdicionais.cod-emitente    = it-doc-fisc.cod-emitente  AND
               ttNotasAdicionais.nat-operacao    = it-doc-fisc.nat-operacao  AND
               ttNotasAdicionais.nr-seq-doc      = it-doc-fisc.nr-seq-doc    EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL ttNotasAdicionais THEN
    DO:
       CREATE ttNotasAdicionais.
       ASSIGN ttNotasAdicionais.c-marca      = "" 
              ttNotasAdicionais.cod-estabel  = it-doc-fisc.cod-estabel  
              ttNotasAdicionais.serie        = it-doc-fisc.serie       
              ttNotasAdicionais.nr-doc-fis   = it-doc-fisc.nr-doc-fis   
              ttNotasAdicionais.cod-emitente = it-doc-fisc.cod-emitente 
              ttNotasAdicionais.nat-operacao = it-doc-fisc.nat-operacao 
              ttNotasAdicionais.nr-seq-doc   = it-doc-fisc.nr-seq-doc                  
              ttNotasAdicionais.dt-emis-doc  = it-doc-fisc.dt-emis-doc 
              ttNotasAdicionais.it-codigo    = it-doc-fisc.it-codigo  
              ttNotasAdicionais.quantidade   = it-doc-fisc.quantidade.            
              qtdTotalBr                     = qtdTotalBr + ttNotasAdicionais.quantidade.

    END.
END.

RUN pi-finalizar IN h-acomp.         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

