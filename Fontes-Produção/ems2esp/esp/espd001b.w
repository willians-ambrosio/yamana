{utp/ut-glob.i}
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation
 
 
/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-esp-sugestao-natureza NO-UNDO LIKE esp-sugestao-natureza
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-estabelec NO-UNDO LIKE estabelec
       field r-rowid as rowid.
 
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMaintenanceNoNavigation 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD001B 2.00.00.004 } /*** 010004 ***/
 
/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
 
/*&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESPD001B ESP}
&ENDIF*/
 
CREATE WIDGET-POOL.
 
/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program           ESPD001B
&GLOBAL-DEFINE Version        2.00.00.004
 
&GLOBAL-DEFINE Folder            NO
&GLOBAL-DEFINE InitialPage       1
 
&GLOBAL-DEFINE FolderLabels      
 
&GLOBAL-DEFINE ttTable           tt-esp-sugestao-natureza
&GLOBAL-DEFINE hDBOTable         h-boes001
&GLOBAL-DEFINE DBOTable          esp-sugestao-natureza
 
&GLOBAL-DEFINE ttParent          tt-estabelec
&GLOBAL-DEFINE DBOParentTable    h-boad107na
 
&GLOBAL-DEFINE page0KeyFields    
&GLOBAL-DEFINE page0Fields       tt-esp-sugestao-natureza.nat-operacao
&GLOBAL-DEFINE page0ParentFields 
&GLOBAL-DEFINE page1Fields       
&GLOBAL-DEFINE page2Fields       
 
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable         AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent        AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller        AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber AS INTEGER   NO-UNDO.
 
/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-campo NO-UNDO
    FIELD seq        AS INT
    FIELD cod-campo  AS CHAR FORMAT "X(22)"
    FIELD desc-valor AS CHAR FORMAT "x(360)"
    INDEX id IS PRIMARY seq.
 
DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.
 
/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}  AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boin245     AS HANDLE NO-UNDO.
DEFINE BUFFER bf-tt-campo FOR tt-campo.
DEFINE VARIABLE wh-pesquisa                      AS HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE  NO-UNDO.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 
 
/* ********************  Preprocessor Definitions  ******************** */
 
&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no
 
/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME BROWSE-2
 
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-campo
 
/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-campo.cod-campo tt-campo.desc-valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-campo
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-campo.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-campo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-campo
 
 
/* Definitions for FRAME fpage0                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fpage0 ~
    ~{&OPEN-QUERY-BROWSE-2}
 
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-esp-sugestao-natureza.nat-operacao 
&Scoped-define ENABLED-TABLES tt-esp-sugestao-natureza
&Scoped-define FIRST-ENABLED-TABLE tt-esp-sugestao-natureza
&Scoped-Define ENABLED-OBJECTS rtKeys rtToolBar RECT-2 c-desc-nat-operacao ~
BROWSE-2 btUpdate btOK btSave btCancel btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-esp-sugestao-natureza.nat-operacao 
&Scoped-define DISPLAYED-TABLES tt-esp-sugestao-natureza
&Scoped-define FIRST-DISPLAYED-TABLE tt-esp-sugestao-natureza
&Scoped-Define DISPLAYED-OBJECTS c-desc-nat-operacao 
 
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
 
/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME
 
 
 
/* ***********************  Control Definitions  ********************** */
 
/* Define the widget handle for the window                              */
DEFINE VAR wMaintenanceNoNavigation AS WIDGET-HANDLE NO-UNDO.
 
/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.
 
DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.
 
DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.
 
DEFINE BUTTON btSave 
     LABEL "Salvar" 
     SIZE 10 BY 1.
 
DEFINE BUTTON btUpdate 
     IMAGE-UP FILE "image\im-mod":U
     IMAGE-INSENSITIVE FILE "image\ii-mod":U
     LABEL "Update" 
     SIZE 4 BY 1.25
     FONT 4.
 
DEFINE VARIABLE c-desc-nat-operacao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.
 
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 13.25.
 
DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 1.5.
 
DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .
 
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-campo SCROLLING.
&ANALYZE-RESUME
 
/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 wMaintenanceNoNavigation _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-campo.cod-campo COLUMN-LABEL "Campo" WIDTH 20
    tt-campo.desc-valor  COLUMN-LABEL "Descricao"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77.72 BY 12.5
         FONT 1 FIT-LAST-COLUMN.
 
 
/* ************************  Frame Definitions  *********************** */
 
DEFINE FRAME fpage0
     tt-esp-sugestao-natureza.nat-operacao AT ROW 1.25 COL 30 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     c-desc-nat-operacao AT ROW 1.25 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     BROWSE-2 AT ROW 3.08 COL 2.29 WIDGET-ID 200
     btUpdate AT ROW 8.75 COL 84 HELP
          "Altera ocorrˆncia corrente" WIDGET-ID 10
     btOK AT ROW 16.63 COL 2
     btSave AT ROW 16.63 COL 13
     btCancel AT ROW 16.63 COL 24
     btHelp AT ROW 16.63 COL 80
     rtKeys AT ROW 1 COL 1
     rtToolBar AT ROW 16.38 COL 1
     RECT-2 AT ROW 2.75 COL 1 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.79
         FONT 1 WIDGET-ID 100.
 
 
/* *********************** Procedure Settings ************************ */
 
&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MaintenanceNoNavigation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-esp-sugestao-natureza T "?" NO-UNDO mgesp esp-sugestao-natureza
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-estabelec T "?" NO-UNDO mgesp estabelec
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS
 
/* *************************  Create Window  ************************** */
 
&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMaintenanceNoNavigation ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 16.79
         WIDTH              = 90
         MAX-HEIGHT         = 17.04
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17.04
         VIRTUAL-WIDTH      = 90
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wMaintenanceNoNavigation 
/* ************************* Included-Libraries *********************** */
 
{maintenancenonavigation/maintenancenonavigation.i}
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
 
 
/* ***********  Runtime Attributes and AppBuilder Settings  *********** */
 
&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMaintenanceNoNavigation
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 c-desc-nat-operacao fpage0 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenanceNoNavigation)
THEN wMaintenanceNoNavigation:HIDDEN = yes.
 
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME
 
 
/* Setting information for Queries and Browse Widgets fields            */
 
&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-campo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME
 
 
 
 
 
/* ************************  Control Triggers  ************************ */
 
&Scoped-define SELF-NAME wMaintenanceNoNavigation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenanceNoNavigation wMaintenanceNoNavigation
ON END-ERROR OF wMaintenanceNoNavigation
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMaintenanceNoNavigation wMaintenanceNoNavigation
ON WINDOW-CLOSE OF wMaintenanceNoNavigation
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME fpage0
DO:
  APPLY "CHOOSE" TO btUpdate IN FRAME fPage0.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wMaintenanceNoNavigation
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMaintenanceNoNavigation
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wMaintenanceNoNavigation
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN pi-valida-nat-oper.
 
    IF RETURN-VALUE = "OK" THEN DO:
        
        RUN pi-salvar  IN THIS-PROCEDURE.
        IF RETURN-VALUE = "OK":U THEN
            RUN saveRecord IN THIS-PROCEDURE.
        IF RETURN-VALUE = "OK":U THEN
            APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wMaintenanceNoNavigation
ON CHOOSE OF btSave IN FRAME fpage0 /* Salvar */
DO:
    RUN pi-valida-nat-oper.
 
    IF RETURN-VALUE = "OK" THEN DO:
        
        RUN pi-salvar  IN THIS-PROCEDURE.
        IF RETURN-VALUE = "OK":U THEN
            RUN saveRecord IN THIS-PROCEDURE.
        IF RETURN-VALUE = "OK":U THEN
            APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate wMaintenanceNoNavigation
ON CHOOSE OF btUpdate IN FRAME fpage0 /* Update */
DO:
     RUN esp\espd001c.w (INPUT        tt-campo.cod-campo,
                         INPUT-OUTPUT tt-campo.desc-valor).
 
    {&OPEN-QUERY-BROWSE-2}
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&Scoped-define SELF-NAME tt-esp-sugestao-natureza.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-esp-sugestao-natureza.nat-operacao wMaintenanceNoNavigation
ON ENTRY OF tt-esp-sugestao-natureza.nat-operacao IN FRAME fpage0 /* Natureza opera‡Æo */
DO:
 
  RUN pi-busca-descricao.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-esp-sugestao-natureza.nat-operacao wMaintenanceNoNavigation
ON F5 OF tt-esp-sugestao-natureza.nat-operacao IN FRAME fpage0 /* Natureza opera‡Æo */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                       &campo=tt-esp-sugestao-natureza.nat-operacao
                       &campozoom=nat-operacao
                       &FRAME=fPage0
                       &campo2=c-desc-nat-operacao
                       &campozoom2=denominacao
                       &FRAME2=fPage0
                       &EnableImplant="NO"}
 
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-esp-sugestao-natureza.nat-operacao wMaintenanceNoNavigation
ON LEAVE OF tt-esp-sugestao-natureza.nat-operacao IN FRAME fpage0 /* Natureza opera‡Æo */
DO:
    {method/referencefields.i 
            &HandleDBOLeave="h-boin245"
            &KeyValue1="tt-esp-sugestao-natureza.nat-operacao:screen-value in frame fPage0"
            &FieldName1="denominacao"
            &FieldScreen1="c-desc-nat-operacao"
            &Frame1="fPage0"}
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-esp-sugestao-natureza.nat-operacao wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-esp-sugestao-natureza.nat-operacao IN FRAME fpage0 /* Natureza opera‡Æo */
DO:
    APPLY "F5":U TO SELF.
END.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
&UNDEFINE SELF-NAME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 
 
 
/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{maintenancenonavigation/mainblock.i}
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
 
/* **********************  Internal Procedures  *********************** */
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wMaintenanceNoNavigation 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    IF VALID-HANDLE(h-boin245) THEN
        DELETE PROCEDURE h-boin245.
 
    ASSIGN h-boin245 = ?.
 
    RETURN "OK":U.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMaintenanceNoNavigation 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
 
    IF pcAction = "ADD" THEN DO:
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 1
               tt-campo.cod-campo  = "Estabelecimento Origem"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 2
               tt-campo.cod-campo  = "Grupo Cliente"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 3
               tt-campo.cod-campo  = "Grupo Estoque"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 4
               tt-campo.cod-campo  = "Fam¡lia Material"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 5
               tt-campo.cod-campo  = "Fam¡lia Comercial"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 6
               tt-campo.cod-campo  = "Classifica‡Æo Fiscal"
               tt-campo.desc-valor = "*".
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = 7
               tt-campo.cod-campo  = "C¢digo do Item"
               tt-campo.desc-valor = "*".
        
    END. /* IF pcAction = "ADD" */
    ELSE DO i = 1 TO 7:
 
        CREATE tt-campo.
        ASSIGN tt-campo.seq        = i
               tt-campo.cod-campo  = tt-esp-sugestao-natureza.cod-campo[i].
        IF tt-esp-sugestao-natureza.desc-valor[i] <> "" THEN
             ASSIGN tt-campo.desc-valor = tt-esp-sugestao-natureza.desc-valor[i].
        ELSE ASSIGN tt-campo.desc-valor = "*".
    
    END.
 
    {&OPEN-QUERY-BROWSE-2}
 
    ENABLE BROWSE-2
           btUpdate
        WITH FRAME fPage0.
 
    tt-esp-sugestao-natureza.nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage0.
 
    IF NOT VALID-HANDLE(h-boin245) THEN
        RUN inbo\boin245.p PERSISTENT SET h-boin245.
 
    RUN openQueryStatic IN h-boin245 (INPUT "Main":U).
 
    APPLY "LEAVE":U TO tt-esp-sugestao-natureza.nat-operacao IN FRAME fPage0.
 
    RETURN "OK":U.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-descricao wMaintenanceNoNavigation 
PROCEDURE pi-busca-descricao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
{method/referencefields.i &HandleDBOLeave="h-boin245"
                              &KeyValue1="tt-esp-sugestao-natureza.nat-operacao:screen-value in frame fPage0"
                              &FieldName1="denominacao"
                              &FieldScreen1="c-desc-nat-operacao"
                              &Frame1="fPage0"}
                              
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salvar wMaintenanceNoNavigation 
PROCEDURE pi-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    FOR EACH tt-campo:
 
        ASSIGN tt-esp-sugestao-natureza.cod-campo[tt-campo.seq]  = tt-campo.cod-campo
               tt-esp-sugestao-natureza.desc-valor[tt-campo.seq] = tt-campo.desc-valor.

    END. /* EACH tt-campo */
 
RETURN "OK":U.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-nat-oper wMaintenanceNoNavigation 
PROCEDURE pi-valida-nat-oper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
  FIND FIRST natur-oper WHERE natur-oper.nat-operacao = tt-esp-sugestao-natureza.nat-operacao:SCREEN-VALUE IN FRAME fPage0 NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN DO:
        /* ERRO */
        run utp\ut-msgs.r (input "show",
                           input 17006,
                           input "Natureza de Opera‡Æo nÆo encontrado." + "~~" + "NÆo foi possivel encontrar a Natureza de Opera‡Æo informado. Verifique se o mesmo foi informado corretamente.").
        apply 'entry' to tt-esp-sugestao-natureza.nat-operacao IN FRAME fPage0.
        RETURN "NOK":U.
    END.
    ELSE RETURN "OK":U.
 
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParentFields wMaintenanceNoNavigation 
PROCEDURE saveParentFields :
/*:T------------------------------------------------------------------------------
  Purpose:     Salva valores dos campos da tabela filho ({&ttTable}) com base 
               nos campos da tabela pai ({&ttParent})
  Parameters:  
  Notes:       Este m‚todo somente ‚ executado quando a vari vel pcAction 
               possuir os valores ADD ou COPY
------------------------------------------------------------------------------*/
    
    ASSIGN tt-esp-sugestao-natureza.cod-estabel     = tt-estabelec.cod-estabel
           /*tt-esp-sugestao-natureza.cod-tipo-pedido = tt-estabelec.cod-tipo-pedido*/.
 
    RETURN "OK":U.
END PROCEDURE.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
 
