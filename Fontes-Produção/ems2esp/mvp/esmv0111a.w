&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mi-espec NO-UNDO LIKE mi-espec
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-func-ofici NO-UNDO LIKE mmv-func-ofici
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-func-relac-especialid NO-UNDO LIKE mmv-func-relac-especialid
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMaintenanceNoNavigation 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0111A 2.00.00.000}  /*** 010000 ***/
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          ESMV0111A
&GLOBAL-DEFINE Version          2.00.00.000

&GLOBAL-DEFINE Folder           NO
&GLOBAL-DEFINE InitialPage      1

&GLOBAL-DEFINE FolderLabels     Especialidade

&GLOBAL-DEFINE ttTable          tt-mmv-func-relac-especialid
&GLOBAL-DEFINE hDBOtable        hDBORelac                   
&GLOBAL-DEFINE DBOTable         mmv-func-relac-especialid

&GLOBAL-DEFINE ttParent         tt-mmv-func-ofici 
&GLOBAL-DEFINE DBOParentTable   hDBOFuncOfici     

&GLOBAL-DEFINE page0KeyFields   tt-mmv-func-ofici.ep-codigo tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr tt-mmv-func-ofici.nom-func
&GLOBAL-DEFINE page0Fields        
&GLOBAL-DEFINE page1Fields      tt-mmv-func-relac-especialid.cod-especialid tt-mmv-func-relac-especialid.num-livre-1   
&GLOBAL-DEFINE page2Fields       
&GLOBAL-DEFINE page3Fields       

&GLOBAL-DEFINE page0Widgets     btOK  btHelp2

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable            AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent           AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction           AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller           AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber    AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE l-del-reg-0111     AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}       AS HANDLE NO-UNDO.
DEFINE VARIABLE {&DBOParentTable}  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-func-ofici.ep-codigo ~
tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr ~
tt-mmv-func-ofici.nom-func 
&Scoped-define ENABLED-TABLES tt-mmv-func-ofici
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-func-ofici
&Scoped-Define ENABLED-OBJECTS c-desc-estab c-desc-emp btOK btSave btCancel ~
btHelp rtKeys rtToolBar 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-func-ofici.ep-codigo ~
tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr ~
tt-mmv-func-ofici.nom-func 
&Scoped-define DISPLAYED-TABLES tt-mmv-func-ofici
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-func-ofici
&Scoped-Define DISPLAYED-OBJECTS c-desc-estab c-desc-emp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMaintenanceNoNavigation AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp  NO-CONVERT-3D-COLORS
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btSave 
     LABEL "&Salvar" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-desc-emp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.72 BY .88 NO-UNDO.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 3.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE fi-especialid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31.14 BY 1.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     c-desc-estab AT ROW 2.42 COL 32.29 COLON-ALIGNED NO-LABEL
     c-desc-emp AT ROW 1.42 COL 32.29 COLON-ALIGNED NO-LABEL
     btOK AT ROW 8.5 COL 2
     btSave AT ROW 8.5 COL 13
     btCancel AT ROW 8.5 COL 24
     btHelp AT ROW 8.5 COL 77.14
     tt-mmv-func-ofici.ep-codigo AT ROW 1.42 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-mmv-func-ofici.cod-estabel AT ROW 2.42 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-mmv-func-ofici.cod-matr AT ROW 3.42 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-mmv-func-ofici.nom-func AT ROW 3.42 COL 37.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22.57 BY .88
     rtKeys AT ROW 1.13 COL 1.43
     rtToolBar AT ROW 8.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.57 BY 9.17
         FONT 1.

DEFINE FRAME fPage1
     tt-mmv-func-relac-especialid.cod-especialid AT ROW 1.21 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.72 BY .88
     fi-especialid AT ROW 1.21 COL 28.14 COLON-ALIGNED NO-LABEL
     tt-mmv-func-relac-especialid.num-livre-1 AT ROW 2.63 COL 23.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Principal", 1,
"Segund ria", 2
          SIZE 25 BY 1
     "Atividade" VIEW-AS TEXT
          SIZE 7 BY .58 AT ROW 2.13 COL 22
     RECT-3 AT ROW 2.33 COL 20.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10 ROW 4.88
         SIZE 72 BY 3.25
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MaintenanceNoNavigation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mi-espec T "?" NO-UNDO mgmnt mi-espec
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-func-ofici T "?" NO-UNDO mgfro mmv-func-ofici
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-func-relac-especialid T "?" NO-UNDO mgesp mmv-func-relac-especialid
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
         HEIGHT             = 8.88
         WIDTH              = 89.72
         MAX-HEIGHT         = 30.33
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.33
         VIRTUAL-WIDTH      = 182.86
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
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fPage1:MOVE-AFTER-TAB-ITEM (c-desc-emp:HANDLE IN FRAME fpage0)
       XXTABVALXX = FRAME fPage1:MOVE-BEFORE-TAB-ITEM (btOK:HANDLE IN FRAME fpage0)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME fPage1
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMaintenanceNoNavigation)
THEN wMaintenanceNoNavigation:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
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


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wMaintenanceNoNavigation
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    ASSIGN l-del-reg-0111 = YES.
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
    RUN saveRecord IN THIS-PROCEDURE.
        
    ASSIGN l-del-reg-0111 = NO.             
       
    IF RETURN-VALUE = "OK":U THEN DO:                    
        APPLY "CLOSE":U TO THIS-PROCEDURE. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wMaintenanceNoNavigation
ON CHOOSE OF btSave IN FRAME fpage0 /* Salvar */
DO:
    RUN saveRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME tt-mmv-func-relac-especialid.cod-especialid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-relac-especialid.cod-especialid wMaintenanceNoNavigation
ON F5 OF tt-mmv-func-relac-especialid.cod-especialid IN FRAME fPage1 /* cod-especialid */
DO:
    {method/zoomfields.i 
           &ProgramZoom="yamzoom/z01yam001.w"
           &FieldZoom1="cod-especialid"
           &FieldScreen1="tt-mmv-func-relac-especialid.cod-especialid"
           &Frame1="fPage1"
           &FieldZoom2="descricao"
           &FieldScreen2="fi-especialid"
           &Frame2="fPage1"
           &EnableImplant="YES"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-relac-especialid.cod-especialid wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-func-relac-especialid.cod-especialid IN FRAME fPage1 /* cod-especialid */
DO:
    {include/leave.i
        &tabela=mmv-especialid-func
        &atributo-ref=descricao
        &variavel-ref=fi-especialid
        &where="mmv-especialid-func.cod-especialid = input frame fPage1 tt-mmv-func-relac-especialid.cod-especialid"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-relac-especialid.cod-especialid wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-func-relac-especialid.cod-especialid IN FRAME fPage1 /* cod-especialid */
DO:
    APPLY "f5":U TO tt-mmv-func-relac-especialid.cod-especialid IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME tt-mmv-func-ofici.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-ofici.cod-estabel wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-func-ofici.cod-estabel IN FRAME fpage0 /* Estabelecimento */
DO:
    for first estabelec
        where estabelec.cod-estabel = tt-mmv-func-ofici.cod-estabel:screen-value in frame fPage0 no-lock:
        assign c-desc-estab = estabelec.nome.
    end.
    display c-desc-estab with frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-func-ofici.ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-ofici.ep-codigo wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-func-ofici.ep-codigo IN FRAME fpage0 /* Empresa */
DO:
    for first empresa
        where empresa.ep-codigo = tt-mmv-func-ofici.ep-codigo:screen-value in frame fPage0 no-lock:
        assign c-desc-emp = empresa.nome.
    end.
    display c-desc-emp with frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{maintenancenonavigation/mainblock.i}
{abp/ab9000.i} /** Procedures controle de hora **/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMaintenanceNoNavigation 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     afterDisplayFields
  Parameters:  <none>
  Notes:       Override (ap¢s) do display dos campos em tela 
------------------------------------------------------------------------------*/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMaintenanceNoNavigation 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override (Ap¢s) inicializa‡Æo da tela
------------------------------------------------------------------------------*/
    {utp/ut-liter.i "Especialidade"}
    ASSIGN tt-mmv-func-relac-especialid.cod-especialid:LABEL IN FRAME fPage1 = RETURN-VALUE.

    apply "LEAVE":U to tt-mmv-func-ofici.ep-codigo                 in frame fPage0.
    apply "LEAVE":U to tt-mmv-func-ofici.cod-estabel               in frame fPage0.
    apply "LEAVE":U to tt-mmv-func-relac-especialid.cod-especialid in frame fPage1.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wMaintenanceNoNavigation 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     beforeInitializeInterface
  Parameters:  <none>
  Notes:       Override (antes) da inicializa‡Æo da tela
------------------------------------------------------------------------------*/

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMaintenanceNoNavigation 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     initializeDBOs
  Parameters:  <none>
  Notes:       Inicializa as BOs no programa
------------------------------------------------------------------------------*/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

