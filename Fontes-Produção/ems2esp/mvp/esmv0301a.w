&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
       fields r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-tar-ord-manut NO-UNDO LIKE mmv-tar-ord-manut
       fields r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-tecnico-tarefa-om NO-UNDO LIKE mmv-tecnico-tarefa-om
       fields r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMaintenanceNoNavigation 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0301A 2.00.00.000}  /*** 010000 ***/
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
&GLOBAL-DEFINE Program           ESMV0301A
&GLOBAL-DEFINE Version           2.00.00.000

&GLOBAL-DEFINE Folder            NO
&GLOBAL-DEFINE InitialPage       1

&GLOBAL-DEFINE FolderLabels      Tarefa

&GLOBAL-DEFINE ttTable           tt-mmv-tecnico-tarefa-om
&GLOBAL-DEFINE hDBOtable         hDBOmmv-tecnico-tarefa-om
&GLOBAL-DEFINE DBOTable          mmv-tecnico-tarefa-om

&GLOBAL-DEFINE ttParent          tt-mmv-tar-ord-manut
&GLOBAL-DEFINE DBOParentTable    hDBOmmv-tar-ord-manut

&GLOBAL-DEFINE page0KeyFields    tt-mmv-tecnico-tarefa-om.dt-prevista
&GLOBAL-DEFINE page0Fields       
&GLOBAL-DEFINE page1Fields       tt-mmv-tecnico-tarefa-om.cod-matr ~
                                 tt-mmv-tecnico-tarefa-om.tempo-previsto
&GLOBAL-DEFINE page2Fields       
&GLOBAL-DEFINE page3Fields       

&GLOBAL-DEFINE page0Widgets   btOK  btHelp2

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable            AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent           AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction           AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller           AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber    AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE i-cancel-formation AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE VARIABLE wh-pesquisa  AS HANDLE    NO-UNDO.
/* Utilizada para a integracao com Ativo Fixo EMS 5 */
def new global shared var v_rec_bem_pat as RECID initial ? no-undo. /* carregada no zoom prgfin/fas/fas701ka.p. */
/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}       AS HANDLE NO-UNDO.
DEFINE VARIABLE {&DBOParentTable}  AS HANDLE NO-UNDO.

{include/i_fnctrad.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-tecnico-tarefa-om.dt-prevista 
&Scoped-define ENABLED-TABLES tt-mmv-tecnico-tarefa-om
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-tecnico-tarefa-om
&Scoped-Define ENABLED-OBJECTS btOK btSave btCancel btHelp rtKeys rtToolBar ~
RECT-20 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-tecnico-tarefa-om.dt-prevista 
&Scoped-define DISPLAYED-TABLES tt-mmv-tecnico-tarefa-om
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-tecnico-tarefa-om


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

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 1.63.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 2.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE cDescFunc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.14 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 8.5 COL 2
     btSave AT ROW 8.5 COL 13
     btCancel AT ROW 8.5 COL 24
     btHelp AT ROW 8.5 COL 77.14
     tt-mmv-tecnico-tarefa-om.dt-prevista AT ROW 2 COL 33.43 COLON-ALIGNED
          LABEL "Data"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     "Data Prevista" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 1.25 COL 31.86
     rtKeys AT ROW 1 COL 1
     rtToolBar AT ROW 8.25 COL 1
     RECT-20 AT ROW 1.63 COL 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.57 BY 9.17
         FONT 1.

DEFINE FRAME fPage1
     tt-mmv-tecnico-tarefa-om.cod-matr AT ROW 1.29 COL 14.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .88
     cDescFunc AT ROW 1.29 COL 24.43 COLON-ALIGNED NO-LABEL
     tt-mmv-tecnico-tarefa-om.tempo-previsto AT ROW 2.25 COL 14.72 COLON-ALIGNED
          LABEL "Tempo (hr)"
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10 ROW 3.75
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
      TABLE: tt-mmv-especialid-func T "?" NO-UNDO mgesp mmv-especialid-func
      ADDITIONAL-FIELDS:
          fields r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-tar-ord-manut T "?" NO-UNDO movfro mmv-tar-ord-manut
      ADDITIONAL-FIELDS:
          fields r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-tecnico-tarefa-om T "?" NO-UNDO mgesp mmv-tecnico-tarefa-om
      ADDITIONAL-FIELDS:
          fields r-rowid as rowid
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
         MAX-HEIGHT         = 28.83
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.83
         VIRTUAL-WIDTH      = 146.29
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

ASSIGN XXTABVALXX = FRAME fPage1:MOVE-BEFORE-TAB-ITEM (btOK:HANDLE IN FRAME fpage0)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN tt-mmv-tecnico-tarefa-om.dt-prevista IN FRAME fpage0
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* SETTINGS FOR FILL-IN tt-mmv-tecnico-tarefa-om.tempo-previsto IN FRAME fPage1
   EXP-LABEL                                                            */
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
    ASSIGN i-cancel-formation = YES.
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
        
    ASSIGN i-cancel-formation = NO.             
       
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
&Scoped-define SELF-NAME tt-mmv-tecnico-tarefa-om.cod-matr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-tecnico-tarefa-om.cod-matr wMaintenanceNoNavigation
ON F5 OF tt-mmv-tecnico-tarefa-om.cod-matr IN FRAME fPage1 /* Funcion rio */
DO:
    {method/zoomfields.i 
           &ProgramZoom="frzoom/z01fr032.w"
           &FieldZoom1="cod-matr"
           &FieldScreen1="tt-mmv-tecnico-tarefa-om.cod-matr"
           &Frame1="fPage1"
           &FieldZoom2="nom-func"
           &FieldScreen2="cDescFunc"
           &Frame2="fPage1"
           &EnableImplant="YES"}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-tecnico-tarefa-om.cod-matr wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-tecnico-tarefa-om.cod-matr IN FRAME fPage1 /* Funcion rio */
DO:
    for first mmv-func-ofici
        where mmv-func-ofici.cod-matr = tt-mmv-tecnico-tarefa-om.cod-matr:screen-value in frame fPage1 no-lock:
    end.
    if avail mmv-func-ofici then
        assign cDescFunc = mmv-func-ofici.nom-func.
    else 
        assign cDescFunc = "":U.

    display cDescFunc with frame fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-tecnico-tarefa-om.cod-matr wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-tecnico-tarefa-om.cod-matr IN FRAME fPage1 /* Funcion rio */
DO:
    APPLY "f5":U TO tt-mmv-tecnico-tarefa-om.cod-matr IN FRAME fPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/

{maintenancenonavigation/mainblock.i}

{abp/ab9000.i} /** Procedures controle de hora **/

ASSIGN tt-mmv-tecnico-tarefa-om.tempo-previsto:SCREEN-VALUE IN FRAME fPage1 = "0,00".

tt-mmv-tecnico-tarefa-om.cod-matr:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame fPage1.

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
    {utp/ut-liter.i "Data"}
    ASSIGN tt-mmv-tecnico-tarefa-om.dt-prevista:LABEL IN FRAME fPage0 = RETURN-VALUE.
    
    {utp/ut-liter.i "Funcion rio"}
    ASSIGN tt-mmv-tecnico-tarefa-om.cod-matr:LABEL IN FRAME fPage1 = RETURN-VALUE.
    
    {utp/ut-liter.i "Tempo_(hr)"}
    ASSIGN tt-mmv-tecnico-tarefa-om.tempo-previsto:LABEL IN FRAME fPage1 = RETURN-VALUE.

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
    ASSIGN tt-mmv-tecnico-tarefa-om.dt-prevista:SENSITIVE IN FRAME fPage0 = YES
           dt-prevista:SCREEN-VALUE IN FRAME fPage0 = "31/12/9999".
    
    IF i-cancel-formation = NO THEN DO:
        FOR FIRST mab-eqpto 
            where mab-eqpto.cod-eqpto = tt-mmv-tar-ord-manut.cod-eqpto NO-LOCK:
    
            FIND FIRST mmv-tempo-padr 
                WHERE  mmv-tempo-padr.cod-evento   = tt-mmv-tar-ord-manut.cod-evento 
                AND    mmv-tempo-padr.cod-sub-sist = tt-mmv-tar-ord-manut.cod-sub-sist NO-LOCK NO-ERROR.
    
            IF AVAIL mmv-tempo-padr THEN             
                ASSIGN tt-mmv-tecnico-tarefa-om.tempo-previsto:SENSITIVE IN FRAME fPage1 = YES
                       tempo-previsto:SCREEN-VALUE IN FRAME fPage1 = string(mmv-tempo-padr.val-tempo-padr).        
            ELSE 
                ASSIGN tt-mmv-tecnico-tarefa-om.tempo-previsto:SENSITIVE IN FRAME fPage1 = YES
                       tempo-previsto:SCREEN-VALUE IN FRAME fPage1 = "0,00".        
        END.
    END.
    
    APPLY "LEAVE":U TO tt-mmv-tecnico-tarefa-om.cod-matr IN FRAME fPage1.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeSaveFields wMaintenanceNoNavigation 
PROCEDURE beforeSaveFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for first mmv-func-ofici
        where mmv-func-ofici.cod-matr = tt-mmv-tecnico-tarefa-om.cod-matr:screen-value in frame fPage1 no-lock:
    end.
    if avail mmv-func-ofici then
        assign {&ttTable}.ep-codigo   = mmv-func-ofici.ep-codigo  
               {&ttTable}.cod-estabel = mmv-func-ofici.cod-estabel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

