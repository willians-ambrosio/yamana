&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-cap-esp-turno NO-UNDO LIKE mmv-cap-esp-turno
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-turno-esp NO-UNDO LIKE mmv-turno-esp
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
{include/i-prgvrs.i ESMV0121C 2.06.00.000}  /*** 010000 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program           ESMV0121C
&GLOBAL-DEFINE Version           2.06.00.000

&GLOBAL-DEFINE ttTable           tt-mmv-cap-esp-turno
&GLOBAL-DEFINE hDBOTable         hDBOCapEspec
&GLOBAL-DEFINE DBOTable          mmv-cap-esp-turno

&GLOBAL-DEFINE ttParent          tt-mmv-turno-esp
&GLOBAL-DEFINE DBOParentTable    mmv-turno-esp
&GLOBAL-DEFINE hDBOParent        hDBOTurnoEsp

&GLOBAL-DEFINE page0KeyFields    tt-mmv-cap-esp-turno.cod-turno ~
                                 tt-mmv-cap-esp-turno.dt-efetivacao
&GLOBAL-DEFINE page0Fields       tt-mmv-cap-esp-turno.nr-tecnico

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable         AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent        AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller        AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE rCurrentAux AS ROWID NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOParent}  AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOMiTurno    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-cap-esp-turno.cod-turno ~
tt-mmv-cap-esp-turno.dt-efetivacao tt-mmv-cap-esp-turno.nr-tecnico 
&Scoped-define ENABLED-TABLES tt-mmv-cap-esp-turno
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-cap-esp-turno
&Scoped-Define ENABLED-OBJECTS rtKeys rtToolBar cDescTurno btOK btSave ~
btCancel btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-cap-esp-turno.cod-turno ~
tt-mmv-cap-esp-turno.dt-efetivacao tt-mmv-cap-esp-turno.nr-tecnico 
&Scoped-define DISPLAYED-TABLES tt-mmv-cap-esp-turno
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-cap-esp-turno
&Scoped-Define DISPLAYED-OBJECTS cDescTurno 

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

DEFINE VARIABLE cDescTurno AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .88 NO-UNDO.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.46.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     tt-mmv-cap-esp-turno.cod-turno AT ROW 1.25 COL 25.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     cDescTurno AT ROW 1.25 COL 34.14 COLON-ALIGNED NO-LABEL
     tt-mmv-cap-esp-turno.dt-efetivacao AT ROW 2.25 COL 25.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tt-mmv-cap-esp-turno.nr-tecnico AT ROW 3.25 COL 25.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .88
     btOK AT ROW 4.83 COL 2
     btSave AT ROW 4.83 COL 13
     btCancel AT ROW 4.83 COL 24
     btHelp AT ROW 4.83 COL 80
     rtKeys AT ROW 1 COL 1
     rtToolBar AT ROW 4.58 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 5.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MaintenanceNoNavigation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mmv-cap-esp-turno T "?" NO-UNDO mgesp mmv-cap-esp-turno
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-turno-esp T "?" NO-UNDO mgesp mmv-turno-esp
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
         HEIGHT             = 5.13
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
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
    IF RETURN-VALUE = "OK":U THEN
        APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME tt-mmv-cap-esp-turno.cod-turno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-cap-esp-turno.cod-turno wMaintenanceNoNavigation
ON F5 OF tt-mmv-cap-esp-turno.cod-turno IN FRAME fpage0 /* Turno */
DO:
  {method/ZoomFields.i
           &ProgramZoom="yamzoom/z01yam002.w"
           &FieldZoom1="cod-turno"
           &FieldScreen1="tt-mmv-cap-esp-turno.cod-turno"
           &Frame1="fPage0"
           &FieldZoom2="desc-turno"
           &FieldScreen2="cDescTurno"
           &Frame2="fPage0"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-cap-esp-turno.cod-turno wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-cap-esp-turno.cod-turno IN FRAME fpage0 /* Turno */
DO:
    FOR FIRST mmv-turno-esp
        WHERE mmv-turno-esp.cod-turno = INT(tt-mmv-cap-esp-turno.cod-turno:SCREEN-VALUE IN FRAME fPage0) NO-LOCK:
    END.
    ASSIGN cDescTurno:SCREEN-VALUE IN FRAME fPage0 = IF AVAIL mmv-turno-esp THEN mmv-turno-esp.desc-turno ELSE "":U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-cap-esp-turno.cod-turno wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-cap-esp-turno.cod-turno IN FRAME fpage0 /* Turno */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


if tt-mmv-cap-esp-turno.cod-turno:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name} then.

/*--- L¢gica para inicializa‡Æo do programam ---*/
{maintenancenonavigation/MainBlock.i}

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
    IF VALID-HANDLE(hDBOMiTurno) THEN 
       RUN destroy in hDBOMiTurno.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMaintenanceNoNavigation 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN tt-mmv-cap-esp-turno.cod-turno:SCREEN-VALUE IN FRAME fPage0     = STRING(tt-mmv-turno-esp.cod-turno)
           tt-mmv-cap-esp-turno.dt-efetivacao:SCREEN-VALUE IN FRAME fPage0 = STRING(tt-mmv-cap-esp-turno.dt-efetivacao,"99/99/9999").

    IF tt-mmv-cap-esp-turno.dt-efetivacao:SCREEN-VALUE IN FRAME fPage0 = "":U THEN
        ASSIGN tt-mmv-cap-esp-turno.dt-efetivacao:SCREEN-VALUE IN FRAME fPage0 = STRING(TODAY,"99/99/9999").

    APPLY "LEAVE":U TO tt-mmv-cap-esp-turno.cod-turno IN FRAME fPage0.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wMaintenanceNoNavigation 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOParent}) THEN DO:
        {btb/btb008za.i1 yambo/ydm017.p YES}
        {btb/btb008za.i2 yambo/ydm017.p '' {&hDBOParent}} 
    END.
    RUN OpenQueryStatic in {&hDBOParent} (input "Main":U).

    IF NOT VALID-HANDLE(hDBOMiTurno) THEN DO:
        {btb/btb008za.i1 mnbo/bomn015.p YES}
        {btb/btb008za.i2 mnbo/bomn015.p '' hDBOMiTurno} 
    END.
    RUN OpenQueryStatic in hDBOMiTurno (input "default":U).

    RETURN "OK":U.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParentFields wMaintenanceNoNavigation 
PROCEDURE saveParentFields :
/*------------------------------------------------------------------------------
  Purpose:     Salva valores dos campos da tabela filho ({&ttTable}) com base 
               nos campos da tabela pai ({&ttParent})
  Parameters:  
  Notes:       Este m‚todo somente ‚ executado quando a vari vel pcAction 
               possuir os valores ADD ou COPY
------------------------------------------------------------------------------*/
    assign {&ttTable}.cod-turno      = {&ttParent}.cod-turno
           {&ttTable}.cod-especialid = {&ttParent}.cod-especialid.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

