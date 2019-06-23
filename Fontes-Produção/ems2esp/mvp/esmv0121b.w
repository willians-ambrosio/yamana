&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-exc-esp-data NO-UNDO LIKE mmv-exc-esp-data
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
{include/i-prgvrs.i ESMV0121B 2.06.00.000}  /*** 010004 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{cdp/cdcfgmnt.i}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program           ESMV0121B
&GLOBAL-DEFINE Version           2.06.00.000

&GLOBAL-DEFINE ttTable           tt-mmv-exc-esp-data
&GLOBAL-DEFINE hDBOTable         hDBOExcEsp
&GLOBAL-DEFINE DBOTable          mmv-exc-esp-data

&GLOBAL-DEFINE ttParent          tt-mmv-turno-esp
&GLOBAL-DEFINE hDBOParent        hDBOTurnoEsp
&GLOBAL-DEFINE DBOParentTable    mmv-turno-esp

&GLOBAL-DEFINE page0KeyFields    tt-mmv-exc-esp-data.cod-turno tt-mmv-exc-esp-data.data

&GLOBAL-DEFINE page0ParentFields tt-mmv-turno-esp.cod-especialid

&GLOBAL-DEFINE page0Fields       tt-mmv-exc-esp-data.hora-inicial ~
                                 tt-mmv-exc-esp-data.hora-termino ~
                                 tt-mmv-exc-esp-data.nr-tecnico ~
                                 tt-mmv-exc-esp-data.obs                                 

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER prTable         AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER prParent        AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER pcAction        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phCaller        AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER piSonPageNumber AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCdCalen AS CHAR NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}    AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOParent}   AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOMiTurno     AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOMiEspec     AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOCapEspTurno AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-turno-esp.cod-especialid ~
tt-mmv-exc-esp-data.cod-turno tt-mmv-exc-esp-data.data ~
tt-mmv-exc-esp-data.hora-inicial tt-mmv-exc-esp-data.hora-termino ~
tt-mmv-exc-esp-data.nr-tecnico tt-mmv-exc-esp-data.obs 
&Scoped-define ENABLED-TABLES tt-mmv-turno-esp tt-mmv-exc-esp-data
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-turno-esp
&Scoped-define SECOND-ENABLED-TABLE tt-mmv-exc-esp-data
&Scoped-Define ENABLED-OBJECTS rtKeys RECT-9 rtToolBar cDescEspec ~
cDescTurno btOK btSave btCancel btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-turno-esp.cod-especialid ~
tt-mmv-exc-esp-data.cod-turno tt-mmv-exc-esp-data.data ~
tt-mmv-exc-esp-data.hora-inicial tt-mmv-exc-esp-data.hora-termino ~
tt-mmv-exc-esp-data.nr-tecnico tt-mmv-exc-esp-data.obs 
&Scoped-define DISPLAYED-TABLES tt-mmv-turno-esp tt-mmv-exc-esp-data
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-turno-esp
&Scoped-define SECOND-DISPLAYED-TABLE tt-mmv-exc-esp-data
&Scoped-Define DISPLAYED-OBJECTS cDescEspec cDescTurno 

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

DEFINE VARIABLE cDescEspec AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 35.43 BY .88 NO-UNDO.

DEFINE VARIABLE cDescTurno AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 4.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     tt-mmv-turno-esp.cod-especialid AT ROW 1.17 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     cDescEspec AT ROW 1.17 COL 40.57 COLON-ALIGNED NO-LABEL
     tt-mmv-exc-esp-data.cod-turno AT ROW 2.25 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     cDescTurno AT ROW 2.25 COL 37.43 COLON-ALIGNED NO-LABEL
     tt-mmv-exc-esp-data.data AT ROW 3.33 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     tt-mmv-exc-esp-data.hora-inicial AT ROW 5 COL 28 COLON-ALIGNED FORMAT "99:99:99"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     tt-mmv-exc-esp-data.hora-termino AT ROW 6 COL 28 COLON-ALIGNED FORMAT "99:99:99"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     tt-mmv-exc-esp-data.nr-tecnico AT ROW 6 COL 54.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-mmv-exc-esp-data.obs AT ROW 7 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36.86 BY .88
     btOK AT ROW 8.75 COL 2
     btSave AT ROW 8.75 COL 13
     btCancel AT ROW 8.75 COL 24
     btHelp AT ROW 8.75 COL 80
     rtKeys AT ROW 1 COL 1
     RECT-9 AT ROW 4.5 COL 1
     rtToolBar AT ROW 8.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.57 BY 9.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MaintenanceNoNavigation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mmv-exc-esp-data T "?" NO-UNDO mgesp mmv-exc-esp-data
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
         HEIGHT             = 9.04
         WIDTH              = 90.86
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
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN tt-mmv-exc-esp-data.hora-inicial IN FRAME fpage0
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN tt-mmv-exc-esp-data.hora-termino IN FRAME fpage0
   EXP-FORMAT                                                           */
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
    RUN gotoKey IN hDBOExcEsp (INPUT tt-mmv-exc-esp-data.cod-turno,
                               INPUT tt-mmv-turno-esp.cod-especialid,
                               INPUT tt-mmv-exc-esp-data.data).

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


&Scoped-define SELF-NAME tt-mmv-turno-esp.cod-especialid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cod-especialid wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-turno-esp.cod-especialid IN FRAME fpage0 /* Especialidade */
DO:
    FOR FIRST mmv-especialid-func
        WHERE mmv-especialid-func.cod-especialid = tt-mmv-turno-esp.cod-especialid:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
    END.
    ASSIGN cDescEspec:SCREEN-VALUE IN FRAME fPage0 = IF AVAIL mmv-especialid-func THEN mmv-especialid-func.descricao ELSE "":U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-exc-esp-data.cod-turno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.cod-turno wMaintenanceNoNavigation
ON F5 OF tt-mmv-exc-esp-data.cod-turno IN FRAME fpage0 /* Turno */
DO:
  {method/ZoomFields.i
           &ProgramZoom="yamzoom/z01yam002.w"
           &FieldZoom1="cod-turno"
           &FieldScreen1="tt-mmv-exc-esp-data.cod-turno"
           &Frame1="fPage0"
           &FieldZoom2="desc-turno"
           &FieldScreen2="cDescTurno"
           &Frame2="fPage0"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.cod-turno wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-exc-esp-data.cod-turno IN FRAME fpage0 /* Turno */
DO:
    FOR FIRST mmv-turno-esp
        WHERE mmv-turno-esp.cod-turno = INT(tt-mmv-exc-esp-data.cod-turno:SCREEN-VALUE IN FRAME fPage0) NO-LOCK:
    END.
    ASSIGN cDescTurno:SCREEN-VALUE IN FRAME fPage0 = IF AVAIL mmv-turno-esp THEN mmv-turno-esp.desc-turno ELSE "":U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.cod-turno wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-exc-esp-data.cod-turno IN FRAME fpage0 /* Turno */
DO:
  APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-exc-esp-data.data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.data wMaintenanceNoNavigation
ON F5 OF tt-mmv-exc-esp-data.data IN FRAME fpage0 /* Data */
DO:
  
    {method/ZoomFields.i &ProgramZoom="mnzoom/z01mn005.w"
                     &FieldZoom1="data"
                     &FieldScreen1="tt-mmv-exc-esp-data.data"
                     &Frame1="fPage0"
                     &FieldZoom2="hora-inicio"
                     &FieldScreen2="tt-mmv-exc-esp-data.hora-inicial"
                     &Frame2="fPage0"
                     &FieldZoom3="hora-termino"
                     &FieldScreen3="tt-mmv-exc-esp-data.hora-termino"
                     &Frame3="fPage0"
                     &RunMethod="RUN piSetaInicial IN hProgramZoom (INPUT cCdCalen, 
                                                                    input input frame fPage0 tt-mmv-exc-esp-data.cod-turno, 
                                                                    input ?)."
                     &EnableImplant="NO"}
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.data wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-exc-esp-data.data IN FRAME fpage0 /* Data */
DO:
    IF tt-mmv-exc-esp-data.data:MODIFIED IN FRAME fPage0 THEN DO:
        IF NOT VALID-HANDLE(hDBOCapEspTurno) THEN DO:
            {btb/btb008za.i1 mnbo/bomn002.p YES}
            {btb/btb008za.i2 mnbo/bomn002.p '' hDBOCapEspTurno} 
            RUN setConstraintTpEspecial IN hDBOCapEspTurno (INPUT FRAME fPage0 tt-mmv-turno-esp.cod-especialid).
        END.
        RUN setConstraintCdTurno IN hDBOCapEspTurno (INPUT FRAME fPage0 tt-mmv-exc-esp-data.cod-turno,
                                                     INPUT FRAME fPage0 tt-mmv-exc-esp-data.cod-turno).  
        RUN OpenQueryStatic IN hDBOCapEspTurno (INPUT "MiEspec":U).
        RUN findlastMenorIgualData IN hDBOCapEspTurno (INPUT FRAME fPage0 tt-mmv-turno-esp.cod-especialid,
                                                       INPUT FRAME fPage0 tt-mmv-exc-esp-data.cod-turno,
                                                       INPUT FRAME fPage0 tt-mmv-exc-esp-data.data). 
        IF RETURN-VALUE = "OK":U THEN 
           RUN getIntField IN hDBOCapEspTurno (INPUT "nr-tecnico":U, OUTPUT tt-mmv-exc-esp-data.nr-tecnico).
        ELSE
           ASSIGN tt-mmv-exc-esp-data.nr-tecnico = 0.
    
        DISPLAY tt-mmv-exc-esp-data.nr-tecnico WITH FRAME fPage0.      
        ASSIGN tt-mmv-exc-esp-data.data:MODIFIED IN FRAME fPage0 = NO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.data wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-exc-esp-data.data IN FRAME fpage0 /* Data */
DO:
  APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-exc-esp-data.hora-inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.hora-inicial wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-exc-esp-data.hora-inicial IN FRAME fpage0 /* Hora In¡cio */
DO:
  /* Include para completar a hora com '00' */
    {cdp/cd9998.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-exc-esp-data.hora-termino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-exc-esp-data.hora-termino wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-exc-esp-data.hora-termino IN FRAME fpage0 /* Hora T‚rmino */
DO:
  /* Include para completar a hora com '00' */
    {cdp/cd9998.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


if tt-mmv-exc-esp-data.cod-turno:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name} then.
if tt-mmv-exc-esp-data.data:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name} then.

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

    IF VALID-HANDLE({&hDBOParent}) THEN 
       RUN destroy in {&hDBOParent}.       
      
    IF VALID-HANDLE(hDBOMiEspec) THEN 
       RUN destroy in hDBOMiEspec.

    IF VALID-HANDLE(hDBOMiTurno) THEN 
       RUN destroy in hDBOMiTurno.  

    RETURN "OK":U.
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

    tt-mmv-exc-esp-data.cod-turno:SCREEN-VALUE IN FRAME fPage0 = STRING(tt-mmv-turno-esp.cod-turno).

    IF pcAction = "ADD" THEN 
         ASSIGN tt-mmv-exc-esp-data.data:SCREEN-VALUE IN FRAME fPage0           = STRING(TODAY)
                tt-mmv-exc-esp-data.hora-inicial:SCREEN-VALUE  IN FRAME fPage0  = "000000"
                tt-mmv-exc-esp-data.hora-termino:SCREEN-VALUE  IN FRAME fPage0  = "000000".
    ELSE DO:
        IF tt-mmv-exc-esp-data.hora-inicial <> "":U THEN
            ASSIGN tt-mmv-exc-esp-data.hora-inicial:SCREEN-VALUE IN FRAME fPage0 = SUBSTRING(tt-mmv-exc-esp-data.hora-inicial,1,2) +
                                                                                   SUBSTRING(tt-mmv-exc-esp-data.hora-inicial,4,2) + 
                                                                                   SUBSTRING(tt-mmv-exc-esp-data.hora-inicial,7,2).
        ELSE
            ASSIGN tt-mmv-exc-esp-data.hora-inicial:SCREEN-VALUE IN FRAME fPage0 = "000000".

        IF tt-mmv-exc-esp-data.hora-termino <> "":U THEN
            ASSIGN tt-mmv-exc-esp-data.hora-termino:SCREEN-VALUE IN FRAME fPage0 = SUBSTRING(tt-mmv-exc-esp-data.hora-termino,1,2) +
                                                                                   SUBSTRING(tt-mmv-exc-esp-data.hora-termino,4,2) + 
                                                                                   SUBSTRING(tt-mmv-exc-esp-data.hora-termino,7,2).
        ELSE 
            ASSIGN tt-mmv-exc-esp-data.hora-termino:SCREEN-VALUE  IN FRAME fPage0 = "000000".
            
        ASSIGN tt-mmv-exc-esp-data.nr-tec:SCREEN-VALUE IN FRAME fPage0 = STRING(tt-mmv-exc-esp-data.nr-tec).
        IF tt-mmv-exc-esp-data.data:SCREEN-VALUE IN FRAME fPage0 = "":U THEN
            ASSIGN tt-mmv-exc-esp-data.data:SCREEN-VALUE IN FRAME fPage0 = STRING(TODAY).
    END.

    APPLY "LEAVE":U TO tt-mmv-turno-esp.cod-especialid IN FRAME fPage0.  
    APPLY "LEAVE":U TO tt-mmv-exc-esp-data.cod-turno   IN FRAME fPage0.

    IF pcAction = "ADD":U THEN
        APPLY "LEAVE":U TO tt-mmv-exc-esp-data.data IN FRAME fPage0.   

    RETURN "OK":U.                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterSaveFields wMaintenanceNoNavigation 
PROCEDURE afterSaveFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN tt-mmv-exc-esp-data.hora-inicial = tt-mmv-exc-esp-data.hora-inicial:SCREEN-VALUE IN FRAME fPage0
           tt-mmv-exc-esp-data.hora-termino = tt-mmv-exc-esp-data.hora-termino:SCREEN-VALUE IN FRAME fPage0.
  
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
    RUN OpenQueryStatic in {&hDBOParent} (INPUT "Main":U) NO-ERROR.
    
    IF NOT VALID-HANDLE(hDBOMiTurno) THEN DO:
        {btb/btb008za.i1 mnbo/bomn015.p YES}
        {btb/btb008za.i2 mnbo/bomn015.p '' hDBOMiTurno} 
    END.
    RUN OpenQueryStatic in hDBOMiTurno (input "default":U).

    IF NOT VALID-HANDLE(hDBOMiEspec) THEN DO:
        {btb/btb008za.i1 mnbo/bomn112.p YES}
        {btb/btb008za.i2 mnbo/bomn112.p '' hDBOMiEspec} 
    END.
    RUN OpenQueryStatic in hDBOMiEspec (INPUT "default":U) NO-ERROR.

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
    assign {&ttTable}.cod-especialid = {&ttParent}.cod-especialid.
  
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

