&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMaintenanceNoNavigation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
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
{include/i-prgvrs.i ESMV0121A 2.06.00.000}  /*** 010004 ***/
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
&GLOBAL-DEFINE Program           ESMV0121A
&GLOBAL-DEFINE Version           2.06.00.000

&GLOBAL-DEFINE ttTable           tt-mmv-turno-esp
&GLOBAL-DEFINE hDBOTable         hDBOTurnoEsp
&GLOBAL-DEFINE DBOTable          mmv-turno-esp

&GLOBAL-DEFINE ttParent          tt-mmv-especialid-func
&GLOBAL-DEFINE DBOParentTable    hDBOEspFunc

&GLOBAL-DEFINE page0KeyFields    tt-mmv-turno-esp.cod-turno
&GLOBAL-DEFINE page0Fields       tt-mmv-turno-esp.desc-turno tt-mmv-turno-esp.cd-calen
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
DEFINE VARIABLE c-cd-calen AS CHAR NO-UNDO.

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable}        AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOMiTurno         AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOTurnoCalenGener AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOEspFunc         AS HANDLE NO-UNDO.

DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MaintenanceNoNavigation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-turno-esp.cod-turno ~
tt-mmv-turno-esp.cd-calen 
&Scoped-define ENABLED-TABLES tt-mmv-turno-esp
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-turno-esp
&Scoped-Define ENABLED-OBJECTS rtKeys rtToolBar btOK btSave btCancel btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-turno-esp.cod-turno ~
tt-mmv-turno-esp.desc-turno tt-mmv-turno-esp.cd-calen 
&Scoped-define DISPLAYED-TABLES tt-mmv-turno-esp
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-turno-esp
&Scoped-Define DISPLAYED-OBJECTS cDescCalen 

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

DEFINE VARIABLE cDescCalen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88.

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.92.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     tt-mmv-turno-esp.cod-turno AT ROW 1.5 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     tt-mmv-turno-esp.desc-turno AT ROW 1.5 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY .88
     tt-mmv-turno-esp.cd-calen AT ROW 2.5 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     cDescCalen AT ROW 2.5 COL 34 COLON-ALIGNED NO-LABEL
     btOK AT ROW 4.38 COL 2
     btSave AT ROW 4.38 COL 13
     btCancel AT ROW 4.38 COL 24
     btHelp AT ROW 4.38 COL 80
     rtKeys AT ROW 1.08 COL 1
     rtToolBar AT ROW 4.13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 4.67
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
         HEIGHT             = 4.71
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
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
/* SETTINGS FOR FILL-IN cDescCalen IN FRAME fpage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-mmv-turno-esp.desc-turno IN FRAME fpage0
   NO-ENABLE                                                            */
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


&Scoped-define SELF-NAME tt-mmv-turno-esp.cd-calen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cd-calen wMaintenanceNoNavigation
ON F5 OF tt-mmv-turno-esp.cd-calen IN FRAME fpage0 /* Calend rio */
DO:
    assign l-implanta = no.
    {include/zoomvar.i &prog-zoom="inzoom/z01in026.w"
                     &campo=tt-mmv-turno-esp.cd-calen
                     &campozoom=cd-calen
                     &campo2=cDescCalen
                     &campozoom2=descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cd-calen wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-turno-esp.cd-calen IN FRAME fpage0 /* Calend rio */
DO:
    FOR FIRST calen-gener 
        WHERE calen-gener.cd-calen = tt-mmv-turno-esp.cd-calen:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
    END.
    ASSIGN cDescCalen = IF AVAIL calen-gener THEN calen-gener.descricao ELSE "":U.

    DISPLAY cDescCalen WITH FRAME fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cd-calen wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-turno-esp.cd-calen IN FRAME fpage0 /* Calend rio */
DO:
    apply 'f5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-turno-esp.cod-turno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cod-turno wMaintenanceNoNavigation
ON F5 OF tt-mmv-turno-esp.cod-turno IN FRAME fpage0 /* Turno */
DO:
    {method/ZoomFields.i 
           &ProgramZoom="mnzoom/z02mn015.w"
           &FieldZoom1="cd-turno"
           &FieldScreen1="tt-mmv-turno-esp.cod-turno"
           &Frame1="fPage0"
           &FieldZoom2="descricao"
           &FieldScreen2="tt-mmv-turno-esp.desc-turno"
           &Frame2="fPage0"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cod-turno wMaintenanceNoNavigation
ON LEAVE OF tt-mmv-turno-esp.cod-turno IN FRAME fpage0 /* Turno */
DO:
    {method/ReferenceFields.i 
          &HandleDBOLeave="hDBOMiTurno"
          &KeyValue1="input frame fPage0 tt-mmv-turno-esp.cod-turno"
          &FieldName1="descricao"
          &FieldScreen1="input frame fPage0 tt-mmv-turno-esp.desc-turno"
          &Frame1="fPage0"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-turno-esp.cod-turno wMaintenanceNoNavigation
ON MOUSE-SELECT-DBLCLICK OF tt-mmv-turno-esp.cod-turno IN FRAME fpage0 /* Turno */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMaintenanceNoNavigation 


if tt-mmv-turno-esp.cod-turno:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name} then.
if tt-mmv-turno-esp.cd-calen:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name} then.

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
       RUN destroy IN hDBOMiTurno.

    IF VALID-HANDLE(hDBOEspFunc) THEN
       RUN destroy IN hDBOEspFunc. 

    IF VALID-HANDLE(hDBOTurnoCalenGener) THEN
       RUN destroy IN hDBOTurnoCalenGener. 

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
    ASSIGN tt-mmv-turno-esp.desc-turno:SENSITIVE IN FRAME fPage0 = NO.
    ASSIGN cDescCalen:SENSITIVE                  IN FRAME fPage0 = NO.

    APPLY "LEAVE":U TO tt-mmv-turno-esp.cod-turno IN FRAME fPage0.
    APPLY "LEAVE":U TO cDescCalen IN FRAME fPage0.
    
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
    IF NOT VALID-HANDLE(hDBOMiTurno) THEN DO:
        {btb/btb008za.i1 mnbo/bomn015.p YES}
        {btb/btb008za.i2 mnbo/bomn015.p '' hDBOMiTurno} 
    END.
    RUN openQueryStatic IN hDBOMiTurno (INPUT "Default":U) NO-ERROR.

    IF NOT VALID-HANDLE(hDBOEspFunc) THEN DO:
        {btb/btb008za.i1 yambo/ydm004.p YES}
        {btb/btb008za.i2 yambo/ydm004.p '' hDBOEspFunc} 
    END.
    RUN openQueryStatic IN hDBOEspFunc (INPUT "Main":U) NO-ERROR.

    IF NOT VALID-HANDLE(hDBOTurnoCalenGener) THEN DO:
        {btb/btb008za.i1 mnbo/bomn030.p YES}
        {btb/btb008za.i2 mnbo/bomn030.p '' hDBOTurnoCalenGener} 
    END.
    RUN openQueryStatic IN hDBOTurnoCalenGener (INPUT "Default":U) NO-ERROR.
   
    ASSIGN tt-mmv-turno-esp.desc-turno:SENSITIVE IN FRAME fPage0 = NO.

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
    ASSIGN {&ttTable}.cod-especialid = {&ttParent}.cod-especialid.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

