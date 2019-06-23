&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{cdp/cdcfgdis.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-14 fi-arquivo fi-estab fi-serie ~
fi-nr-nota-fisc fi-cst btn-executar btn-sair 
&Scoped-Define DISPLAYED-OBJECTS fi-arquivo fi-estab fi-serie ~
fi-nr-nota-fisc fi-cst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-executar 
     LABEL "Executar" 
     SIZE 11 BY 1.13 TOOLTIP "Altera o CST da Nota Fiscal".

DEFINE BUTTON btn-sair 
     LABEL "Sair" 
     SIZE 11 BY 1.13 TOOLTIP "Altera o CST da Nota Fiscal".

DEFINE VARIABLE fi-arquivo AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\temp~\cst.txt" 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-cst AS CHARACTER FORMAT "X(256)":U 
     LABEL "CST" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-estab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fisc AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nr Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-serie AS CHARACTER FORMAT "X(5)":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 6.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 1.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi-arquivo AT ROW 2.75 COL 30 COLON-ALIGNED WIDGET-ID 2
     fi-estab AT ROW 3.75 COL 30 COLON-ALIGNED WIDGET-ID 4
     fi-serie AT ROW 4.75 COL 30 COLON-ALIGNED WIDGET-ID 6
     fi-nr-nota-fisc AT ROW 5.75 COL 30 COLON-ALIGNED WIDGET-ID 8
     fi-cst AT ROW 6.75 COL 30 COLON-ALIGNED WIDGET-ID 12
     btn-executar AT ROW 8.46 COL 50.14 WIDGET-ID 14
     btn-sair AT ROW 8.46 COL 61.14 WIDGET-ID 20
     "Nota Fiscal" VIEW-AS TEXT
          SIZE 12.72 BY .67 AT ROW 1.75 COL 7 WIDGET-ID 10
          FONT 0
     RECT-12 AT ROW 2 COL 6 WIDGET-ID 18
     RECT-14 AT ROW 8.25 COL 6 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.14 BY 9.25 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Altera CST na Nota Fiscal"
         HEIGHT             = 9.42
         WIDTH              = 75.14
         MAX-HEIGHT         = 28.79
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.79
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Altera CST na Nota Fiscal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Altera CST na Nota Fiscal */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-executar wWin
ON CHOOSE OF btn-executar IN FRAME fMain /* Executar */
DO:
    DEFINE VARIABLE c-cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEFINE VARIABLE c-serie       LIKE nota-fiscal.serie NO-UNDO.
    DEFINE VARIABLE c-nota-fiscal LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEFINE VARIABLE c-cst         AS CHARACTER  FORMAT "x(02)" NO-UNDO.

    DEFINE BUFFER bf-sit-tribut-relacto FOR sit-tribut-relacto.
    DEFINE BUFFER b2-sit-tribut-relacto FOR sit-tribut-relacto.

    DEFINE VARIABLE c-arq AS CHARACTER  FORMAT "x(50)" LABEL "Arquivo" NO-UNDO.

    ASSIGN FRAME {&FRAME-NAME} fi-arquivo fi-estab fi-nr-nota-fisc fi-serie fi-cst.

    /*ASSIGN c-arq = "C:\temp\cst.txt".
    UPDATE c-arq
           c-cod-estabel
           c-serie
           c-nota-fiscal
           c-cst
           WITH 1 COL. */

    IF fi-arquivo = "" THEN MESSAGE "NÆo foi selecionado nenhum caminho para o arquivo!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    FIND FIRST nota-fiscal WHERE nota-fiscal.cod-estabel = fi-estab
                             AND nota-fiscal.serie       = fi-serie
                             AND nota-fiscal.nr-nota-fis = fi-nr-nota-fisc NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN MESSAGE "Nota fiscal nÆo encontrada!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    OUTPUT TO VALUE(fi-arquivo).
    FOR EACH nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel = fi-estab
        AND   nota-fiscal.serie       = fi-serie
        AND   nota-fiscal.nr-nota-fis = fi-nr-nota-fisc:
        FOR EACH it-nota-fisc OF nota-fiscal EXCLUSIVE-LOCK:
            DISP nota-fiscal.dt-emis-nota
                 nota-fiscal.cod-estabel 
                 nota-fiscal.cod-emitente 
                 "   "
                 nota-fiscal.nr-nota-fis 
                 nota-fiscal.serie
                 nota-fiscal.nat-operacao            COLUMN-LABEL "NATUR. NF"
                 it-nota-fisc.nr-seq-fat
                 it-nota-fisc.it-codigo
                 &if '{&bf_dis_versao_ems}' >= '2.07':U 
                 &then string(it-nota-fisc.cod-sit-tributar-pis)
                 &else substring(it-nota-fisc.char-1,77,2) &endif COLUMN-LABEL "CST PIS"
                 &if '{&bf_dis_versao_ems}' >= '2.07':U 
                 &then string(it-nota-fisc.cod-sit-tributar-cofins)
                 &else substring(it-nota-fisc.char-1,79,2) &endif COLUMN-LABEL "CST COFINS"
                 "   "
                 it-nota-fisc.vl-tot-item
                 WITH WIDTH 333.

            ASSIGN &if '{&bf_dis_versao_ems}' >= '2.07':U 
                   &then it-nota-fisc.cod-sit-tributar-pis
                   &else OVERLAY(it-nota-fisc.char-1,77,2) &endif = fi-cst /* pis */
                   &if '{&bf_dis_versao_ems}' >= '2.07':U 
                   &then it-nota-fisc.cod-sit-tributar-cofins
                   &else OVERLAY(it-nota-fisc.char-1,79,2) &endif = fi-cst. /* Cofins */
        END.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-sair wWin
ON CHOOSE OF btn-sair IN FRAME fMain /* Sair */
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-arquivo fi-estab fi-serie fi-nr-nota-fisc fi-cst 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-12 RECT-14 fi-arquivo fi-estab fi-serie fi-nr-nota-fisc fi-cst 
         btn-executar btn-sair 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

