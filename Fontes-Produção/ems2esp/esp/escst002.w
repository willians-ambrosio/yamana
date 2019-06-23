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
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-13 fi-arquivo btn-executar ~
btn-close 
&Scoped-Define DISPLAYED-OBJECTS fi-arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close 
     LABEL "Sair" 
     SIZE 11 BY 1.13 TOOLTIP "Altera o CST de Item da Nota Fiscal".

DEFINE BUTTON btn-executar 
     LABEL "Executar" 
     SIZE 11 BY 1.13 TOOLTIP "Altera o CST da Nota Fiscal".

DEFINE VARIABLE fi-arquivo AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\temp~\cst-dif.txt" 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 2.42.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 1.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi-arquivo AT ROW 2.5 COL 25 COLON-ALIGNED WIDGET-ID 2
     btn-executar AT ROW 4.54 COL 50.14 WIDGET-ID 14
     btn-close AT ROW 4.54 COL 61.14 WIDGET-ID 20
     "Parƒmetros" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 1.42 COL 7 WIDGET-ID 10
          FONT 0
     RECT-12 AT ROW 1.75 COL 6 WIDGET-ID 18
     RECT-13 AT ROW 4.33 COL 6 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77 BY 8.25 WIDGET-ID 100.


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
         TITLE              = "Altera CST nos itens da Nota Fiscal"
         HEIGHT             = 5.58
         WIDTH              = 74.72
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
ON END-ERROR OF wWin /* Altera CST nos itens da Nota Fiscal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Altera CST nos itens da Nota Fiscal */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close wWin
ON CHOOSE OF btn-close IN FRAME fMain /* Sair */
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-executar wWin
ON CHOOSE OF btn-executar IN FRAME fMain /* Executar */
DO:
    DEFINE BUFFER bf-sit-tribut-relacto FOR sit-tribut-relacto.
    DEFINE BUFFER b2-sit-tribut-relacto FOR sit-tribut-relacto.

    DEFINE VARIABLE c-arq AS CHARACTER  FORMAT "x(50)" NO-UNDO.

    /*ASSIGN c-arq = "V:\temp\cst-dif.txt".
    UPDATE c-arq.*/

    ASSIGN FRAME {&FRAME-NAME} fi-arquivo. 

    IF fi-arquivo = "" THEN MESSAGE "NÆo foi selecionado nenhum caminho para o arquivo!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    OUTPUT TO VALUE(fi-arquivo).
    PUT UNFORMAT 
    "Estabel.    Emit    N.F.              Serie     Natur NF         Seq    item              PIS N.F.     COFINS N.F.     PIS     COFINS".

    FOR EACH nota-fiscal NO-LOCK
        WHERE nota-fiscal.dt-emis-nota >= DATE("01/04/2011"):
        FOR EACH it-nota-fisc OF nota-fiscal EXCLUSIVE-LOCK:
            FIND sit-tribut-relacto NO-LOCK
                WHERE sit-tribut-relacto.cod-natur-operac = nota-fiscal.nat-operacao
                  AND sit-tribut-relacto.cdn-tribut       = 2 /* PIS */ NO-ERROR.
            IF NOT AVAILABLE sit-tribut-relacto THEN DO:
                DISP "ERRO PIS - nenhuma cst para a natureza:" FORMAT "X(50)"
                 nota-fiscal.nat-operacao
                 WITH WIDTH 333 NO-LABELS FRAME f-a.
            END.

            FIND b2-sit-tribut-relacto NO-LOCK
                WHERE b2-sit-tribut-relacto.cod-natur-operac = nota-fiscal.nat-operacao
                AND   b2-sit-tribut-relacto.cdn-tribut       = 3 /* COFINS */ NO-ERROR.
            IF NOT AVAILABLE b2-sit-tribut-relacto THEN DO:
                DISP "ERRO COFINS - nenhuma cst para a natureza:" FORMAT "X(50)"
                     nota-fiscal.nat-operacao
                     WITH WIDTH 333 NO-LABELS FRAME f-a.
            END.

            IF &if '{&bf_dis_versao_ems}' >= '2.07':U &then
               it-nota-fisc.cod-sit-tributar-pis <> it-nota-fisc.cod-sit-tributar-cofins OR
               int(it-nota-fisc.cod-sit-tributar-pis) <> sit-tribut-relacto.cdn-sit-tribut /* pis */ OR
               int(it-nota-fisc.cod-sit-tributar-cofins) <> b2-sit-tribut-relacto.cdn-sit-tribut /* Cofins */ 
               &else
               substring(it-nota-fisc.char-1,77,2) <> SUBSTRING(it-nota-fisc.char-1,79,2) OR
               INTEGER(substring(it-nota-fisc.char-1,77,2)) <> sit-tribut-relacto.cdn-sit-tribut /* pis */ OR
               INTEGER(substring(it-nota-fisc.char-1,79,2)) <> b2-sit-tribut-relacto.cdn-sit-tribut /* Cofins */ 
               &endif
               THEN DO:
                DISP nota-fiscal.cod-estabel 
                     nota-fiscal.cod-emitente 
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
                     /*
                     sit-tribut-relacto.cod-natur-operac COLUMN-LABEL "NATUR. CST"
                     sit-tribut-relacto.cdn-tribut 
                     */
                     sit-tribut-relacto.cdn-sit-tribut /* pis */       WHEN AVAILABLE sit-tribut-relacto
                     b2-sit-tribut-relacto.cdn-sit-tribut /* Cofins */ WHEN AVAILABLE b2-sit-tribut-relacto
                     WITH WIDTH 333 NO-LABELS FRAME f-b.

                ASSIGN &if '{&bf_dis_versao_ems}' >= '2.07':U 
                       &then it-nota-fisc.cod-sit-tributar-pis
                       &else OVERLAY(it-nota-fisc.char-1,77,2) &endif = STRING(sit-tribut-relacto.cdn-sit-tribut,"99") /* pis */
                       &if '{&bf_dis_versao_ems}' >= '2.07':U 
                       &then it-nota-fisc.cod-sit-tributar-cofins
                       &else OVERLAY(it-nota-fisc.char-1,79,2) &endif = STRING(b2-sit-tribut-relacto.cdn-sit-tribut,"99"). /* Cofins */
            END.

            IF NOT AVAILABLE sit-tribut-relacto OR NOT AVAILABLE b2-sit-tribut-relacto THEN DO:
                FOR EACH bf-sit-tribut-relacto NO-LOCK
                    WHERE bf-sit-tribut-relacto.cod-natur-operac = nota-fiscal.nat-operacao:
                    DISP "ERRO - mais de uma ou nenhuma cst para a natureza:" FORMAT "X(50)"
                         bf-sit-tribut-relacto.cod-natur-operac COLUMN-LABEL "NATUR. CST"
                         bf-sit-tribut-relacto.cdn-tribut 
                         bf-sit-tribut-relacto.cdn-sit-tribut
                         WITH WIDTH 333 NO-LABELS FRAME f-c.
                END.
            END.
        END.
    END.
    OUTPUT CLOSE.
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
  DISPLAY fi-arquivo 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-12 RECT-13 fi-arquivo btn-executar btn-close 
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

