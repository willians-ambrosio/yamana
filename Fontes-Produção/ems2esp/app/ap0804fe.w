&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rtb              PROGRESS
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804FE 2.00.00.000}  /*** 010000 ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def new global shared var rw-tit as rowid no-undo.

DEF TEMP-TABLE tt-relacto
    FIELD ep-codigo   LIKE tit-ap.ep-codigo
    FIELD cod-est     LIKE tit-ap.cod-est
    FIELD cod-esp     LIKE tit-ap.cod-esp
    FIELD serie       LIKE tit-ap.serie
    FIELD nr-docto    LIKE tit-ap.nr-docto
    FIELD parcela     LIKE tit-ap.parcela 
    FIELD cod-fornec  LIKE tit-ap.cod-forn
    FIELD referencia  LIKE tit-ap.referencia
    FIELD data        LIKE tit-ap.dt-transacao
    FIELD portador    LIKE tit-ap.portador
    FIELD valor       LIKE tit-ap.vl-original
    FIELD tipo        LIKE mov-ap.tipo
    FIELD desc-baixa  AS CHAR FORMAT "x(15)"
    FIELD nr-cheque   LIKE mov-ap.nr-cheque
    FIELD nr-bordero  LIKE mov-ap.nr-bordero
    FIELD esp-2       LIKE mov-ap.esp-ant
    FIELD serie-2     LIKE mov-ap.serie-ant
    FIELD docto-2     LIKE mov-ap.docto-ant
    FIELD parc-2      LIKE mov-ap.parc-ant
    FIELD forn-2      LIKE mov-ap.fornec-ant
    FIELD existe-lote AS LOGICAL FORMAT "Sim/NÆo" INIT NO
    INDEX documento  IS PRIMARY ep-codigo
                              cod-est
                              cod-esp
                              serie
                              nr-docto
                              parcela
                              cod-fornec.

DEF BUFFER b-tt-relacto FOR tt-relacto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-relacto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-relacto

/* Definitions for BROWSE br-relacto                                    */
&Scoped-define FIELDS-IN-QUERY-br-relacto tt-relacto.desc-baixa tt-relacto.referencia tt-relacto.data tt-relacto.valor tt-relacto.nr-cheque tt-relacto.nr-bordero tt-relacto.esp-2 tt-relacto.serie-2 tt-relacto.docto-2 tt-relacto.parc-2 tt-relacto.portador tt-relacto.existe-lote   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-relacto   
&Scoped-define SELF-NAME br-relacto
&Scoped-define QUERY-STRING-br-relacto FOR EACH tt-relacto
&Scoped-define OPEN-QUERY-br-relacto OPEN QUERY {&SELF-NAME} FOR EACH tt-relacto.
&Scoped-define TABLES-IN-QUERY-br-relacto tt-relacto
&Scoped-define FIRST-TABLE-IN-QUERY-br-relacto tt-relacto


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-relacto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 br-relacto bt-ok bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS i-empresa c-estabel c-especie c-serie ~
c-docto c-parcela i-fornec c-nome-abrev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-docto AS CHARACTER FORMAT "X(16)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE c-especie AS CHARACTER FORMAT "!!":U 
     LABEL "Esp‚cie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-abrev AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-parcela AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(5)":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa AS CHARACTER FORMAT "X(3)":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-fornec AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 3.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-relacto FOR 
      tt-relacto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-relacto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-relacto w-window _FREEFORM
  QUERY br-relacto DISPLAY
      tt-relacto.desc-baixa FORMAT "x(15)"       COLUMN-LABEL "Tipo Baixa"
     tt-relacto.referencia FORMAT "x(10)"        COLUMN-LABEL "Referˆncia"
     tt-relacto.data FORMAT "99/99/9999"         COLUMN-LABEL "Data"
     tt-relacto.valor FORMAT "ZZ,ZZZ,ZZZ,ZZ9.99" COLUMN-LABEL "Valor"
     tt-relacto.nr-cheque FORMAT ">>>>>>>>9"     COLUMN-LABEL "Nr Cheque"
     tt-relacto.nr-bordero FORMAT ">>>>>>>9"     COLUMN-LABEL "Nr Border“"
     tt-relacto.esp-2 FORMAT "!!"                COLUMN-LABEL "Esp Ant/Prev"
     tt-relacto.serie-2 FORMAT "x(5)"            COLUMN-LABEL "Serie Ant/Prev"
     tt-relacto.docto-2 FORMAT "x(16)"           COLUMN-LABEL "Docto Ant/Prev"
     tt-relacto.parc-2 FORMAT "x(2)"             COLUMN-LABEL "Parc Ant/Prev"
     tt-relacto.portador FORMAT ">>>>9"          COLUMN-LABEL "Portador"
     tt-relacto.existe-lote FORMAT "Sim/NÆo"     COLUMN-LABEL "Existe Lote ?"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 8.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-empresa AT ROW 1.75 COL 13 COLON-ALIGNED
     c-estabel AT ROW 1.75 COL 37 COLON-ALIGNED
     c-especie AT ROW 1.75 COL 53 COLON-ALIGNED
     c-serie AT ROW 1.75 COL 67 COLON-ALIGNED
     c-docto AT ROW 3 COL 13 COLON-ALIGNED
     c-parcela AT ROW 3 COL 29.43 COLON-ALIGNED NO-LABEL
     i-fornec AT ROW 3 COL 49 COLON-ALIGNED
     c-nome-abrev AT ROW 3 COL 60 COLON-ALIGNED NO-LABEL
     br-relacto AT ROW 4.5 COL 2
     bt-ok AT ROW 13.46 COL 3
     bt-cancelar AT ROW 13.46 COL 14
     bt-ajuda AT ROW 13.46 COL 69
     RECT-1 AT ROW 13.25 COL 2
     RECT-2 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.43 BY 13.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 13.75
         WIDTH              = 80.43
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB br-relacto c-nome-abrev F-Main */
/* SETTINGS FOR FILL-IN c-docto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-especie IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-abrev IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-parcela IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-empresa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-fornec IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-relacto
/* Query rebuild information for BROWSE br-relacto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-relacto.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-relacto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-relacto
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{utp/ut-liter.i Empresa}
assign i-empresa:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Estabelecimento}
assign c-estabel:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Esp‚cie}
assign c-especie:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i S‚rie}
assign c-serie:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Documento}
assign c-docto:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Fornecedor}
assign i-fornec:label in frame {&FRAME-NAME} = trim(return-value).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY i-empresa c-estabel c-especie c-serie c-docto c-parcela i-fornec 
          c-nome-abrev 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 br-relacto bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
{utp/ut9000.i "AP0804FE" "2.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-leitura.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt-relacto w-window 
PROCEDURE pi-cria-tt-relacto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
      IF  tit-ap.tipo = 1 THEN DO: /* Normal */

          FOR EACH lin-b-ap
              WHERE lin-b-ap.ep-codigo   = tit-ap.ep-codigo
              AND   lin-b-ap.cod-estabel = tit-ap.cod-est
              AND   lin-b-ap.cod-esp     = tit-ap.cod-esp
              AND   lin-b-ap.serie       = tit-ap.serie
              AND   lin-b-ap.nr-docto    = tit-ap.nr-docto
              AND   lin-b-ap.parcela     = tit-ap.parcela
              AND   lin-b-ap.cod-forn    = tit-ap.cod-forn NO-LOCK:
              
              IF  lin-b-ap.favorecido BEGINS "Pg_Esc" THEN
                  {utp/ut-liter.i ESCRITURAL}
              ELSE
                  {utp/ut-liter.i CHEQUE}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = lin-b-ap.ep-codigo
                     tt-relacto.cod-est    = lin-b-ap.cod-estabel
                     tt-relacto.cod-esp    = lin-b-ap.cod-esp
                     tt-relacto.serie      = lin-b-ap.serie
                     tt-relacto.nr-docto   = lin-b-ap.nr-docto
                     tt-relacto.parcela    = lin-b-ap.parcela 
                     tt-relacto.cod-fornec = lin-b-ap.cod-forn
                     tt-relacto.referencia = lin-b-ap.referencia
                     tt-relacto.portador   = lin-b-ap.portador
                     tt-relacto.data       = lin-b-ap.dt-baixa
                     tt-relacto.valor      = lin-b-ap.vl-baixa
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = lin-b-ap.nr-cheque
                     tt-relacto.nr-bordero = 0
                     tt-relacto.tipo       = 1.

              FIND FIRST doc-b-ap
                  WHERE doc-b-ap.ep-codigo  = lin-b-ap.ep-codigo
                  AND   doc-b-ap.referencia = lin-b-ap.referencia
                  NO-LOCK NO-ERROR.
              
              IF  AVAIL doc-b-ap THEN
                  ASSIGN tt-relacto.existe-lote = YES.
              ELSE
                  ASSIGN tt-relacto.existe-lote = NO.
          END.

          FOR EACH l-ant-ap
              WHERE l-ant-ap.ep-codigo   = tit-ap.ep-codigo
              AND   l-ant-ap.cod-estabel = tit-ap.cod-est
              AND   l-ant-ap.cod-esp     = tit-ap.cod-esp
              AND   l-ant-ap.serie       = tit-ap.serie
              AND   l-ant-ap.nr-docto    = tit-ap.nr-docto
              AND   l-ant-ap.parcela     = tit-ap.parcela
              AND   l-ant-ap.cod-forn    = tit-ap.cod-forn NO-LOCK:
              
              {utp/ut-liter.i ANTECIPA€ÇO}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = l-ant-ap.ep-codigo
                     tt-relacto.cod-est    = l-ant-ap.cod-estabel
                     tt-relacto.cod-esp    = l-ant-ap.cod-esp
                     tt-relacto.serie      = l-ant-ap.serie
                     tt-relacto.nr-docto   = l-ant-ap.nr-docto
                     tt-relacto.parcela    = l-ant-ap.parcela 
                     tt-relacto.cod-fornec = l-ant-ap.cod-forn
                     tt-relacto.portador   = 0
                     tt-relacto.valor      = l-ant-ap.vl-antecip
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = 0
                     tt-relacto.nr-bordero = 0
                     tt-relacto.esp-2      = l-ant-ap.cod-esp-ant
                     tt-relacto.serie-2    = l-ant-ap.serie-ant
                     tt-relacto.docto-2    = l-ant-ap.nr-docto-ant
                     tt-relacto.parc-2     = l-ant-ap.parcela-ant
                     tt-relacto.forn-2     = l-ant-ap.fornec-ant
                     tt-relacto.tipo       = 2.

              FIND FIRST b-tt-relacto
                  WHERE b-tt-relacto.ep-codigo   = l-ant-ap.ep-codigo
                  AND   b-tt-relacto.cod-fornec  = l-ant-ap.cod-fornec
                  AND   b-tt-relacto.cod-est     = l-ant-ap.cod-estabel
                  AND   b-tt-relacto.serie       = l-ant-ap.serie
                  AND   b-tt-relacto.cod-esp     = l-ant-ap.cod-esp
                  AND   b-tt-relacto.nr-docto    = l-ant-ap.nr-docto
                  AND   b-tt-relacto.parcela     = l-ant-ap.parcela 
                  AND   b-tt-relacto.tipo        = 1
                  NO-LOCK NO-ERROR.
              
              IF  AVAIL b-tt-relacto THEN
                  ASSIGN tt-relacto.referencia  = b-tt-relacto.referencia
                         tt-relacto.data        = b-tt-relacto.data
                         tt-relacto.existe-lote = b-tt-relacto.existe-lote.
          END.
          

          FOR EACH l-pre-ap
              WHERE l-pre-ap.ep-codigo   = tit-ap.ep-codigo
              AND   l-pre-ap.cod-estabel = tit-ap.cod-est
              AND   l-pre-ap.cod-esp     = tit-ap.cod-esp
              AND   l-pre-ap.serie       = tit-ap.serie
              AND   l-pre-ap.nr-docto    = tit-ap.nr-docto
              AND   l-pre-ap.parcela     = tit-ap.parcela
              AND   l-pre-ap.cod-forn    = tit-ap.cod-forn NO-LOCK:
              
              {utp/ut-liter.i PREVISÇO}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = l-pre-ap.ep-codigo
                     tt-relacto.cod-est    = l-pre-ap.cod-estabel
                     tt-relacto.cod-esp    = l-pre-ap.cod-esp
                     tt-relacto.serie      = l-pre-ap.serie
                     tt-relacto.nr-docto   = l-pre-ap.nr-docto
                     tt-relacto.parcela    = l-pre-ap.parcela 
                     tt-relacto.cod-fornec = l-pre-ap.cod-forn
                     tt-relacto.portador   = 0
                     tt-relacto.valor      = l-pre-ap.vl-previsao
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = 0
                     tt-relacto.nr-bordero = 0
                     tt-relacto.esp-2      = l-pre-ap.cod-esp-pre
                     tt-relacto.serie-2    = l-pre-ap.serie-pre
                     tt-relacto.docto-2    = l-pre-ap.nr-docto-pre
                     tt-relacto.parc-2     = l-pre-ap.parcela-pre
                     tt-relacto.forn-2     = l-pre-ap.cod-fornec
                     tt-relacto.tipo       = 3.

              FIND FIRST b-tt-relacto
                  WHERE b-tt-relacto.ep-codigo   = l-pre-ap.ep-codigo
                  AND   b-tt-relacto.cod-fornec  = l-pre-ap.cod-fornec
                  AND   b-tt-relacto.cod-est     = l-pre-ap.cod-estabel
                  AND   b-tt-relacto.serie       = l-pre-ap.serie
                  AND   b-tt-relacto.cod-esp     = l-pre-ap.cod-esp
                  AND   b-tt-relacto.nr-docto    = l-pre-ap.nr-docto
                  AND   b-tt-relacto.parcela     = l-pre-ap.parcela 
                  AND   b-tt-relacto.tipo        = 1
                  NO-LOCK NO-ERROR.
              
              IF  AVAIL b-tt-relacto THEN
                  ASSIGN tt-relacto.referencia  = b-tt-relacto.referencia
                         tt-relacto.data        = b-tt-relacto.data
                         tt-relacto.existe-lote = b-tt-relacto.existe-lote.
          END.

          FOR EACH bord-tit
              WHERE bord-tit.ep-codigo   = tit-ap.ep-codigo
              AND   bord-tit.cod-estabel = tit-ap.cod-est
              AND   bord-tit.cod-esp     = tit-ap.cod-esp
              AND   bord-tit.serie       = tit-ap.serie
              AND   bord-tit.nr-docto    = tit-ap.nr-docto
              AND   bord-tit.parcela     = tit-ap.parcela
              AND   bord-tit.cod-forn    = tit-ap.cod-forn NO-LOCK:
              
              {utp/ut-liter.i BORDERâ}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = bord-tit.ep-codigo
                     tt-relacto.cod-est    = bord-tit.cod-estabel
                     tt-relacto.cod-esp    = bord-tit.cod-esp
                     tt-relacto.serie      = bord-tit.serie
                     tt-relacto.nr-docto   = bord-tit.nr-docto
                     tt-relacto.parcela    = bord-tit.parcela 
                     tt-relacto.cod-fornec = bord-tit.cod-forn
                     tt-relacto.referencia = ""
                     tt-relacto.portador   = bord-tit.portador
                     tt-relacto.data       = bord-tit.dt-vencimen
                     tt-relacto.valor      = bord-tit.vl-saldo
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = 0
                     tt-relacto.nr-bordero = bord-tit.nr-bordero
                     tt-relacto.tipo       = 4.

              FIND FIRST bord-rec
                  WHERE bord-rec.ep-codigo  = bord-tit.ep-codigo
                  AND   bord-rec.portador   = bord-tit.portador
                  AND   bord-rec.nr-bordero = bord-tit.nr-bordero NO-LOCK NO-ERROR.
              
              IF  AVAIL bord-rec THEN
                  ASSIGN tt-relacto.existe-lote = YES.
              ELSE
                  ASSIGN tt-relacto.existe-lote = NO.
          END.
      END.


      IF  tit-ap.tipo = 2 THEN DO: /* Antecipa‡Æo */
          FOR EACH l-ant-ap
              WHERE l-ant-ap.ep-codigo    = tit-ap.ep-codigo
              AND   l-ant-ap.cod-estabel  = tit-ap.cod-est
              AND   l-ant-ap.cod-esp-ant  = tit-ap.cod-esp
              AND   l-ant-ap.serie-ant    = tit-ap.serie
              AND   l-ant-ap.nr-docto-ant = tit-ap.nr-docto
              AND   l-ant-ap.parcela-ant  = tit-ap.parcela
              AND   l-ant-ap.fornec-ant   = tit-ap.cod-forn NO-LOCK:

              {utp/ut-liter.i ANTECIPA€ÇO}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = l-ant-ap.ep-codigo
                     tt-relacto.cod-est    = l-ant-ap.cod-estabel
                     tt-relacto.cod-esp    = l-ant-ap.cod-esp-ant
                     tt-relacto.serie      = l-ant-ap.serie-ant
                     tt-relacto.nr-docto   = l-ant-ap.nr-docto-ant
                     tt-relacto.parcela    = l-ant-ap.parcela-ant 
                     tt-relacto.cod-fornec = l-ant-ap.fornec-ant
                     tt-relacto.portador   = 0
                     tt-relacto.valor      = l-ant-ap.vl-antecip
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = 0
                     tt-relacto.nr-bordero = 0
                     tt-relacto.esp-2      = l-ant-ap.cod-esp
                     tt-relacto.serie-2    = l-ant-ap.serie
                     tt-relacto.docto-2    = l-ant-ap.nr-docto
                     tt-relacto.parc-2     = l-ant-ap.parcela
                     tt-relacto.forn-2     = l-ant-ap.cod-forn
                     tt-relacto.tipo       = 2.

              FIND FIRST lin-b-ap
                  WHERE lin-b-ap.ep-codigo   = l-ant-ap.ep-codigo
                  AND   lin-b-ap.cod-fornec  = l-ant-ap.cod-fornec
                  AND   lin-b-ap.cod-estabel = l-ant-ap.cod-estabel
                  AND   lin-b-ap.serie       = l-ant-ap.serie
                  AND   lin-b-ap.cod-esp     = l-ant-ap.cod-esp
                  AND   lin-b-ap.nr-docto    = l-ant-ap.nr-docto
                  AND   lin-b-ap.parcela     = l-ant-ap.parcela 
                  NO-LOCK NO-ERROR.
              
              IF  AVAIL lin-b-ap THEN
                  ASSIGN tt-relacto.referencia  = lin-b-ap.referencia
                         tt-relacto.data        = lin-b-ap.dt-baixa
                         tt-relacto.existe-lote = YES.
              ELSE
                  ASSIGN tt-relacto.referencia  = ""
                         tt-relacto.data        = ?
                         tt-relacto.existe-lote = NO.
          END.

          FOR EACH mov-pef-pend
              WHERE mov-pef-pend.ep-codigo   = tit-ap.ep-codigo
              AND   mov-pef-pend.cod-estabel = tit-ap.cod-est
              AND   mov-pef-pend.cod-esp     = tit-ap.cod-esp
              AND   mov-pef-pend.serie       = tit-ap.serie
              AND   mov-pef-pend.nr-docto    = tit-ap.nr-docto
              AND   mov-pef-pend.parcela     = tit-ap.parcela
              AND   mov-pef-pend.cod-forn    = tit-ap.cod-forn NO-LOCK:
              
              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = mov-pef-pend.ep-codigo
                     tt-relacto.cod-est    = mov-pef-pend.cod-estabel
                     tt-relacto.cod-esp    = mov-pef-pend.cod-esp
                     tt-relacto.serie      = mov-pef-pend.serie
                     tt-relacto.nr-docto   = mov-pef-pend.nr-docto
                     tt-relacto.parcela    = mov-pef-pend.parcela 
                     tt-relacto.cod-fornec = mov-pef-pend.cod-forn
                     tt-relacto.referencia = mov-pef-pend.referencia
                     tt-relacto.portador   = mov-pef-pend.cod-port
                     tt-relacto.data       = mov-pef-pend.dt-transacao
                     tt-relacto.valor      = mov-pef-pend.vl-original
                     tt-relacto.desc-baixa = "PEF"
                     tt-relacto.nr-cheque  = mov-pef-pend.nr-cheque
                     tt-relacto.nr-bordero = 0
                     tt-relacto.tipo       = 5.

          END.
      END.

      IF  tit-ap.tipo = 3 THEN DO:
          FOR EACH l-pre-ap
              WHERE l-pre-ap.ep-codigo    = tit-ap.ep-codigo
              AND   l-pre-ap.cod-estabel  = tit-ap.cod-est
              AND   l-pre-ap.cod-esp-pre  = tit-ap.cod-esp
              AND   l-pre-ap.serie-pre    = tit-ap.serie
              AND   l-pre-ap.nr-docto-pre = tit-ap.nr-docto
              AND   l-pre-ap.parcela-pre  = tit-ap.parcela
              AND   l-pre-ap.cod-forn     = tit-ap.cod-forn NO-LOCK:

              {utp/ut-liter.i PREVISÇO}

              CREATE tt-relacto.
              ASSIGN tt-relacto.ep-codigo  = l-pre-ap.ep-codigo
                     tt-relacto.cod-est    = l-pre-ap.cod-estabel
                     tt-relacto.cod-esp    = l-pre-ap.cod-esp-pre     
                     tt-relacto.serie      = l-pre-ap.serie-pre       
                     tt-relacto.nr-docto   = l-pre-ap.nr-docto-pre    
                     tt-relacto.parcela    = l-pre-ap.parcela-pre     
                     tt-relacto.cod-fornec = l-pre-ap.cod-forn 
                     tt-relacto.portador   = 0
                     tt-relacto.valor      = l-pre-ap.vl-previsao
                     tt-relacto.desc-baixa = RETURN-VALUE
                     tt-relacto.nr-cheque  = 0
                     tt-relacto.nr-bordero = 0
                     tt-relacto.esp-2      = l-pre-ap.cod-esp
                     tt-relacto.serie-2    = l-pre-ap.serie
                     tt-relacto.docto-2    = l-pre-ap.nr-docto
                     tt-relacto.parc-2     = l-pre-ap.parcela
                     tt-relacto.forn-2     = l-pre-ap.cod-forn
                     tt-relacto.tipo       = 3.

              FIND FIRST lin-b-ap
                  WHERE lin-b-ap.ep-codigo   = l-pre-ap.ep-codigo
                  AND   lin-b-ap.cod-fornec  = l-pre-ap.cod-fornec
                  AND   lin-b-ap.cod-estabel = l-pre-ap.cod-estabel
                  AND   lin-b-ap.serie       = l-pre-ap.serie
                  AND   lin-b-ap.cod-esp     = l-pre-ap.cod-esp
                  AND   lin-b-ap.nr-docto    = l-pre-ap.nr-docto
                  AND   lin-b-ap.parcela     = l-pre-ap.parcela 
                  NO-LOCK NO-ERROR.
              
              IF  AVAIL lin-b-ap THEN
                  ASSIGN tt-relacto.referencia  = lin-b-ap.referencia
                         tt-relacto.data        = lin-b-ap.dt-baixa
                         tt-relacto.existe-lote = YES.
              ELSE
                  ASSIGN tt-relacto.referencia  = ""
                         tt-relacto.data        = ?
                         tt-relacto.existe-lote = NO.
          END.
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leitura w-window 
PROCEDURE pi-leitura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST tit-ap
      WHERE ROWID(tit-ap) = rw-tit NO-LOCK NO-ERROR.

  IF AVAIL tit-ap THEN DO:
      ASSIGN i-empresa = tit-ap.ep-codigo
             c-estabel = tit-ap.cod-est
             c-especie = tit-ap.cod-esp
             c-serie   = tit-ap.serie
             c-docto   = tit-ap.nr-docto
             c-parcela = tit-ap.parcela
             i-fornec  = tit-ap.cod-fornec.

      FIND FIRST emitente
          WHERE emitente.cod-emit = tit-ap.cod-fornec NO-LOCK NO-ERROR.
      
      IF  AVAIL emitente THEN
          ASSIGN c-nome-abrev = emitente.nome-abrev.

      DISP i-empresa
           c-estabel
           c-especie
           c-serie
           c-docto
           c-parcela
           i-fornec 
           c-nome-abrev WITH FRAME {&FRAME-NAME}.

      FOR EACH tt-relacto EXCLUSIVE-LOCK:
          DELETE tt-relacto.
      END.

      RUN pi-cria-tt-relacto.

      {&OPEN-QUERY-br-relacto} 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-relacto"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

