&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESAP007A 2.06.00.001}

def buffer fornecedor for ems5.fornecedor.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

def temp-table tt-tit_ap no-undo
    field selecionado as log format "Sim/NÆo"
    field ep-codigo   like tit_ap.cod_empresa
    field cod-estabel like tit_ap.cod_estab
    field cod-fornec  like tit_ap.cdn_fornecedor
    field nome-abrev  like fornecedor.nom_abrev
    field cod-esp     like tit_ap.cod_espec_docto
    field serie       like tit_ap.cod_ser_docto
    field nr-docto    like tit_ap.cod_tit_ap
    field parcela     like tit_ap.cod_parcela
    field dt-vencimen like tit_ap.dat_vencto_tit_ap
    field vl-original like tit_ap.val_origin_tit_ap
    field valor-saldo like tit_ap.val_sdo_tit_ap
    field excecao     as log
    field dt-maquina  as date
    field fora-prazo  as log
    index idx_1 is primary is unique ep-codigo
                                     cod-fornec
                                     cod-estabel
                                     cod-esp
                                     serie
                                     nr-docto
                                     parcela
    index idx_2 selecionado.

/* Parameters Definitions ---                                           */

def input-output parameter table for tt-tit_ap.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 ~
IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 RECT-2 ~
RECT-3 fi-ep-codigo-ini fi-ep-codigo-fim fi-cod-estabel-ini ~
fi-cod-estabel-fim fi-cod-fornec-ini fi-cod-fornec-fim fi-cod-esp-ini ~
fi-cod-esp-fim fi-serie-ini fi-serie-fim fi-nr-docto-ini fi-nr-docto-fim ~
fi-parcela-ini fi-parcela-fim fi-dt-vencimen-ini fi-dt-vencimen-fim ~
fi-cod-gr-forn-ini fi-cod-gr-forn-fim fi-dt-maquina-ini fi-dt-maquina-fim ~
fi-fora-prazo bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-ep-codigo-ini fi-ep-codigo-fim ~
fi-cod-estabel-ini fi-cod-estabel-fim fi-cod-fornec-ini fi-cod-fornec-fim ~
fi-cod-esp-ini fi-cod-esp-fim fi-serie-ini fi-serie-fim fi-nr-docto-ini ~
fi-nr-docto-fim fi-parcela-ini fi-parcela-fim fi-dt-vencimen-ini ~
fi-dt-vencimen-fim fi-cod-gr-forn-ini fi-cod-gr-forn-fim fi-dt-maquina-ini ~
fi-dt-maquina-fim fi-fora-prazo 

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

DEFINE VARIABLE fi-cod-esp-fim AS CHARACTER FORMAT "x(3)":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-esp-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Esp‚cie":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "Estab":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-fornec-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-fornec-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-gr-forn-fim AS CHARACTER FORMAT "X(4)":U INITIAL "ZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-gr-forn-ini AS CHARACTER FORMAT "X(4)" INITIAL "0" 
     LABEL "Grupo":R7 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-maquina-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-maquina-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Dt Maquina":R16 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-vencimen-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-vencimen-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Dt Vencimento":R16 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ep-codigo-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ep-codigo-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Empresa":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fora-prazo AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Venc. Fora Prazo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-docto-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-docto-ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Documento":R11 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-parcela-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-parcela-ini AS CHARACTER FORMAT "x(2)" 
     LABEL "Parcela":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 10.58.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 1.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-ep-codigo-ini AT ROW 1.75 COL 25 COLON-ALIGNED HELP
          "C¢digo da empresa" WIDGET-ID 6
     fi-ep-codigo-fim AT ROW 1.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-cod-estabel-ini AT ROW 2.75 COL 25 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 14
     fi-cod-estabel-fim AT ROW 2.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-cod-fornec-ini AT ROW 3.75 COL 19.86 COLON-ALIGNED HELP
          "C¢digo do fornecedor" WIDGET-ID 22
     fi-cod-fornec-fim AT ROW 3.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi-cod-esp-ini AT ROW 4.75 COL 25 COLON-ALIGNED HELP
          "C¢digo da esp‚cie do documento" WIDGET-ID 30
     fi-cod-esp-fim AT ROW 4.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fi-serie-ini AT ROW 5.75 COL 19.86 COLON-ALIGNED WIDGET-ID 38
     fi-serie-fim AT ROW 5.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fi-nr-docto-ini AT ROW 6.75 COL 15 COLON-ALIGNED HELP
          "N£mero do documento" WIDGET-ID 46
     fi-nr-docto-fim AT ROW 6.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     fi-parcela-ini AT ROW 7.75 COL 25 COLON-ALIGNED WIDGET-ID 54
     fi-parcela-fim AT ROW 7.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fi-dt-vencimen-ini AT ROW 8.75 COL 19.86 COLON-ALIGNED HELP
          "Data de vencimento do documento" WIDGET-ID 62
     fi-dt-vencimen-fim AT ROW 8.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fi-cod-gr-forn-ini AT ROW 9.75 COL 22 COLON-ALIGNED HELP
          "C¢digo do grupo de fornecedores" WIDGET-ID 70
     fi-cod-gr-forn-fim AT ROW 9.75 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fi-dt-maquina-ini AT ROW 10.75 COL 20 COLON-ALIGNED HELP
          "Data de vencimento do documento" WIDGET-ID 78
     fi-dt-maquina-fim AT ROW 10.75 COL 48.29 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fi-fora-prazo AT ROW 12.75 COL 20 COLON-ALIGNED WIDGET-ID 92
     bt-ok AT ROW 14.46 COL 2
     bt-cancelar AT ROW 14.46 COL 13
     bt-ajuda AT ROW 14.46 COL 68
     "Parametros" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 12.04 COL 2 WIDGET-ID 90
     "Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.17 COL 1.86 WIDGET-ID 86
     RECT-1 AT ROW 14.25 COL 1
     IMAGE-1 AT ROW 1.75 COL 31.86 WIDGET-ID 8
     IMAGE-2 AT ROW 1.75 COL 47.29 WIDGET-ID 10
     IMAGE-3 AT ROW 2.75 COL 31.86 WIDGET-ID 16
     IMAGE-4 AT ROW 2.75 COL 47.29 WIDGET-ID 18
     IMAGE-5 AT ROW 3.75 COL 31.86 WIDGET-ID 24
     IMAGE-6 AT ROW 3.75 COL 47.29 WIDGET-ID 26
     IMAGE-7 AT ROW 4.75 COL 31.86 WIDGET-ID 32
     IMAGE-8 AT ROW 4.75 COL 47.29 WIDGET-ID 34
     IMAGE-9 AT ROW 5.75 COL 31.86 WIDGET-ID 40
     IMAGE-10 AT ROW 5.75 COL 47.29 WIDGET-ID 42
     IMAGE-11 AT ROW 6.75 COL 31.86 WIDGET-ID 48
     IMAGE-12 AT ROW 6.75 COL 47.29 WIDGET-ID 50
     IMAGE-13 AT ROW 7.75 COL 31.86 WIDGET-ID 56
     IMAGE-14 AT ROW 7.75 COL 47.29 WIDGET-ID 58
     IMAGE-15 AT ROW 8.75 COL 31.86 WIDGET-ID 64
     IMAGE-16 AT ROW 8.75 COL 47.29 WIDGET-ID 66
     IMAGE-17 AT ROW 9.75 COL 31.86 WIDGET-ID 72
     IMAGE-18 AT ROW 9.75 COL 47.29 WIDGET-ID 74
     IMAGE-19 AT ROW 10.75 COL 32 WIDGET-ID 80
     IMAGE-20 AT ROW 10.75 COL 47.43 WIDGET-ID 82
     RECT-2 AT ROW 1.42 COL 1 WIDGET-ID 84
     RECT-3 AT ROW 12.33 COL 1 WIDGET-ID 88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.72 BY 14.71 WIDGET-ID 100.


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
         HEIGHT             = 14.71
         WIDTH              = 78.72
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    do on error undo, return no-apply:
        run pi-carrega-tt.
    end.
    find first es-param-alt-venc where exclusive-lock no-error.
    if not avail es-param-alt-venc then
        create es-param-alt-venc.

    assign es-param-alt-venc.ep-codigo-ini   = input frame F-main fi-ep-codigo-ini
           es-param-alt-venc.ep-codigo-fim   = input frame F-main fi-ep-codigo-fim
           es-param-alt-venc.cod-estabel-ini = input frame F-main fi-cod-estabel-ini
           es-param-alt-venc.cod-estabel-fim = input frame F-main fi-cod-estabel-fim
           es-param-alt-venc.cod-fornec-ini  = input frame F-main fi-cod-fornec-ini
           es-param-alt-venc.cod-fornec-fim  = input frame F-main fi-cod-fornec-fim
           es-param-alt-venc.cod-esp-ini     = input frame F-main fi-cod-esp-ini
           es-param-alt-venc.cod-esp-fim     = input frame F-main fi-cod-esp-fim
           es-param-alt-venc.serie-ini       = input frame F-main fi-serie-ini
           es-param-alt-venc.serie-fim       = input frame F-main fi-serie-fim
           es-param-alt-venc.nr-docto-ini    = input frame F-main fi-nr-docto-ini
           es-param-alt-venc.nr-docto-fim    = input frame F-main fi-nr-docto-fim
           es-param-alt-venc.parcela-ini     = input frame F-main fi-parcela-ini
           es-param-alt-venc.parcela-fim     = input frame F-main fi-parcela-fim
           es-param-alt-venc.dt-vencimen-ini = input frame F-main fi-dt-vencimen-ini
           es-param-alt-venc.dt-vencimen-fim = input frame F-main fi-dt-vencimen-fim
           es-param-alt-venc.cod-gr-forn-ini = input frame F-main fi-cod-gr-forn-ini
           es-param-alt-venc.cod-gr-forn-fim = input frame F-main fi-cod-gr-forn-fim
           es-param-alt-venc.dt-maquina-ini  = input frame F-main fi-dt-maquina-ini
           es-param-alt-venc.dt-maquina-fim  = input frame F-main fi-dt-maquina-fim
           es-param-alt-venc.fora-prazo      = input frame F-main fi-fora-prazo.

    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  DISPLAY fi-ep-codigo-ini fi-ep-codigo-fim fi-cod-estabel-ini 
          fi-cod-estabel-fim fi-cod-fornec-ini fi-cod-fornec-fim fi-cod-esp-ini 
          fi-cod-esp-fim fi-serie-ini fi-serie-fim fi-nr-docto-ini 
          fi-nr-docto-fim fi-parcela-ini fi-parcela-fim fi-dt-vencimen-ini 
          fi-dt-vencimen-fim fi-cod-gr-forn-ini fi-cod-gr-forn-fim 
          fi-dt-maquina-ini fi-dt-maquina-fim fi-fora-prazo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 
         IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 RECT-2 RECT-3 fi-ep-codigo-ini 
         fi-ep-codigo-fim fi-cod-estabel-ini fi-cod-estabel-fim 
         fi-cod-fornec-ini fi-cod-fornec-fim fi-cod-esp-ini fi-cod-esp-fim 
         fi-serie-ini fi-serie-fim fi-nr-docto-ini fi-nr-docto-fim 
         fi-parcela-ini fi-parcela-fim fi-dt-vencimen-ini fi-dt-vencimen-fim 
         fi-cod-gr-forn-ini fi-cod-gr-forn-fim fi-dt-maquina-ini 
         fi-dt-maquina-fim fi-fora-prazo bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESAP007A" "2.06.00.001"}

  find first es-param-alt-venc where no-lock no-error.
  if avail es-param-alt-venc then
      assign fi-ep-codigo-ini   = es-param-alt-venc.ep-codigo-ini
             fi-ep-codigo-fim   = es-param-alt-venc.ep-codigo-fim
             fi-cod-estabel-ini = es-param-alt-venc.cod-estabel-ini
             fi-cod-estabel-fim = es-param-alt-venc.cod-estabel-fim
             fi-cod-fornec-ini  = es-param-alt-venc.cod-fornec-ini
             fi-cod-fornec-fim  = es-param-alt-venc.cod-fornec-fim
             fi-cod-esp-ini     = es-param-alt-venc.cod-esp-ini
             fi-cod-esp-fim     = es-param-alt-venc.cod-esp-fim
             fi-serie-ini       = es-param-alt-venc.serie-ini
             fi-serie-fim       = es-param-alt-venc.serie-fim
             fi-nr-docto-ini    = es-param-alt-venc.nr-docto-ini
             fi-nr-docto-fim    = es-param-alt-venc.nr-docto-fim
             fi-parcela-ini     = es-param-alt-venc.parcela-ini
             fi-parcela-fim     = es-param-alt-venc.parcela-fim
             fi-dt-vencimen-ini = es-param-alt-venc.dt-vencimen-ini
             fi-dt-vencimen-fim = es-param-alt-venc.dt-vencimen-fim
             fi-cod-gr-forn-ini = es-param-alt-venc.cod-gr-forn-ini
             fi-cod-gr-forn-fim = es-param-alt-venc.cod-gr-forn-fim
             fi-dt-maquina-ini  = es-param-alt-venc.dt-maquina-ini
             fi-dt-maquina-fim  = es-param-alt-venc.dt-maquina-fim
             fi-fora-prazo      = es-param-alt-venc.fora-prazo.
  else
      assign fi-fora-prazo = 7
             fi-dt-maquina-ini = today - 1
             fi-dt-maquina-fim = today - 1.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt w-window 
PROCEDURE pi-carrega-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var h-acomp    as handle no-undo.
    def var i-qtde-reg as int    no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Imprimindo}

    run pi-inicializar in h-acomp(input "Aguarde ...").

    empty temp-table tt-tit_ap.

    for each movto_tit_ap use-index mvtttp_gerac_movto no-lock
       where movto_tit_ap.dat_gerac_movto   >= input frame F-Main fi-dt-maquina-ini
         and movto_tit_ap.dat_gerac_movto   <= input frame F-Main fi-dt-maquina-fim
         and movto_tit_ap.log_movto_estordo  = no
         and(movto_tit_ap.ind_trans_ap_abrev = "IMPL"
          or movto_tit_ap.ind_trans_ap_abrev = "IMCR"
          or movto_tit_ap.ind_trans_ap_abrev = "IMDB"
          or movto_tit_ap.ind_trans_ap_abrev = "SBND"
    /*
    antes
          or movto_tit_ap.transacao  = 16), /* NDC */

    depois???
          or movto_tit_ap.ind_trans_ap matches '*corr*' /* NDC */
    */
             ),
       first tit_ap no-lock use-index titap_token
       where tit_ap.cod_estab           = movto_tit_ap.cod_estab
         and tit_ap.num_id_tit_ap       = movto_tit_ap.num_id_tit_ap
         and tit_ap.cod_empresa        >= input frame F-Main fi-ep-codigo-ini
         and tit_ap.cod_empresa        <= input frame F-Main fi-ep-codigo-fim
         and tit_ap.cdn_fornecedor     >= input frame F-Main fi-cod-fornec-ini
         and tit_ap.cdn_fornecedor     <= input frame F-Main fi-cod-fornec-fim
         and tit_ap.cod_estab          >= input frame F-Main fi-cod-estabel-ini
         and tit_ap.cod_estab          <= input frame F-Main fi-cod-estabel-fim
         and tit_ap.cod_espec_docto    >= input frame F-Main fi-cod-esp-ini
         and tit_ap.cod_espec_docto    <= input frame F-Main fi-cod-esp-fim
         and tit_ap.cod_ser_docto      >= input frame F-Main fi-serie-ini
         and tit_ap.cod_ser_docto      <= input frame F-Main fi-serie-fim
         and tit_ap.cod_tit_ap         >= input frame F-Main fi-nr-docto-ini
         and tit_ap.cod_tit_ap         <= input frame F-Main fi-nr-docto-fim
         and tit_ap.cod_parcela        >= input frame F-Main fi-parcela-ini
         and tit_ap.cod_parcela        <= input frame F-Main fi-parcela-fim
         and tit_ap.dat_vencto_tit_ap  >= input frame F-Main fi-dt-vencimen-ini
         and tit_ap.dat_vencto_tit_ap  <= input frame F-Main fi-dt-vencimen-fim
         and tit_ap.dat_liquidac_tit_ap = 12/31/9999
         and tit_ap.log_sdo_tit_ap
         and tit_ap.log_tit_ap_estordo = no
         and tit_ap.ind_tip_espec_docto = "Normal":

        /* se estiver no bordero entÆo nÆo deve aparecer */
        if  can-find(first item_bord_ap no-lock
                     where item_bord_ap.cod_estab            = tit_ap.cod_estab
                       and item_bord_ap.cod_espec_docto      = tit_ap.cod_espec_docto
                       and item_bord_ap.cod_ser_docto        = tit_ap.cod_ser_docto
                       and item_bord_ap.cdn_fornecedor       = tit_ap.cdn_fornecedor
                       and item_bord_ap.cod_tit_ap           = tit_ap.cod_tit_ap
                       and item_bord_ap.cod_parcela          = tit_ap.cod_parcela
                       and item_bord_ap.ind_sit_item_bord_ap = "em aberto") then next.

        find first fornecedor no-lock
             where fornecedor.cdn_fornecedor  = tit_ap.cdn_fornecedor
               and fornecedor.cod_grp_fornec >= input frame f-main fi-cod-gr-forn-ini
               and fornecedor.cod_grp_fornec <= input frame f-main fi-cod-gr-forn-fim no-error.
        if  not avail fornecedor then next.

        if  can-find(first tt-tit_ap no-lock
                     where tt-tit_ap.ep-codigo   = tit_ap.cod_empresa
                       and tt-tit_ap.cod-fornec  = tit_ap.cdn_fornecedor
                       and tt-tit_ap.cod-estabel = tit_ap.cod_estab
                       and tt-tit_ap.cod-esp     = tit_ap.cod_espec_docto
                       and tt-tit_ap.serie       = tit_ap.cod_ser_docto
                       and tt-tit_ap.nr-docto    = tit_ap.cod_tit_ap
                       and tt-tit_ap.parcela     = tit_ap.cod_parcela) then next.

        run pi-acompanhar in h-acomp(input "Dt Maquina : " + string(movto_tit_ap.dat_gerac_movto)).

        create tt-tit_ap.
        assign tt-tit_ap.selecionado = yes
               tt-tit_ap.ep-codigo   = tit_ap.cod_empresa
               tt-tit_ap.cod-estabel = tit_ap.cod_estab
               tt-tit_ap.cod-fornec  = tit_ap.cdn_fornecedor
               tt-tit_ap.nome-abrev  = fornecedor.nom_abrev
               tt-tit_ap.cod-esp     = tit_ap.cod_espec_docto
               tt-tit_ap.serie       = tit_ap.cod_ser_docto
               tt-tit_ap.nr-docto    = tit_ap.cod_tit_ap
               tt-tit_ap.parcela     = tit_ap.cod_parcela
               tt-tit_ap.dt-vencimen = tit_ap.dat_vencto_tit_ap
               tt-tit_ap.vl-original = tit_ap.val_origin_tit_ap
               tt-tit_ap.valor-saldo = tit_ap.val_sdo_tit_ap
               tt-tit_ap.dt-maquina  = movto_tit_ap.dat_gerac_movto.

        find es-fornec-ap no-lock
            where es-fornec-ap.cod-emitente = tt-tit_ap.cod-fornec no-error.
        assign tt-tit_ap.selecionado = not avail es-fornec-ap
               tt-tit_ap.excecao     = avail es-fornec-ap.

    /*     if tt-tit_ap.dt-vencimen >= today then  */
    /*         assign tt-tit_ap.selecionado = yes. */

    end.

    run pi-finalizar in h-acomp.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

