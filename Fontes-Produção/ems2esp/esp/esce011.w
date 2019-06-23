&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 2.06.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   no
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
def temp-table tt-param no-undo
    field destino   as int
    field arquivo   as char format "x(35)"
    field usuario   as char format "x(12)"
    field data-exec as date
    field hora-exec as int.

def temp-table tt-digita no-undo
    field ordem   as int  format ">>>>9"
    field exemplo as char format "x(30)"
    index id ordem.

def temp-table tt-item-fornec no-undo
    field cod_empresa    as char
    field cod-emitente   like emitente.cod-emitente
    field nome-emit      like emitente.nome-emit     
    field nome-abrev     like emitente.nome-abrev    
    field tp-desp-padrao like emitente.tp-desp-padrao
    field tp-desp-para   like emitente.tp-desp-padrao
    field it-codigo      like item.it-codigo
    field desc-item      like item.desc-item
    field tp-desp-item   like item.tp-desp-padrao
    field l-atualiza     as log
    index ix-1 as primary unique cod_empresa cod-emitente it-codigo
    index ix-2 cod-emitente tp-desp-item
    index ix-3 l-atualiza cod-emitente it-codigo.

def temp-table tt-item-fornec-cont no-undo
    field cod-emitente like emitente.cod-emitente
    field tp-desp-item like item.tp-desp-padrao
    field i-qtd        as int
    index ix-1 as primary unique cod-emitente tp-desp-item desc.

def buffer b-tt-digita            for tt-digita.
def buffer bf-tt-item-fornec      for tt-item-fornec.
def buffer bf-tt-item-fornec-cont for tt-item-fornec-cont.

/* Transfer Definitions */
def var raw-param as raw no-undo.

def temp-table tt-raw-digita
    field raw-digita as raw.
                    
/* Local Variable Definitions ---                                       */
def var l-ok         as log    no-undo.
def var c-arq-digita as char   no-undo.
def var c-terminal   as char   no-undo.
def var h-acomp      as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE ed-help AS CHARACTER INITIAL "Caso o tipo de despesa dos itens sejam iguais, atualiza o tipo de despesa do Fornecedor." 
     VIEW-AS EDITOR
     SIZE 71 BY 2 NO-UNDO.

DEFINE VARIABLE l-atualiza AS LOGICAL INITIAL no 
     LABEL "Atualiza Fornecedor pelo Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 4.96 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-modo AT ROW 4.21 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 4.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5.

DEFINE FRAME f-pg-par
     l-atualiza AT ROW 1.29 COL 3.29
     ed-help AT ROW 2.5 COL 3 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.

DEFINE FRAME f-pg-sel
     i-cod-emitente-ini AT ROW 1.25 COL 15 COLON-ALIGNED
     i-cod-emitente-fim AT ROW 1.25 COL 48.14 COLON-ALIGNED NO-LABEL
     c-it-codigo-ini AT ROW 2.25 COL 15 COLON-ALIGNED HELP
          "C¢digo do Item"
     c-it-codigo-fim AT ROW 2.25 COL 48.14 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.25 COL 31.86
     IMAGE-2 AT ROW 1.25 COL 47.29
     IMAGE-3 AT ROW 2.25 COL 31.86
     IMAGE-4 AT ROW 2.25 COL 47.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "<Title>"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
ASSIGN 
       ed-help:READ-ONLY IN FRAME f-pg-par        = TRUE.

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* <Title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* <Title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
    do  with frame f-pg-imp:
        case self:screen-value:
            when "1" then
                assign c-arquivo     :sensitive = no
                       bt-arquivo    :visible   = no
                       bt-config-impr:visible   = yes.
            when "2" then
                assign c-arquivo     :sensitive = yes
                       bt-arquivo    :visible   = yes
                       bt-config-impr:visible   = no.
            when "3" then
                assign c-arquivo     :sensitive = no
                       bt-arquivo    :visible   = no
                       bt-config-impr:visible   = no.
        end case.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "XX9999" "9.99.99.999"}

/*:T inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
    
    {include/i-rpmbl.i}
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY i-cod-emitente-ini i-cod-emitente-fim c-it-codigo-ini c-it-codigo-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 i-cod-emitente-ini i-cod-emitente-fim 
         c-it-codigo-ini c-it-codigo-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY l-atualiza ed-help 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE l-atualiza ed-help 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados w-relat 
PROCEDURE pi-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    empty temp-table tt-item-fornec.

    run pi-item-fornec(input "YDS", input "ems2cademp").
    run pi-item-fornec(input "YDM", input "ems2cademp_YDM").
    run pi-item-fornec(input "STE", input "ems2cademp_STE").
    run pi-item-fornec(input "MRC", input "ems2cademp_MRC").
    run pi-item-fornec(input "MFB", input "ems2cademp_MFB").
    run pi-item-fornec(input "MBC", input "ems2cademp_MBC").
    run pi-item-fornec(input "JMC", input "ems2cademp_JMC").
    run pi-item-fornec(input "CGO", input "ems2cademp_CGO").

    run pi-acompanhar in h-acomp(input "Tp Desp: Fornec x Item...1").
    for each tt-item-fornec:
        find first tt-item-fornec-cont
             where tt-item-fornec-cont.cod-emitente = tt-item-fornec.cod-emitente
               and tt-item-fornec-cont.tp-desp-item = tt-item-fornec.tp-desp-item no-error.
        if  not avail tt-item-fornec-cont then do:
            create tt-item-fornec-cont.
            assign tt-item-fornec-cont.cod-emitente = tt-item-fornec.cod-emitente
                   tt-item-fornec-cont.tp-desp-item = tt-item-fornec.tp-desp-item
                   tt-item-fornec-cont.i-qtd        = 1.
        end.
        else
            assign tt-item-fornec-cont.i-qtd = tt-item-fornec-cont.i-qtd + 1.
    end.

    run pi-acompanhar in h-acomp(input "Tp Desp: Fornec x Item...2").
    for each bf-tt-item-fornec-cont,
        each tt-item-fornec-cont
       where tt-item-fornec-cont.cod-emitente = bf-tt-item-fornec-cont.cod-emitente
         and tt-item-fornec-cont.i-qtd        < bf-tt-item-fornec-cont.i-qtd:
        delete tt-item-fornec-cont.
    end.

    run pi-acompanhar in h-acomp(input "Tp Desp: Fornec x Item...3").
    for each bf-tt-item-fornec-cont:
        if  not can-find(first tt-item-fornec-cont
                         where tt-item-fornec-cont.cod-emitente  = bf-tt-item-fornec-cont.cod-emitente
                           and tt-item-fornec-cont.tp-desp-item <> bf-tt-item-fornec-cont.tp-desp-item) then do:
            for each tt-item-fornec
               where tt-item-fornec.cod-emitente = bf-tt-item-fornec-cont.cod-emitente:
                assign tt-item-fornec.tp-desp-para = bf-tt-item-fornec-cont.tp-desp-item
                       tt-item-fornec.l-atualiza   = yes.

                if  tt-item-fornec.tp-desp-padrao = tt-item-fornec.tp-desp-para then
                    delete tt-item-fornec.
            end.
        end.
    end.

    run pi-acompanhar in h-acomp(input "Tp Desp: Fornec x Item...4").
    if  l-atualiza:checked in frame f-pg-par then
        for each tt-item-fornec
           where tt-item-fornec.l-atualiza:
            if  tt-item-fornec.tp-desp-para <> 999999999 then do:
                find first emitente exclusive-lock
                     where emitente.cod-emitente = tt-item-fornec.cod-emitente no-error.
                if  avail emitente then
                    assign emitente.tp-desp-padrao = tt-item-fornec.tp-desp-para.
            end.
        end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel w-relat 
PROCEDURE pi-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    find first tt-item-fornec no-lock no-error.
    if  not avail tt-item-fornec then do:
        run utp/ut-msgs.p(input "show":U, input 17006, input "Nenhum Registro encontrado":U).
        return.
    end.

    def var ch-excel    as com-handle no-undo.
    def var ch-workbook as com-handle no-undo.
    def var i-linha     as int        no-undo.
    def var i-folder    as int        no-undo.

    create "Excel.Application" ch-excel no-error.
    if  not valid-handle(ch-excel) then do:
        run utp/ut-msgs.p(input "show":U, input 17006,
                          input "Erro ao criar Excel~~Verifique se o Excel est  instalado.").

        return "NOK":U.
    end. /* if  not valid-handle(ch-excel) */

    assign ch-excel:visible       = no
           ch-excel:DisplayAlerts = no.

    ch-workbook = ch-excel:Workbooks:add().

    ch-workbook:Sheets(1):name = "Tipo Despesas Diferentes".

    if  l-atualiza:checked in frame f-pg-par then
        ch-workbook:Sheets(2):name = "Atualizado".
    else
        ch-workbook:Sheets(2):name = "Atualizar".

    for each tt-item-fornec no-lock use-index ix-3
       break by tt-item-fornec.l-atualiza:
        if  first-of(tt-item-fornec.l-atualiza) then do:
            assign i-linha  = 1
                   i-folder = if  not tt-item-fornec.l-atualiza then 1 else 2.

            ch-workbook:sheets(i-folder):select.

            ch-workbook:Sheets(i-folder):Range("A1"):value = "Emp".
            ch-workbook:Sheets(i-folder):Range("B1"):value = "C¢digo".
            ch-workbook:Sheets(i-folder):Range("C1"):value = "Nome Abrev".
            ch-workbook:Sheets(i-folder):Range("D1"):value = "Nome".
            ch-workbook:Sheets(i-folder):Range("E1"):value = "Tp Despesa".
            ch-workbook:Sheets(i-folder):Range("F1"):value = "Tp Desp Para".
            ch-workbook:Sheets(i-folder):Range("G1"):value = "Item".
            ch-workbook:Sheets(i-folder):Range("H1"):value = "Descri‡Æo".
            ch-workbook:Sheets(i-folder):Range("I1"):value = "Tp Despesa".
            ch-workbook:Sheets(i-folder):Rows("1:1"):font:Bold = yes.
            ch-workbook:Sheets(i-folder):Range("A2"):select.
            ch-excel:activewindow:FreezePanes = yes.

            ch-workbook:Sheets(i-folder):columns("A:A"):NumberFormat = "@".
            ch-workbook:Sheets(i-folder):columns("C:D"):NumberFormat = "@".
            ch-workbook:Sheets(i-folder):columns("G:H"):NumberFormat = "@".
        end.

        run pi-acompanhar in h-acomp(input (if  i-folder = 1 then ch-workbook:Sheets(1):name
                                            else ch-workbook:Sheets(2):name) + ' / Linha: ' + string(i-linha)).

        assign i-linha = i-linha + 1.
        ch-workbook:Sheets(i-folder):Range("A" + string(i-linha)):value = tt-item-fornec.cod_empresa.
        ch-workbook:Sheets(i-folder):Range("B" + string(i-linha)):value = tt-item-fornec.cod-emitente.
        ch-workbook:Sheets(i-folder):Range("C" + string(i-linha)):value = tt-item-fornec.nome-abrev.
        ch-workbook:Sheets(i-folder):Range("D" + string(i-linha)):value = tt-item-fornec.nome-emit.
        ch-workbook:Sheets(i-folder):Range("E" + string(i-linha)):value = tt-item-fornec.tp-desp-padrao.

        if  tt-item-fornec.tp-desp-para <> 999999999 then 
            ch-workbook:Sheets(i-folder):Range("F" + string(i-linha)):value = tt-item-fornec.tp-desp-para.

        ch-workbook:Sheets(i-folder):Range("G" + string(i-linha)):value = tt-item-fornec.it-codigo.
        ch-workbook:Sheets(i-folder):Range("H" + string(i-linha)):value = tt-item-fornec.desc-item.
        ch-workbook:Sheets(i-folder):Range("I" + string(i-linha)):value = tt-item-fornec.tp-desp-item.

        if  i-linha mod 2 = 0 then
            ch-workbook:Sheets(i-folder):Range("A" + string(i-linha) + ":I" + string(i-linha)):Interior:ColorIndex = 15.

        if  last-of(tt-item-fornec.l-atualiza) then
            ch-workbook:Sheets(i-folder):Range("A:I"):EntireColumn:AutoFit.
    end.

    ch-workbook:sheets(1):select.

    find first tt-param no-lock no-error.

    case tt-param.destino:
        when 1 then /* Impressora */
            ch-workbook:PrintOut().

        when 2 then /* Arquivo */
            ch-workbook:SaveAs(tt-param.arquivo,,,,,,).

        when 3 then /* Terminal */
            ch-excel:visible = yes.
    end case. /* case tt-param.destino */

    if  tt-param.destino <> 3 then do:
        ch-workbook:close().
        ch-excel:quit().
    end. /* if tt-param.destino <> 3 */

    if  valid-handle(ch-workbook) then release object ch-workbook.
    if  valid-handle(ch-excel)    then release object ch-excel.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    do on error undo, return error
       on stop  undo, return error:
        {include/i-rpexa.i}

        if input frame f-pg-imp rs-destino = 2 and
           input frame f-pg-imp rs-execucao = 1 then do:
            run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).

            if return-value = "NOK":U then do:
                run utp/ut-msgs.p (input "show":U, input 73, input "").

                apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
                apply "ENTRY":U to c-arquivo in frame f-pg-imp.
                return error.
            end.
        end.

        /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem
           apresentar uma mensagem de erro cadastrada, posicionar na p gina com
           problemas e colocar o focus no campo com problemas */

        /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
           para o programa RP.P */

        create tt-param.
        assign tt-param.usuario   = c-seg-usuario
               tt-param.destino   = input frame f-pg-imp rs-destino
               tt-param.data-exec = today
               tt-param.hora-exec = time
               .

        if  tt-param.destino = 1 then
            assign tt-param.arquivo = "".
        else
            if  tt-param.destino = 2 then
                assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
            else
                assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
        /*Fim alteracao 14/02/2005*/

        /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
           como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */



        /*:T Executar do programa RP.P que ir  criar o relat¢rio */
        {include/i-rpexb.i}
        session:set-wait-state("general":U).
/*         {include/i-rprun.i xxp/xx9999rp.p} */


        run utp/ut-acomp.p persistent set h-acomp.
        run pi-inicializar in h-acomp(input "Analisando Dados...").

        run pi-dados.
        run pi-inicializar in h-acomp(input "Gerando Excel...").

        run pi-excel.

        run pi-finalizar in h-acomp.

        {include/i-rpexc.i}
        session:set-wait-state("":U).
/*         {include/i-rptrm.i} */
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-item-fornec w-relat 
PROCEDURE pi-item-fornec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param c-emp   as char no-undo.
    def input param c-banco as char no-undo.

    def var wghBufferTT1  as widget-handle no-undo.
    def var wghBufferTT2  as widget-handle no-undo.
    def var wghQuery1     as widget-handle no-undo.
    def var wghQuery2     as widget-handle no-undo.
    def var hBufferField1 as widget-handle no-undo.
    def var hBufferField2 as widget-handle no-undo.
    def var h-buffer1     as handle        no-undo.
    def var h-buffer2     as handle        no-undo.
    def var c-prepare1    as char          no-undo.
    def var c-prepare2    as char          no-undo.
    def var c-tabela1     as char          no-undo.
    def var c-tabela2     as char          no-undo.

    assign c-tabela1 = trim(c-banco) + ".item-fornec"
           c-tabela2 = trim(c-banco) + ".item".

    assign c-prepare1 = "for each " + c-tabela1 +
                          " where " + c-tabela1 + ".cod-emitente >= " + string(i-cod-emitente-ini:input-value in frame f-pg-sel) +
                            " and " + c-tabela1 + ".cod-emitente <= " + string(i-cod-emitente-fim:input-value in frame f-pg-sel) +
                            " and " + c-tabela1 + ".it-codigo    >= '" + c-it-codigo-ini:input-value in frame f-pg-sel + "'" +
                            " and " + c-tabela1 + ".it-codigo    <= '" + c-it-codigo-fim:input-value in frame f-pg-sel + "'".

    create buffer h-buffer1 for table c-tabela1.
    create query wghQuery1.
    wghQuery1:set-buffers(h-buffer1).
    wghQuery1:query-prepare(c-prepare1).
    wghQuery1:query-open.

    repeat:
        wghQuery1:get-next(no-lock).
        if  wghQuery1:query-off-end then leave.

        assign c-prepare2 = "for each " + c-tabela2 +
                            " where " + c-tabela2 + ".it-codigo = '" + h-buffer1:buffer-field("it-codigo"):buffer-value + "'".

        create buffer h-buffer2 for table c-tabela2.
        create query wghQuery2.
        wghQuery2:set-buffers(h-buffer2).
        wghQuery2:query-prepare(c-prepare2).
        wghQuery2:query-open.

        wghQuery2:get-first(no-lock).
        if  wghQuery2:query-off-end then leave.

        find first emitente no-lock
             where emitente.cod-emitente = h-buffer1:buffer-field("cod-emitente"):buffer-value no-error.
        if  not avail emitente then next.

        run pi-acompanhar in h-acomp(input 'Emp: ' + c-emp +
                                     ' / Fornec: ' + string(emitente.cod-emitente) +
                                       ' / Item: ' + h-buffer1:buffer-field("it-codigo"):buffer-value).

        find first tt-item-fornec no-lock
             where tt-item-fornec.cod_empresa  = c-emp
               and tt-item-fornec.cod-emitente = emitente.cod-emitente 
               and tt-item-fornec.it-codigo    = h-buffer1:buffer-field("it-codigo"):buffer-value no-error.
        if  not avail tt-item-fornec then do:
            create tt-item-fornec.
            assign tt-item-fornec.cod_empresa    = c-emp
                   tt-item-fornec.cod-emitente   = emitente.cod-emitente
                   tt-item-fornec.nome-emit      = emitente.nome-emit     
                   tt-item-fornec.nome-abrev     = emitente.nome-abrev    
                   tt-item-fornec.tp-desp-padrao = emitente.tp-desp-padrao
                   tt-item-fornec.tp-desp-para   = 999999999
                   tt-item-fornec.it-codigo      = h-buffer1:buffer-field("it-codigo")     :buffer-value
                   tt-item-fornec.desc-item      = h-buffer2:buffer-field("desc-item")     :buffer-value
                   tt-item-fornec.tp-desp-item   = h-buffer2:buffer-field("tp-desp-padrao"):buffer-value
                   tt-item-fornec.l-atualiza     = no.
        end.

        wghQuery2:query-close().
        assign wghQuery2 = ?.
    end.

    wghQuery1:query-close().
    assign wghQuery1 = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/i-rptrp.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

