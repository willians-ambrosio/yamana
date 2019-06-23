&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/************************************************************************
**  Programa: ESMAT003
**  Objetivo: Relatorio de Gastos de Materiais
**     Autor: Roberto Leandro - Kraft
**      Data: 10/08/2011
************************************************************************/
DEF BUFFER empresa FOR ems2cadme.empresa.
DEF BUFFER ccusto  FOR ems5.ccusto.

{include/i-prgvrs.i ESMAT003 12.01.19.001}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE PGSEL f-pg-sel
  
define temp-table tt-param no-undo
    FIELD fi-est-ini   AS CHAR
    FIELD fi-est-fim   AS CHAR
    FIELD fi-item-ini  AS CHAR
    FIELD fi-item-fim  AS CHAR
    FIELD dt-base-ini  AS DATE
    FIELD dt-base-fim  AS DATE
    FIELD fi-ct-ini    AS CHAR
    FIELD fi-ct-fim    AS CHAR
    FIELD fi-cc-ini    AS CHAR
    FIELD fi-cc-fim    AS CHAR
    FIELD fi-ge-ini    AS INT
    FIELD fi-ge-fim    AS INT
    FIELD l-fisico     AS LOG
    FIELD l-total      AS LOG
    FIELD l-consig     AS LOG
    FIELD l-ddireto    AS LOG
    field l-cep    as log
    field l-app    as log.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-sel

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 ~
IMAGE-18 IMAGE-19 IMAGE-20 RECT-10 RECT-11 IMAGE-21 IMAGE-22 IMAGE-23 ~
IMAGE-24 fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-ge-codigo-ini ~
fi-ge-codigo-fim dt-corte-ini dt-corte-fim fi-ct-ini fi-ct-fim fi-cc-ini ~
fi-cc-fim l-fisico l-total l-consig l-dd l-cep l-app 
&Scoped-Define DISPLAYED-OBJECTS fi-est-ini fi-est-fim fi-item-ini ~
fi-item-fim fi-ge-codigo-ini fi-ge-codigo-fim dt-corte-ini dt-corte-fim ~
fi-ct-ini fi-ct-fim fi-cc-ini fi-cc-fim l-fisico l-total l-consig l-dd ~
l-cep l-app 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE dt-corte-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-corte-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Cta Contabil" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-ini AS CHARACTER FORMAT "x(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-fim AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-ini AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 1.5.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 1.5.

DEFINE VARIABLE l-app AS LOGICAL INITIAL yes 
     LABEL "Contas a Pagar (APP)" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .83 NO-UNDO.

DEFINE VARIABLE l-cep AS LOGICAL INITIAL yes 
     LABEL "Controle de Estoque (CEP)" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE l-consig AS LOGICAL INITIAL yes 
     LABEL "Consignado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE l-dd AS LOGICAL INITIAL yes 
     LABEL "Debito Direto" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE l-fisico AS LOGICAL INITIAL yes 
     LABEL "Fisico" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE l-total AS LOGICAL INITIAL yes 
     LABEL "Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

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
          "Dispara a execuªío do relatΩrio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-est-ini AT ROW 1.21 COL 15 COLON-ALIGNED
     fi-est-fim AT ROW 1.21 COL 50 COLON-ALIGNED NO-LABEL
     fi-item-ini AT ROW 2.21 COL 15 COLON-ALIGNED
     fi-item-fim AT ROW 2.21 COL 50 COLON-ALIGNED NO-LABEL
     fi-ge-codigo-ini AT ROW 3.17 COL 15 COLON-ALIGNED WIDGET-ID 30
     fi-ge-codigo-fim AT ROW 3.17 COL 50 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     dt-corte-ini AT ROW 4.17 COL 15 COLON-ALIGNED
     dt-corte-fim AT ROW 4.17 COL 50 COLON-ALIGNED NO-LABEL
     fi-ct-ini AT ROW 5.17 COL 15 COLON-ALIGNED
     fi-ct-fim AT ROW 5.17 COL 50 COLON-ALIGNED NO-LABEL
     fi-cc-ini AT ROW 6.17 COL 15 COLON-ALIGNED WIDGET-ID 22
     fi-cc-fim AT ROW 6.17 COL 50 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     l-fisico AT ROW 8.21 COL 13 WIDGET-ID 2
     l-total AT ROW 8.21 COL 24.57 WIDGET-ID 4
     l-consig AT ROW 8.21 COL 36 WIDGET-ID 6
     l-dd AT ROW 8.21 COL 52 WIDGET-ID 8
     l-cep AT ROW 10.13 COL 15.29 WIDGET-ID 18
     l-app AT ROW 10.13 COL 42.57 WIDGET-ID 16
     " Controle do Item" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 7.5 COL 10.14
     " Origem dos Dados" VIEW-AS TEXT
          SIZE 13 BY .75 AT ROW 9.42 COL 10.14 WIDGET-ID 14
     IMAGE-13 AT ROW 4.17 COL 35
     IMAGE-14 AT ROW 4.17 COL 46
     IMAGE-15 AT ROW 5.17 COL 46
     IMAGE-16 AT ROW 2.21 COL 46
     IMAGE-17 AT ROW 5.17 COL 35
     IMAGE-18 AT ROW 2.21 COL 35
     IMAGE-19 AT ROW 1.21 COL 35
     IMAGE-20 AT ROW 1.21 COL 46
     RECT-10 AT ROW 7.83 COL 9
     RECT-11 AT ROW 9.75 COL 9 WIDGET-ID 12
     IMAGE-21 AT ROW 6.17 COL 46 WIDGET-ID 24
     IMAGE-22 AT ROW 6.17 COL 35 WIDGET-ID 26
     IMAGE-23 AT ROW 3.17 COL 46 WIDGET-ID 32
     IMAGE-24 AT ROW 3.17 COL 35 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.88
         SIZE 76.86 BY 10.62
         FONT 1.


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
         TITLE              = "ESMAT003 - Relatorio por Conta/Centro de Custo"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 30.25
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.25
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
/* SETTINGS FOR FRAME f-pg-sel
   FRAME-NAME                                                           */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* ESMAT003 - Relatorio por Conta/Centro de Custo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* ESMAT003 - Relatorio por Conta/Centro de Custo */
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


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:

   do  on error undo, return no-apply
       on stop  undo, return no-apply:

       do  with frame f-pg-sel:

           if  fi-est-ini > fi-est-fim then do:
               MESSAGE "Estabelecimento inicial n∆o pode ser maior que o final!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               apply "entry":u to fi-est-fim.
               stop.
           end.

           if  fi-item-ini > fi-item-fim then do:
               MESSAGE "Item inicial n∆o pode ser maior que o final!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               apply "entry":u to fi-item-fim.
               stop.
           end.

           if  dt-corte-ini > dt-corte-fim then do:
               MESSAGE "Data inicial n∆o pode ser maior que a final!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               apply "entry":u to dt-corte-fim.
               stop.
           end.

           if  fi-cc-ini > fi-cc-fim then do:
               MESSAGE "Centro de Custo inicial n∆o pode ser maior que o final!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               apply "entry":u to fi-cc-fim.
               stop.
           end.

           if  not l-fisico:checked and
               not l-total:checked and
               not l-consig:checked and
               not l-dd:checked then do:
               MESSAGE "Pelo menos um tipo de controle do item deve ser selecionado!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               stop.
           end.

           if  not l-cep:checked and
               not l-app:checked then do:
               MESSAGE "Pelo menos uma das Origens dos Dados deve ser selecionada!"
                   VIEW-AS ALERT-BOX error BUTTONS OK.
               stop.
           end.

       end.

       run pi-executar.

   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME dt-corte-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt-corte-fim w-relat
ON LEAVE OF dt-corte-fim IN FRAME f-pg-sel
DO:
  
    assign dt-corte-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dt-corte-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt-corte-ini w-relat
ON LEAVE OF dt-corte-ini IN FRAME f-pg-sel /* Data */
DO:
  
    assign dt-corte-ini
           dt-corte-fim = if day(dt-corte-ini) = 1 then min(date(month(dt-corte-ini + 45),1,year(dt-corte-ini + 45)) - 1,today) else dt-corte-ini.
    display dt-corte-fim with frame f-pg-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cc-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cc-fim w-relat
ON LEAVE OF fi-cc-fim IN FRAME f-pg-sel
DO:
  
    assign fi-cc-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cc-ini w-relat
ON LEAVE OF fi-cc-ini IN FRAME f-pg-sel /* Centro de Custo */
DO:
  
    assign fi-cc-ini
           fi-cc-fim = fi-cc-ini + "ZZZZZZZZ".
    display fi-cc-fim with frame f-pg-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-fim w-relat
ON LEAVE OF fi-ct-fim IN FRAME f-pg-sel
DO:
  
    assign fi-ct-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-ini w-relat
ON LEAVE OF fi-ct-ini IN FRAME f-pg-sel /* Cta Contabil */
DO:
  
    assign fi-ct-ini
           fi-ct-fim = fi-ct-ini + "ZZZZZZZZ".
    display fi-ct-fim with frame f-pg-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-est-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-est-fim w-relat
ON LEAVE OF fi-est-fim IN FRAME f-pg-sel
DO:
  
    assign fi-est-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-est-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-est-ini w-relat
ON LEAVE OF fi-est-ini IN FRAME f-pg-sel /* Estabelecimento */
DO:
  
    assign fi-est-ini
           fi-est-fim = fi-est-ini + "ZZZ".
    display fi-est-fim with frame f-pg-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ge-codigo-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ge-codigo-fim w-relat
ON LEAVE OF fi-ge-codigo-fim IN FRAME f-pg-sel
DO:
  
    assign fi-ge-codigo-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ge-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ge-codigo-ini w-relat
ON LEAVE OF fi-ge-codigo-ini IN FRAME f-pg-sel /* Grupo Estoque */
DO:
  
    assign fi-ge-codigo-ini.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-item-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-item-fim w-relat
ON LEAVE OF fi-item-fim IN FRAME f-pg-sel
DO:
  
    assign fi-item-fim.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-item-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-item-ini w-relat
ON LEAVE OF fi-item-ini IN FRAME f-pg-sel /* Item */
DO:
  
    assign fi-item-ini.
           fi-item-fim = fi-item-ini + "ZZZZZZZZZZZZZZZZ".
    display fi-item-fim with frame f-pg-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESMAT003" "12.01.19.002"}

/* inicializaªÑes do template de relatΩrio */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    ASSIGN dt-corte-ini = date(month(today),1,year(today))
           dt-corte-fim = today.

/*     for first item fields(it-codigo) no-lock:        */
/*         assign fi-item-ini = item.it-codigo.         */
/*     end.                                             */
/*     for last item fields(it-codigo) no-lock:         */
/*         assign fi-item-fim = item.it-codigo.         */
/*     end.                                             */
/*                                                      */
/*     for first estabelec fields(cod-estabel) no-lock: */
/*         assign fi-est-ini = estabelec.cod-estabel.   */
/*     end.                                             */
/*     for last estabelec fields(cod-estabel) no-lock:  */
/*         assign fi-est-fim = estabelec.cod-estabel.   */
/*     end.                                             */

    RUN enable_UI.
  
    IF NOT THIS-PROCEDURE:PERSISTENT then WAIT-FOR CLOSE OF THIS-PROCEDURE.

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
  ENABLE bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-ge-codigo-ini 
          fi-ge-codigo-fim dt-corte-ini dt-corte-fim fi-ct-ini fi-ct-fim 
          fi-cc-ini fi-cc-fim l-fisico l-total l-consig l-dd l-cep l-app 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 
         IMAGE-20 RECT-10 RECT-11 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 
         fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-ge-codigo-ini 
         fi-ge-codigo-fim dt-corte-ini dt-corte-fim fi-ct-ini fi-ct-fim 
         fi-cc-ini fi-cc-fim l-fisico l-total l-consig l-dd l-cep l-app 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*----------------------------------------------------------------------------*/

    do  on error undo, return error 
        on stop  undo, return error:

        empty temp-table tt-param.
        create tt-param.

        do  with frame f-pg-sel:

            ASSIGN tt-param.fi-est-ini  = fi-est-ini
                   tt-param.fi-est-fim  = fi-est-fim
                   tt-param.fi-item-ini = fi-item-ini
                   tt-param.fi-item-fim = fi-item-fim
                   tt-param.dt-base-ini = dt-corte-ini
                   tt-param.dt-base-fim = dt-corte-fim
                   tt-param.fi-cc-ini   = fi-cc-ini
                   tt-param.fi-cc-fim   = fi-cc-fim
                   tt-param.fi-ct-ini   = fi-ct-ini
                   tt-param.fi-ct-fim   = fi-ct-fim
                   tt-param.fi-ge-ini   = fi-ge-codigo-ini
                   tt-param.fi-ge-fim   = fi-ge-codigo-fim

                   tt-param.l-fisico    = l-fisico:checked
                   tt-param.l-total     = l-total :checked
                   tt-param.l-consig    = l-consig:checked
                   tt-param.l-ddireto   = l-dd    :checked
                   tt-param.l-cep       = l-cep   :checked
                   tt-param.l-app       = l-app   :checked.

        end.
    
        SESSION:SET-WAIT-STATE("general":U).
        run esp/esmat003rp.p (input table tt-param).
        SESSION:SET-WAIT-STATE("":U).
        
    end.

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

