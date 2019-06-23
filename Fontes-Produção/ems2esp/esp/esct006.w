&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE  tt-ge-mov
    FIELD  referencia   LIKE movto-estoq.ct-codigo /*conta-contabil*/
    FIELD  sc-codigo    LIKE movto-estoq.sc-codigo
    FIELD  dt-trans     LIKE movto-estoq.dt-trans
    FIELD  fantasma     LIKE movto-estoq.tipo-trans
    FIELD  saldo        LIKE movto-estoq.tipo-trans
    FIELD  esp-docto    AS   CHAR
    FIELD  numero-ordem LIKE movto-estoq.numero-ordem
    FIELD  cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD  cod-depos    LIKE movto-estoq.cod-depos
    FIELD  serie-docto  LIKE movto-estoq.serie-docto
    FIELD  nro-docto    LIKE movto-estoq.nro-docto
    FIELD  cod-emitente LIKE movto-estoq.cod-emitente
    FIELD  it-codigo    LIKE movto-estoq.it-codigo
    FIELD  un           LIKE item.un
    FIELD  descricao-db LIKE movto-estoq.descricao-db
    FIELD  qtde-movto   LIKE movto-estoq.quantidade
    FIELD  descricao    AS   CHAR
    FIELD  quantidade   LIKE movto-estoq.quantidade
    FIELD  cod-emit     LIKE movto-estoq.cod-emit
    FIELD  vl-01        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-02        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-03        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-04        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-05        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-06        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-07        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-08        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-09        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-10        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-11        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  vl-12        LIKE movto-estoq.valor-ggf-m[1]
    FIELD  nome-emit    LIKE emitente.nome-emit.


/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha     AS INT.
DEF VAR data-de     AS CHAR FORMAT "99/9999".
DEF VAR i-mes-aux   AS INT  FORMAT "99".
DEF VAR i-ano-aux   AS INT  FORMAT "9999".
DEF VAR de-vl-tot LIKE movto-estoq.valor-ggf-m[1].

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 RECT-21 BtnOK BtnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Sair" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Imprimir" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 10.5.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.75.

DEFINE VARIABLE ano AS INTEGER FORMAT "9999" INITIAL 0 
     LABEL "Periodo":R21 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-conta-fim AS CHARACTER FORMAT "x(17)" INITIAL "99999999999999999" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE c-conta-ini AS CHARACTER FORMAT "x(17)" INITIAL "0000000000000000" 
     LABEL "Conta Cont bil":R17 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R20 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-fim AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-ini AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnOK AT ROW 11.75 COL 25
     BtnCancel AT ROW 11.75 COL 43
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 11.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.43 BY 12.42
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME f-pg-sel
     c-conta-ini AT ROW 1.5 COL 14 COLON-ALIGNED HELP
          "Conta cont bil"
     c-conta-fim AT ROW 1.5 COL 46 COLON-ALIGNED HELP
          "Conta cont bil" NO-LABEL
     i-ge-ini AT ROW 2.5 COL 14 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     i-ge-fim AT ROW 2.5 COL 46 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     c-estab-ini AT ROW 3.5 COL 14 COLON-ALIGNED HELP
          "Estabelecimento padrao do Item"
     c-estab-fim AT ROW 3.5 COL 46 COLON-ALIGNED HELP
          "Estabelecimento padrao do Item" NO-LABEL
     ano AT ROW 4.5 COL 14 COLON-ALIGNED
     IMAGE-1 AT ROW 1.5 COL 39
     IMAGE-2 AT ROW 1.5 COL 44
     IMAGE-5 AT ROW 2.5 COL 39
     IMAGE-6 AT ROW 2.5 COL 44
     IMAGE-7 AT ROW 3.5 COL 39
     IMAGE-8 AT ROW 3.5 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3
         SIZE 69 BY 7.5
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Movimenta‡Æo de Fornecedores>"
         HEIGHT             = 12.33
         WIDTH              = 72
         MAX-HEIGHT         = 31.29
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 31.29
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-sel:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Movimenta‡Æo de Fornecedores> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Movimenta‡Æo de Fornecedores> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Sair */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
 
  RUN pi-executa.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-conta-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-fim C-Win
ON ENTRY OF c-conta-fim IN FRAME f-pg-sel
DO:
  if avail param-global then
     assign c-conta-fim:format in frame {&frame-name} = param-global.formato-conta-contabil.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-conta-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-ini C-Win
ON ENTRY OF c-conta-ini IN FRAME f-pg-sel /* Conta Cont bil */
DO:
  if avail param-global then
     assign c-conta-ini:format in frame f-pg-sel = param-global.formato-conta-contabil.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

  
ASSIGN ano = int(YEAR(today)).
     /*data-ate = string(MONTH(TODAY),"99")  + string(YEAR(today),"9999"). */

/*
ASSIGN fi-dt-corte = TODAY
       fi-dt-ini   = TODAY
       fi-dt-fim   = TODAY.
  */
       
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

      


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

APPLY 'entry' TO INPUT c-conta-ini.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE RECT-20 RECT-21 BtnOK BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY c-conta-ini c-conta-fim i-ge-ini i-ge-fim c-estab-ini c-estab-fim ano 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 c-conta-ini 
         c-conta-fim i-ge-ini i-ge-fim c-estab-ini c-estab-fim ano 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executa C-Win 
PROCEDURE pi-executa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME f-pg-sel:

 ASSIGN c-conta-ini
        c-conta-fim
        ano
        i-ge-ini
        i-ge-fim
        c-estab-ini
        c-estab-fim.

 FOR EACH tt-ge-mov:
     DELETE  tt-ge-mov.
 END.

 ASSIGN de-vl-tot = 0.
 
   FOR EACH movto-estoq 
      WHERE movto-estoq.conta-contabil  >= c-conta-ini
        AND movto-estoq.conta-contabil  <= c-conta-fim
        AND movto-estoq.cod-estabel     >= c-estab-ini
        AND movto-estoq.cod-estabel     <= c-estab-fim
        AND YEAR (movto-estoq.dt-trans)  = ano NO-LOCK
       BREAK BY movto-estoq.cod-emit
             BY movto-estoq.ct-codigo /* conta-contabil*/
             BY movto-estoq.sc-codigo 
             BY month(movto-estoq.dt-trans)  
             BY YEAR (movto-estoq.dt-trans):
       
      
        /* tipo-trans =  saida */ 
        IF movto-estoq.tipo-trans = 2 THEN
           assign de-vl-tot = de-vl-tot  +
                             (movto-estoq.valor-ggf-m[1]
                            + movto-estoq.valor-mat-m[1]
                            + movto-estoq.valor-mob-m[1]
                            + movto-estoq.valor-icm
                            + movto-estoq.valor-ipi
                            + movto-estoq.valor-iss
                            + dec(substring(movto-estoq.char-1,29,14))  /* PIS */
                            + dec(substring(movto-estoq.char-1,43,14))). /* COFINS */
        ELSE
        /* tipo-trans =  entrada */ 
        IF movto-estoq.tipo-trans = 1 THEN
           assign de-vl-tot = de-vl-tot - 
                             (movto-estoq.valor-ggf-m[1]
                            + movto-estoq.valor-mat-m[1]
                            + movto-estoq.valor-mob-m[1]
                            + movto-estoq.valor-icm
                            + movto-estoq.valor-ipi
                            + movto-estoq.valor-iss
                            + dec(substring(movto-estoq.char-1,29,14))  /* PIS */
                            + dec(substring(movto-estoq.char-1,43,14))). /* COFINS */

        IF LAST-OF(month(movto-estoq.dt-trans)) AND  
           LAST-OF(YEAR(movto-estoq.dt-trans)) THEN DO:
        
           FIND tt-ge-mov WHERE 
                tt-ge-mov.referencia   = movto-estoq.ct-codigo      AND 
                tt-ge-mov.sc-codigo    = movto-estoq.sc-codigo      AND
                tt-ge-mov.cod-emitente = movto-estoq.cod-emitente   NO-ERROR.

           IF NOT AVAIL tt-ge-mov THEN DO:
                            
              FIND emitente WHERE emitente.cod-emit = movto-estoq.cod-emit NO-LOCK NO-ERROR.

              create tt-ge-mov.
              assign tt-ge-mov.referencia   = movto-estoq.ct-codigo /* conta-contabil*/ 
                     tt-ge-mov.sc-codigo    = movto-estoq.sc-codigo 
                     tt-ge-mov.numero-ordem = movto-estoq.numero-ordem 
                     tt-ge-mov.cod-estabel  = movto-estoq.cod-estabel
                     tt-ge-mov.cod-emitente = movto-estoq.cod-emitente
                     tt-ge-mov.cod-emit     = movto-estoq.cod-emit
                     tt-ge-mov.nome-emit    = emitente.nome-emit.
           END.

              IF MONTH(movto-estoq.dt-trans) = 01 THEN
                 ASSIGN vl-01 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 02 THEN
                 ASSIGN vl-02 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 03 THEN
                 ASSIGN vl-03 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 04 THEN
                 ASSIGN vl-04 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 05 THEN
                 ASSIGN vl-05 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 06 THEN
                 ASSIGN vl-06 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 07 THEN
                 ASSIGN vl-07 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 08 THEN
                 ASSIGN vl-08 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 09 THEN
                 ASSIGN vl-09 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 10 THEN
                 ASSIGN vl-10 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 11 THEN
                 ASSIGN vl-11 = de-vl-tot.
              ELSE
              IF MONTH(movto-estoq.dt-trans) = 12 THEN
                 ASSIGN vl-12 = de-vl-tot.
              
              ASSIGN de-vl-tot = 0.

           
        END.

   end.

   RUN pi-imprime.


   FOR EACH tt-ge-mov:
       DELETE tt-ge-mov.
   END.

   /* movimentacao do contas a pagar */
   ASSIGN de-vl-tot = 0.
   FOR EACH mov-ap 
      WHERE mov-ap.conta-contabil  >= c-conta-ini
        AND mov-ap.conta-contabil  <= c-conta-fim
        AND mov-ap.cod-estabel     >= c-estab-ini
        AND mov-ap.cod-estabel     <= c-estab-fim
        AND YEAR (mov-ap.dt-trans)  = ano 
        AND (mov-ap.transacao       = 1 
         OR  mov-ap.transacao       = 5) NO-LOCK
       BREAK BY mov-ap.cod-fornec
             BY mov-ap.ct-conta /* conta-contabil*/
             BY mov-ap.sc-conta 
             BY month(mov-ap.dt-trans)  
             BY YEAR (mov-ap.dt-trans):

       IF mov-ap.lancamento = 1 THEN
          assign de-vl-tot = de-vl-tot  + mov-ap.valor-mov. /*mov-ap.vl-original. - pegar AVA */
       ELSE 
       IF mov-ap.lancamento = 2 THEN
          assign de-vl-tot = de-vl-tot  - mov-ap.valor-mov. /*mov-ap.vl-original. - pegar AVA */
                            
        IF LAST-OF(month(mov-ap.dt-trans)) AND  
           LAST-OF(YEAR(mov-ap.dt-trans)) THEN DO:
        
           FIND tt-ge-mov WHERE 
                tt-ge-mov.referencia   = mov-ap.ct-conta      AND 
                tt-ge-mov.sc-codigo    = mov-ap.sc-conta      AND
                tt-ge-mov.cod-emitente = mov-ap.cod-fornec    NO-ERROR.

           IF NOT AVAIL tt-ge-mov THEN DO:
                            
              FIND emitente WHERE emitente.cod-emit = mov-ap.cod-fornec NO-LOCK NO-ERROR.

              create tt-ge-mov.
              assign tt-ge-mov.referencia   = mov-ap.ct-conta /* conta-contabil*/ 
                     tt-ge-mov.sc-codigo    = mov-ap.sc-conta 
                     /* tt-ge-mov.numero-ordem = mov-apto-estoq.numero-ordem */
                     tt-ge-mov.cod-estabel  = mov-ap.cod-estabel
                     tt-ge-mov.cod-emitente = mov-ap.cod-fornec
                     tt-ge-mov.cod-emit     = mov-ap.cod-fornec
                     tt-ge-mov.nome-emit    = emitente.nome-emit.
           END.

              IF MONTH(mov-ap.dt-trans) = 01 THEN
                 ASSIGN vl-01 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 02 THEN
                 ASSIGN vl-02 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 03 THEN
                 ASSIGN vl-03 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 04 THEN
                 ASSIGN vl-04 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 05 THEN
                 ASSIGN vl-05 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 06 THEN
                 ASSIGN vl-06 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 07 THEN
                 ASSIGN vl-07 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 08 THEN
                 ASSIGN vl-08 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 09 THEN
                 ASSIGN vl-09 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 10 THEN
                 ASSIGN vl-10 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 11 THEN
                 ASSIGN vl-11 = de-vl-tot.
              ELSE
              IF MONTH(mov-ap.dt-trans) = 12 THEN
                 ASSIGN vl-12 = de-vl-tot.
              
              ASSIGN de-vl-tot = 0.

           
        END.

   end.
          
   RUN pi-imprime.
      
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
DO: 
 
 /*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iTotalNumberOfOrders    AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 3.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

DEF VAR COUNT  AS INT.

    
/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

    /*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/ 
    chWorkSheet:Range("A1"):Value = "Movimenta‡Æo de Fornecedores  -  "    + STRING(ano) + "                        Est: " + c-estab-ini  + 
                                    "    a     " + c-estab-fim.
                    /*Value  " + string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
    chWorkSheet:Range("A1"):Font:Size = 12.
    chWorkSheet:Range("A1"):Font:Bold = TRUE.
    chWorkSheet:Range("A3"):Value = "C.Contabil". 
    chWorkSheet:Range("B3"):Value = "C.Custo  ". 

    chWorkSheet:Range("C3"):Value = "Cod Emit    ".     
    chWorkSheet:Range("D3"):Value = "Nome        ".    
    chWorkSheet:Range("E3"):Value = "Jan       ".   
    chWorkSheet:Range("F3"):Value = "Fev       ".   
    chWorkSheet:Range("G3"):Value = "Mar       ".   
    chWorkSheet:Range("H3"):Value = "Abr       ".   
    chWorkSheet:Range("I3"):Value = "Mai       ".   
    chWorkSheet:Range("J3"):Value = "Jun       ".   
    chWorkSheet:Range("K3"):Value = "Jul       ".   
    chWorkSheet:Range("L3"):Value = "Ago       ".   
    chWorkSheet:Range("M3"):Value = "Set       ".   
    chWorkSheet:Range("N3"):Value = "Out       ".   
    chWorkSheet:Range("O3"):Value = "Nov       ".   
    chWorkSheet:Range("P3"):Value = "Dez       ".   
                        

    chWorkSheet:Range("A3:P3"):Font:Bold = TRUE.
    chWorkSheet:Columns("A"):ColumnWidth = 10.
    chWorkSheet:Columns("B"):ColumnWidth = 09.
    chWorkSheet:Columns("C"):ColumnWidth = 12.
    chWorkSheet:Columns("D"):ColumnWidth = 40.
    chWorkSheet:Columns("E"):ColumnWidth = 10.
    chWorkSheet:Columns("F"):ColumnWidth = 10.
    chWorkSheet:Columns("G"):ColumnWidth = 10.
    chWorkSheet:Columns("H"):ColumnWidth = 10.
    chWorkSheet:Columns("I"):ColumnWidth = 10.
    chWorkSheet:Columns("J"):ColumnWidth = 10.
    chWorkSheet:Columns("K"):ColumnWidth = 10.
    chWorkSheet:Columns("L"):ColumnWidth = 10.
    chWorkSheet:Columns("M"):ColumnWidth = 10.
    chWorkSheet:Columns("N"):ColumnWidth = 10.
    chWorkSheet:Columns("O"):ColumnWidth = 10.
    chWorkSheet:Columns("P"):ColumnWidth = 10.
    
    ASSIGN i-linha = 5.


    FOR EACH tt-ge-mov BY 
             tt-ge-mov.referencia BY tt-ge-mov.sc-codigo  :
       
        /*MESSAGE "vai gravar a conta = " tt-ge-mov.referencia VIEW-AS ALERT-BOX.
          */

        /*
        assign c-range = "A" + string(i-linha-narrativ) + ":C" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):merge.
                        assign c-range = "A" + string(i-linha-narrativ).
                        chExcelApplication:Range(c-range):WrapText = true.
                        chExcelApplication:Range(c-range):value = trim(tt-editor.conteudo).
        */
       
        ASSIGN chExcelApplication:Range( "A" + STRING(i-linha) ):VALUE = tt-ge-mov.referencia
               /*
               chExcelApplication:Range( "A" + STRING(i-linha)):WrapText = true       
               chExcelApplication:Range( "A" + STRING(i-linha)):value = string(tt-ge-mov.referencia)  */
               chExcelApplication:Range( "B" + STRING(i-linha) ):VALUE          = tt-ge-mov.sc-codigo 
               chExcelApplication:Range( "B" + STRING(i-linha) ):NumberFormat   = "00000000" 
               chExcelApplication:Range( "C" + STRING(i-linha) ):VALUE          = tt-ge-mov.cod-emitente  
               chExcelApplication:Range( "D" + STRING(i-linha) ):VALUE          = tt-ge-mov.nome-emit /*tt-ge-mov.vl-01  */
               chExcelApplication:Range( "E" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-01
               chExcelApplication:Range( "F" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-02 
               chExcelApplication:Range( "G" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-03
               chExcelApplication:Range( "H" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-04
               chExcelApplication:Range( "I" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-05
               chExcelApplication:Range( "J" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-06
               chExcelApplication:Range( "K" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-07 
               chExcelApplication:Range( "L" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-08
               chExcelApplication:Range( "M" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-09
               chExcelApplication:Range( "N" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-10
               chExcelApplication:Range( "O" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-11
               chExcelApplication:Range( "P" + STRING(i-linha) ):VALUE          = tt-ge-mov.vl-12.
                       
        ASSIGN i-linha = i-linha + 1.
  
    END.
      
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

