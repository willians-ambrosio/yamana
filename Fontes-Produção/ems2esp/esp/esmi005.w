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
/**
DEF TEMP-TABLE  tt-movto
    FIELD  cd-equipto    LIKE ord-manut.cd-equipto
    FIELD  nr-ord-prod   LIKE movto-estoq.nr-ord-prod
    FIELD  dt-trans      LIKE movto-estoq.dt-trans
    FIELD  nro-docto     LIKE movto-estoq.nro-docto
    FIELD  numero-ordem  LIKE movto-estoq.numero-ordem /* ordem compra */
    FIELD  num-pedido    LIKE ordem-compra.num-pedido
    FIELD  esp-docto     AS   CHAR
    FIELD  it-codigo     LIKE movto-estoq.it-codigo
    FIELD  un            LIKE item.un
    FIELD  cod-emit      LIKE movto-estoq.cod-emitente
    FIELD  nome-emit     LIKE emitente.nome-emit
    FIELD  nr-requisicao LIKE ordem-compra.nr-requisicao
    FIELD  dt-entrega    LIKE prazo-compra.data-entrega
    FIELD  preco-unit    LIKE ordem-compra.preco-unit
    FIELD  preco-total   LIKE ordem-compra.preco-orig
    FIELD  situacao      LIKE prazo-compra.situacao.
**/

DEF TEMP-TABLE  tt-movto
    FIELD  tt-cod-eqpto         LIKE ord-manut.cd-equipto        
    FIELD  tt-nr-ord-produc     LIKE req-ord-produc.nr-ord-produ    
    FIELD  tt-it-codigo         LIKE item.it-codigo                 
    FIELD  tt-qt-requisitada    LIKE it-requisicao.qt-requisitada   
    FIELD  tt-nr-requisicao     LIKE ordem-compra.nr-requisicao     
    FIELD  tt-numero-ordem      LIKE ordem-compra.numero-ordem      
    FIELD  tt-num-pedido        LIKE ordem-compra.num-pedido        
    FIELD  tt-preco-fornec      LIKE ordem-compra.preco-fornec      
    FIELD  tt-data-entrega      LIKE prazo-compra.data-entrega      
    FIELD  tt-situacao          LIKE prazo-compra.situacao             
    FIELD  tt-ct-codigo         LIKE ord-manut.ct-codigo  
    FIELD  tt-cc-codigo         LIKE ord-manut.sc-codigo  
    FIELD  tt-num-ord-inv       LIKE ordem-compra.num-ord-inv       
    FIELD  tt-desc-item         LIKE item.desc-item
    FIELD  tt-cod-emit          LIKE ordem-compra.cod-emitente
    FIELD  tt-quant-receb       LIKE prazo-compra.quant-receb
    FIELD  tt-val-item          LIKE it-requisicao.val-item
    FIELD  tt-sequencia         LIKE it-requisicao.sequencia
    FIELD  tt-debito            AS   CHAR.         


DEF VAR c-situacao AS CHAR FORMAT "x(15)".
DEF VAR tot-vl     LIKE it-requisicao.val-item.
DEF VAR tot-qtde   LIKE it-requisicao.qt-requisitada.

/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha       AS   INT.
DEF VAR c-cd-equipto  LIKE ord-manut.cd-equipto.
DEF VAR de-qtde-abe   LIKE prazo-compra.quant-saldo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnOK BtnCancel RECT-20 RECT-21 

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
     LABEL "Executar" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 13.25.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 1.75.

DEFINE VARIABLE cd-equipto-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE cd-equipto-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Equipamento":R16 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ent-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ent-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(8)" 
     LABEL "C.Custo":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-fim AS CHARACTER FORMAT "X(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-ini AS CHARACTER FORMAT "X(8)" 
     LABEL "Conta":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R16 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ord-fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ord-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Ord.Manut.":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-req-fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-req-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Requisi‡Æo":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fl-texto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnOK AT ROW 14.5 COL 22
     BtnCancel AT ROW 14.5 COL 40
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 14.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.43 BY 15.13
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME f-pg-sel
     cd-equipto-ini AT ROW 1.5 COL 11 COLON-ALIGNED
     cd-equipto-fim AT ROW 1.5 COL 44 COLON-ALIGNED HELP
          "Equipamento" NO-LABEL
     dt-ent-ini AT ROW 2.5 COL 11 COLON-ALIGNED
     dt-ent-fim AT ROW 2.5 COL 44 COLON-ALIGNED NO-LABEL
     fi-ct-ini AT ROW 3.5 COL 11 COLON-ALIGNED HELP
          "Eqiuipamento"
     fi-ct-fim AT ROW 3.5 COL 44 COLON-ALIGNED NO-LABEL
     fi-cc-ini AT ROW 4.5 COL 11 COLON-ALIGNED HELP
          "Eqiuipamento"
     fi-cc-fim AT ROW 4.5 COL 44 COLON-ALIGNED NO-LABEL
     fi-req-ini AT ROW 5.5 COL 11 COLON-ALIGNED
     fi-req-fim AT ROW 5.5 COL 44 COLON-ALIGNED NO-LABEL
     fi-item-ini AT ROW 6.5 COL 11 COLON-ALIGNED
     fi-item-fim AT ROW 6.5 COL 44 COLON-ALIGNED HELP
          "Equipamento" NO-LABEL
     fi-ord-ini AT ROW 7.5 COL 11 COLON-ALIGNED
     fi-ord-fim AT ROW 7.5 COL 44 COLON-ALIGNED NO-LABEL
     fl-texto AT ROW 9.75 COL 12 COLON-ALIGNED NO-LABEL
     IMAGE-15 AT ROW 3.5 COL 41
     IMAGE-16 AT ROW 3.5 COL 33
     IMAGE-17 AT ROW 4.5 COL 41
     IMAGE-18 AT ROW 4.5 COL 33
     IMAGE-19 AT ROW 5.5 COL 41
     IMAGE-20 AT ROW 5.5 COL 33
     IMAGE-21 AT ROW 6.5 COL 33
     IMAGE-22 AT ROW 6.5 COL 41
     IMAGE-27 AT ROW 2.5 COL 41
     IMAGE-28 AT ROW 2.5 COL 33
     IMAGE-29 AT ROW 7.5 COL 33
     IMAGE-30 AT ROW 7.5 COL 41
     IMAGE-5 AT ROW 1.5 COL 33
     IMAGE-6 AT ROW 1.5 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.5
         SIZE 69 BY 12
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
         TITLE              = "<Relat¢rio de Custos- Manuten‡Æo Industrial - ESMI005>"
         HEIGHT             = 15.13
         WIDTH              = 72.29
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
                                                                        */
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
ON END-ERROR OF C-Win /* <Relat¢rio de Custos- Manuten‡Æo Industrial - ESMI005> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Relat¢rio de Custos- Manuten‡Æo Industrial - ESMI005> */
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
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Executar */
DO:
 
  RUN pi-executa.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*  
ASSIGN dt-ord-ini = TODAY
       dt-ord-fim = TODAY
       dt-ent-ini = TODAY
       dt-ent-fim = TODAY.
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

APPLY 'entry' TO INPUT cd-equipto-ini.

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
  ENABLE BtnOK BtnCancel RECT-20 RECT-21 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY cd-equipto-ini cd-equipto-fim dt-ent-ini dt-ent-fim fi-ct-ini 
          fi-ct-fim fi-cc-ini fi-cc-fim fi-req-ini fi-req-fim fi-item-ini 
          fi-item-fim fi-ord-ini fi-ord-fim fl-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE cd-equipto-ini cd-equipto-fim dt-ent-ini dt-ent-fim fi-ct-ini 
         fi-ct-fim fi-cc-ini fi-cc-fim fi-req-ini fi-req-fim fi-item-ini 
         fi-item-fim fi-ord-ini fi-ord-fim fl-texto IMAGE-15 IMAGE-16 IMAGE-17 
         IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-27 IMAGE-28 
         IMAGE-29 IMAGE-30 IMAGE-5 IMAGE-6 
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

 ASSIGN cd-equipto-ini
        cd-equipto-fim
        dt-ent-ini
        dt-ent-fim
        fi-ct-ini
        fi-ct-fim
        fi-cc-ini
        fi-cc-fim
        fi-req-ini
        fi-req-fim
        fi-item-ini
        fi-item-fim
        fi-ord-ini
        fi-ord-fim.

 FOR EACH tt-movto:
     DELETE  tt-movto.
 END.

              
FOR each req-ord-produc no-lock
         where req-ord-produc.nr-requisicao >= fi-req-ini and 
               req-ord-produc.nr-requisicao <= fi-req-fim AND 
               req-ord-produc.nr-ord-produ  >= fi-ord-ini AND 
               req-ord-produc.nr-ord-produ  <= fi-ord-fim,
    each ord-manut no-lock
         where ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ and
               ord-manut.sc-codigo   >= fi-cc-ini and 
               ord-manut.sc-codigo   <= fi-cc-fim and
               ord-manut.cd-equipto  >= cd-equipto-ini and 
               ord-manut.cd-equipto  <= cd-equipto-fim and
               ord-manut.ct-codigo   >= fi-ct-ini and 
               ord-manut.ct-codigo   <= fi-ct-fim ,
    each it-requisicao no-lock
         where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao and
               it-requisicao.sequencia     = req-ord-produc.sequencia and
               it-requisicao.it-codigo    >= fi-item-ini and 
               it-requisicao.it-codigo    <= fi-item-fim AND 
               it-requisicao.dt-atend     >= dt-ent-ini  and 
               it-requisicao.dt-atend     <= dt-ent-fim 
    break by ord-manut.cd-equipto
          by it-requisicao.nr-requisicao
          by it-requisicao.sequencia:
  
   FIND ITEM WHERE ITEM.it-codigo = it-requisicao.it-codigo NO-LOCK NO-ERROR.

   
   CREATE tt-movto.
   ASSIGN tt-cod-eqpto         = ord-manut.cd-equipto     
          tt-nr-ord-produ      = req-ord-produc.nr-ord-produ 
          tt-it-codigo         = item.it-codigo              
          tt-qt-requisitada    = it-requisicao.qt-requisitada
          tt-nr-requisicao     = req-ord-produc.nr-requisicao  
          /*tt-numero-ordem      = ord-manut.numero-ordem   
            tt-num-pedido        = ordem-compra.num-pedido     
          tt-preco-fornec      = ordem-compra.preco-fornec - ordem-compra.valor-descto */ 
          tt-data-entrega      = it-requisicao.dt-atend  
          /*tt-situacao          = prazo-compra.situacao*/
          tt-ct-codigo         = ord-manut.ct-desp  /*ord-manut.ct-codigo     */
          tt-cc-codigo         = ord-manut.sc-desp  /*ord-manut.sc-codigo     */
          /*tt-num-ord-inv       = ordem-compra.num-ord-inv    */
          tt-desc-item         = item.desc-item
          /*tt-cod-emit          = ordem-compra.cod-emitente*/
          tt-quant-receb       = it-requisicao.qt-requisitada 
          tt-val-item          = it-requisicao.val-item
          tt-sequencia         = it-requisicao.sequencia.

   ASSIGN tt-debito = "Sim".

   FIND FIRST movto-estoq
      WHERE movto-estoq.nr-ord-prod = req-ord-produc.nr-ord-prod 
        AND movto-estoq.it-codigo   = it-requisicao.it-codigo NO-LOCK NO-ERROR.

   IF AVAIL movto-estoq THEN DO:
      IF movto-estoq.esp-docto = 5 OR
         movto-estoq.esp-docto = 7 THEN 
         ASSIGN tt-debito = "NÆo"
                 tt-val-item =  tt-val-item * -1.
      ELSE 
         ASSIGN tt-debito = "Sim".
   END.  
  

   ASSIGN fl-texto:SCREEN-VALUE = "Pesquisando Equipamento ....... "
                                  + STRING(ord-manut.cd-equipto   ).

  
 END.  

   
   ASSIGN fl-texto:SCREEN-VALUE = "Gerando Excel ....... ".

   RUN pi-imprime.

   ASSIGN fl-texto:SCREEN-VALUE = "Fim de Processamento !".

   
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
    chWorkSheet:Range("a1"):Value = "Relat¢rio de Custos / Manuten‡Æo Industrial  -  "    + string(TODAY) .
                    /*Value  " + string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Equipamento". 
    chWorkSheet:Range("b3"):Value = "Ord Manut ".
    chWorkSheet:Range("c3"):Value = "Requisi‡Æo".
    chWorkSheet:Range("d3"):Value = "Seq ".     
    chWorkSheet:Range("e3"):Value = "Item      ".  
    chWorkSheet:Range("f3"):Value = "Descri‡Æo  ". 
    chWorkSheet:Range("g3"):Value = "Qt Requis.".     
    chWorkSheet:Range("h3"):Value = "Valor ".  
    chWorkSheet:Range("i3"):Value = "Conta Cont bil". 
    chWorkSheet:Range("j3"):Value = "Centro Custo ".     
    chWorkSheet:Range("k3"):Value = "Dt Entrega ".  
    chWorkSheet:Range("l3"):Value = "Debito".     
   
   
    chWorkSheet:Range("a3:l3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 12.
    chWorkSheet:Columns("b"):ColumnWidth = 12.
    chWorkSheet:Columns("C"):ColumnWidth = 12.
    chWorkSheet:Columns("d"):ColumnWidth = 5.
    chWorkSheet:Columns("e"):ColumnWidth = 10.
    chWorkSheet:Columns("f"):ColumnWidth = 30.
    chWorkSheet:Columns("g"):ColumnWidth = 10.
    chWorkSheet:Columns("h"):ColumnWidth = 10.
    chWorkSheet:Columns("i"):ColumnWidth = 15.
    chWorkSheet:Columns("j"):ColumnWidth = 15.
    chWorkSheet:Columns("k"):ColumnWidth = 12.
    chWorkSheet:Columns("l"):ColumnWidth = 10.
    
                   
    ASSIGN i-linha = 5.
       
     FOR EACH tt-movto 
         BREAK BY  tt-cod-eqpto:
   
        ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-cod-eqpto   
               chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-nr-ord-produ     
               chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-nr-requisicao  
               chExcelApplication:range( "d" + STRING(i-linha) ):value = tt-sequencia
               chExcelApplication:range( "e" + STRING(i-linha) ):value = tt-it-codigo
               chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-desc-item
               chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-qt-requisitada 
               chExcelApplication:range( "h" + STRING(i-linha) ):value = tt-val-item 
               chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-ct-codigo  
               chExcelApplication:range( "j" + STRING(i-linha) ):value = tt-cc-codigo  
               chExcelApplication:range( "k" + STRING(i-linha) ):value = tt-data-entrega 
               chExcelApplication:range( "l" + STRING(i-linha) ):value = tt-debito.

        ASSIGN tot-vl   = tot-vl + tt-val-item
               tot-qtde = tot-qtde + tt-qt-requisitada. 

       
        ASSIGN i-linha = i-linha + 1.

        IF LAST-OF(tt-cod-eqpto)  THEN DO:

           ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "Total do Equipamento -->"
                  chExcelApplication:range( "h" + STRING(i-linha) ):value = tot-vl.
           
           ASSIGN i-linha  = i-linha + 2
                  tot-qtde = 0
                  tot-vl   = 0.

       END.





       
    END.
      
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

