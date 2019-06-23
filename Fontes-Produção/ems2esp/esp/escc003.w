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

DEF TEMP-TABLE  tt-movto
    FIELD  tt-cod-estabel       LIKE ordem-compra.cod-estabel 
    FIELD  tt-fm-codigo         AS   CHAR FORMAT "X(8)" /*LIKE item.fm-codigo */   
    FIELD  tt-nr-ord-produc     LIKE req-ord-produc.nr-ord-produ    
    FIELD  tt-it-codigo         AS   CHAR FORMAT "x(16)" /* LIKE item.it-codigo */                 
    FIELD  tt-qt-requisitada    LIKE it-requisicao.qt-requisitada   
    FIELD  tt-nr-requisicao     LIKE ordem-compra.nr-requisicao     
    FIELD  tt-numero-ordem      LIKE ordem-compra.numero-ordem      
    FIELD  tt-num-pedido        LIKE ordem-compra.num-pedido        
    FIELD  tt-preco-fornec      LIKE ordem-compra.preco-fornec      
    FIELD  tt-data-entrega      LIKE prazo-compra.data-entrega      
    FIELD  tt-situacao          LIKE prazo-compra.situacao             
    FIELD  tt-ct-codigo         LIKE mmv-ord-manut.ct-codigo        
    FIELD  tt-cc-codigo         LIKE mmv-ord-manut.cc-codigo        
    FIELD  tt-num-ord-inv       LIKE ordem-compra.num-ord-inv       
    FIELD  tt-desc-item         LIKE item.desc-item
    FIELD  tt-cod-emit          LIKE ordem-compra.cod-emitente
    FIELD  tt-quant-receb       LIKE prazo-compra.quant-receb
    FIELD  tt-preco-unit        LIKE ordem-compra.preco-unit
    FIELD  tt-aliquota-ipi      LIKE ordem-compra.aliquota-ipi      
    FIELD  tt-aliquota-icm      LIKE ordem-compra.aliquota-icm 
    FIELD  tt-aliquota-iss      LIKE ordem-compra.aliquota-iss 
    FIELD  tt-valor-frete       LIKE ordem-compra.valor-frete
    FIELD  tt-preco-orig        LIKE ordem-compra.preco-orig
    FIELD  tt-data-emis         LIKE ordem-compra.data-emis
    FIELD  tt-data-pedido       LIKE ordem-compra.data-pedido
    FIELD  tt-qtidade-atu       LIKE saldo-estoq.qtidade-atu
    FIELD  tt-dt-emissao        LIKE doc-fisico.dt-emissao
    FIELD  tt-dt-trans          LIKE doc-fisico.dt-trans
    FIELD  tt-perc-descto       LIKE ordem-compra.perc-descto
    FIELD  tt-prazo-entreg      LIKE cotacao-item.prazo-entreg
    FIELD  tt-cod-comprado      LIKE ordem-compra.cod-comprado
    FIELD  tt-dat-trans         LIKE req-ord-produc.dat-trans
    FIELD  tt-nome-emit         LIKE emitente.nome-emit
    FIELD  tt-pais              LIKE emitente.pais
    FIELD  tt-estado            LIKE emitente.estado
    FIELD  tt-cidade            LIKE emitente.cidade.


DEF VAR c-situacao AS CHAR FORMAT "x(15)".


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
     SIZE 72 BY 13.25.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.75.

DEFINE VARIABLE dt-ped-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ped-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Ordem" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estabel":R16 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fam-fim AS CHARACTER FORMAT "X(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fam-ini AS CHARACTER FORMAT "X(8)" 
     LABEL "Fam¡lia":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R16 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fl-texto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
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
     fi-est-ini AT ROW 2.5 COL 11 COLON-ALIGNED
     fi-est-fim AT ROW 2.5 COL 44 COLON-ALIGNED HELP
          "Equipamento" NO-LABEL
     fi-fam-ini AT ROW 3.75 COL 11 COLON-ALIGNED
     fi-fam-fim AT ROW 3.75 COL 44 COLON-ALIGNED HELP
          "Equipamento" NO-LABEL
     fi-item-ini AT ROW 5 COL 11 COLON-ALIGNED
     fi-item-fim AT ROW 5 COL 44 COLON-ALIGNED HELP
          "Equipamento" NO-LABEL
     dt-ped-ini AT ROW 6.25 COL 11 COLON-ALIGNED
     dt-ped-fim AT ROW 6.25 COL 44 COLON-ALIGNED NO-LABEL
     fl-texto AT ROW 9.75 COL 12 COLON-ALIGNED NO-LABEL
     IMAGE-11 AT ROW 6.25 COL 41
     IMAGE-12 AT ROW 6.25 COL 33
     IMAGE-21 AT ROW 5 COL 33
     IMAGE-22 AT ROW 5 COL 41
     IMAGE-25 AT ROW 3.75 COL 33
     IMAGE-26 AT ROW 3.75 COL 41
     IMAGE-27 AT ROW 2.5 COL 33
     IMAGE-28 AT ROW 2.5 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1
         SIZE 70 BY 12.75
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
         TITLE              = "<Relat¢rio Regional de Compras -  ESCC003>"
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
ON END-ERROR OF C-Win /* <Relat¢rio Regional de Compras -  ESCC003> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Relat¢rio Regional de Compras -  ESCC003> */
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

/* APPLY 'entry' TO INPUT cd-equipto-ini.*/

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
  DISPLAY fi-est-ini fi-est-fim fi-fam-ini fi-fam-fim fi-item-ini fi-item-fim 
          dt-ped-ini dt-ped-fim fl-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-11 IMAGE-12 IMAGE-21 IMAGE-22 IMAGE-25 IMAGE-26 IMAGE-27 
         IMAGE-28 fi-est-ini fi-est-fim fi-fam-ini fi-fam-fim fi-item-ini 
         fi-item-fim dt-ped-ini dt-ped-fim fl-texto 
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

 ASSIGN fi-est-ini 
        fi-est-fim
        dt-ped-ini
        dt-ped-fim
        fi-fam-ini
        fi-fam-fim
        fi-item-ini
        fi-item-fim.

 FOR EACH tt-movto:
     DELETE  tt-movto.
 END.

FOR EACH ITEM NO-LOCK 
    WHERE item.fm-codigo >= fi-fam-ini                      AND 
          item.fm-codigo <= fi-fam-fim                      AND 
          item.it-codigo >= fi-item-ini                     AND 
          item.it-codigo <= fi-item-fim: 
    FOR EACH ordem-compra NO-LOCK
         WHERE ordem-compra.dat-ordem    >= dt-ped-ini          and 
               ordem-compra.dat-ordem    <= dt-ped-fim          AND /*  
               ordem-compra.num-ord-inv  >= xxx                 and 
               ordem-compra.num-ord-inv  <= xxxxxx              and 
               ordem-compra.numero-ordem >= nr-ord-ini          and 
               ordem-compra.numero-ordem <= nr-ord-fim          AND */
               ordem-compra.cod-estabel  >= fi-est-ini            and 
               ordem-compra.cod-estabel  <= fi-est-fim            AND 
               ordem-compra.it-codigo     = item.it-codigo :
        IF ordem-compra.situacao <> 2 AND ordem-compra.situacao <> 6  THEN
           NEXT.

      
    FOR each emitente no-lock
         where emitente.cod-emitente = ordem-compra.cod-emitente,
    /*each it-requisicao no-lock
         where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao and
               it-requisicao.sequencia     = req-ord-produc.sequencia and
               it-requisicao.it-codigo    >= fi-item-ini and 
               it-requisicao.it-codigo    <= fi-item-fim, */
    each prazo-compra no-lock
         where prazo-compra.numero-ordem  = ordem-compra.numero-ordem  /*and
               prazo-compra.data-entrega >= dt-ent-ini and 
               prazo-compra.data-entrega <= dt-ent-fim */
    break by emitente.nome-abrev
          /*by req-ord-produc.nr-requisicao
          by it-requisicao.nr-requisicao*/
          by ordem-compra.num-pedido
          /*by it-requisicao.it-codigo*/
          by item.it-codigo:


   CREATE tt-movto.
   ASSIGN tt-cod-estabel       = ordem-compra.cod-estabel
          tt-fm-codigo         = item.fm-codigo
          /*tt-nr-ord-produ      = req-ord-produc.nr-ord-produ */
          tt-it-codigo         = item.it-codigo              
          /*tt-qt-requisitada    = it-requisicao.qt-requisitada*/
          tt-nr-requisicao     = ordem-compra.nr-requisicao  
          tt-numero-ordem      = ordem-compra.numero-ordem   
          tt-num-pedido        = ordem-compra.num-pedido     
          tt-preco-fornec      = ordem-compra.preco-fornec   
          tt-data-entrega      = prazo-compra.data-entrega   
          tt-situacao          = prazo-compra.situacao
          tt-num-ord-inv       = ordem-compra.num-ord-inv    
          tt-desc-item         = item.desc-item
          tt-cod-emit          = ordem-compra.cod-emitente
          tt-quant-receb       = prazo-compra.quant-receb
          tt-preco-unit        = ordem-compra.preco-fornec 
          tt-preco-orig        = ordem-compra.pre-unit-for 
          tt-aliquota-ipi      = ordem-compra.aliquota-ipi 
          tt-aliquota-icm      = ordem-compra.aliquota-icm 
          tt-aliquota-iss      = ordem-compra.aliquota-iss
          tt-valor-frete       = ordem-compra.valor-frete
          tt-data-emis         = ordem-compra.data-emis
          tt-data-pedido       = ordem-compra.dat-ordem /*ordem-compra.data-pedido*/
          tt-dt-emissao        = ?
          tt-dt-trans          = ?
          tt-perc-descto       = ordem-compra.perc-descto
          tt-cod-comprado      = ordem-compra.cod-comprado
          tt-nome-emit         = emitente.nome-emit
          tt-pais              = emitente.pais     
          tt-estado            = emitente.estado   
          tt-cidade            = emitente.cidade.  


     assign fl-texto:screen-value  = "Nr Pedido Compra......" + STRING(ordem-compra.num-pedido).


    END.
    

    END.


END.

assign fl-texto:screen-value  = "Gerando Excel...".

RUN pi-imprime.

assign fl-texto:screen-value  = "".
 
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
    chWorkSheet:Range("a1"):Value = "Relat¢rio Regional de Compras  -  "    + string(TODAY) .
                    /*Value  " + string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Ordem ". 
    chWorkSheet:Range("b3"):Value = "Fam¡lia ".
    chWorkSheet:Range("c3"):Value = "Item ".
    chWorkSheet:Range("d3"):Value = "Dt Ordem  ".
    chWorkSheet:Range("e3"):Value = "Pedido ". 
    chWorkSheet:Range("f3"):Value = "Valor ".
    chWorkSheet:Range("g3"):Value = "Cod Fornec ". 
    chWorkSheet:Range("h3"):Value = "Nome da Empresa ".     
    chWorkSheet:Range("i3"):Value = "Pais  ".  
    chWorkSheet:Range("j3"):Value = "UF ".
    chWorkSheet:Range("k3"):Value = "Cidade ".     
  

    chWorkSheet:Range("a3:k3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 12.
    chWorkSheet:Columns("b"):ColumnWidth = 15.
    chWorkSheet:Columns("C"):ColumnWidth = 15.
    chWorkSheet:Columns("d"):ColumnWidth = 15.
    chWorkSheet:Columns("e"):ColumnWidth = 15.
    chWorkSheet:Columns("f"):ColumnWidth = 15.
    chWorkSheet:Columns("g"):ColumnWidth = 15.
    chWorkSheet:Columns("h"):ColumnWidth = 35.
    chWorkSheet:Columns("i"):ColumnWidth = 15.
    chWorkSheet:Columns("j"):ColumnWidth = 10.
    chWorkSheet:Columns("k"):ColumnWidth = 15.
  

    ASSIGN i-linha = 5.
       
     FOR EACH tt-movto   :

        ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-numero-ordem /*tt-fm-codigo*/   
               chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-fm-codigo /*tt-it-codigo */ 
               chExcelApplication:Range( "b" + STRING(i-linha)):NumberFormat="0000" 
               chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-it-codigo /*tt-desc-item */  
               chExcelApplication:Range( "c" + STRING(i-linha)):NumberFormat="00000000" 
               chExcelApplication:range( "d" + STRING(i-linha) ):value = tt-data-pedido
               chExcelApplication:range( "e" + STRING(i-linha) ):value = tt-num-pedido /*tt-preco-unit*/
               chExcelApplication:range( "f" + STRING(i-linha) ):value = tt-preco-fornec /* tt-perc-descto*/
               chExcelApplication:range( "g" + STRING(i-linha) ):value = tt-cod-emit
               chExcelApplication:range( "h" + STRING(i-linha) ):value = tt-nome-emit              
               chExcelApplication:range( "i" + STRING(i-linha) ):value = tt-pais                
               chExcelApplication:range( "j" + STRING(i-linha) ):value = tt-estado               
               chExcelApplication:range( "k" + STRING(i-linha) ):value = tt-cidade.
     
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

