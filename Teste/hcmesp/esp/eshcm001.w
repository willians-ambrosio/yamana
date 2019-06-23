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

DEF TEMP-TABLE tt-imprime
    FIELD tt-cdn-empresa       LIKE  funcionario.cdn_empresa                             
    FIELD tt-cdn-estab         LIKE  funcionario.cdn_estab
    FIELD tt-cdn-funcionario   LIKE  funcionario.cdn_funcionario                           
    FIELD tt-cod-rh-ccusto     LIKE  funcionario.cod_rh_ccusto
    FIELD tt-des-centro-custo  LIKE  rh_ccusto.des_rh_ccusto
    FIELD tt-des-cargo-basic   LIKE  cargo_basic.des_cargo_basic
    FIELD tt-qtde-func           AS  INT
    FIELD tt-sal-bas-ini       LIKE  funcionario.val_salario_atual
    FIELD tt-desc-aumento      LIKE  cargo_basic.des_cargo_basic
    FIELD tt-perc-aumen          AS  DEC FORMAT ">>9.99"
    FIELD tt-sal-bas-fim       LIKE  funcionario.val_salario_atual
    FIELD tt-dat-liber-sal     LIKE  histor_sal_func.dat_liber_sal
    FIELD tt-vl-aumento        LIKE  funcionario.val_salario_atual
    FIELD tt-dat-desligto      LIKE  funcionario.dat_desligto_func
    FIELD tt-nome-func         LIKE  funcionario.nom_abrev_pessoa_fisic
    FIELD tt-dat-admis-func    LIKE  funcionario.dat_admis_func.
   
/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha       AS    INT.
DEF VAR i-qtde        AS    INT.
DEF VAR i-sal-bas-ini LIKE  funcionario.val_salario_atual.

DEF VAR tot-sal-bas-ini LIKE tt-sal-bas-ini .
DEF VAR tot-vl-aumento LIKE tt-vl-aumento . 
DEF VAR tot-perc-aumen LIKE tt-perc-aumen . 
DEF VAR tot-sal-bas-fim LIKE tt-sal-bas-fim.

DEF VAR ger-tot-sal-bas-ini LIKE tt-sal-bas-ini .
DEF VAR ger-tot-vl-aumento  LIKE tt-vl-aumento . 
DEF VAR ger-tot-perc-aumen  LIKE tt-perc-aumen . 
DEF VAR ger-tot-sal-bas-fim LIKE tt-sal-bas-fim.

DEF VAR tot-ger-folha LIKE funcionario.val_salario_atual.

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
     LABEL "Imprimir" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 10.5.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 1.75.

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(08)":U INITIAL "zzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-texto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .88 NO-UNDO.

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

DEFINE VARIABLE rs-func AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ativos", 1,
"Desligados", 2,
"Todos", 3
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 1.75.


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
     fi-empresa AT ROW 1.25 COL 13 COLON-ALIGNED
     fi-est-ini AT ROW 2.25 COL 13 COLON-ALIGNED
     fi-est-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-cc-ini AT ROW 3.25 COL 13 COLON-ALIGNED
     fi-cc-fim AT ROW 3.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-dt-ini AT ROW 4.25 COL 13 COLON-ALIGNED
     fi-dt-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     rs-func AT ROW 6.5 COL 18 NO-LABEL
     fi-texto AT ROW 9 COL 13 COLON-ALIGNED NO-LABEL
     IMAGE-15 AT ROW 2.25 COL 41
     IMAGE-16 AT ROW 3.25 COL 41
     IMAGE-17 AT ROW 2.25 COL 34
     IMAGE-18 AT ROW 3.25 COL 34
     IMAGE-19 AT ROW 4.25 COL 34
     IMAGE-20 AT ROW 4.25 COL 41
     RECT-22 AT ROW 6 COL 3
     "Sit. Funcion rio:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 6.5 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 1.58
         SIZE 69 BY 9.17
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
         TITLE              = "<Hist¢rico Evolu‡Æo Salarial - ESHCM001.001 >"
         HEIGHT             = 12.29
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
ON END-ERROR OF C-Win /* <Hist¢rico Evolu‡Æo Salarial - ESHCM001.001 > */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Hist¢rico Evolu‡Æo Salarial - ESHCM001.001 > */
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
ASSIGN fi-dt-corte = TODAY.
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

  assign fi-dt-ini:SCREEN-VALUE = STRING(TODAY)
         fi-dt-fim:SCREEN-VALUE = STRING(TODAY).

  APPLY 'entry' TO INPUT fi-empresa.

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
  DISPLAY fi-empresa fi-est-ini fi-est-fim fi-cc-ini fi-cc-fim fi-dt-ini 
          fi-dt-fim rs-func fi-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa fi-est-ini fi-est-fim fi-cc-ini fi-cc-fim fi-dt-ini 
         fi-dt-fim rs-func fi-texto IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 
         IMAGE-19 IMAGE-20 RECT-22 
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

ASSIGN fi-empresa 
       fi-est-ini
       fi-est-fim
       fi-cc-ini
       fi-cc-fim 
       fi-dt-ini 
       fi-dt-fim
       rs-func.      

FOR EACH tt-imprime :
    DELETE tt-imprime.
END.

ASSIGN tot-ger-folha = 0.

 FOR EACH funcionario 
    WHERE funcionario.cdn_empresa            =  fi-empresa
      AND funcionario.cdn_estab             >=  fi-est-ini
      AND funcionario.cdn_estab             <=  fi-est-fim
      and STRING(funcionario.cod_rh_ccusto) >= (fi-cc-ini) 
      and STRING(funcionario.cod_rh_ccusto) <= (fi-cc-fim) 
      
     NO-LOCK
    BREAK BY funcionario.cod_rh_ccusto 
          BY funcionario.cdn_cargo_basic:

     IF funcionario.dat_desligto_func = ? THEN
        ASSIGN tot-ger-folha = tot-ger-folha + val_salario_atual.

    
      FIND cargo_basic WHERE 
           cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic NO-LOCK NO-ERROR.
                         
     ASSIGN i-qtde        = i-qtde + 1
            i-sal-bas-ini = i-sal-bas-ini + funcionario.val_salario_atual.                                


     CREATE tt-imprime.
     ASSIGN tt-cdn-empresa       =  funcionario.cdn_empresa                             
            tt-cdn-estab         =  funcionario.cdn_estab
            tt-cdn-funcionario   =  funcionario.cdn_funcionario
            tt-cod-rh-ccusto     =  funcionario.cod_rh_ccusto
            tt-des-cargo-basic   =  cargo_basic.des_cargo_basic
            tt-qtde-func         =  i-qtde
            tt-sal-bas-ini       =  funcionario.val_salario_atual
            tt-dat-desligto      =  funcionario.dat_desligto_func
            tt-sal-bas-fim       =  funcionario.val_salario_atual
            tt-nome-func         =  funcionario.nom_pessoa_fisic
            tt-dat-liber-sal     =  ?
            tt-dat-admis-func    =  funcionario.dat_admis_func.  

     FIND rh_ccusto WHERE 
          rh_ccusto.cdn_empresa   = funcionario.cdn_empresa   AND
          rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.

     IF AVAIL rh_ccusto THEN
        ASSIGN tt-des-centro-custo  =  rh_ccusto.des_rh_ccusto . 
     
    FOR EACH  histor_sal_func 
       WHERE histor_sal_func.dat_liber_sal  >= fi-dt-ini
         AND histor_sal_func.dat_liber_sal  <= fi-dt-fim
         AND histor_sal_func.cdn_empresa     = funcionario.cdn_empresa 
         AND histor_sal_func.cdn_estab       = funcionario.cdn_estab
         AND histor_sal_func.cdn_funcionario = funcionario.cdn_funcionario NO-LOCK:
                                                                
    
        FIND motiv_liber_sal
          WHERE motiv_liber_sal.cdn_motiv_liber_sal = histor_sal_func.cdn_motiv_liber_sal NO-LOCK NO-ERROR.
           
        IF motiv_liber_sal.des_motiv_liber_sal BEGINS "acordo" THEN 
          NEXT.

        ASSIGN tt-desc-aumento  = motiv_liber_sal.des_motiv_liber_sal   
               tt-sal-bas-fim   = histor_sal_func.val_salario_mensal
               tt-dat-liber-sal = histor_sal_func.dat_liber_sal. 

        ASSIGN fi-texto:SCREEN-VALUE = "Gerando Dados......." + 
                                        STRING(funcionario.cod_rh_ccusto).

    END.
       
 END.

 FOR EACH tt-imprime 
    WHERE tt-imprime.tt-desc-aumento <> "" 
      AND  tt-dat-liber-sal <> ? :

    FIND LAST histor_sal_func 
         WHERE histor_sal_func.cdn_empresa     =   tt-cdn-empresa     
           AND histor_sal_func.cdn_estab       =   tt-cdn-estab       
           AND histor_sal_func.cdn_funcionario =   tt-cdn-funcionario 
           AND histor_sal_func.dat_liber_sal   <   tt-dat-liber-sal NO-LOCK NO-ERROR.

    IF AVAIL histor_sal_func THEN
       ASSIGN tt-sal-bas-ini =  histor_sal_func.val_salario_mensal.

    IF tt-sal-bas-fim > tt-sal-bas-ini  THEN
       ASSIGN tt-vl-aumento = tt-sal-bas-fim - tt-sal-bas-ini.

    IF tt-vl-aumento > 0 THEN
       ASSIGN tt-perc-aumen = tt-vl-aumento * 100 / tt-sal-bas-ini.

        
 END.


 ASSIGN fi-texto:SCREEN-VALUE = "Gerando Excel...".

 RUN pi-imprime.

 ASSIGN fi-texto:SCREEN-VALUE = "Fim de Processamento !!!".
      
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


FOR EACH tt-imprime:
    IF  (tt-dat-desligto <> ? AND  
        tt-dat-desligto < fi-dt-ini or
        tt-dat-desligto > fi-dt-fim) OR 
        (rs-func = 1 AND             
         tt-dat-desligto <> ? AND        
         tt-dat-desligto < fi-dt-ini) OR   
        (rs-func = 2 AND  
        tt-dat-desligto =  ?) THEN 
        DELETE tt-imprime.
  
END.



/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

    /*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/
    chWorkSheet:Range("a1"):Value =   "Empresa: " + string(fi-empresa) + 
                "                                                    " +  
                                     "  Hist¢rico Evolu‡Æo Salarial  " + 
                "                                                    " +
                                      " Periodo: " + string(fi-dt-ini) +  
                                                            "    a   " + string(fi-dt-fim) .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Centro de Custo    ".   
    chWorkSheet:Range("b3"):Value = "Cargo              ".       
    chWorkSheet:Range("c3"):Value = "Funcion rio        ".   
    chWorkSheet:Range("d3"):Value = "Sal B sico Inicial ".   
    chWorkSheet:Range("e3"):Value = "Movimenta‡Æo       ".       
    chWorkSheet:Range("f3"):Value = "Vl do Aumento      ".    
    chWorkSheet:Range("g3"):Value = "% do Aumento ".  
    chWorkSheet:Range("h3"):Value = "Sal B sico Final   ". 
    chWorkSheet:Range("i3"):Value = "Dt Altera‡Æo ". 
    
    chWorkSheet:Range("a3:i3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 15.
    chWorkSheet:Columns("b"):ColumnWidth = 35.
    chWorkSheet:Columns("C"):ColumnWidth = 35.
    chWorkSheet:Columns("d"):ColumnWidth = 15.
    chWorkSheet:Columns("e"):ColumnWidth = 15.
    chWorkSheet:Columns("f"):ColumnWidth = 15.
    chWorkSheet:Columns("g"):ColumnWidth = 15.
    chWorkSheet:Columns("h"):ColumnWidth = 15.
    chWorkSheet:Columns("i"):ColumnWidth = 15.
   
    ASSIGN i-linha = 5.



FOR EACH tt-imprime 
    WHERE tt-imprime.tt-desc-aumento <> "" 
      AND (tt-dat-admis-func < fi-dt-ini OR 
           tt-dat-admis-func > fi-dt-fim)
    BREAK BY tt-imprime.tt-cod-rh-ccusto :

    ASSIGN tot-sal-bas-ini = tot-sal-bas-ini + tt-sal-bas-ini 
           tot-vl-aumento  = tot-vl-aumento  + tt-vl-aumento 
           tot-perc-aumen  = tot-perc-aumen  + tt-perc-aumen
           tot-sal-bas-fim = tot-sal-bas-fim + tt-sal-bas-fim.
   
    
    ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value =  tt-imprime.tt-cod-rh-ccusto
           chExcelApplication:range( "B" + STRING(i-linha) ):value =  tt-imprime.tt-des-cargo-basic
           chExcelApplication:range( "c" + STRING(i-linha) ):value =  tt-nome-func /* tt-imprime.tt-cdn-funcionario */
           /*chExcelApplication:Range( "C" + STRING(i-linha)):NumberFormat="###.###.##0,00" .*/
           chExcelApplication:range( "d" + STRING(i-linha) ):value =  tt-imprime.tt-sal-bas-ini
           chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
           chExcelApplication:range( "e" + STRING(i-linha) ):value =  tt-imprime.tt-desc-aumento
           chExcelApplication:range( "f" + STRING(i-linha) ):value =  tt-imprime.tt-vl-aumento
           chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
           chExcelApplication:range( "g" + STRING(i-linha) ):value =  tt-imprime.tt-perc-aumen
           chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00"
           chExcelApplication:range( "h" + STRING(i-linha) ):value =  tt-imprime.tt-sal-bas-fim
           chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
           chExcelApplication:range( "i" + STRING(i-linha) ):value =  tt-dat-liber-sal /*tt-imprime.tt-dat-desligto*/.
          
    ASSIGN i-linha = i-linha + 1.

    IF LAST-OF(tt-imprime.tt-cod-rh-ccusto)  THEN DO:
       ASSIGN chExcelApplication:range( "c" + STRING(i-linha) ):value =  "Total -->"
              chExcelApplication:range( "d" + STRING(i-linha) ):value =  tot-sal-bas-ini
              chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "f" + STRING(i-linha) ):value =  tot-vl-aumento
              chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "g" + STRING(i-linha) ):value =  tot-perc-aumen
              chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00"
              chExcelApplication:range( "h" + STRING(i-linha) ):value =  tot-sal-bas-fim
              chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0,00" .

       ASSIGN i-linha = i-linha + 2.

       ASSIGN ger-tot-sal-bas-ini = tot-sal-bas-ini    + ger-tot-sal-bas-ini 
              ger-tot-vl-aumento  = tot-vl-aumento     + ger-tot-vl-aumento
              ger-tot-perc-aumen  = ger-tot-perc-aumen +  ger-tot-perc-aumen
              ger-tot-sal-bas-fim = ger-tot-sal-bas-fim  + tot-sal-bas-fim 
              tot-sal-bas-ini     = 0
              tot-vl-aumento      = 0
              tot-perc-aumen      = 0
              tot-sal-bas-fim     = 0.
          
    END.

END.

 ASSIGN i-linha = i-linha + 2.
       ASSIGN chExcelApplication:range( "c" + STRING(i-linha) ):value =  "Total Geral -->"
              chExcelApplication:range( "d" + STRING(i-linha) ):value =  ger-tot-sal-bas-ini
              chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "f" + STRING(i-linha) ):value =  ger-tot-vl-aumento
              chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "g" + STRING(i-linha) ):value =  (ger-tot-vl-aumento / ger-tot-sal-bas-ini) * 100
              chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00"
              chExcelApplication:range( "h" + STRING(i-linha) ):value =  ger-tot-sal-bas-fim
              chExcelApplication:Range( "h" + STRING(i-linha)):NumberFormat="###.###.##0,00" .


     

 ASSIGN i-linha = i-linha + 2.
       ASSIGN chExcelApplication:range( "c" + STRING(i-linha) ):value =  "Total Geral da Folha  -->"
              chExcelApplication:range( "d" + STRING(i-linha) ):value =  tot-ger-folha
              chExcelApplication:Range( "d" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "f" + STRING(i-linha) ):value =  ger-tot-vl-aumento
              chExcelApplication:Range( "f" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
              chExcelApplication:range( "g" + STRING(i-linha) ):value =  (ger-tot-vl-aumento / tot-ger-folha) * 100
              chExcelApplication:Range( "g" + STRING(i-linha)):NumberFormat="##0,00".

        ASSIGN ger-tot-sal-bas-ini = 0
              ger-tot-vl-aumento   = 0
              ger-tot-sal-bas-fim  = 0.




/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

