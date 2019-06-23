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
    FIELD tt-cdn_empresa        LIKE sit_afast_func.cdn_empresa        
    FIELD tt-cdn_estab          LIKE sit_afast_func.cdn_estab          
    FIELD tt-cdn_funcionario    LIKE sit_afast_func.cdn_funcionario
    FIELD tt-nome               LIKE funcionario.nom_pessoa_fisic
    FIELD tt-dat_inic_sit_afast LIKE sit_afast_func.dat_inic_sit_afast 
    FIELD tt-dat_term_sit_afast LIKE sit_afast_func.dat_term_sit_afast 
    FIELD tt-cdn_sit_afast_func LIKE sit_afast_func.cdn_sit_afast_func 
    FIELD tt-des_sit_afast_func LIKE sit_afast.des_sit_afast_func
    FIELD tt-dat_admis_func     LIKE funcionario.dat_admis_func.     




/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha AS INT.

DEF VAR nr-apolice AS INT FORMAT "9999999".
DEF VAR nr-fatura  AS INT FORMAT "999".
DEF VAR c-sexo     AS CHAR.
DEF VAR c-salario  LIKE funcionario.val_salario_atual.

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

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS CHARACTER FORMAT "x(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-fir":U
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
     fi-empresa-ini AT ROW 1.25 COL 13 COLON-ALIGNED
     fi-empresa-fim AT ROW 1.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-est-ini AT ROW 2.25 COL 13 COLON-ALIGNED
     fi-est-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-dt-ini AT ROW 3.25 COL 13 COLON-ALIGNED
     IMAGE-15 AT ROW 2.25 COL 41
     IMAGE-17 AT ROW 2.25 COL 34
     IMAGE-21 AT ROW 1.25 COL 41
     IMAGE-22 AT ROW 1.25 COL 34
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
         TITLE              = "<Seguro de Vida - ESHCM0015.001 >"
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
ON END-ERROR OF C-Win /* <Seguro de Vida - ESHCM0015.001 > */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Seguro de Vida - ESHCM0015.001 > */
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

ASSIGN fi-dt-ini = TODAY.
       
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

APPLY 'entry' TO INPUT fi-empresa-ini.

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
  DISPLAY fi-empresa-ini fi-empresa-fim fi-est-ini fi-est-fim fi-dt-ini 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa-ini fi-empresa-fim fi-est-ini fi-est-fim fi-dt-ini IMAGE-15 
         IMAGE-17 IMAGE-21 IMAGE-22 
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
  
ASSIGN fi-empresa-ini fi-empresa-fim
       fi-est-ini     fi-est-fim
       fi-dt-ini.     

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
    chWorkSheet:Range("a1"):Value = "Seguro de Vida " /*+ string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Matricula".
    chWorkSheet:Range("b3"):Value = "Nome".
    chWorkSheet:Range("c3"):Value = "Dt Nascimento".
    chWorkSheet:Range("d3"):Value = "Sal rio".
    chWorkSheet:Range("e3"):Value = "Ap¢lice".
    chWorkSheet:Range("f3"):Value = "Cod Fatura". 
    chWorkSheet:Range("g3"):Value = "Sexo". 
    chWorkSheet:Range("h3"):Value = "Dt AdmissÆo".   
    chWorkSheet:Range("i3"):Value = "Mes Vigencia". 
    chWorkSheet:Range("j3"):Value = "CPF". 
      
                                                                    
    chWorkSheet:Range("a3:j3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 08.
    chWorkSheet:Columns("b"):ColumnWidth = 08.
    chWorkSheet:Columns("C"):ColumnWidth = 12.
    chWorkSheet:Columns("d"):ColumnWidth = 20.
    chWorkSheet:Columns("e"):ColumnWidth = 12.
    chWorkSheet:Columns("f"):ColumnWidth = 12.
    chWorkSheet:Columns("g"):ColumnWidth = 12.
    chWorkSheet:Columns("h"):ColumnWidth = 12.
    chWorkSheet:Columns("i"):ColumnWidth = 12.
    chWorkSheet:Columns("j"):ColumnWidth = 12.
 
    ASSIGN i-linha = 5.     

    
   FOR EACH  funcionario 
      WHERE funcionario.cdn_empresa       >= fi-empresa-ini
        AND funcionario.cdn_empresa       <= fi-empresa-fim
        AND funcionario.cdn_estab         >= fi-est-ini
        AND funcionario.cdn_estab         <= fi-est-fim
     /* AND funcionario.dat_admis_func    >= fi-dt-ini
        AND funcionario.dat_admis_func    <= fi-dt-fim */
        AND funcionario.dat_desligto_func  = ? NO-LOCK:
   
      FIND rh_pessoa_fisic 
         WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.

      IF  rh_pessoa_fisic.idi_sexo = 1 THEN
          ASSIGN c-sexo = "M".
      ELSE
          ASSIGN c-sexo = "F".
      
      ASSIGN nr-apolice = 0850745.

       IF funcionario.cdn_empresa  = "1"   THEN ASSIGN nr-fatura  = 001.
       IF funcionario.cdn_empresa  = "201" THEN ASSIGN nr-fatura  = 008.
       IF funcionario.cdn_empresa  = "211" THEN ASSIGN nr-fatura  = 007.
       IF funcionario.cdn_empresa  = "301" THEN ASSIGN nr-fatura  = 006.
       IF funcionario.cdn_empresa  = "800" THEN ASSIGN nr-fatura  = 011.

       ASSIGN c-salario = funcionario.val_salario_atual * 100.

       
        ASSIGN /* chExcelApplication:range( "A" + STRING(i-linha) ):value = funcionario.cdn_empresa
               chExcelApplication:range( "B" + STRING(i-linha) ):value = funcionario.cdn_estab*/
               chExcelApplication:range( "a" + STRING(i-linha) ):value = funcionario.cdn_funcionario
               chExcelApplication:range( "b" + STRING(i-linha) ):value = funcionario.nom_pessoa_fisic
               chExcelApplication:range( "c" + STRING(i-linha) ):value = funcionario.dat_nascimento
               chExcelApplication:range( "d" + STRING(i-linha) ):value = c-salario /* funcionario.val_salario_atual*/
               chExcelApplication:Range( "d" + STRING(i-linha)):Numberformat = "00000000000000"
               chExcelApplication:range( "e" + STRING(i-linha) ):value = nr-apolice
               chExcelApplication:Range( "e" + STRING(i-linha)):Numberformat = "0000000"
               chExcelApplication:range( "f" + STRING(i-linha) ):value = nr-fatura
               chExcelApplication:Range( "f" + STRING(i-linha)):Numberformat = "000"
               chExcelApplication:range( "g" + STRING(i-linha) ):value = c-sexo
               chExcelApplication:range( "h" + STRING(i-linha) ):value = funcionario.dat_admis_func
               chExcelApplication:range( "i" + STRING(i-linha) ):value = month(fi-dt-ini)
               chExcelApplication:range( "j" + STRING(i-linha) ):value = rh_pessoa_fisic.cod_id_feder
               chExcelApplication:Range( "j" + STRING(i-linha)):Numberformat = "00000000000". 
               
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

