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

DEF VAR c-sexo     AS CHAR.
DEF VAR c-estado-civil AS CHAR FORMAT "x(20)".
DEF VAR c-salario  LIKE funcionario.val_salario_atual.
DEF VAR i-idade    AS  DEC.
DEF VAR c-situacao AS CHAR FORMAT "x(10)".
DEF VAR c-categ    as char.

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

DEFINE VARIABLE fi-centro-fim AS CHARACTER FORMAT "x(8)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-centro-ini AS CHARACTER FORMAT "x(8)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fim AS DATE FORMAT "99/99/9999":U initial 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U initial 01/01/1900 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS CHARACTER FORMAT "x(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS CHARACTER FORMAT "x(3)":U INITIAL "" 
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

DEFINE VARIABLE fi-lotacao-fim AS CHARACTER FORMAT "x(11)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lotacao-ini AS CHARACTER FORMAT "x(11)":U 
     LABEL "Lota‡Æo" 
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

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
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

     fi-dt-ini AT ROW 4.25 COL 13 COLON-ALIGNED
     fi-dt-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-centro-ini AT ROW 5.25 COL 13 COLON-ALIGNED
     fi-centro-fim AT ROW 5.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-lotacao-ini AT ROW 6.25 COL 13 COLON-ALIGNED
     fi-lotacao-fim AT ROW 6.25 COL 45 COLON-ALIGNED NO-LABEL
     IMAGE-15 AT ROW 2.25 COL 41
     IMAGE-17 AT ROW 2.25 COL 34
     IMAGE-21 AT ROW 1.25 COL 41
     IMAGE-22 AT ROW 1.25 COL 34
     IMAGE-23 AT ROW 3.25 COL 41
     IMAGE-24 AT ROW 3.25 COL 34
     IMAGE-25 AT ROW 5.25 COL 41
     IMAGE-26 AT ROW 5.25 COL 34
     IMAGE-27 AT ROW 6.25 COL 41
     IMAGE-28 AT ROW 6.25 COL 34
     IMAGE-29 AT ROW 4.25 COL 41
     IMAGE-30 AT ROW 4.25 COL 34
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
         TITLE              = "<Funcionarios Admitidos_uso restrito - ESHCM901>"
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
ON END-ERROR OF C-Win /* <Funcionarios Admitidos_uso restrito - ESHCM901> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Funcionarios Admitidos_uso restrito - ESHCM901> */
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

ASSIGN fi-dt-ini = TODAY
       fi-dt-fim = TODAY.

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
          fi-dt-fim fi-centro-ini fi-centro-fim 
          fi-lotacao-ini fi-lotacao-fim
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa-ini fi-empresa-fim fi-est-ini fi-est-fim fi-dt-ini 
         fi-dt-fim fi-centro-ini fi-centro-fim fi-lotacao-ini fi-lotacao-fim
         IMAGE-15 IMAGE-17 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 
         IMAGE-25 IMAGE-26 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30
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
       fi-dt-ini      fi-dt-fim
       fi-centro-ini  fi-centro-fim
       fi-lotacao-ini fi-lotacao-fim.

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

DEF VAR cdn-sit        LIKE habilit_rescis.cdn_sit_rescis_contratual.    
DEF VAR sit-afast      LIKE sit_afast.des_sit_afast_func.  
DEF VAR c-estado-civil AS CHAR FORMAT "x(20)".
DEF VAR c-tipo-sangue  AS CHAR FORMAT "x(20)".
DEF VAR c-grau_instruc AS CHAR FORMAT "x(20)".

/************************************************************/
/*          Abre planilha Excel                             */
CREATE "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = true.
/************************************************************/

    /*************** Imprime altera‡äes **************************/
    /*************** Cabe»alho ***********************************/
    chWorkSheet:Range("a1"):Value = "Funcion rios Admitidos_uso restrito" /*+ string(fi-mes-ini,"99") + " / " + string(fi-ano-ini)*/ .
    chWorkSheet:Range("a1"):Font:Size = 12.
    chWorkSheet:Range("a1"):Font:Bold = TRUE.
    chWorkSheet:Range("a3"):Value = "Empresa".
    chWorkSheet:Range("b3"):Value = "Estab".
    chWorkSheet:Range("c3"):Value = "Matricula".
    chWorkSheet:Range("d3"):Value = "Cod.Pessoa F¡sica".
    chWorkSheet:Range("e3"):Value = "Nome do Funcionario".
    chWorkSheet:Range("f3"):Value = "Estado Civil".
    chWorkSheet:Range("g3"):Value = "Sexo".
    chWorkSheet:Range("h3"):Value = "Num.Depend.IRF".
    chWorkSheet:Range("i3"):Value = "Depend.SalFamilia".
    chWorkSheet:Range("j3"):Value = "CPF".
    chWorkSheet:Range("k3"):Value = "RG".
    chWorkSheet:Range("l3"):Value = "OrgÆo Emissor".
    chWorkSheet:Range("m3"):Value = "PIS".
    chWorkSheet:Range("n3"):Value = "No. CPTS".
    chWorkSheet:Range("o3"):Value = "Serie CPTS".
    chWorkSheet:Range("p3"):Value = "UF CPTS".
    chWorkSheet:Range("q3"):Value = "Dt Nascimento".
    chWorkSheet:Range("r3"):Value = "Cidade Residˆncia".
    chWorkSheet:Range("s3"):Value = "Estado".
    chWorkSheet:Range("t3"):Value = "Dt AdmissÆo".
    chWorkSheet:Range("u3"):Value = "Grau Instru‡Æo".
    chWorkSheet:Range("v3"):Value = "Sal rio".
    chWorkSheet:Range("w3"):Value = "Categoria".
    chWorkSheet:Range("x3"):Value = "Cod.M.Obra".
    chWorkSheet:Range("y3"):Value = "Desc_Tipo M.Obra".
    chWorkSheet:Range("z3"):Value = "Cod Cargo".
    chWorkSheet:Range("aa3"):Value = "N¡vel Cargo".
    chWorkSheet:Range("ab3"):Value = "Cargo".
    chWorkSheet:Range("ac3"):Value = "CBO".
    chWorkSheet:Range("ad3"):Value = "Turno".
    chWorkSheet:Range("ae3"):Value = "Turma".
    chWorkSheet:Range("af3"):Value = "Unid Lota‡Æo". 
    chWorkSheet:Range("ag3"):Value = "Descri‡Æo Lota‡Æo". 
    chWorkSheet:Range("ah3"):Value = "Cod CCusto". 
    chWorkSheet:Range("ai3"):Value = "Descri‡Æo CCusto".

    chWorkSheet:Range("a3:ai3"):Font:Bold = TRUE.
    chWorkSheet:Columns("a"):ColumnWidth = 08.
    chWorkSheet:Columns("b"):ColumnWidth = 08.
    chWorkSheet:Columns("C"):ColumnWidth = 08.
    chWorkSheet:Columns("d"):ColumnWidth = 08.
    chWorkSheet:Columns("e"):ColumnWidth = 35.
    chWorkSheet:Columns("f"):ColumnWidth = 14.
    chWorkSheet:Columns("g"):ColumnWidth = 06.
    chWorkSheet:Columns("h"):ColumnWidth = 16.
    chWorkSheet:Columns("i"):ColumnWidth = 16.
    chWorkSheet:Columns("j"):ColumnWidth = 12.
    chWorkSheet:Columns("k"):ColumnWidth = 20.
    chWorkSheet:Columns("l"):ColumnWidth = 15.
    chWorkSheet:Columns("m"):ColumnWidth = 12.
    chWorkSheet:Columns("n"):ColumnWidth = 10.
    chWorkSheet:Columns("o"):ColumnWidth = 08.
    chWorkSheet:Columns("p"):ColumnWidth = 08.
    chWorkSheet:Columns("q"):ColumnWidth = 15.
    chWorkSheet:Columns("r"):ColumnWidth = 15.
    chWorkSheet:Columns("s"):ColumnWidth = 06.
    chWorkSheet:Columns("t"):ColumnWidth = 12.
    chWorkSheet:Columns("u"):ColumnWidth = 20.
    chWorkSheet:Columns("v"):ColumnWidth = 12.
    chWorkSheet:Columns("w"):ColumnWidth = 06.
    chWorkSheet:Columns("x"):ColumnWidth = 06.
    chWorkSheet:Columns("y"):ColumnWidth = 15.
    chWorkSheet:Columns("z"):ColumnWidth = 08.
    chWorkSheet:Columns("aa"):ColumnWidth = 04.
    chWorkSheet:Columns("ab"):ColumnWidth = 25.
    chWorkSheet:Columns("ac"):ColumnWidth = 06.
    chWorkSheet:Columns("ad"):ColumnWidth = 06.
    chWorkSheet:Columns("ae"):ColumnWidth = 06.
    chWorkSheet:Columns("af"):ColumnWidth = 10.
    chWorkSheet:Columns("ag"):ColumnWidth = 30.
    chWorkSheet:Columns("ah"):ColumnWidth = 10.
    chWorkSheet:Columns("ai"):ColumnWidth = 30.

    ASSIGN i-linha = 5.
      
   FOR EACH  funcionario 
      WHERE funcionario.cdn_empresa       >= fi-empresa-ini
        AND funcionario.cdn_empresa       <= fi-empresa-fim
        AND funcionario.cdn_estab         >= fi-est-ini
        AND funcionario.cdn_estab         <= fi-est-fim
        AND funcionario.dat_admis_func    >= fi-dt-ini
        AND funcionario.dat_admis_func    <= fi-dt-fim
        AND funcionario.cod_rh_ccusto     >= fi-centro-ini 
        AND funcionario.cod_rh_ccusto     <= fi-centro-fim
        AND funcionario.cod_unid_lotac    >= fi-lotacao-ini
        AND funcionario.cod_unid_lotac    <= fi-lotacao-fim 
        AND funcionario.dat_desligto_func  = ? NO-LOCK:
   
        FIND rh_pessoa_fisic 
           WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.

        FIND unid_lotac 
           WHERE unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac NO-LOCK NO-ERROR.
   
        FIND cargo_basic WHERE 
             cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic NO-LOCK NO-ERROR.

        FIND FIRST cargo NO-LOCK
             WHERE cargo.cdn_cargo_basic =   cargo_basic.cdn_cargo_basic NO-ERROR.
        
        FIND FIRST tip_mdo NO-LOCK
             WHERE tip_mdo.cod_tip_mdo   =   funcionario.cod_tip_mdo NO-ERROR.

        FIND rh_ccusto WHERE 
             rh_ccusto.cdn_empresa   = funcionario.cdn_empresa   AND
             rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.
        
        /* estado civil */
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 1 THEN                                                      
            ASSIGN c-estado-civil =  "CASADO".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_estado_civil) = 2 THEN                                                 
            ASSIGN c-estado-civil = "SOLTEIRO".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 3 THEN                                                     
            ASSIGN c-estado-civil = "DESQUITADO".              
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 4 THEN                                                     
            ASSIGN c-estado-civil = "DIVORCIADO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 5 THEN                                                     
            ASSIGN c-estado-civil = "VIUVO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 6 THEN                                                     
            ASSIGN c-estado-civil = "SEPARADO JUD.".  
         ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 7 THEN                                                     
            ASSIGN c-estado-civil = "OUTROS".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 8 THEN                                                     
            ASSIGN c-estado-civil = "UNIÇO ESTAVEL".   

        /* tipo sanguinio */
        IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 1 THEN                                                      
            ASSIGN c-tipo-sangue = "O".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 2 THEN                                                 
            ASSIGN c-tipo-sangue = "A".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 3 THEN                                                     
            ASSIGN c-tipo-sangue = "B".              
        ELSE    
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 4 THEN                                                     
            ASSIGN c-tipo-sangue = "AB".              
        ELSE   
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 5 THEN                                                     
            ASSIGN c-tipo-sangue = "NAO INFORMADO".  

        FIND grau_instruc WHERE 
             grau_instruc.cdn_grau_instruc = rh_pessoa_fisic.cdn_grau_instruc NO-LOCK NO-ERROR.
 
        IF AVAIL grau_instruc THEN
            ASSIGN c-grau_instruc =  grau_instruc.des_grau_instruc.
        ELSE
            ASSIGN c-grau_instruc = "".

        /* sexo */      
        IF  rh_pessoa_fisic.idi_sexo = 1 THEN
            ASSIGN c-sexo = "M".
        ELSE
            ASSIGN c-sexo = "F".

        /* categoria salarial */
        IF  INT(funcionario.cdn_categ_sal) = 1 THEN                                                      
            ASSIGN c-categ = "Mensal".             
        ELSE                                                                                      
             IF  INT(funcionario.cdn_categ_sal) = 2 THEN                                                 
            ASSIGN c-categ = "Horista".  

        ASSIGN chExcelApplication:range( "a" + STRING(i-linha) ):value = funcionario.cdn_empresa
               chExcelApplication:range( "b" + STRING(i-linha) ):value = funcionario.cdn_estab 
               chExcelApplication:range( "c" + STRING(i-linha) ):value = funcionario.cdn_funcionario 
               chExcelApplication:range( "d" + STRING(i-linha) ):value = rh_pessoa_fisic.num_pessoa_fisic  
               chExcelApplication:range( "e" + STRING(i-linha) ):value = funcionario.nom_pessoa_fisic /* funcionario.val_salario_atual*/
               chExcelApplication:range( "f" + STRING(i-linha) ):value = c-estado-civil
               chExcelApplication:range( "g" + STRING(i-linha) ):value = c-sexo
               /* chExcelApplication:Range( "g" + STRING(i-linha)):Numberformat = "0000000" */
               chExcelApplication:range( "h" + STRING(i-linha) ):value = funcionario.qti_depend_irf
               chExcelApplication:range( "i" + STRING(i-linha) ):value = funcionario.qti_depend_salfam
               chExcelApplication:range( "j" + STRING(i-linha) ):value = rh_pessoa_fisic.cod_id_feder
               chExcelApplication:Range( "j" + STRING(i-linha)):Numberformat = "00000000000"
               chExcelApplication:range( "k" + STRING(i-linha) ):value = rh_pessoa_fisic.cod_id_estad_fisic
               chExcelApplication:range( "l" + STRING(i-linha) ):value = rh_pessoa_fisic.cod_orgao_emis_id_estad
               chExcelApplication:range( "m" + STRING(i-linha) ):value = funcionario.cod_pis
               chExcelApplication:Range( "m" + STRING(i-linha)):Numberformat = "00000000000"
               chExcelApplication:range( "n" + STRING(i-linha) ):value = funcionario.cod_cart_trab
               chExcelApplication:range( "o" + STRING(i-linha) ):value = funcionario.cod_ser_cart_trab
               chExcelApplication:range( "p" + STRING(i-linha) ):value = funcionario.cod_unid_federac_cart_trab


               chExcelApplication:range( "q" + STRING(i-linha) ):value = funcionario.dat_nascimento
               chExcelApplication:range( "r" + STRING(i-linha) ):value = rh_pessoa_fisic.nom_cidad_rh
               chExcelApplication:range( "s" + STRING(i-linha) ):value = rh_pessoa_fisic.cod_unid_federac_rh
               chExcelApplication:range( "t" + STRING(i-linha) ):value = funcionario.dat_admis_func 
               chExcelApplication:range( "u" + STRING(i-linha) ):value = c-grau_instruc
               chExcelApplication:range( "v" + STRING(i-linha) ):value = funcionario.val_salario_atual
               chExcelApplication:Range( "v" + STRING(i-linha)):NumberFormat="###.###.##0,00" 
               chExcelApplication:range( "w" + STRING(i-linha) ):value = c-categ
               chExcelApplication:range( "x" + STRING(i-linha) ):value = funcionario.cod_tip_mdo
               chExcelApplication:range( "y" + STRING(i-linha) ):value = tip_mdo.des_tip_mdo
               chExcelApplication:range( "z" + STRING(i-linha) ):value = funcionario.cdn_cargo_basic
               chExcelApplication:range( "aa" + STRING(i-linha) ):value = funcionario.cdn_niv_cargo
               chExcelApplication:range( "ab" + STRING(i-linha) ):value = cargo_basic.des_cargo_basic /*funcionario.cdn_cargo_basic*/
               chExcelApplication:range( "ac" + STRING(i-linha) ):value = cargo_basic.cod_classif_ocupac
               chExcelApplication:range( "ad" + STRING(i-linha) ):value = funcionario.cdn_turno_trab
               chExcelApplication:Range( "ad" + STRING(i-linha)):Numberformat = "0000"
               chExcelApplication:range( "ae" + STRING(i-linha) ):value = funcionario.cdn_turma_trab   
               chExcelApplication:range( "af" + STRING(i-linha) ):value = funcionario.cod_unid_lotac 
               chExcelApplication:range( "ag" + STRING(i-linha) ):value = unid_lotac.des_unid_lotac 
               chExcelApplication:range( "ah" + STRING(i-linha) ):value = funcionario.cod_rh_ccusto 
               chExcelApplication:range( "ai" + STRING(i-linha) ):value = rh_ccusto.des_rh_ccusto.
               
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

