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

DEF TEMP-TABLE tt-detalhado
    FIELD tt-cod-rel        LIKE dsc-param-cta-gerencial.cod-rel
    FIELD tt-nr-seq         LIKE dsc-param-cta-gerencial.nr-seq
    FIELD tt-cta-ger        LIKE dsc-param-cta-gerencial.cta-ger
    FIELD tt-des-conta-ger  LIKE dsc-param-cta-gerencial.des-conta-ger
    FIELD tt-tip-taxa-dolar LIKE dsc-param-cta-gerencial.tip-taxa-dolar
    FIELD tt-ld-borda       LIKE dsc-param-cta-gerencial.ld-borda
    FIELD tt-tipo-linha     LIKE dsc-param-cta-gerencial.tipo-linha
    FIELD tt-saldo-cbl      LIKE sdo_cta_ctbl.val_sdo_ctbl_fim 
    FIELD tt-saldo-dol      AS DEC FORMAT ">>>>,>>9.99999"
    FIELD tt-taxa-dolar     AS DEC FORMAT ">9.99999".
                   
DEF TEMP-TABLE tt-resumido
    FIELD tt-cod-rel        LIKE dsc-param-cta-gerencial.cod-rel
    FIELD tt-nr-seq         LIKE dsc-param-cta-gerencial.nr-seq
    FIELD tt-cta-ger        LIKE dsc-param-cta-gerencial.cta-ger
    FIELD tt-des-conta-ger  LIKE dsc-param-cta-gerencial.des-conta-ger
    FIELD tt-cta-ctbl       LIKE cta_ctbl.cod_cta_ctbl
    FIELD tt-des-cta-ctbl   LIKE dsc-param-cta-gerencial.des-conta-ger
    FIELD tt-saldo-inicial  LIKE sdo_cta_ctbl.val_sdo_ctbl_fim 
    FIELD tt-debitos        LIKE sdo_cta_ctbl.val_sdo_ctbl_fim 
    FIELD tt-creditos       LIKE sdo_cta_ctbl.val_sdo_ctbl_fim 
    FIELD tt-saldo-fim      LIKE sdo_cta_ctbl.val_sdo_ctbl_fim .
                   
DEF VAR dt-tx-dol  AS DATE.
DEF VAR i-dia      AS INT FORMAT "99".
DEF VAR i-mes      AS INT FORMAT "99".
DEF VAR i-ano      AS INT FORMAT "9999".
DEF VAR i-mes-aux  AS INT FORMAT "99".
DEF VAR i-ano-aux  AS INT FORMAT "9999".
                   
DEF VAR tot-cta-ctbl LIKE sdo_cta_ctbl.val_sdo_ctbl_fim.
DEF VAR tot-tt-deb   LIKE sdo_cta_ctbl.val_sdo_ctbl_fim.
DEF VAR tot-tt-cred  LIKE sdo_cta_ctbl.val_sdo_ctbl_fim.
DEF VAR tot-sdo-ini  LIKE sdo_cta_ctbl.val_sdo_ctbl_fim.
DEF VAR tot-sdo-fim  LIKE sdo_cta_ctbl.val_sdo_ctbl_fim.

/* Variaveis para Gerar em Excel */
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkbook as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

DEF VAR i-linha AS INT.

DEF VAR tx-dol-med   AS   DEC FORMAT ">9,9999".
DEF VAR tot-cta-3290 LIKE tt-saldo-cbl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnOK BtnCancel IMAGE-1 IMAGE-2 RECT-20 ~
RECT-21 

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

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 12.5.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 2.

DEFINE BUTTON bt-verifica 
     LABEL "VERIFICA PLANO DE CONTAS GERENCIAL  X  DATASUL" 
     SIZE 48 BY 1.13.

DEFINE VARIABLE cod-rel AS INTEGER FORMAT ">>>9":U INITIAL 1 
     LABEL "Codigo do Relat¢rio" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descr AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-taxa AS DATE FORMAT "99/99/9999":U 
     LABEL "Taxa do D¢lar em" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-emis AS CHARACTER FORMAT "99/9999":U 
     LABEL "Per°odo de emiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-est-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tx AS DECIMAL FORMAT ">9.99999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tp-calc AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mes Corrente", 1,
"Acumulado no ano do periodo", 2
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE rs-tp-emis AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detalhado", 1,
"Resumido", 2,
"Ambos", 3
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 1.5.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 1.5.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnOK AT ROW 14 COL 24
     BtnCancel AT ROW 14 COL 45
     IMAGE-1 AT ROW 3.5 COL 34.86
     IMAGE-2 AT ROW 3.5 COL 45
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 13.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.14 BY 14.63
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME f-pg-sel
     bt-verifica AT ROW 1.5 COL 13
     cod-rel AT ROW 4.25 COL 17 COLON-ALIGNED
     fi-descr AT ROW 4.25 COL 24 COLON-ALIGNED NO-LABEL
     fi-est-ini AT ROW 5.5 COL 17 COLON-ALIGNED
     fi-est-fim AT ROW 5.5 COL 35 COLON-ALIGNED NO-LABEL
     fi-emis AT ROW 6.75 COL 17 COLON-ALIGNED
     fi-dt-taxa AT ROW 6.75 COL 44 COLON-ALIGNED
     fi-tx AT ROW 6.75 COL 57 COLON-ALIGNED NO-LABEL
     rs-tp-calc AT ROW 9 COL 15 NO-LABEL
     rs-tp-emis AT ROW 11.5 COL 15 NO-LABEL
     IMAGE-13 AT ROW 5.5 COL 26
     IMAGE-14 AT ROW 5.5 COL 33
     RECT-18 AT ROW 8.75 COL 12
     RECT-19 AT ROW 11.25 COL 12
     RECT-23 AT ROW 1 COL 4
     "Tipo de C†lculo" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 8.5 COL 27
     "Tipo Emiss∆o" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 11 COL 25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 1.25
         SIZE 71 BY 12
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
         TITLE              = "<Relat¢rio Gerencial dos Dados Cont†beis>"
         HEIGHT             = 14.63
         WIDTH              = 76.14
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
/* SETTINGS FOR FILL-IN fi-descr IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-taxa IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tx IN FRAME f-pg-sel
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* <Relat¢rio Gerencial dos Dados Cont†beis> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Relat¢rio Gerencial dos Dados Cont†beis> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-verifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-verifica C-Win
ON CHOOSE OF bt-verifica IN FRAME f-pg-sel /* VERIFICA PLANO DE CONTAS GERENCIAL  X  DATASUL */
DO:
 
/************* Criacao do Aplicativo EXCEL **********/
CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = "esp/esct005-conf.xls " . 
chexcelapplication:workbooks:open(file-info:full-pathname,TRUE).
chexcelapplication:sheets:item(1).

ASSIGN i-linha = 3.

 FOR EACH cta_ctbl no-lock
     WHERE cta_ctbl.ind_espec_cta_ctbl = "anal°tica":  
     
     IF  cta_ctbl.cod_cta_ctbl BEGINS "9" THEN
         NEXT.
   
     FIND FIRST dsc-param-cta-datasul
          WHERE dsc-param-cta-datasul.cta-ctbl =  cta_ctbl.cod_cta_ctbl NO-LOCK NO-ERROR.

     
     IF NOT AVAIL dsc-param-cta-datasul THEN DO:

          ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = cta_ctbl.cod_cta_ctbl .
          ASSIGN chExcelApplication:range( "B" + STRING(i-linha) ):value = cta_ctbl.des_tit_ctbl .

          ASSIGN i-linha = i-linha + 1.                       

     END.

 END.
     
 
chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication NO-ERROR.
release object chWorksheet no-error.
release object chWorkbook no-error.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
   
  IF fi-dt-taxa = ? THEN DO:
   
  /* pesquisa taxa dolar do mes anterior */
  IF int(SUBSTR(fi-emis,1,2)) < 12 THEN DO:
     ASSIGN i-mes = int(substr(fi-emis,1,2))  + 1 
            i-dia = 01
            i-ano = int(SUBSTR(fi-emis,3,4)).
  END.
  ELSE DO:
   IF int(SUBSTR(fi-emis,1,2)) = 12 THEN DO:
      ASSIGN i-mes = 01
             i-ano = int(SUBSTR(fi-emis,3,4)) + 1 
             i-dia = 01.
   END.
  END.

  /* pesquisa taxa dolar */
  FIND FIRST cotac_parid 
      WHERE cod_indic_econ_base = "real" 
        AND cod_indic_econ_idx  = "dolar"
        AND  month(dat_cotac_indic_econ) = i-mes
        AND  YEAR(dat_cotac_indic_econ) = i-ano NO-LOCK NO-ERROR.

  DO WITH FRAME f-pg-sel:   
  
     IF AVAIL cotac_parid  THEN
        ASSIGN fi-tx = val_cotac_indic_econ
            fi-tx:SCREEN-VALUE = string(fi-tx)
            fi-dt-taxa = dat_cotac_indic_econ
            fi-dt-taxa:SCREEN-VALUE = string(fi-dt-taxa).

     IF NOT AVAIL cotac_parid THEN DO:
        RUN utp/ut-msgs.p ("show",17006,
                         " Periodo Inv†lido, n∆o h† Moeda cadastrada ~~ Verifique...").
        APPLY 'entry' TO INPUT fi-emis.
        RETURN NO-APPLY.
     END.
  END.                      

  END.


  /****** pesquisa dolar medio ******/
  FIND FIRST cotac_parid 
      WHERE cod_indic_econ_base = "real" 
        AND cod_indic_econ_idx  = "dolarmed"
        AND  month(dat_cotac_indic_econ) = int(SUBSTR(fi-emis,1,2))
        AND  YEAR(dat_cotac_indic_econ) = int(SUBSTR(fi-emis,3,4)) NO-LOCK NO-ERROR.
  
   IF AVAIL cotac_parid  THEN
     ASSIGN tx-dol-med = val_cotac_indic_econ.
  
   RUN pi-executa.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME cod-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cod-rel C-Win
ON LEAVE OF cod-rel IN FRAME f-pg-sel /* Codigo do Relat¢rio */
DO:
  ASSIGN cod-rel.
  
  FIND dsc-param-rel-ctbl 
      WHERE dsc-param-rel-ctbl.cod-rel = cod-rel NO-ERROR.
  IF AVAIL dsc-param-rel-ctb THEN
     ASSIGN fi-descr:SCREEN-VALUE = dsc-param-rel-ctbl.des-rel.
  ELSE DO:
       ASSIGN cod-rel = ?.
      RUN utp/ut-msgs.p ("show",17006,
                         " Real¢rio n∆o cadastrado ~~ Verifique...").
      APPLY 'entry' TO INPUT cod-rel.
      RETURN NO-APPLY.
  END.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-emis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-emis C-Win
ON LEAVE OF fi-emis IN FRAME f-pg-sel /* Per°odo de emiss∆o */
DO:
  ASSIGN fi-emis.


  /* pesquisa taxa dolar do mes anterior */
  IF int(SUBSTR(fi-emis,1,2)) < 12 THEN DO:
     ASSIGN i-mes = int(substr(fi-emis,1,2))  + 1 
            i-dia = 01
            i-ano = int(SUBSTR(fi-emis,3,4)).
  END.
  ELSE DO:
   IF int(SUBSTR(fi-emis,1,2)) = 12 THEN DO:
      ASSIGN i-mes = 01
             i-ano = int(SUBSTR(fi-emis,3,4)) + 1 
             i-dia = 01.
   END.
  END.
  
  /* pesquisa taxa dolar */
  FIND FIRST cotac_parid 
      WHERE cod_indic_econ_base = "real" 
        AND cod_indic_econ_idx  = "dolar"
        AND  month(dat_cotac_indic_econ) = i-mes
        AND  YEAR(dat_cotac_indic_econ) = i-ano NO-LOCK NO-ERROR.

  IF AVAIL cotac_parid  THEN
     ASSIGN fi-tx = val_cotac_indic_econ
            fi-tx:SCREEN-VALUE = string(fi-tx)
            fi-dt-taxa = dat_cotac_indic_econ
            fi-dt-taxa:SCREEN-VALUE = string(fi-dt-taxa).

  IF NOT AVAIL cotac_parid THEN DO:
      RUN utp/ut-msgs.p ("show",17006,
                         " Periodo Inv†lido, n∆o h† Moeda cadastrada ~~ Verifique...").
      APPLY 'entry' TO INPUT fi-emis.
      RETURN NO-APPLY.



  END.



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

  

ASSIGN fi-emis = string(MONTH(TODAY),"99")  + string(YEAR(today),"9999"). 


       
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
/*
  IF MONTH(TODAY) <> 1 THEN 
     ASSIGN fi-emis:SCREEN-VALUE  = string((MONTH(TODAY)- 1),"99") + string(YEAR(today),"9999"). 

  IF MONTH(TODAY) = 1 THEN
     ASSIGN fi-emis:SCREEN-VALUE  = "12"  + string((YEAR(today) - 1),"9999").

  */
APPLY 'entry' TO INPUT cod-rel.

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
  ENABLE BtnOK BtnCancel IMAGE-1 IMAGE-2 RECT-20 RECT-21 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY cod-rel fi-descr fi-est-ini fi-est-fim fi-emis fi-dt-taxa fi-tx 
          rs-tp-calc rs-tp-emis 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE bt-verifica cod-rel fi-est-ini fi-est-fim fi-emis rs-tp-calc 
         rs-tp-emis IMAGE-13 IMAGE-14 RECT-18 RECT-19 RECT-23 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calculo-anual C-Win 
PROCEDURE pi-calculo-anual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME f-pg-sel:

ASSIGN cod-rel fi-descr fi-est-ini fi-est-fim  fi-emis
       fi-dt-taxa  fi-tx rs-tp-calc rs-tp-emis.

/* calculo do mes */
IF rs-tp-calc = 2 THEN DO:      

   FOR EACH tt-detalhado:
       DELETE tt-detalhado.
   END.                             
   /* tipo do relatorio - 1 - Detalhado, 3- Ambos */
    IF rs-tp-emis = 1 OR  
       rs-tp-emis = 3 THEN DO:

       FOR EACH dsc-param-rel-ctbl 
           WHERE dsc-param-rel-ctbl.cod-rel = cod-rel NO-LOCK:
          
           FOR EACH dsc-param-cta-gerencial OF dsc-param-rel-ctbl NO-LOCK:
               /* zera valor acumulado tot-cta-ctbl */
               ASSIGN tot-cta-ctbl  = 0.
               FOR EACH dsc-param-cta-datasul OF dsc-param-cta-gerencial NO-LOCK:
               /* pesquisa as contas na contabilidade do EMS5 */
                   FOR EACH cta_ctbl 
                      WHERE cta_ctbl.cod_cta_ctbl = dsc-param-cta-datasul.cta-ctbl 
                        AND day(cta_ctbl.dat_fim_valid)   >= 01 
                        AND month(cta_ctbl.dat_fim_valid) >= 01
                        AND YEAR(cta_ctbl.dat_fim_valid)  >= int(substring(fi-emis,3,4)) NO-LOCK:
                       /*
                       FIND LAST sdo_ctbl 
                           WHERE sdo_ctbl.cod_estab           = fi-est-ini /* verificar como fica o est na selecao */
                             AND sdo_ctbl.cod_finalid_econ    = "corrente"
                             AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                             AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                             AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                             AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK NO-ERROR.
                           
                          IF AVAIL sdo_ctbl THEN 
                             ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                   sdo_ctbl.val_sdo_ctbl_db   + 
                                                   sdo_ctbl.val_sdo_ctbl_fim  +
                                                   tot-cta-ctbl.
                         */

                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            AND sdo_ctbl.cod_plano_ccusto    = " "  
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            /* AND sdo_ctbl.cod_unid_negoc      =  */
                            /* AND sdo_ctbl.dat_sdo_ctbl        = fi-dt-taxa */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK:
                            
                            IF  cta_ctbl.cod_cta_ctbl BEGINS "1" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "2" THEN
                                ASSIGN tot-cta-ctbl = /* sdo_ctbl.val_sdo_ctbl_cr   + 
                                                     sdo_ctbl.val_sdo_ctbl_db   +   */
                                                     sdo_ctbl.val_sdo_ctbl_fim  +
                                                     tot-cta-ctbl.
                            ELSE
                            IF  cta_ctbl.cod_cta_ctbl BEGINS "3" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "4" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "5" THEN
                                 ASSIGN tot-cta-ctbl = (sdo_ctbl.val_sdo_ctbl_db  - 
                                                        sdo_ctbl.val_sdo_ctbl_cr)  + 
                                                        tot-cta-ctbl.

                       END. /* sdo_ctbl */ 


                      /*****************
                     /* acha o saldo das contas */
                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            /* AND sdo_ctbl.cod_plano_ccusto    = ""  */
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            /* AND sdo_ctbl.cod_unid_negoc      =  */
                            /* AND sdo_ctbl.dat_sdo_ctbl        = fi-dt-taxa */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK
                            :
                            ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                  sdo_ctbl.val_sdo_ctbl_db   + 
                                                  sdo_ctbl.val_sdo_ctbl_fim  +
                                                  tot-cta-ctbl.
                            
                       END. /* sdo_ctbl */
                       *************************/
                   END. /* cta_ctbl */    
               END. /* dsc-param-cta-datasul */ 

               /* criar arq temporario detalhado */
               CREATE tt-detalhado.
               ASSIGN tt-cod-rel        = dsc-param-cta-gerencial.cod-rel        
                      tt-nr-seq         = dsc-param-cta-gerencial.nr-seq         
                      tt-cta-ger        = dsc-param-cta-gerencial.cta-ger        
                      tt-des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger  
                      tt-ld-borda       = dsc-param-cta-gerencial.ld-borda       
                      tt-tipo-linha     = dsc-param-cta-gerencial.tipo-linha     
                      tt-saldo-cbl      = tot-cta-ctbl     
                      /* tt-saldo-dol      =       
                      IF dsc-param-cta-gerencial. = 1  THEN */
                      tt-taxa-dolar     =  fi-tx. /* depois fazer o calculo */

                
           END. /* dsc-param-cta-gerencial */ 
               
       END. /* dsc-param-rel-ctbl */
       
       RUN pi-imprime-det.

    END.

    /* tipo do relatorio - 2 - Resumido, 3- Ambos */
    IF rs-tp-emis = 2 OR  
       rs-tp-emis = 3 THEN DO:

       FOR EACH dsc-param-rel-ctbl 
           WHERE dsc-param-rel-ctbl.cod-rel = cod-rel NO-LOCK:
          
           FOR EACH dsc-param-cta-gerencial OF dsc-param-rel-ctbl NO-LOCK:
             
               FOR EACH dsc-param-cta-datasul OF dsc-param-cta-gerencial NO-LOCK:
                    
                   /* zera valor acumulado tot-cta-ctbl */
                    ASSIGN tot-cta-ctbl  = 0
                           tot-tt-deb    = 0
                           tot-tt-cred   = 0
                           tot-sdo-ini   = 0
                           tot-sdo-fim   = 0
                           i-mes-aux     = 0
                           i-ano-aux     = 0.

                   /* pesquisa as contas na contabilidade do EMS5 */
                   FOR EACH cta_ctbl 
                      WHERE cta_ctbl.cod_cta_ctbl = dsc-param-cta-datasul.cta-ctbl 
                        AND day(cta_ctbl.dat_fim_valid)   >= 01 
                        AND month(cta_ctbl.dat_fim_valid) >= 01
                        AND YEAR(cta_ctbl.dat_fim_valid)  >= int(substring(fi-emis,3,4)) NO-LOCK:
                                                            
                       /***********  
                       FIND LAST sdo_ctbl 
                           WHERE sdo_ctbl.cod_estab           = fi-est-ini /* verificar como fica o est na selecao */
                             AND sdo_ctbl.cod_finalid_econ    = "corrente"
                             AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                             AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                             AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                             AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK NO-ERROR.
                           
                          IF AVAIL sdo_ctbl THEN DO:
                           
                             ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                   sdo_ctbl.val_sdo_ctbl_db   + 
                                                   sdo_ctbl.val_sdo_ctbl_fim  +
                                                   tot-cta-ctbl.

                             ASSIGN tot-tt-deb   =  sdo_ctbl.val_sdo_ctbl_db  +
                                                    tot-tt-deb.
                             ASSIGN tot-tt-cred  =  sdo_ctbl.val_sdo_ctbl_cr  +
                                                    tot-tt-cred.

                             ASSIGN tot-sdo-fim  =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                    tot-sdo-fim.
                             
                             /* le sdo da conta do mes anterior para pegar saldo inicial */
                             
                             IF month(sdo_ctbl.dat_sdo_ctbl) = 01 THEN
                             ASSIGN i-ano-aux = YEAR(sdo_ctbl.dat_sdo_ctbl) - 1
                                    i-mes-aux = 12.
                             ELSE
                             ASSIGN i-ano-aux = YEAR(sdo_ctbl.dat_sdo_ctbl)
                                    i-mes-aux = month(sdo_ctbl.dat_sdo_ctbl) - 1.

                             FIND LAST sdo_ctbl 
                               WHERE sdo_ctbl.cod_estab           = fi-est-ini /* verificar como fica o est na selecao */
                                 AND sdo_ctbl.cod_finalid_econ    = "corrente"
                                 AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                                 AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                                 AND MONTH(sdo_ctbl.dat_sdo_ctbl) = i-mes-aux
                                 AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = i-ano-aux NO-LOCK NO-ERROR.
                             
                             IF AVAIL sdo_ctbl THEN
                                 ASSIGN tot-sdo-ini  =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                        tot-sdo-ini.
                          END.
                          ****************/

                       /* acha o saldo das contas */                         
                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            AND sdo_ctbl.cod_plano_ccusto    = " "  
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK:
                           
                          /*IF AVAIL sdo_ctbl THEN DO: */
                           
                             ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                   sdo_ctbl.val_sdo_ctbl_db   + 
                                                   sdo_ctbl.val_sdo_ctbl_fim  +
                                                   tot-cta-ctbl.

                             ASSIGN tot-tt-deb   =  sdo_ctbl.val_sdo_ctbl_db  +
                                                    tot-tt-deb.
                             ASSIGN tot-tt-cred  =  sdo_ctbl.val_sdo_ctbl_cr  +
                                                    tot-tt-cred.

                             ASSIGN tot-sdo-fim   =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                     tot-sdo-fim.
                          
                       END.

                             /* le sdo da conta do mes anterior para pegar saldo inicial */
                             
                             IF int(SUBString(fi-emis,1,2)) = 01 THEN
                                ASSIGN i-ano-aux = int(SUBString(fi-emis,3,4)) - 1
                                       i-mes-aux = 12.
                             ELSE
                                ASSIGN i-ano-aux = int(SUBString(fi-emis,3,4))
                                       i-mes-aux = int(SUBString(fi-emis,1,2)) - 1.

                             FOR EACH sdo_ctbl 
                              WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                                AND sdo_ctbl.cod_estab          <= fi-est-fim
                                AND sdo_ctbl.cod_finalid_econ    = "corrente"
                                AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                                AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                                AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                                AND sdo_ctbl.cod_plano_ccusto    = " "  
                                AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                                AND MONTH(sdo_ctbl.dat_sdo_ctbl) = i-mes-aux
                                AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = i-ano-aux NO-LOCK:

                                ASSIGN tot-sdo-ini  =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                        tot-sdo-ini.
                             END.
                                  
                       /*****************************************************
                      /* acha o saldo das contas */
                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            /* AND sdo_ctbl.cod_plano_ccusto    = ""  */
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            /* AND sdo_ctbl.cod_unid_negoc      =  
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = 06
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = 2007  */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4))  NO-LOCK:
                     
                            ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                  sdo_ctbl.val_sdo_ctbl_db   + 
                                                  sdo_ctbl.val_sdo_ctbl_fim  +
                                                  tot-cta-ctbl.

                                   tot-tt-deb   =  sdo_ctbl.val_sdo_ctbl_db  +
                                                   tot-tt-deb.
                                   tot-tt-cred  =  sdo_ctbl.val_sdo_ctbl_cr  +
                                                   tot-tt-cred.                                    
                            
                       END. /* sdo_ctbl */
                       ***********************************************************/

                   END. /* cta_ctbl */
                            
                   /* criar arq temporario resumido */
                   CREATE tt-resumido.
                   ASSIGN tt-resumido.tt-cod-rel        = dsc-param-cta-gerencial.cod-rel        
                          tt-resumido.tt-nr-seq         = dsc-param-cta-gerencial.nr-seq         
                          tt-resumido.tt-cta-ger        = dsc-param-cta-gerencial.cta-ger        
                          tt-resumido.tt-des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger  
                          tt-resumido.tt-cta-ctbl       = dsc-param-cta-datasul.cta-ctbl
                          /* tt-des-cta-ctbl              = des_tit_ctbl  */
                          tt-resumido.tt-debitos        = tot-tt-deb
                          tt-resumido.tt-creditos       = tot-tt-cred
                          tt-saldo-inicial              = tot-sdo-ini
                          tt-saldo-fim                  = tot-sdo-fim.     
                         
                   FIND cta_ctbl 
                       WHERE cta_ctbl.cod_cta_ctbl = dsc-param-cta-datasul.cta-ctbl NO-LOCK NO-ERROR.

                   IF AVAIL cta_ctbl THEN
                      ASSIGN tt-des-cta-ctbl  = des_tit_ctbl. 
                       
               END. /* dsc-param-cta-datasul */ 
                            
           END. /* dsc-param-cta-gerencial */ 
               
       END. /* dsc-param-rel-ctbl */

    RUN pi-imprime-res.

    END.

END.                        

END.

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

ASSIGN cod-rel     fi-descr fi-est-ini fi-est-fim  fi-emis
       fi-dt-taxa  fi-tx    rs-tp-calc rs-tp-emis.
                                    
/* calculo do mes */
IF rs-tp-calc = 1 THEN DO:          
   FOR EACH tt-detalhado:
       DELETE tt-detalhado.
   END.                             
   /* tipo do relatorio - 1 - Detalhado, 3- Ambos */
    IF rs-tp-emis = 1 OR  
       rs-tp-emis = 3 THEN DO:

       FOR EACH dsc-param-rel-ctbl 
           WHERE dsc-param-rel-ctbl.cod-rel = cod-rel NO-LOCK:
          
           FOR EACH dsc-param-cta-gerencial OF dsc-param-rel-ctbl NO-LOCK:
               /* zera valor acumulado tot-cta-ctbl */
               ASSIGN tot-cta-ctbl  = 0.
               FOR EACH dsc-param-cta-datasul OF dsc-param-cta-gerencial NO-LOCK:
               /* pesquisa as contas na contabilidade do EMS5 */
                   FOR EACH cta_ctbl 
                      WHERE cta_ctbl.cod_cta_ctbl = dsc-param-cta-datasul.cta-ctbl 
                        AND day(cta_ctbl.dat_fim_valid)   >= 01 
                        AND month(cta_ctbl.dat_fim_valid) >= int(substring(fi-emis,1,2))
                        AND YEAR(cta_ctbl.dat_fim_valid)  >= int(substring(fi-emis,3,4)) NO-LOCK:

                    
                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            AND sdo_ctbl.cod_plano_ccusto    = " "  
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            /* AND sdo_ctbl.cod_unid_negoc      =  */
                            /* AND sdo_ctbl.dat_sdo_ctbl        = fi-dt-taxa */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK
                            :
                            
                            IF  cta_ctbl.cod_cta_ctbl BEGINS "1" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "2" THEN
                                ASSIGN tot-cta-ctbl = /* sdo_ctbl.val_sdo_ctbl_cr   + 
                                                     sdo_ctbl.val_sdo_ctbl_db   +   */
                                                     sdo_ctbl.val_sdo_ctbl_fim  +
                                                     tot-cta-ctbl.
                            ELSE
                            IF  cta_ctbl.cod_cta_ctbl BEGINS "3" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "4" OR 
                                cta_ctbl.cod_cta_ctbl BEGINS "5" THEN
                                 ASSIGN tot-cta-ctbl =  (sdo_ctbl.val_sdo_ctbl_db  - 
                                                        sdo_ctbl.val_sdo_ctbl_cr)  + 
                                                        tot-cta-ctbl.

                       END. /* sdo_ctbl */ 
                                         
                    
                   END. /* cta_ctbl */
                   
               END. /* dsc-param-cta-datasul */ 

               /* criar arq temporario detalhado */
               CREATE tt-detalhado.
               ASSIGN tt-cod-rel        = dsc-param-cta-gerencial.cod-rel        
                      tt-nr-seq         = dsc-param-cta-gerencial.nr-seq         
                      tt-cta-ger        = dsc-param-cta-gerencial.cta-ger        
                      tt-des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger  
                      tt-ld-borda       = dsc-param-cta-gerencial.ld-borda       
                      tt-tipo-linha     = dsc-param-cta-gerencial.tipo-linha     
                      tt-saldo-cbl      = tot-cta-ctbl.

               IF  dsc-param-cta-gerencial.tip-taxa-dolar = 3 THEN
                   ASSIGN tt-taxa-dolar =  tx-dol-med.
               ELSE
                   ASSIGN tt-taxa-dolar =  fi-tx.


           END. /* dsc-param-cta-gerencial */ 
               
       END. /* dsc-param-rel-ctbl */

       /* rotina para a conta gerencial 32090 */
       
       ASSIGN tot-cta-3290 = 0.

       FOR EACH tt-detalhado 
          WHERE int(tt-detalhado.tt-cta-ger) >= 32020 
            AND int(tt-detalhado.tt-cta-ger) <= 32089 NO-LOCK:
          ASSIGN tot-cta-3290 = tot-cta-3290 + tt-saldo-cbl.
       END.

       FOR EACH tt-detalhado 
          WHERE int(tt-detalhado.tt-cta-ger) = 32090:
          ASSIGN tt-detalhado.tt-saldo-cbl = tt-detalhado.tt-saldo-cbl - tot-cta-3290.
       END. 
       
       RUN pi-imprime-det.

    END.

    /* tipo do relatorio - 2 - Resumido, 3- Ambos */
    IF rs-tp-emis = 2 OR  
       rs-tp-emis = 3 THEN DO:

       FOR EACH dsc-param-rel-ctbl 
           WHERE dsc-param-rel-ctbl.cod-rel = cod-rel NO-LOCK:
          
           FOR EACH dsc-param-cta-gerencial OF dsc-param-rel-ctbl NO-LOCK:
             
               FOR EACH dsc-param-cta-datasul OF dsc-param-cta-gerencial NO-LOCK:
                    
                   /* zera valor acumulado tot-cta-ctbl */
                    ASSIGN tot-cta-ctbl  = 0
                           tot-tt-deb    = 0
                           tot-tt-cred   = 0
                           tot-sdo-ini   = 0
                           tot-sdo-fim   = 0
                           i-mes-aux     = 0
                           i-ano-aux     = 0.

                   /* pesquisa as contas na contabilidade do EMS5 */
                   FOR EACH cta_ctbl 
                      WHERE cta_ctbl.cod_cta_ctbl          = dsc-param-cta-datasul.cta-ctbl 
                        AND   day(cta_ctbl.dat_fim_valid) >= 01 
                        AND month(cta_ctbl.dat_fim_valid) >= int(substring(fi-emis,1,2))
                        AND  YEAR(cta_ctbl.dat_fim_valid) >= int(substring(fi-emis,3,4)) NO-LOCK:
                    
                       /* acha o saldo das contas */                         
                       FOR EACH sdo_ctbl 
                          WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                            AND sdo_ctbl.cod_estab          <= fi-est-fim
                            AND sdo_ctbl.cod_finalid_econ    = "corrente"
                            AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                            AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                            AND sdo_ctbl.cod_plano_ccusto    = " "  
                            AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                            AND MONTH(sdo_ctbl.dat_sdo_ctbl) = int(SUBString(fi-emis,1,2))
                            AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = int(SUBString(fi-emis,3,4)) NO-LOCK:
                           
                          /*IF AVAIL sdo_ctbl THEN DO: */
                           
                             ASSIGN tot-cta-ctbl = sdo_ctbl.val_sdo_ctbl_cr   + 
                                                   sdo_ctbl.val_sdo_ctbl_db   + 
                                                   sdo_ctbl.val_sdo_ctbl_fim  +
                                                   tot-cta-ctbl.

                             ASSIGN tot-tt-deb   =  sdo_ctbl.val_sdo_ctbl_db  +
                                                    tot-tt-deb.
                             ASSIGN tot-tt-cred  =  sdo_ctbl.val_sdo_ctbl_cr  +
                                                    tot-tt-cred.

                             ASSIGN tot-sdo-fim   =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                     tot-sdo-fim.
                          
                       END.

                             /* le sdo da conta do mes anterior para pegar saldo inicial */
                             
                             IF int(SUBString(fi-emis,1,2)) = 01 THEN
                             ASSIGN i-ano-aux = int(SUBString(fi-emis,3,4)) - 1
                                    i-mes-aux = 12.
                             ELSE
                             ASSIGN i-ano-aux = int(SUBString(fi-emis,3,4))
                                    i-mes-aux = int(SUBString(fi-emis,1,2)) - 1.

                             FOR EACH sdo_ctbl 
                              WHERE sdo_ctbl.cod_estab          >= fi-est-ini
                                AND sdo_ctbl.cod_estab          <= fi-est-fim
                                AND sdo_ctbl.cod_finalid_econ    = "corrente"
                                AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                                AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                                AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                                AND sdo_ctbl.cod_plano_ccusto    = " "  
                                AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"  /*  gerenbvi */
                                AND MONTH(sdo_ctbl.dat_sdo_ctbl) = i-mes-aux
                                AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = i-ano-aux NO-LOCK:

                                ASSIGN tot-sdo-ini  =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                        tot-sdo-ini.
                             END.
                               
                             /*
                             FIND LAST sdo_ctbl 
                               WHERE sdo_ctbl.cod_estab           = fi-est-ini
                                 AND sdo_ctbl.cod_finalid_econ    = "corrente"
                                 AND sdo_ctbl.cod_plano_cta_ctbl  = "contsoc"
                                 AND sdo_ctbl.cod_cenar_ctbl      = "contsoc"
                                 AND sdo_ctbl.cod_cta_ctbl        = cta_ctbl.cod_cta_ctbl
                                 AND sdo_ctbl.cod_plano_ccusto    = " "  
                                 AND MONTH(sdo_ctbl.dat_sdo_ctbl) = i-mes-aux
                                 AND YEAR(sdo_ctbl.dat_sdo_ctbl)  = i-ano-aux NO-LOCK NO-ERROR.
                                 
                             IF AVAIL sdo_ctbl THEN
                                 ASSIGN tot-sdo-ini  =  sdo_ctbl.val_sdo_ctbl_fim  +
                                                        tot-sdo-ini.*/ 
                      

                   END. /* cta_ctbl */

             
                   /* criar arq temporario resumido */
                   CREATE tt-resumido.
                   ASSIGN tt-resumido.tt-cod-rel        = dsc-param-cta-gerencial.cod-rel        
                          tt-resumido.tt-nr-seq         = dsc-param-cta-gerencial.nr-seq         
                          tt-resumido.tt-cta-ger        = dsc-param-cta-gerencial.cta-ger        
                          tt-resumido.tt-des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger  
                          tt-resumido.tt-cta-ctbl       = dsc-param-cta-datasul.cta-ctbl
                          /* tt-des-cta-ctbl              = des_tit_ctbl  */
                          tt-resumido.tt-debitos        = tot-tt-deb
                          tt-resumido.tt-creditos       = tot-tt-cred
                          tt-saldo-inicial              = tot-sdo-ini
                          tt-saldo-fim                  = tot-sdo-fim.
                        
                   FIND cta_ctbl 
                       WHERE cta_ctbl.cod_cta_ctbl = dsc-param-cta-datasul.cta-ctbl NO-LOCK NO-ERROR.

                   IF AVAIL cta_ctbl THEN
                      ASSIGN tt-des-cta-ctbl  = des_tit_ctbl.
                       
               END. /* dsc-param-cta-datasul */              
               
           END. /* dsc-param-cta-gerencial */ 
               
       END. /* dsc-param-rel-ctbl */

      RUN pi-imprime-res.

    END.

END.

ELSE DO:
 /* calculo anual */
 IF rs-tp-calc = 2 THEN DO:

     RUN pi-calculo-anual.

     
 END.
END.

END.

 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-det C-Win 
PROCEDURE pi-imprime-det :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /************* Criacao do Aplicativo EXCEL **********/
CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = "esp/esct005-det.xls". 
chexcelapplication:workbooks:open(file-info:full-pathname,TRUE).
chexcelapplication:sheets:item(1).


ASSIGN i-linha = 13.


FOR EACH tt-detalhado BY tt-nr-seq:

   ASSIGN chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-detalhado.tt-cta-ger 
            chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-detalhado.tt-des-conta-ger
           /* chExcelApplication:Range( "C" + STRING(i-linha)):NumberFormat="###.###.##0,00" */
            chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-saldo-cbl 
          /*  chExcelApplication:Range( "J" + STRING(i-linha)):NumberFormat="#.##0,00000"   */
            chExcelApplication:range( "J" + STRING(i-linha) ):value =  tt-taxa-dolar 
           /* chExcelApplication:Range( "L" + STRING(i-linha)):NumberFormat="###.###.##0" */
            chExcelApplication:range( "L" + STRING(i-linha) ):value = tt-saldo-cbl  /  tt-taxa-dolar .
    
           i-linha = i-linha + 1.

END.

/* ex do
chExcelApplication:Workbooks:Application:quit.
        
        /* release com-handles */
        release object chExcelApplication no-error. 
        release object chWorksheet no-error.
        release object chWorkbook no-error.
        */


/*
CASE tt-param.idi-tipo-imp:
    WHEN 1 THEN DO: /* Impressora */ 
        chExcelApplication:ActiveSheet:PrintOut.
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:QUIT(). */
 

   /* WHEN 2 THEN DO:  Arquivo    
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:ActiveSheet:SaveAs(tt-param.arquivo,,,,,,).
        chExcelApplication:QUIT().
    END. 
   WHEN 2 THEN DO: /* Terminal   */  */
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:Visible = TRUE.
   /* END. 

END CASE.*/



 /* release com-handles */
RELEASE OBJECT chExcelApplication NO-ERROR.
release object chWorksheet no-error.
release object chWorkbook no-error.

/*
RUN pi-finalizar IN h-acomp.

/* {include/i-rpclo.i}  */

RETURN "OK":U.

  */

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-res C-Win 
PROCEDURE pi-imprime-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /************* Criacao do Aplicativo EXCEL **********/
CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = "esp/esct005-res.xls". 
chexcelapplication:workbooks:open(file-info:full-pathname,TRUE).
chexcelapplication:sheets:item(1).

ASSIGN i-linha = 3.

FOR EACH tt-resumido BY tt-nr-seq:

   ASSIGN   chExcelApplication:range( "A" + STRING(i-linha) ):value = tt-resumido.tt-cta-ger 
            chExcelApplication:range( "B" + STRING(i-linha) ):value = tt-resumido.tt-cta-ctbl
            chExcelApplication:range( "C" + STRING(i-linha) ):value = tt-des-cta-ctbl 
           /* chExcelApplication:Range( "D" + STRING(i-linha)):NumberFormat="#.##0,00"    */
            chExcelApplication:range( "D" + STRING(i-linha) ):value = tt-saldo-inicial 
           /* chExcelApplication:Range( "E" + STRING(i-linha)):NumberFormat="#.##0,00"    */
            chExcelApplication:range( "E" + STRING(i-linha) ):value = tt-debitos
           /* chExcelApplication:Range( "F" + STRING(i-linha)):NumberFormat="#.##0,00"   */
            chExcelApplication:range( "F" + STRING(i-linha) ):value = tt-creditos 
           /* chExcelApplication:Range( "G" + STRING(i-linha)):NumberFormat="#.##0,00"   */
            chExcelApplication:range( "G" + STRING(i-linha) ):value = tt-saldo-fim.
            
           i-linha = i-linha + 1.

END. 

/* ex do
chExcelApplication:Workbooks:Application:quit.
        
        /* release com-handles */
        release object chExcelApplication no-error. 
        release object chWorksheet no-error.
        release object chWorkbook no-error.
        */


/*
CASE tt-param.idi-tipo-imp:
    WHEN 1 THEN DO: /* Impressora */ 
        chExcelApplication:ActiveSheet:PrintOut.
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:QUIT(). */
 

   /* WHEN 2 THEN DO:  Arquivo    
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:ActiveSheet:SaveAs(tt-param.arquivo,,,,,,).
        chExcelApplication:QUIT().
    END. 
   WHEN 2 THEN DO: /* Terminal   */  */
        chExcelApplication:APPLICATION:DisplayAlerts = FALSE.
        chExcelApplication:Visible = TRUE.
   /* END. 

END CASE.*/


 /* release com-handles */
RELEASE OBJECT chExcelApplication NO-ERROR.
release object chWorksheet no-error.
release object chWorkbook no-error.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

