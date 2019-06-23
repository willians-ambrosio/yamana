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
{include/buffers_RH.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-imprime
    FIELD tt-cdn-empresa       LIKE  funcionario.cdn_empresa                             
    FIELD tt-cdn-estab         LIKE  funcionario.cdn_estab
    FIELD tt-cdn-funcionario   LIKE  funcionario.cdn_funcionario
    FIELD tt-cod-rh-ccusto     LIKE  funcionario.cod_rh_ccusto
    FIELD tt-des-centro-custo  LIKE  rh_ccusto.des_rh_ccusto
    FIELD tt-efet-mes-ant      AS INT
    FIELD tt-adm-mes           AS INT
    FIELD tt-transf-entr       AS INT
    FIELD tt-transf-sai        AS INT
    FIELD tt-dem-emp           AS INT
    FIELD tt-dem-ped           AS INT
    FIELD tt-tot-efet          AS INT
    FIELD tt-estag             AS INT
    FIELD tt-afast-15          AS INT
    FIELD tt-jov-aprend        AS INT
    FIELD tt-c-susp            AS INT
    FIELD tt-diretor-estat     AS INT
    FIELD tt-turn-over         AS INT.


/* /* Variaveis para Gerar em Excel */               */
/* DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO. */
/* def var chWorkbook as com-handle no-undo.         */
/* def var chWorksheet as com-handle no-undo.        */

DEF VAR i-linha AS INT.

DEF VAR i-adm-mes      AS INT.
DEF VAR i-transf-entr  AS INT.
DEF VAR i-transf-saida AS INT.
DEF VAR i-dem-emp      AS INT.
DEF VAR i-dem-ped      AS INT.
DEF VAR i-afast-15     AS INT.
DEF VAR i-dir-estat    AS INT.
DEF VAR i-estag        AS INT.

DEF VAR i-efet-mes-ant AS INT.
DEF VAR i-tot-efet     AS INT.


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

/* variaveis utilizadas para a gera‡Æo do EXCEL */
def new shared var c-planilha-envio as char       no-undo.
def var chActiveWorkbook            as com-handle no-undo.
def var c-planilha                  as char       no-undo.
def var c-range                     as char       no-undo.
def var i-celula                    as int        no-undo.
def var i-conta-linha               as int        no-undo.
def var i-linha-ini                 as int        no-undo.
def var qtd-linha                   as int        no-undo.

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

DEFINE VARIABLE fi-cc-fim AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Funcionario" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-centro-fim AS CHARACTER FORMAT "x(8)":U INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-centro-ini AS CHARACTER FORMAT "x(8)":U 
     LABEL "Centro de Custo" 
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

DEFINE VARIABLE fi-turno-fim AS INTEGER FORMAT "9999":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-turno-ini AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Turno" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

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
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
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
     fi-empresa AT ROW 1.25 COL 13 COLON-ALIGNED
     fi-est-ini AT ROW 2.25 COL 13 COLON-ALIGNED
     fi-est-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-cc-ini AT ROW 3.25 COL 13 COLON-ALIGNED
     fi-cc-fim AT ROW 3.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-centro-ini AT ROW 4.25 COL 13 COLON-ALIGNED
     fi-centro-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-turno-ini AT ROW 5.25 COL 13 COLON-ALIGNED
     fi-turno-fim AT ROW 5.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-texto AT ROW 8.5 COL 12 COLON-ALIGNED NO-LABEL
     IMAGE-15 AT ROW 2.25 COL 41
     IMAGE-16 AT ROW 3.25 COL 41
     IMAGE-17 AT ROW 2.25 COL 34
     IMAGE-18 AT ROW 3.25 COL 34
     IMAGE-19 AT ROW 4.25 COL 41
     IMAGE-20 AT ROW 4.25 COL 34
     IMAGE-21 AT ROW 5.25 COL 41
     IMAGE-22 AT ROW 5.25 COL 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.5
         SIZE 70 BY 9.5
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
         TITLE              = "<Informa‡äes Cadastrais - Funcion rio - ESHCM014.001>"
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
ON END-ERROR OF C-Win /* <Informa‡äes Cadastrais - Funcion rio - ESHCM014.001> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Informa‡äes Cadastrais - Funcion rio - ESHCM014.001> */
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
  DISPLAY fi-empresa fi-est-ini fi-est-fim fi-cc-ini fi-cc-fim fi-centro-ini 
          fi-centro-fim fi-turno-ini fi-turno-fim fi-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-empresa fi-est-ini fi-est-fim fi-cc-ini fi-cc-fim fi-centro-ini 
         fi-centro-fim fi-turno-ini fi-turno-fim fi-texto IMAGE-15 IMAGE-16 
         IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 
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
       fi-centro-ini
       fi-centro-fim
       fi-turno-ini
       fi-turno-fim.


FOR EACH tt-imprime :
    DELETE tt-imprime.
END.
    
 FOR EACH funcionario NO-LOCK
    WHERE funcionario.cdn_empresa            =  fi-empresa
      AND funcionario.cdn_estab             >=  fi-est-ini
      AND funcionario.cdn_estab             <=  fi-est-fim
      and funcionario.cdn_funcionario       >= (fi-cc-ini) 
      and funcionario.cdn_funcionario       <= (fi-cc-fim)
      and funcionario.cod_rh_ccusto         >= (fi-centro-ini) 
      and funcionario.cod_rh_ccusto         <= (fi-centro-fim)
      and funcionario.cdn_turno_trab        >= (fi-turno-ini) 
      and funcionario.cdn_turno_trab        <= (fi-turno-fim) 
      AND funcionario.dat_desligto_func      = ? 
       BY funcionario.cdn_empresa
       BY funcionario.cdn_estab:

    ASSIGN fi-texto:SCREEN-VALUE = "Gerando Dados......." + 
                                          STRING(funcionario.cdn_funcionario).

    ASSIGN
        FILE-INFO:FILE-NAME = "esp\atualizacao_cadastral.xls"
        c-planilha          = FILE-INFO:FULL-PATHNAME
        i-linha       = 1.

    run pi-planilha-envio.

    /* Inicio da Cria‡Æo da Planilha */
    create "Excel.Application" chExcelApplication.
    assign chExcelApplication:visible = no /* Nao mostra a planilha Excel na tela enquanto esta sendo criada */
           chWorkbook  = chExcelApplication:Workbooks:add(c-planilha) /* Cria uma nova planilha excel */
           chWorkSheet = chExcelApplication:Sheets:item(1).

    assign i-conta-linha = 5.
    FIND rh_pessoa_fisic                                                                                       
       WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.


    find empresa no-lock where empresa.ep-codigo = fi-empresa  no-error.
    ASSIGN i-linha = 6
           chExcelApplication:range( "c" + STRING(i-linha) ):Value = empresa.razao-social.

   
    ASSIGN i-linha = 10
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = funcionario.nom_pessoa_fisic
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = funcionario.cdn_funcionario
           chExcelApplication:range( "h" + STRING(i-linha) ):Value = funcionario.num_cartao_pto.

    ASSIGN i-linha = 11
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_pai_pessoa_fisic
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_mae_pessoa_fisic. 
   
    /* estado civil */
     ASSIGN i-linha = 12.

     ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.dat_nascimento.

        IF  INT(rh_pessoa_fisic.idi_estado_civil) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "CASADO".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_estado_civil) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "SOLTEIRO".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "DESQUITADO".              
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "DIVORCIADO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "VIUVO".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 6 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "SEPARADO JUD.".  
         ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 7 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "OUTROS".       
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_estado_civil) = 8 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "UNIÇO ESTAVEL".   
    
    ASSIGN i-linha = 17
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_ender_rh +  ", nr " +
                                                                     SUBSTR(rh_pessoa_fisic.cod_livre_1,70,4)
           chExcelApplication:range( "h" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_pto_refer.

    ASSIGN i-linha = 19
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_bairro_rh 
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_cidad_rh.

    ASSIGN i-linha = 21
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.cod_cep_rh
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = rh_pessoa_fisic.cod_unid_federac_rh.
    
    ASSIGN i-linha = 23
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = string(rh_pessoa_fisic.num_DDD) + "-" +  string(rh_pessoa_fisic.num_telefone)
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = string(rh_pessoa_fisic.num_DDD_contat) + "-" + string(rh_pessoa_fisic.num_telef_contat).

    ASSIGN i-linha = 25
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.nom_e_Mail
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = substr(rh_pessoa_fisic.cod_livre_1,25,30) /* rh_pessoa_fisic.nom_Mail_contat*/ .

    FIND grau_instruc WHERE 
         grau_instruc.cdn_grau_instruc = rh_pessoa_fisic.cdn_grau_instruc NO-LOCK NO-ERROR.

    IF AVAIL grau_instruc THEN
    ASSIGN i-linha = 26
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = grau_instruc.des_grau_instruc.

    ASSIGN i-linha = 32
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.cod_id_feder 
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = funcionario.cod_cart_trab 
           chExcelApplication:range( "h" + STRING(i-linha) ):Value = funcionario.cod_ser_cart_trab  .
         
    ASSIGN i-linha = 33
          chExcelApplication:range( "d" + STRING(i-linha) ):Value = rh_pessoa_fisic.cod_id_estad_fisic
          chExcelApplication:range( "f" + STRING(i-linha) ):Value = rh_pessoa_fisic.cod_orgao_emis_id_estad.
    
    ASSIGN i-linha = 34
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = funcionario.cod_tit_eletral
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = funcionario.num_zona_tit_eletral  
           chExcelApplication:range( "h" + STRING(i-linha) ):Value = funcionario.num_secao_tit_eletral.

    ASSIGN i-linha = 35
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = funcionario.nom_cidad_emit_tit_eletral
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = funcionario.cod_unid_federac_tit_eletral.

   /* cnh */
    ASSIGN i-linha = 36
           chExcelApplication:range( "d" + STRING(i-linha) ):Value = funcionario.num_cart_habilit
           chExcelApplication:range( "f" + STRING(i-linha) ):Value = funcionario.dat_vencto_habilit.
    
    ASSIGN i-linha = 42.
        /* cutis */
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "BRANCA".             
        ELSE  
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 2 THEN                                                      
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "NEGRA".             
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 3 THEN                                                 
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "PARDA".           
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "AMARELA".              
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "NAO INFORMADA". 
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cutis) = 6 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "INDIGENA". 
    
        /* cabelo */
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "CASTANHO".             
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "PRETO".           
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "LOIRO".              
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "RUIVO". 
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "GRISALHO". 
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_cabelo) = 6 THEN                                                     
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = "OUTROS". 
    
        ASSIGN i-linha = 42. /* doador de sangue */
        IF  log_pessoa_fisic_doador THEN                                                      
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "SIM".             
        ELSE   
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "NAO". 

        ASSIGN i-linha = 43.
        /* tipo sanguinio */
        IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "O".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "A".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "B".              
        ELSE    
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "AB".              
        ELSE   
         IF  INT(rh_pessoa_fisic.idi_tip_sangue) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "NAO INFORMADO".  
        
         /* olhos */
        IF  INT(rh_pessoa_fisic.idi_cor_olhos) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "CASTANHO".             
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_olhos) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "PRETO".           
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_olhos) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "AZUL".              
        ELSE                                                                                      
        IF  INT(rh_pessoa_fisic.idi_cor_olhos) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "VERDE". 
        ELSE                                                                               
        IF  INT(rh_pessoa_fisic.idi_cor_olhos) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = "OUTROS". 

        /* estatura */                          
            ASSIGN chExcelApplication:range( "h" + STRING(i-linha) ):value = val_estatur_pessoa.    

        ASSIGN i-linha = 44.
        /* fator rh */
        IF  INT(rh_pessoa_fisic.idi_fatorrh) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "POSITIVO".             
        ELSE                                                                                      
             IF  INT(rh_pessoa_fisic.idi_fatorrh) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "NEGATIVO".           
        ELSE                                                                                      
         IF  INT(rh_pessoa_fisic.idi_fatorrh) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "d" + STRING(i-linha) ):value = "NAO INFORMADO".   
                         
        ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = vli_peso_pessoa
               chExcelApplication:range( "h" + STRING(i-linha) ):value = substr(rh_pessoa_fisic.cod_livre_1,1,2).

        ASSIGN i-linha = 45.
        /* calcado e camisa */
        ASSIGN chExcelApplication:range( "f" + STRING(i-linha) ):value = num_calcad_func
               chExcelApplication:range( "h" + STRING(i-linha) ):value = substr(rh_pessoa_fisic.cod_livre_1,6,5).

        ASSIGN i-linha = 49.
    FOR EACH depend_func 
       WHERE depend_func.cdn_empresa     = funcionario.cdn_empresa
         AND depend_func.cdn_estab       = funcionario.cdn_estab
         AND depend_func.cdn_funcionario = funcionario.cdn_funcionario 
         NO-LOCK
        BREAK BY depend_func.cdn_funcionario:

        ASSIGN chExcelApplication:range( "c" + STRING(i-linha) ):value = depend_func.nom_depend_func
               chExcelApplication:range( "f" + STRING(i-linha) ):value = depend_func.dat_nascimento.

        IF  INT(idi_grau_depen_fun) = 1 THEN                                                      
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "FILHO".             
        ELSE                                                                                      
             IF  INT(idi_grau_depen_fun) = 2 THEN                                                 
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "CONJUGE".           
        ELSE                                                                                      
         IF  INT(idi_grau_depen_fun) = 3 THEN                                                     
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "PAIS".              
        ELSE                                                                                      
         IF  INT(idi_grau_depen_fun) = 4 THEN                                                     
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "COMPANHEIRO".       
        ELSE                                                                                      
         IF  INT(idi_grau_depen_fun) = 5 THEN                                                     
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "DEPEND.ECONOM.".       
        ELSE                                                                                      
         IF  INT(idi_grau_depen_fun) = 6 THEN                                                     
            ASSIGN chExcelApplication:range( "g" + STRING(i-linha) ):value = "CONSIGNADO".   

        ASSIGN i-linha = i-linha + 1.
        
    END.
       
   ASSIGN i-linha = i-linha + 1.
   assign  i-linha       = i-linha + 2
           i-conta-linha = i-conta-linha + 1
           c-range       = "A" + string(i-linha).
           chExcelApplication:Range(c-range):select.

    chExcelApplication:range("A1"):select.
    chExcelApplication:ActiveSheet:Enableselection ='1'.
    chExcelApplication:DisplayAlerts = false.

    put "Planilhas Geradas:"
        c-planilha-envio format "x(60)"   skip.

    chExcelApplication:range("A1"):select.

    assign chExcelApplication:visible = yes. /* Mostra a planilha Excel na tela */
   
    release object chWorkSheet.
    release object chWorkbook.
    release object chExcelApplication.
    
 END.                                 
 
 ASSIGN fi-texto:SCREEN-VALUE = "Fim de Processamento !!!".
      
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-planilha-envio C-Win 
PROCEDURE pi-planilha-envio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /*find first usuar_mestre no-lock
         where usuar_mestre.cod_usuario = c-seg-usuario no-error.

    if  avail usuar_mestre then do:
        assign c-planilha-envio = usuar_mestre.nom_dir_spool + '\' +
                                  (if  usuar_mestre.nom_subdir_spool <> '' then
                                       usuar_mestre.nom_subdir_spool + '\'
                                  else '') + 'Pedido_' +
                                  trim(string(pedido-compr.num-pedido)) + '.xls'.
    end.  
    else*/  
        /*assign c-planilha-envio = "d:\temp\20199917\HCM_" +
                                  trim(string(funcionario.cdn_funcio)) + ".xls".

    ASSIGN
        c-planilha-envio = replace(c-planilha-envio,'\','/').

    if  search(c-planilha-envio) <> "" and
        search(c-planilha-envio) <> ? then
        dos silent del value(replace(c-planilha-envio,'/','\')).
          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

