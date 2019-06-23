&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    ARQUIVO: 
    DESCRI«√O: 
    AUTOR: Rodrigo Baione
    DATA: 24/08/2009
    OBJETIVO: Agrupamento de Informaá‰es Fiscais
------------------------------------------------------------------------*/
CREATE WIDGET-POOL.

/* ----------> DEFINIÄ«O DE VARIAVEIS LOCAIS <---------- */ 
DEF VAR chExcelApplication AS COM-HANDLE       NO-UNDO.
DEF VAR chWorkbook         AS COM-HANDLE       NO-UNDO.
DEF var chworksheet        AS COM-HANDLE       NO-UNDO.
DEF VAR c-file-name        AS CHAR             NO-UNDO.
DEF VAR c-sintegra         AS CHAR FORMAT "X(2500)" NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.

define temp-table tt-planilha
    FIELD data         AS date
    FIELD esp          AS CHAR
    FIELD serie        AS CHAR
    FIELD nr-nota-fisc AS CHAR
    FIELD data-doc     AS date
    FIELD cod-emit     AS INT
    FIELD cnpj         AS CHAR
    FIELD uf-ori       AS CHAR
    FIELD contabil     AS DEC
    FIELD fiscal       AS DEC
    FIELD cfop         AS INT
    FIELD imp          AS CHAR
    FIELD bc           AS DEC
    FIELD aliq         AS DEC
    FIELD imp-cred     AS DEC
    FIELD isentas      AS DEC
    FIELD outras       AS DEC
    index ind-plan-ent nr-nota-fisc cnpj
    INDEX ind-plan-sai nr-nota-fisc serie.

define temp-table tt-sintegra
    FIELD tipo            AS CHAR
    FIELD cnpj            AS CHAR
    FIELD modelo          AS CHAR
    FIELD serie           AS CHAR
    FIELD nr-nota-fisc    AS CHAR
    FIELD cfop            AS CHAR
    FIELD cst             AS CHAR
    FIELD nr-item         AS CHAR
    FIELD it-codigo       AS CHAR
    FIELD qtde            AS dec
    FIELD vl-prod         AS dec
    FIELD vl-desc         AS dec
    FIELD vl-bs-icms      AS dec
    FIELD vl-bd-icms-subs AS dec
    FIELD vl-ipi          AS dec
    FIELD aliq-icms       AS dec
    index ind-sintegra nr-nota-fisc cnpj.

DEF VAR c-arquivo   AS CHAR NO-UNDO.
DEF VAR l-retorno   AS LOG  NO-UNDO.
DEF VAR i-linha     AS INT  NO-UNDO.
DEF VAR i-linha-aux AS INT  NO-UNDO.

/* ------------------------------------------------------------------------- *\
|*                          VARIAVEIS EXCEL                                  *|
\* ------------------------------------------------------------------------- */
DEFINE VARIABLE vchExcel          AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE vchWorkBook       AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE vchWorkSheet      AS COM-HANDLE NO-UNDO. 
DEFINE VARIABLE viLinha           AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-4 rd-tipo-livro c-plan-livro ~
bt-plan i-linha-ini i-linha-fin c-arq-sintegra bt-sintegra c-plan-agrup ~
bt-agrup bt-executar bt-fechar 
&Scoped-Define DISPLAYED-OBJECTS rd-tipo-livro c-plan-livro i-linha-ini ~
i-linha-fin c-arq-sintegra c-plan-agrup 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-agrup 
     IMAGE-UP FILE "adeicon/open.bmp":U
     IMAGE-DOWN FILE "adeicon/open.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon/open.bmp":U
     LABEL "" 
     SIZE 4 BY .88 TOOLTIP "Determine o nome da planilha a ser gerada".

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13 TOOLTIP "Executar Impress∆o".

DEFINE BUTTON bt-fechar 
     LABEL "Fechar" 
     SIZE 15 BY 1.13 TOOLTIP "Fechar".

DEFINE BUTTON bt-plan 
     IMAGE-UP FILE "adeicon/open.bmp":U
     IMAGE-DOWN FILE "adeicon/open.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon/open.bmp":U
     LABEL "" 
     SIZE 4 BY .88 TOOLTIP "Clique para buscar a planilha".

DEFINE BUTTON bt-sintegra 
     IMAGE-UP FILE "adeicon/open.bmp":U
     IMAGE-DOWN FILE "adeicon/open.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon/open.bmp":U
     LABEL "" 
     SIZE 4 BY .88 TOOLTIP "Clique para localizar o arquivo do SINTEGRA a ser utilizado".

DEFINE VARIABLE c-arq-sintegra AS CHARACTER FORMAT "x(250)":U 
     LABEL "Arq. SINTEGRA" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88 TOOLTIP "Arquivo de texto contendo as informaá‰es enviadas ao SINTEGRA" NO-UNDO.

DEFINE VARIABLE c-plan-agrup AS CHARACTER FORMAT "x(250)":U 
     LABEL "Planilha - Agrupada" 
     VIEW-AS FILL-IN 
     SIZE 31.72 BY .88 TOOLTIP "Determine o local que ser† gravada a planilha contendo as informaá‰es agrupadas" NO-UNDO.

DEFINE VARIABLE c-plan-livro AS CHARACTER FORMAT "x(250)":U 
     LABEL "Planilha - Livro" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88 TOOLTIP "Planilha contendo informaá‰es do Livros Fiscais de Entrada" NO-UNDO.

DEFINE VARIABLE i-linha-fin AS INTEGER FORMAT ">>>>>>>>>>>9":U INITIAL 65536 
     LABEL "Linha Final" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 TOOLTIP "N£mero da linha em que inciam os dados das NFs na planilha" NO-UNDO.

DEFINE VARIABLE i-linha-ini AS INTEGER FORMAT ">>>>>>>>>>>9":U INITIAL 1 
     LABEL "Linha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 TOOLTIP "N£mero da linha em que inciam os dados das NFs na planilha" NO-UNDO.

DEFINE VARIABLE rd-tipo-livro AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Livro de Entrada", 1,
"Livro de Sa°da", 2
     SIZE 43 BY 1 TOOLTIP "Determina se o livro utilizado Ç de Entrada ou Sa°da" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63.43 BY 1.58
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.43 BY 6.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rd-tipo-livro AT ROW 1.75 COL 4.14 NO-LABEL WIDGET-ID 16
     c-plan-livro AT ROW 3 COL 17.57 COLON-ALIGNED WIDGET-ID 6 DISABLE-AUTO-ZAP 
     bt-plan AT ROW 3 COL 54.86 WIDGET-ID 26
     i-linha-ini AT ROW 4.08 COL 17.57 COLON-ALIGNED WIDGET-ID 20 DISABLE-AUTO-ZAP 
     i-linha-fin AT ROW 4.08 COL 39 COLON-ALIGNED WIDGET-ID 22 DISABLE-AUTO-ZAP 
     c-arq-sintegra AT ROW 5.17 COL 17.57 COLON-ALIGNED WIDGET-ID 30 DISABLE-AUTO-ZAP 
     bt-sintegra AT ROW 5.17 COL 54.86 WIDGET-ID 28
     c-plan-agrup AT ROW 6.29 COL 20.43 COLON-ALIGNED WIDGET-ID 24 DISABLE-AUTO-ZAP 
     bt-agrup AT ROW 6.33 COL 54.86 WIDGET-ID 36
     bt-executar AT ROW 8.17 COL 3.43
     bt-fechar AT ROW 8.17 COL 19.43
     RECT-1 AT ROW 7.96 COL 1.57
     RECT-4 AT ROW 1.33 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.86 BY 8.75.


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
         TITLE              = "ESOF003 - Agrupamento Inf. SINTEGRA e Livros"
         HEIGHT             = 8.75
         WIDTH              = 64.43
         MAX-HEIGHT         = 30.75
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.75
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ESOF003 - Agrupamento Inf. SINTEGRA e Livros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ESOF003 - Agrupamento Inf. SINTEGRA e Livros */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-agrup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-agrup C-Win
ON CHOOSE OF bt-agrup IN FRAME DEFAULT-FRAME
DO:
  SYSTEM-DIALOG GET-FILE c-arquivo
      FILTERS "Planilha Excel (*.xl*)" "*.xl*"
      ASK-OVERWRITE
      CREATE-TEST-FILE
      DEFAULT-EXTENSION ".xls"
      INITIAL-DIR "C:\"
      SAVE-AS
      TITLE "Determine a planilha a ser gerada"
      UPDATE l-retorno.

    IF l-retorno THEN 
      c-plan-agrup:SCREEN-VALUE = c-arquivo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME DEFAULT-FRAME /* Executar */
DO:
  run pi-validate.

  run utp/ut-acomp.p persistent set h-acomp.

  run pi-inicializar in h-acomp (input RETURN-VALUE).

  RUN pi-processa.

  run pi-finalizar in h-acomp.
      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fechar C-Win
ON CHOOSE OF bt-fechar IN FRAME DEFAULT-FRAME /* Fechar */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-plan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-plan C-Win
ON CHOOSE OF bt-plan IN FRAME DEFAULT-FRAME
DO:
  SYSTEM-DIALOG GET-FILE c-arquivo
      FILTERS "Planilha Excel (*.xl*)" "*.xl*"
      ASK-OVERWRITE
      CREATE-TEST-FILE
      DEFAULT-EXTENSION ".xls"
      INITIAL-DIR "C:\"
     /* SAVE-AS*/
      TITLE "Indique a planilha de Livro Fiscal"
      UPDATE l-retorno.

    IF l-retorno THEN 
      c-plan-livro:SCREEN-VALUE = c-arquivo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sintegra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sintegra C-Win
ON CHOOSE OF bt-sintegra IN FRAME DEFAULT-FRAME
DO:
  SYSTEM-DIALOG GET-FILE c-arquivo
      FILTERS "Arquivo Texto (*.txt)" "*.txt",
              "Todos os Arquivos (*.*)" "*.*"
      ASK-OVERWRITE
      CREATE-TEST-FILE
      DEFAULT-EXTENSION ".txt"
      INITIAL-DIR "C:\"
     /* SAVE-AS*/
      TITLE "Indique o arquivo do SINTEGRA"
      UPDATE l-retorno.

    IF l-retorno THEN 
      c-arq-sintegra:SCREEN-VALUE = c-arquivo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-arq-sintegra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arq-sintegra C-Win
ON LEAVE OF c-arq-sintegra IN FRAME DEFAULT-FRAME /* Arq. SINTEGRA */
DO:
/*   assign i-periodo-fin:screen-value in frame {&frame-name} = i-periodo-ini:screen-value in frame {&frame-name}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-plan-agrup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-plan-agrup C-Win
ON LEAVE OF c-plan-agrup IN FRAME DEFAULT-FRAME /* Planilha - Agrupada */
DO:
/*   assign i-periodo-fin:screen-value in frame {&frame-name} = i-periodo-ini:screen-value in frame {&frame-name}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-plan-livro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-plan-livro C-Win
ON LEAVE OF c-plan-livro IN FRAME DEFAULT-FRAME /* Planilha - Livro */
DO:
/*   assign i-periodo-fin:screen-value in frame {&frame-name} = i-periodo-ini:screen-value in frame {&frame-name}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-linha-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-linha-fin C-Win
ON LEAVE OF i-linha-fin IN FRAME DEFAULT-FRAME /* Linha Final */
DO:
/*   assign i-periodo-fin:screen-value in frame {&frame-name} = i-periodo-ini:screen-value in frame {&frame-name}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-linha-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-linha-ini C-Win
ON LEAVE OF i-linha-ini IN FRAME DEFAULT-FRAME /* Linha Inicial */
DO:
/*   assign i-periodo-fin:screen-value in frame {&frame-name} = i-periodo-ini:screen-value in frame {&frame-name}.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
  DISPLAY rd-tipo-livro c-plan-livro i-linha-ini i-linha-fin c-arq-sintegra 
          c-plan-agrup 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-4 rd-tipo-livro c-plan-livro bt-plan i-linha-ini 
         i-linha-fin c-arq-sintegra bt-sintegra c-plan-agrup bt-agrup 
         bt-executar bt-fechar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tts C-Win 
PROCEDURE pi-carrega-tts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
INPUT FROM VALUE (INPUT FRAME {&FRAME-NAME} c-arq-sintegra) NO-CONVERT.

RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando dados do SINTEGRA ").

REPEAT:

  IMPORT DELIMITER "^"
      c-sintegra.

  RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando dados do SINTEGRA ").

  if SUBSTRING(c-sintegra,1,2) = "54" then do:
  
      CREATE tt-sintegra.
      ASSIGN tt-sintegra.tipo            = SUBSTRING(c-sintegra,1,2)
             tt-sintegra.cnpj            = SUBSTRING(c-sintegra,3,14) 
             tt-sintegra.modelo          = SUBSTRING(c-sintegra,17,2) 
             tt-sintegra.serie           = SUBSTRING(c-sintegra,19,3) 
             tt-sintegra.nr-nota-fisc    = SUBSTRING(c-sintegra,22,6) 
             tt-sintegra.cfop            = SUBSTRING(c-sintegra,28,4) 
             tt-sintegra.cst             = SUBSTRING(c-sintegra,32,3) 
             tt-sintegra.nr-item         = SUBSTRING(c-sintegra,35,3) 
             tt-sintegra.it-codigo       = SUBSTRING(c-sintegra,38,14) 
             tt-sintegra.qtde            = dec(SUBSTRING(c-sintegra,52,8) + "," + SUBSTRING(c-sintegra,60,3))
             tt-sintegra.vl-prod         = dec(SUBSTRING(c-sintegra,63,10) + "," + SUBSTRING(c-sintegra,73,2) )
             tt-sintegra.vl-desc         = dec(SUBSTRING(c-sintegra,75,10) + "," + SUBSTRING(c-sintegra,85,2))
             tt-sintegra.vl-bs-icms      = dec(SUBSTRING(c-sintegra,87,10) + "," + SUBSTRING(c-sintegra,97,2))
             tt-sintegra.vl-bd-icms-subs = dec(SUBSTRING(c-sintegra,99,10) + "," + SUBSTRING(c-sintegra,109,2))
             tt-sintegra.vl-ipi          = dec(SUBSTRING(c-sintegra,111,10) + "," + SUBSTRING(c-sintegra,121,2))
             tt-sintegra.aliq-icms       = dec(SUBSTRING(c-sintegra,123,2) + "," + SUBSTRING(c-sintegra,125,2)).

      ASSIGN tt-sintegra.tipo            = TRIM(tt-sintegra.tipo)           
             tt-sintegra.cnpj            = TRIM(tt-sintegra.cnpj)          
             tt-sintegra.modelo          = TRIM(tt-sintegra.modelo)         
             tt-sintegra.serie           = TRIM(tt-sintegra.serie)          
             tt-sintegra.nr-nota-fisc    = TRIM(tt-sintegra.nr-nota-fisc)   
             tt-sintegra.cfop            = TRIM(tt-sintegra.cfop)           
             tt-sintegra.cst             = TRIM(tt-sintegra.cst)            
             tt-sintegra.nr-item         = TRIM(tt-sintegra.nr-item)        
             tt-sintegra.it-codigo       = TRIM(tt-sintegra.it-codigo)      
             . 
  end.

END.

IF INT(INPUT FRAME {&FRAME-NAME} rd-tipo-livro) = 1 THEN DO:

    ASSIGN c-file-name = "modelos\ESOF003_ENTRADAS.xlt".
    
    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando Livro - NF ").

    CREATE "Excel.Application":U vchExcel. 
    assign vchExcel:VISIBLE = FALSE  
           vchWorkBook      = vchExcel:Workbooks:OPEN(INPUT FRAME {&FRAME-NAME} c-plan-livro) 
           vchWorkSheet     = vchExcel:Sheets:Item(1). 
    
    REPEAT viLinha = 4 TO INT(i-linha-fin:SCREEN-VALUE):

        if STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999") = ? then
            leave.

        RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando Livro - NF "  + STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999")).
        CREATE tt-planilha.
        ASSIGN tt-planilha.data         = date(STRING(vchWorkSheet:Range("A":U + STRING(viLinha)):VALUE))
               tt-planilha.esp          = STRING(vchWorkSheet:Range("B":U + STRING(viLinha)):VALUE)                                                   
               tt-planilha.serie        = STRING(vchWorkSheet:Range("C":U + STRING(viLinha)):VALUE)                                                   
               tt-planilha.nr-nota-fisc = STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999")                                  
               tt-planilha.data-doc     = date(STRING(vchWorkSheet:Range("E":U + STRING(viLinha)):VALUE))
               tt-planilha.cod-emit     = INT(vchWorkSheet:Range("F":U + STRING(viLinha)):VALUE)                                          
               tt-planilha.cnpj         = string(vchWorkSheet:Range("G":U + STRING(viLinha)):VALUE)                                               
               tt-planilha.uf-ori       = STRING(vchWorkSheet:Range("H":U + STRING(viLinha)):VALUE)
               tt-planilha.contabil     = DEC(vchWorkSheet:Range("I":U + STRING(viLinha)):VALUE)                                              
               tt-planilha.fiscal       = DEC(vchWorkSheet:Range("J":U + STRING(viLinha)):VALUE)                                              
               tt-planilha.cfop         = INT(vchWorkSheet:Range("K":U + STRING(viLinha)):VALUE)                                         
               tt-planilha.imp          = vchWorkSheet:Range("L":U + STRING(viLinha)):VALUE
               tt-planilha.bc           = DEC(vchWorkSheet:Range("N":U + STRING(viLinha)):VALUE)                                              
               tt-planilha.aliq         = DEC(vchWorkSheet:Range("O":U + STRING(viLinha)):VALUE)                                          
               tt-planilha.imp-cred     = DEC(vchWorkSheet:Range("P":U + STRING(viLinha)):VALUE)                                              
               tt-planilha.cnpj         = REPLACE(REPLACE(REPLACE(REPLACE(tt-planilha.cnpj,"/",""),".",""),"\",""),"-","")
            . 
        

    END.            

END.
ELSE DO:
    ASSIGN c-file-name = "modelos\ESOF003_SAIDAS.xlt".
    
    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando Livro - NF ").

    CREATE "Excel.Application":U vchExcel. 
    ASSIGN
      vchExcel:VISIBLE = false
      vchWorkBook      = vchExcel:Workbooks:OPEN(INPUT FRAME {&FRAME-NAME} c-plan-livro) 
      vchWorkSheet     = vchExcel:Sheets:Item(1). 
    
    REPEAT viLinha = 3 TO INT(i-linha-fin:SCREEN-VALUE): 

     if STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999") = ? then
            leave.

     RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, carregando Livro - NF "  + STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999")).
     CREATE tt-planilha.
     ASSIGN tt-planilha.data         = date(vchWorkSheet:Range("A":U + STRING(viLinha)):value)
            tt-planilha.esp          = vchWorkSheet:Range("B":U + STRING(viLinha)):VALUE 
            tt-planilha.serie        = IF vchWorkSheet:Range("C":U + STRING(viLinha)):VALUE = "S2" THEN "2" ELSE vchWorkSheet:Range("C":U + STRING(viLinha)):value
            tt-planilha.nr-nota-fisc = STRING(vchWorkSheet:Range("D":U + STRING(viLinha)):VALUE,"999999")
            tt-planilha.data-doc     = date(vchWorkSheet:Range("E":U + STRING(viLinha)):VALUE)
            tt-planilha.uf-ori       = vchWorkSheet:Range("F":U + STRING(viLinha)):VALUE
            tt-planilha.contabil     = dec(vchWorkSheet:Range("G":U + STRING(viLinha)):VALUE)
            tt-planilha.cfop         = INT(vchWorkSheet:Range("I":U + STRING(viLinha)):VALUE)
            tt-planilha.bc           = DEC(vchWorkSheet:Range("J":U + STRING(viLinha)):VALUE)
            tt-planilha.aliq         = DEC(vchWorkSheet:Range("K":U + STRING(viLinha)):VALUE)
            tt-planilha.imp-cred     = vchWorkSheet:Range("L":U + STRING(viLinha)):VALUE
            tt-planilha.isentas      = DEC(vchWorkSheet:Range("M":U + STRING(viLinha)):VALUE)
            tt-planilha.outras       = DEC(vchWorkSheet:Range("N":U + STRING(viLinha)):VALUE)        
            tt-planilha.esp          = TRIM(tt-planilha.esp)         
            tt-planilha.serie        = TRIM(tt-planilha.serie)       
            tt-planilha.nr-nota-fisc = TRIM(tt-planilha.nr-nota-fisc)
            tt-planilha.uf-ori       = TRIM(tt-planilha.uf-ori)
            tt-planilha.cnpj         = TRIM(REPLACE(REPLACE(REPLACE(REPLACE(tt-planilha.cnpj,"/",""),".",""),"\",""),"-",""))        
            .      
    END.
    
END.

/* Elimina as handles e fecha a planilha */
IF VALID-HANDLE(vchWorkSheet) THEN 
  RELEASE OBJECT vchWorkSheet. 
IF VALID-HANDLE(vchWorkBook) then do:
    vchWorkBook:CLOSE(). 
    RELEASE OBJECT vchWorkBook.
end.
IF VALID-HANDLE(vchExcel) THEN do:
    vchExcel:QUIT.  
    RELEASE OBJECT vchExcel. 
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-agrup-ent C-Win 
PROCEDURE pi-monta-agrup-ent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-planilha
    WHERE tt-planilha.nr-nota-fisc <> ?
    BREAK BY tt-planilha.data-doc
          BY tt-planilha.nr-nota-fisc:
  
    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, Gerando Planilha "  + STRING(tt-planilha.nr-nota-fisc)).

    ASSIGN i-linha = i-linha + 1.

    ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):VALUE   = tt-planilha.data              
           chexcelapplication:Range("B" + STRING(i-linha)):VALUE   = tt-planilha.esp               
           chexcelapplication:Range("C" + STRING(i-linha)):VALUE   = tt-planilha.serie             
           chexcelapplication:Range("D" + STRING(i-linha)):VALUE   = tt-planilha.nr-nota-fisc      
           chexcelapplication:Range("E" + STRING(i-linha)):VALUE   = tt-planilha.data-doc          
           chexcelapplication:Range("F" + STRING(i-linha)):VALUE   = tt-planilha.cod-emit    
           chexcelapplication:Range("G" + STRING(i-linha)):VALUE   = STRING(tt-planilha.cnpj,"99.999.999/9999-99")
           chexcelapplication:Range("H" + STRING(i-linha)):VALUE   = tt-planilha.uf-ori            
           chexcelapplication:Range("I" + STRING(i-linha)):VALUE   = tt-planilha.contabil          
           chexcelapplication:Range("J" + STRING(i-linha)):VALUE   = tt-planilha.fiscal            
           chexcelapplication:Range("K" + STRING(i-linha)):VALUE   = tt-planilha.cfop              
           chexcelapplication:Range("L" + STRING(i-linha)):VALUE   = tt-planilha.imp               
           chexcelapplication:Range("M" + STRING(i-linha)):VALUE   = tt-planilha.bc                
           chexcelapplication:Range("N" + STRING(i-linha)):VALUE   = tt-planilha.aliq              
           chexcelapplication:Range("O" + STRING(i-linha)):VALUE   = tt-planilha.imp-cred
        . 
     
         FOR EACH  tt-sintegra
            WHERE tt-sintegra.tipo = "54"
            AND   tt-sintegra.cnpj = tt-planilha.cnpj
            AND   tt-sintegra.nr-nota-fisc = tt-planilha.nr-nota-fisc
            BREAK BY tt-sintegra.nr-nota-fisc:
            
            RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, Gerando Planilha "  + STRING(tt-sintegra.nr-nota-fisc)).

            ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):VALUE   = tt-planilha.data              
                   chexcelapplication:Range("B" + STRING(i-linha)):VALUE   = tt-planilha.esp               
                   chexcelapplication:Range("C" + STRING(i-linha)):VALUE   = tt-planilha.serie             
                   chexcelapplication:Range("D" + STRING(i-linha)):VALUE   = tt-planilha.nr-nota-fisc      
                   chexcelapplication:Range("E" + STRING(i-linha)):VALUE   = tt-planilha.data-doc          
                   chexcelapplication:Range("F" + STRING(i-linha)):VALUE   = tt-planilha.cod-emit    
                   chexcelapplication:Range("G" + STRING(i-linha)):VALUE   = STRING(tt-planilha.cnpj,"99.999.999/9999-99")
                   chexcelapplication:Range("H" + STRING(i-linha)):VALUE   = tt-planilha.uf-ori            
/*                    chexcelapplication:Range("I" + STRING(i-linha)):VALUE   = tt-planilha.contabil */
                   chexcelapplication:Range("J" + STRING(i-linha)):VALUE   = tt-planilha.fiscal            
                   chexcelapplication:Range("K" + STRING(i-linha)):VALUE   = tt-planilha.cfop              
                   chexcelapplication:Range("L" + STRING(i-linha)):VALUE   = tt-planilha.imp               
                   chexcelapplication:Range("M" + STRING(i-linha)):VALUE   = tt-planilha.bc                
                   chexcelapplication:Range("N" + STRING(i-linha)):VALUE   = tt-planilha.aliq              
                   chexcelapplication:Range("O" + STRING(i-linha)):VALUE   = tt-planilha.imp-cred
                   chexcelapplication:Range("P" + STRING(i-linha)):VALUE   = tt-sintegra.cst              
                   chexcelapplication:Range("Q" + STRING(i-linha)):VALUE   = tt-sintegra.nr-item               
                   chexcelapplication:Range("R" + STRING(i-linha)):VALUE   = tt-sintegra.it-codigo        
                   chexcelapplication:Range("S" + STRING(i-linha)):VALUE   = tt-sintegra.qtde             
                   chexcelapplication:Range("T" + STRING(i-linha)):VALUE   = tt-sintegra.vl-prod               
                   chexcelapplication:Range("U" + STRING(i-linha)):VALUE   = tt-sintegra.vl-desc          
                   chexcelapplication:Range("V" + STRING(i-linha)):VALUE   = tt-sintegra.vl-bs-icms       
                   chexcelapplication:Range("W" + STRING(i-linha)):VALUE   = tt-sintegra.vl-bd-icms-subs       
                   chexcelapplication:Range("x" + STRING(i-linha)):VALUE   = tt-sintegra.vl-ipi                
                   chexcelapplication:Range("y" + STRING(i-linha)):VALUE   = tt-sintegra.aliq-icms
                . 

            ASSIGN i-linha = i-linha + 1.

            IF LAST-OF(tt-sintegra.nr-nota-fisc) THEN
                ASSIGN i-linha = i-linha - 1.
         END.
     
END.

/*Borda*/                         
chexcelapplication:Range("A2:Y"  + STRING(i-linha)):Borders():LineStyle  = 1.
/*Auto Ajuste de Coluna*/         
chexcelapplication:Range("A2:Y"  + STRING(i-linha)):EntireColumn:AutoFit.
/*Formato Data para cÇlulas*/
chexcelapplication:Range("A2:A" + STRING(i-linha)):NumberFormat = "dd/mm/aaaa;@".
chexcelapplication:Range("E2:E" + STRING(i-linha)):NumberFormat = "dd/mm/aaaa;@".
/*Formato N£mero com casas decimais para cÇlulas*/
chexcelapplication:Range("I2:I"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("J2:J"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("M2:M"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("N2:N"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("O2:O"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("S2:S"   + STRING(i-linha)):NumberFormat = "###.##0,000".
chexcelapplication:Range("T2:T"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("U2:U"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("V2:V"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("W2:W"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("x2:x"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("y2:y"   + STRING(i-linha)):NumberFormat = "###.##0,00".
/*Formato CNPJ para cÇlulas*/
/* chexcelapplication:Range("G2:G"   + STRING(i-linha)):NumberFormat = "00\.000\.000\/0000-00".  */
/*Protege a Planilha*/
/* chExcelApplication:ActiveSheet:Protect("YAMANAGOLD"). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-agrup-sai C-Win 
PROCEDURE pi-monta-agrup-sai :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-planilha
    WHERE tt-planilha.nr-nota-fisc <> ?
    BREAK BY tt-planilha.nr-nota-fisc
          BY tt-planilha.data-doc:
  
    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, Gerando Planilha "  + STRING(tt-planilha.nr-nota-fisc)).

    ASSIGN i-linha = i-linha + 1.

    ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):VALUE   = tt-planilha.data
           chexcelapplication:Range("B" + STRING(i-linha)):VALUE   = tt-planilha.esp
           chexcelapplication:Range("C" + STRING(i-linha)):VALUE   = tt-planilha.serie
           chexcelapplication:Range("D" + STRING(i-linha)):VALUE   = tt-planilha.nr-nota-fisc
           chexcelapplication:Range("E" + STRING(i-linha)):VALUE   = tt-planilha.data-doc
           chexcelapplication:Range("F" + STRING(i-linha)):VALUE   = tt-planilha.uf-ori
           chexcelapplication:Range("G" + STRING(i-linha)):VALUE   = tt-planilha.contabil
           chexcelapplication:Range("H" + STRING(i-linha)):VALUE   = tt-planilha.cfop
           chexcelapplication:Range("I" + STRING(i-linha)):VALUE   = tt-planilha.bc
           chexcelapplication:Range("J" + STRING(i-linha)):VALUE   = tt-planilha.aliq
           chexcelapplication:Range("K" + STRING(i-linha)):VALUE   = tt-planilha.imp-cred
           chexcelapplication:Range("L" + STRING(i-linha)):VALUE   = tt-planilha.isentas
           chexcelapplication:Range("M" + STRING(i-linha)):VALUE   = tt-planilha.outras
           .

        FOR EACH  tt-sintegra
            WHERE tt-sintegra.tipo = "54"
            AND   tt-sintegra.nr-nota-fisc = tt-planilha.nr-nota-fisc 
            AND   tt-sintegra.serie        = tt-planilha.serie        
            BREAK BY tt-sintegra.nr-nota-fisc:
            
            RUN pi-acompanhar IN h-acomp (INPUT "Aguarde, Gerando Planilha "  + STRING(tt-sintegra.nr-nota-fisc)).

            ASSIGN chexcelapplication:Range("A" + STRING(i-linha)):VALUE   = tt-planilha.data        
                   chexcelapplication:Range("B" + STRING(i-linha)):VALUE   = tt-planilha.esp         
                   chexcelapplication:Range("C" + STRING(i-linha)):VALUE   = tt-planilha.serie       
                   chexcelapplication:Range("D" + STRING(i-linha)):VALUE   = tt-planilha.nr-nota-fisc
                   chexcelapplication:Range("E" + STRING(i-linha)):VALUE   = tt-planilha.data-doc    
                   chexcelapplication:Range("F" + STRING(i-linha)):VALUE   = tt-planilha.uf-ori      
/*                    chexcelapplication:Range("G" + STRING(i-linha)):VALUE   = tt-planilha.contabil */
                   chexcelapplication:Range("H" + STRING(i-linha)):VALUE   = tt-planilha.cfop        
                   chexcelapplication:Range("I" + STRING(i-linha)):VALUE   = tt-planilha.bc          
                   chexcelapplication:Range("J" + STRING(i-linha)):VALUE   = tt-planilha.aliq        
                   chexcelapplication:Range("K" + STRING(i-linha)):VALUE   = tt-planilha.imp-cred    
                   chexcelapplication:Range("L" + STRING(i-linha)):VALUE   = tt-planilha.isentas     
/*                    chexcelapplication:Range("M" + STRING(i-linha)):VALUE   = tt-planilha.outras */
                   chexcelapplication:Range("N" + STRING(i-linha)):VALUE   = tt-sintegra.cst
                   chexcelapplication:Range("O" + STRING(i-linha)):VALUE   = tt-sintegra.nr-item
                   chexcelapplication:Range("P" + STRING(i-linha)):VALUE   = tt-sintegra.it-codigo
                   chexcelapplication:Range("Q" + STRING(i-linha)):VALUE   = tt-sintegra.qtde
                   chexcelapplication:Range("R" + STRING(i-linha)):VALUE   = tt-sintegra.vl-prod
                   chexcelapplication:Range("S" + STRING(i-linha)):VALUE   = tt-sintegra.vl-desc
                   chexcelapplication:Range("T" + STRING(i-linha)):VALUE   = tt-sintegra.vl-bs-icms
                   chexcelapplication:Range("U" + STRING(i-linha)):VALUE   = tt-sintegra.vl-bd-icms-subs
                   chexcelapplication:Range("V" + STRING(i-linha)):VALUE   = tt-sintegra.vl-ipi
                   chexcelapplication:Range("W" + STRING(i-linha)):VALUE   = tt-sintegra.aliq-icms
                  .
  
            ASSIGN i-linha = i-linha + 1.

            IF LAST-OF(tt-sintegra.nr-nota-fisc) THEN
                ASSIGN i-linha = i-linha - 1.
         END.

end.

/*Borda*/                         
chexcelapplication:Range("A2:W"  + STRING(i-linha)):Borders():LineStyle  = 1.
/*Auto Ajuste de Coluna*/         
chexcelapplication:Range("A2:Y"  + STRING(i-linha)):EntireColumn:AutoFit.
/*Formato Data para cÇlulas*/
chexcelapplication:Range("A2:A" + STRING(i-linha)):NumberFormat = "dd/mm/aaaa;@".
chexcelapplication:Range("E2:E" + STRING(i-linha)):NumberFormat = "dd/mm/aaaa;@".
/*Formato N£mero com casas decimais para cÇlulas*/
chexcelapplication:Range("G2:G"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("I2:I"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("J2:J"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("K2:K"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("L2:L"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("M2:M"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("Q2:Q"   + STRING(i-linha)):NumberFormat = "###.##0,000".
chexcelapplication:Range("R2:R"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("S2:S"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("T2:T"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("U2:U"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("V2:V"   + STRING(i-linha)):NumberFormat = "###.##0,00".
chexcelapplication:Range("W2:W"   + STRING(i-linha)):NumberFormat = "###.##0,00".
/*Protege a Planilha*/
/* chExcelApplication:ActiveSheet:Protect("YAMANAGOLD"). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa C-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN i-linha = 1.

empty temp-table tt-planilha.
empty temp-table tt-sintegra.

run pi-carrega-tts.

/*Inicializa Excel*/

CREATE "Excel.Application" chExcelApplication.
FILE-INFO:FILE-NAME = c-file-name.
chexcelapplication:workbooks:OPEN(FILE-INFO:FULL-PATHNAME,TRUE).
chexcelapplication:sheets:ITEM(1).
chexcelapplication:sheets:ITEM(1):activate.
chexcelapplication:APPLICATION:DisplayAlerts = FALSE.
chExcelApplication:ScreenUpdating = no.


IF INT(INPUT FRAME {&FRAME-NAME} rd-tipo-livro) = 1 THEN
    RUN pi-monta-agrup-ent.
ELSE
    RUN pi-monta-agrup-sai.

 /* Finalizar Excel */
chexcelapplication:Range("A1"):SELECT.
chExcelApplication:ScreenUpdating = true.
chExcelApplication:VISIBLE = TRUE.
chExcelApplication:DisplayAlerts = FALSE.
chExcelApplication:ActiveWorkbook:SaveAs(INPUT FRAME {&FRAME-NAME} c-plan-agrup,,,,,,).
RELEASE OBJECT chExcelApplication.                                                                                                       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate C-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF INPUT FRAME {&FRAME-NAME} c-plan-livro = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Planilha em Branco!~~ê necess†rio informar o caminho de uma planilha de Livro de Entrada ou Sa°da!").
        
       APPLY 'entry' TO c-plan-livro IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF INPUT FRAME {&FRAME-NAME} i-linha-ini = 0 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Linha Inicial Zerada!~~ê necess†rio informar o n£mero da linha inicial da Planilha!").
        
       APPLY 'entry' TO i-linha-ini IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF INPUT FRAME {&FRAME-NAME} i-linha-fin = 0 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Linha Final Zerada!~~ê necess†rio informar o n£mero da linha final da Planilha!").
        
       APPLY 'entry' TO i-linha-fin IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF INPUT FRAME {&FRAME-NAME} i-linha-ini > INPUT FRAME {&FRAME-NAME} i-linha-fin THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Linhas da Planilha!~~O n£mero da linha final Ç menor do que o n£mero informado na linha inicial!").
        
       APPLY 'entry' TO i-linha-ini IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF INPUT FRAME {&FRAME-NAME} c-arq-sintegra = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Arq. SINTEGRA em Branco!~~ê necess†rio informar o caminho de um Arquivo do Sintegra!").
        
       APPLY 'entry' TO c-arq-sintegra IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF INPUT FRAME {&FRAME-NAME} c-plan-agrup = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Planilha em Branco!~~ê necess†rio informar o caminho e o nome da planilha a ser gerada!").
        
       APPLY 'entry' TO c-plan-agrup IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF NOT INPUT FRAME {&FRAME-NAME} c-plan-livro MATCHES("*xl*") THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Planilha Invalida!~~A planilha informada n∆o se refere a uma planilha do Excel(xls, xlsx, etc).").
        
       APPLY 'entry' TO c-plan-livro IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF NOT INPUT FRAME {&FRAME-NAME} c-arq-sintegra MATCHES("*txt*") THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Arquivo Inv†lido!~~O arquivo do SINTEGRA n∆o se refere a um arquivo de texto (txt).").
        
       APPLY 'entry' TO c-arq-sintegra IN FRAME {&frame-name}.
       RETURN ERROR.

END.

IF NOT INPUT FRAME {&FRAME-NAME} c-plan-agrup MATCHES("*xl*") THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Planilha Invalida!~~A planilha informada n∆o se refere a uma planilha do Excel(xls, xlsx, etc).").
        
       APPLY 'entry' TO c-plan-agrup IN FRAME {&frame-name}.
       RETURN ERROR.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

