&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    ARQUIVO: 
    DESCRI«√O: 
    AUTOR: 
    DATA:
    OBJETIVO: 
------------------------------------------------------------------------*/
create widget-pool.

/* ---------------> DEFINIÄ«O DE TEMP-TABLE <--------------- */
def temp-table tt-imp
         field cod-estabel like item.cod-estabel 
         field cod-depto   as char /*like es-it-depto.cod-depto */
         field it-codigo   like item.it-codigo                 
         field desc-item   like item.desc-item
         field fm-codigo   like item.fm-codigo
         field cod-depos   like movto-estoq.cod-depos
         field qtidade-atu like saldo-estoq.qtidade-atu
         field preco-medio like preco-item.preco-venda
         field valor-estoq like preco-item.preco-venda
         field dt-ult-sai  like saldo-estoq.data-ult-ent
         field esp-docto   as char /*like  movto-estoq.esp-docto */
         field dt-ult-ent  like saldo-estoq.data-ult-ent
         field docto       like movto-estoq.nro-docto
         FIELD fm-cod-com  LIKE ITEM.fm-cod-com
         FIELD narrativa   LIKE ITEM.narrativa.

/* ----------> DEFINIÄ«O DE VARIAVEIS LOCAIS <---------- */ 
define variable h-acomp             as handle                    no-undo.
define variable r-row-esp-prod-rec  as rowid                     no-undo.
define variable l-ok                as logical                   no-undo.
define variable chExcelApplication  as com-handle                no-undo.
define variable chWorkbook          as com-handle                no-undo.
define variable chWorkSheet         as com-handle                no-undo.
define variable chActiveWorkbook    as com-handle                no-undo.
define variable c-range             as char                      no-undo.
define variable c-range-aux         as char                      no-undo.
define variable i-celula            as int                       no-undo.
define variable i-linha             as int                       no-undo.
define variable c-destino           as character                 no-undo.
define variable l-nenhum            as logical                   no-undo.
define variable c-referencia        as character                 no-undo.
define variable c-aux-spool         as character format "x(100)" no-undo.
define variable c-aux-spool-aux     as character format "x(100)" no-undo.
define variable c-resp              as character                 no-undo.
define variable l-composto          as logical                   no-undo.
define variable l-operacao          as logical initial no        no-undo.
define variable gsvc-cod_programa   as char.
define variable c-planilha          as char                      no-undo.
DEFINE VARIABLE d-saldo-calculado   like saldo-estoq.qtidade-atu no-undo.

def var c-esp-docto as char no-undo.
define buffer bf-movto-estoq for movto-estoq.

ASSIGN
    FILE-INFO:FILE-NAME = "modelos\ModeloESCD004.xls"
    c-planilha          = FILE-INFO:FULL-PATHNAME.


/* ---------------> DEFINIÄ«O DE STREAM <--------------- */
define stream s-imp.

for each tt-imp:
  delete tt-imp.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 RECT-7 RECT-1 IMAGE-21 ~
IMAGE-22 IMAGE-25 IMAGE-26 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 ~
IMAGE-32 RECT-8 fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-fam-ini ~
fi-fam-fim fi-depto-ini fi-depto-fim fi-fm-cod-com-ini fi-fm-cod-com-fim ~
rd-descricao tg-saldo bt-executar bt-fechar 
&Scoped-Define DISPLAYED-OBJECTS fi-est-ini fi-est-fim fi-item-ini ~
fi-item-fim fi-fam-ini fi-fam-fim fi-depto-ini fi-depto-fim ~
fi-fm-cod-com-ini fi-fm-cod-com-fim rd-descricao tg-saldo rs-destino ~
c-arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 5.57 BY 1.21.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 5.57 BY 1.21.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13 TOOLTIP "Executar Impress∆o".

DEFINE BUTTON bt-fechar 
     LABEL "Fechar" 
     SIZE 15 BY 1.13 TOOLTIP "Fechar".

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-depto-fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 6 BY .79.

DEFINE VARIABLE fi-depto-ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Departamento" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .79.

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
     LABEL "Fam°lia":R16 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-cod-com-fim AS CHARACTER FORMAT "X(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-cod-com-ini AS CHARACTER FORMAT "X(8)" 
     LABEL "Familia Comercial" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R16 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

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

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rd-descricao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Listar Descriá∆o", 1,
"Listar Narrativa Completa", 2
     SIZE 27 BY 1.5 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Excel", 3
     SIZE 45.14 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.58
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.42.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.63.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 4.75.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.57 BY 3.13.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 2.

DEFINE VARIABLE tg-saldo AS LOGICAL INITIAL no 
     LABEL "Saldo Zerado" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.57 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-est-ini AT ROW 1.71 COL 23.43 COLON-ALIGNED WIDGET-ID 126
     fi-est-fim AT ROW 1.71 COL 53.43 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     fi-item-ini AT ROW 2.71 COL 23.43 COLON-ALIGNED WIDGET-ID 134
     fi-item-fim AT ROW 2.71 COL 53.43 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     fi-fam-ini AT ROW 3.71 COL 23.43 COLON-ALIGNED WIDGET-ID 130
     fi-fam-fim AT ROW 3.71 COL 53.43 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fi-depto-ini AT ROW 4.71 COL 23.43 COLON-ALIGNED HELP
          "C¢digo do departamento a que o Item foi vinculado" WIDGET-ID 122
     fi-depto-fim AT ROW 4.71 COL 53.43 COLON-ALIGNED HELP
          "C¢digo do departamento a que o Item foi vinculado" NO-LABEL WIDGET-ID 120
     fi-fm-cod-com-ini AT ROW 5.71 COL 23.43 COLON-ALIGNED HELP
          "Familia Comercial" WIDGET-ID 156
     fi-fm-cod-com-fim AT ROW 5.71 COL 53.43 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     rd-descricao AT ROW 8 COL 63 NO-LABEL WIDGET-ID 160
     tg-saldo AT ROW 8.21 COL 26.43 WIDGET-ID 82
     rs-destino AT ROW 11.79 COL 6.86 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL WIDGET-ID 46
     bt-arquivo AT ROW 13.04 COL 47 HELP
          "Escolha do nome do arquivo" WIDGET-ID 58
     bt-config-impr AT ROW 13.04 COL 47 HELP
          "Configuraá∆o da impressora" WIDGET-ID 50
     c-arquivo AT ROW 13.25 COL 6.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 52
     bt-executar AT ROW 15.33 COL 3.43 WIDGET-ID 60
     bt-fechar AT ROW 15.33 COL 19.43 WIDGET-ID 62
     text-destino AT ROW 10.79 COL 6 NO-LABEL WIDGET-ID 56
     "Descriá∆o Item" VIEW-AS TEXT
          SIZE 14.57 BY .67 AT ROW 7.38 COL 60.43 WIDGET-ID 166
     "Impress∆o" VIEW-AS TEXT
          SIZE 14 BY .63 AT ROW 10 COL 3 WIDGET-ID 18
          FONT 1
     "   Seleá∆o" VIEW-AS TEXT
          SIZE 13.57 BY .63 AT ROW 1.25 COL 3 WIDGET-ID 10
          FONT 1
     "Parametros" VIEW-AS TEXT
          SIZE 16 BY .63 AT ROW 7 COL 3 WIDGET-ID 14
          FONT 1
     RECT-2 AT ROW 1.5 COL 1.57 WIDGET-ID 8
     RECT-3 AT ROW 7.29 COL 1.43 WIDGET-ID 12
     RECT-4 AT ROW 10.29 COL 1.57 WIDGET-ID 16
     RECT-7 AT ROW 11.21 COL 4.43 WIDGET-ID 54
     RECT-1 AT ROW 15.08 COL 1.57 WIDGET-ID 64
     IMAGE-21 AT ROW 3.71 COL 43.86 WIDGET-ID 136
     IMAGE-22 AT ROW 3.71 COL 52.29 WIDGET-ID 138
     IMAGE-25 AT ROW 1.71 COL 43.86 WIDGET-ID 140
     IMAGE-26 AT ROW 2.71 COL 52.29 WIDGET-ID 142
     IMAGE-27 AT ROW 2.71 COL 43.86 WIDGET-ID 144
     IMAGE-28 AT ROW 1.71 COL 52.29 WIDGET-ID 146
     IMAGE-29 AT ROW 4.71 COL 43.86 WIDGET-ID 148
     IMAGE-30 AT ROW 4.71 COL 52.29 WIDGET-ID 150
     IMAGE-31 AT ROW 5.71 COL 43.86 WIDGET-ID 152
     IMAGE-32 AT ROW 5.71 COL 52.29 WIDGET-ID 154
     RECT-8 AT ROW 7.75 COL 60 WIDGET-ID 164
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.86 BY 19.58.


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
         TITLE              = "ESCD004 - Itens por Departamento com Saldo de Estoque"
         HEIGHT             = 15.83
         WIDTH              = 91.14
         MAX-HEIGHT         = 28.92
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.92
         VIRTUAL-WIDTH      = 146.29
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-arquivo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-config-impr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Destino".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ESCD004 - Itens por Departamento com Saldo de Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ESCD004 - Itens por Departamento com Saldo de Estoque */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo C-Win
ON CHOOSE OF bt-arquivo IN FRAME DEFAULT-FRAME
DO:
  DEF VAR c-arq-conv  as char no-undo.

    ASSIGN c-arq-conv = replace(input frame {&frame-name} c-arquivo, "/", "\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.txt" "*.txt",
               "*.*" "*.*"
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "lst"
       INITIAL-DIR "spool" 
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    IF l-ok = YES THEN DO:
      ASSIGN c-arquivo = REPLACE(c-arq-conv, "\", "/"). 
      DISPLAY c-arquivo WITH FRAME {&frame-name}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr C-Win
ON CHOOSE OF bt-config-impr IN FRAME DEFAULT-FRAME
DO:
   SYSTEM-DIALOG PRINTER-SETUP.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME DEFAULT-FRAME /* Executar */
DO:
  run utp/ut-acomp.p persistent set h-acomp.
  run pi-inicializar in h-acomp (input "Saldo Estoque por Departamento").

  RUN pi-processa.      

  IF  INPUT FRAME {&FRAME-NAME} rs-destino = 1 THEN DO:
      RUN pi-imp-excel.
    END.
    ELSE IF  INPUT FRAME {&FRAME-NAME} rs-destino = 2 THEN DO:
      ASSIGN c-destino = INPUT FRAME {&FRAME-NAME} c-arquivo.
      RUN pi-imp-txt.
    END.
    ELSE
      RUN pi-imp-excel.
    
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


&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME DEFAULT-FRAME
DO:
  
  DO WITH FRAME {&FRAME-NAME}:
    
    CASE SELF:SCREEN-VALUE:
      WHEN "1" THEN DO:
        
        ASSIGN 
          c-arquivo:SENSITIVE    = NO
          bt-arquivo:VISIBLE     = no
          bt-config-impr:VISIBLE = YES.
      END.
      WHEN "2" THEN DO:

      ASSIGN 
        c-arquivo:SENSITIVE     = YES
        bt-arquivo:visible      = YES
        bt-config-impr:VISIBLE  = NO.
      END.
      WHEN "3" THEN DO:
        ASSIGN 
          c-arquivo:SENSITIVE     = NO
          bt-arquivo:VISIBLE      = NO
          bt-config-impr:VISIBLE  = NO.
      END.
    END CASE.
  END.

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

  ASSIGN bt-config-impr:VISIBLE IN FRAME {&FRAME-NAME} = NO.
  
  ASSIGN gsvc-cod_programa = "ESCD004".

  /* {includes\verifica-permissao.i} */
      
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
  DISPLAY fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-fam-ini fi-fam-fim 
          fi-depto-ini fi-depto-fim fi-fm-cod-com-ini fi-fm-cod-com-fim 
          rd-descricao tg-saldo rs-destino c-arquivo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-4 RECT-7 RECT-1 IMAGE-21 IMAGE-22 IMAGE-25 IMAGE-26 
         IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 RECT-8 
         fi-est-ini fi-est-fim fi-item-ini fi-item-fim fi-fam-ini fi-fam-fim 
         fi-depto-ini fi-depto-fim fi-fm-cod-com-ini fi-fm-cod-com-fim 
         rd-descricao tg-saldo bt-executar bt-fechar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-saldo C-Win 
PROCEDURE pi-calcula-saldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    assign d-saldo-calculado = saldo-estoq.qtidade-atu.

    if item.tipo-con-est = 1 then do:
        for each movto-estoq use-index item-data 
           where movto-estoq.it-codigo   = saldo-estoq.it-codigo    
             and movto-estoq.cod-refer   = saldo-estoq.cod-refer    
             and movto-estoq.cod-estabel = saldo-estoq.cod-estabel  
             and movto-estoq.cod-depos   = saldo-estoq.cod-depos    
             and movto-estoq.lote        = saldo-estoq.lote         
             and movto-estoq.cod-localiz = saldo-estoq.cod-localiz  
             and movto-estoq.esp-docto  <> 37                       
             and movto-estoq.dt-trans    > today no-lock:
            if movto-estoq.tipo-trans = 1 then
               assign d-saldo-calculado = d-saldo-calculado - movto-estoq.quantidade.
            else
               assign d-saldo-calculado = d-saldo-calculado + movto-estoq.quantidade.
        end.
    end.
    else do:
        for each movto-estoq use-index item-estab 
           where movto-estoq.it-codigo   = saldo-estoq.it-codigo    
             and movto-estoq.cod-refer   = saldo-estoq.cod-refer    
             and movto-estoq.cod-estabel = saldo-estoq.cod-estabel  
             and movto-estoq.cod-depos   = saldo-estoq.cod-depos    
             and movto-estoq.lote        = saldo-estoq.lote         
             and movto-estoq.cod-localiz = saldo-estoq.cod-localiz  
             and movto-estoq.esp-docto  <> 37                         
             and movto-estoq.dt-trans    > today no-lock:
            if movto-estoq.tipo-trans = 1 then
               assign d-saldo-calculado = d-saldo-calculado - movto-estoq.quantidade.
            else
               assign d-saldo-calculado = d-saldo-calculado + movto-estoq.quantidade.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-esp-docto C-Win 
PROCEDURE pi-esp-docto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input-output param c-esp-docto as char.

case bf-movto-estoq.esp-docto:
    when 1  then assign c-esp-docto = "ACA".
    when 2  then assign c-esp-docto = "ACT".
    when 3  then assign c-esp-docto = "NU1".
    when 4  then assign c-esp-docto = "DD" .
    when 5  then assign c-esp-docto = "DEV".
    when 6  then assign c-esp-docto = "DIV".
    when 7  then assign c-esp-docto = "DRM".
    when 8  then assign c-esp-docto = "EAC".
    when 9  then assign c-esp-docto = "EGF".
    when 10 then assign c-esp-docto = "BEM".
    when 11 then assign c-esp-docto = "NU2".
    when 12 then assign c-esp-docto = "NU3".
    when 13 then assign c-esp-docto = "NU4".
    when 14 then assign c-esp-docto = "ICM".
    when 15 then assign c-esp-docto = "INV".
    when 16 then assign c-esp-docto = "IPL".
    when 17 then assign c-esp-docto = "MOB".
    when 18 then assign c-esp-docto = "NC" .
    when 19 then assign c-esp-docto = "NF" .
    when 20 then assign c-esp-docto = "NFD".
    when 21 then assign c-esp-docto = "NFE".
    when 22 then assign c-esp-docto = "NFS".
    when 23 then assign c-esp-docto = "NFT".
    when 24 then assign c-esp-docto = "NU5".
    when 25 then assign c-esp-docto = "REF".
    when 26 then assign c-esp-docto = "RCS".
    when 27 then assign c-esp-docto = "RDD".
    when 28 then assign c-esp-docto = "REQ".
    when 29 then assign c-esp-docto = "RFS".
    when 30 then assign c-esp-docto = "RM" .
    when 31 then assign c-esp-docto = "RRQ".
    when 32 then assign c-esp-docto = "STR".
    when 33 then assign c-esp-docto = "TRA".
    when 34 then assign c-esp-docto = "ZZZ".
    when 35 then assign c-esp-docto = "SOB".
    when 36 then assign c-esp-docto = "EDD".
    when 37 then assign c-esp-docto = "VAR".
end case.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-excel C-Win 
PROCEDURE pi-imp-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-achou AS LOGICAL     NO-UNDO.

CREATE "Excel.Application" chExcelApplication.
ASSIGN 
  chExcelApplication:VISIBLE = NO                            /* Nao mostra a planilha Excel na tela enquanto esta sendo criada */
  chWorkbook  = chExcelApplication:Workbooks:ADD(c-planilha) /* Cria uma nova planilha excel */
  chWorkSheet = chExcelApplication:Sheets:ITEM(1).

i-linha = 10.

l-achou = no.
for each tt-imp
    break by tt-imp.cod-depto
          by tt-imp.it-codigo
          by tt-imp.cod-depos:

    assign l-achou = yes.

    if input frame {&frame-name} tg-saldo = no and
       tt-imp.qtidade-atu                 = 0 then next.
  
      run pi-acompanhar in h-acomp (input "Imprimindo Depto/item: " + tt-imp.cod-depto + "/" + tt-imp.it-codigo).
  /* ---> CABEÄALHO <--- */

      find first item no-lock
           where item.it-codigo = tt-imp.it-codigo no-error.

      ASSIGN 
        chexcelapplication:Range("A" + STRING(i-linha)):VALUE = STRING(tt-imp.cod-estabel)                
        chexcelapplication:Range("B" + STRING(i-linha)):VALUE = STRING(tt-imp.cod-depto)         
        chexcelapplication:Range("C" + STRING(i-linha)):VALUE = tt-imp.it-codigo                       
        chexcelapplication:Range("D" + STRING(i-linha)):VALUE = IF int(rd-descricao:SCREEN-VALUE) = 1 THEN tt-imp.desc-item ELSE tt-imp.narrativa
        chexcelapplication:Range("E" + STRING(i-linha)):VALUE = item.codigo-refer                       
        chexcelapplication:Range("F" + STRING(i-linha)):VALUE = tt-imp.fm-codigo                        
        chexcelapplication:Range("G" + STRING(i-linha)):VALUE = tt-imp.cod-depos                        
        chexcelapplication:Range("H" + STRING(i-linha)):VALUE = tt-imp.qtidade-atu
        chexcelapplication:Range("I" + STRING(i-linha)):VALUE = tt-imp.preco-medio
        chexcelapplication:Range("I" + STRING(i-linha)):Numberformat = "#.##0,00"
        chexcelapplication:Range("J" + STRING(i-linha)):VALUE = tt-imp.valor-estoq
        chexcelapplication:Range("J" + STRING(i-linha)):Numberformat = "#.##0,00"
        chexcelapplication:Range("K" + STRING(i-linha)):VALUE = tt-imp.dt-ult-sai
        chexcelapplication:Range("L" + STRING(i-linha)):VALUE = tt-imp.esp-docto
        chexcelapplication:Range("M" + STRING(i-linha)):VALUE = tt-imp.dt-ult-ent
        chexcelapplication:Range("N" + STRING(i-linha)):VALUE = tt-imp.fm-cod-com

/*         c-range       = "M" + STRING(i-linha)                  */
/*         chexcelapplication:Range(c-range):VALUE = tt-imp.docto */
        i-linha = i-linha + 1.
END.
if l-achou = no then do:

    ASSIGN 
                                          i-linha    = i-linha + 2
                                          c-range    = "c" + STRING(i-linha)                                                      
      chexcelapplication:Range(c-range):font:size    = 16
      chexcelapplication:Range(c-range):font:bold    = true
      chexcelapplication:Range(c-range):VALUE = " NENHUM ITEM FOI ENCONTRADO".                


end.

IF INPUT FRAME {&FRAME-NAME} rs-destino = 1 THEN DO:      
  ASSIGN
    chexcelapplication:ActiveSheet:PageSetup:PaperSize    = 9             /* --- PAPEL A4 --- */
    chexcelapplication:ActiveSheet:PageSetup:LeftMargin   = 1             /* margem esquerda     */
    chexcelapplication:ActiveSheet:PageSetup:RightMargin  = 1             /* margem direita      */
    chexcelapplication:ActiveSheet:PageSetup:TopMargin    = 1             /* margem superior     */
    chexcelapplication:ActiveSheet:PageSetup:BottomMargin = 1.             /* margem inferior     */      
    chExcelApplication:VISIBLE = FALSE.

chExcelApplication:ActiveWindow:SelectedSheets:PrintOut(,,1,,,).
chExcelApplication:workbooks:item(1):Close(NO).

  RELEASE object chExcelApplication.      
  RELEASE object chWorkbook.
  RELEASE object chWorksheet.
END.
ELSE DO:    

  ASSIGN
    chexcelapplication:ActiveSheet:PageSetup:PaperSize    = 9             /* --- PAPEL A4 --- */
    chexcelapplication:ActiveSheet:PageSetup:ORIENTATION  = 1              /* Orientaªío da impressío: 1 - Retrato , 2 Paisagem */
    chexcelapplication:ActiveSheet:PageSetup:LeftMargin   = 1             /* margem esquerda     */
    chexcelapplication:ActiveSheet:PageSetup:RightMargin  = 1             /* margem direita      */
    chexcelapplication:ActiveSheet:PageSetup:TopMargin    = 1             /* margem superior     */
    chexcelapplication:ActiveSheet:PageSetup:BottomMargin = 1.             /* margem inferior     */

    chexcelapplication:ActiveSheet:Protect('senha_padrao',TRUE,TRUE).
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:DisplayAlerts = FALSE.    
    RELEASE OBJECT chExcelApplication.   
    RELEASE object chWorkbook.
    RELEASE object chWorksheet.

END.


    /*  chExcelApplication:QUIT.  

FOR EACH tt-imp:
  DELETE tt-imp.
END.
                                  */
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
DEFINE VARIABLE c-it-depto AS CHARACTER   NO-UNDO.

empty temp-table tt-imp. 

for each item no-lock
   where item.it-codigo   >= input frame {&frame-name} fi-item-ini
     and item.it-codigo   <= input frame {&frame-name} fi-item-fim
     and item.cod-estabel >= input frame {&frame-name} fi-est-ini
     and item.cod-estabel <= input frame {&frame-name} fi-est-fim
     and item.fm-codigo   >= input frame {&frame-name} fi-fam-ini
     and item.fm-codigo   <= input frame {&frame-name} fi-fam-fim
     AND ITEM.fm-cod-com  >= input frame {&frame-name} fi-fm-cod-com-ini
     AND ITEM.fm-cod-com  <= input frame {&frame-name} fi-fm-cod-com-fim:

    
    for each saldo-estoq of item no-lock:

        run pi-calcula-saldo.
        c-it-depto = "".

        find first es-it-depto 
             where es-it-depto.it-codigo  = item.it-codigo no-lock no-error.
        if avail es-it-depto then do:
            if es-it-depto.cod-depto <= input frame {&frame-name} fi-depto-ini and 
               es-it-depto.cod-depto >= input frame {&frame-name} fi-depto-fim then next.
            find first es-depto where es-depto.codigo = es-it-depto.cod-depto no-lock no-error.
            if avail es-depto then
                assign c-it-depto = es-depto.descricao.
        end.


        find first item-estab no-lock
             where item-estab.it-codigo   = saldo-estoq.it-codigo
               and item-estab.cod-estabel = saldo-estoq.cod-estab no-error.

        /* Entrada */
        find last bf-movto-estoq 
            where bf-movto-estoq.it-codigo   = item.it-codigo 
              and bf-movto-estoq.cod-estabel = item.cod-estabel
              and bf-movto-estoq.tipo-trans  = 1
              and (bf-movto-estoq.esp-docto  = 5  or /* DEV */
                   bf-movto-estoq.esp-docto  = 6  or /* DIV */
                   bf-movto-estoq.esp-docto  = 7  or /* DRM */
                   bf-movto-estoq.esp-docto  = 20 or /* NFD */
                   bf-movto-estoq.esp-docto  = 21 or /* NFE */
                   bf-movto-estoq.esp-docto  = 31)   /* RRQ */
            no-lock no-error.

        if avail bf-movto-estoq then 
            run pi-esp-docto( input-output c-esp-docto).

        run pi-acompanhar in h-acomp (input "Saldo item " + saldo-estoq.it-codigo).


        create tt-imp.
        assign tt-imp.cod-depto   = c-it-depto
               tt-imp.cod-estabel = saldo-estoq.cod-estabel
               tt-imp.it-codigo   = saldo-estoq.it-codigo
               tt-imp.desc-item   = item.desc-item
               tt-imp.fm-codigo   = item.fm-codigo
               tt-imp.qtidade-atu = d-saldo-calculado
               tt-imp.cod-depos   = saldo-estoq.cod-depos
               tt-imp.preco-medio = (item-estab.val-unit-mat-m[1] + item-estab.val-unit-mob-m[1] + item-estab.val-unit-ggf-m[1]) 
               tt-imp.valor-estoq = if avail bf-movto-estoq then ((bf-movto-estoq.valor-mat-m[1]   + bf-movto-estoq.valor-mob-m[1]   + bf-movto-estoq.valor-ggf-m[1]) / bf-movto-estoq.quantidade) else ?
               tt-imp.dt-ult-sai  = IF AVAIL ITEM AND item.data-ult-sai <> ? THEN item.data-ult-sai ELSE ?  /* if avail movto-estoq then movto-estoq.dt-trans  else ? */
               tt-imp.esp-docto   = c-esp-docto
               tt-imp.dt-ult-ent  = IF AVAIL ITEM AND item.data-ult-ent <> ? THEN item.data-ult-ent ELSE ? /*if avail bf-movto-estoq then bf-movto-estoq.dt-trans else ?*/
               tt-imp.fm-cod-com  = ITEM.fm-cod-com
               tt-imp.narrativa   = ITEM.narrativa.

        end.
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

