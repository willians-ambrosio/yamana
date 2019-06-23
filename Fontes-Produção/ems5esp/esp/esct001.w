&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          shems5           PROGRESS
*/
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

DEF TEMP-TABLE tt-cta-ger
    FIELD cod-rel        LIKE dsc-param-cta-gerencial.cod-rel
    FIELD nr-seq         LIKE dsc-param-cta-gerencial.nr-seq
    FIELD cta-ger        LIKE dsc-param-cta-gerencial.cta-ger
    FIELD des-conta-ger  LIKE dsc-param-cta-gerencial.des-conta-ger
    FIELD tip-taxa-dolar LIKE dsc-param-cta-gerencial.tip-taxa-dolar
    FIELD ld-borda       LIKE dsc-param-cta-gerencial.ld-borda 
    FIELD tipo-linha     LIKE dsc-param-cta-gerencial.tipo-linha.


DEF TEMP-TABLE tt-cta-datasul
    FIELD cod-rel        LIKE dsc-param-cta-datasul.cod-rel
    FIELD nr-seq         LIKE dsc-param-cta-datasul.nr-seq
    FIELD cta-ctbl       LIKE dsc-param-cta-datasul.cta-ctbl.

DEF VAR  cod-rel-aux     LIKE dsc-param-cta-gerencial.cod-rel.
DEF VAR  nr-seq-aux      LIKE dsc-param-cta-gerencial.nr-seq.
DEF VAR  cta-ger-aux     LIKE dsc-param-cta-gerencial.cta-ger.
DEF VAR  fi-aux-cta      AS INT FORMAT "99999999".


DEF VAR i AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cta-ger tt-cta-datasul

/* Definitions for BROWSE BR-3                                          */
&Scoped-define FIELDS-IN-QUERY-BR-3 /* */ tt-cta-ger.nr-seq tt-cta-ger.cta-ger tt-cta-ger.des-conta-ger tt-cta-ger.tip-taxa-dolar tt-cta-ger.ld-borda tt-cta-ger.tipo-linha   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-3   
&Scoped-define SELF-NAME BR-3
&Scoped-define QUERY-STRING-BR-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-3 OPEN QUERY br-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-3 tt-cta-ger
&Scoped-define FIRST-TABLE-IN-QUERY-BR-3 tt-cta-ger


/* Definitions for BROWSE br-7                                          */
&Scoped-define FIELDS-IN-QUERY-br-7 tt-cta-datasul.cta-ctbl   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-7   
&Scoped-define SELF-NAME br-7
&Scoped-define QUERY-STRING-br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-7 OPEN QUERY br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-7 tt-cta-datasul
&Scoped-define FIRST-TABLE-IN-QUERY-br-7 tt-cta-datasul


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-3}~
    ~{&OPEN-QUERY-br-7}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod-rel fi-desc BtnOK-3 BR-3 bt-inclui ~
bt-altera bt-exclui br-7 bt-inclui-2 bt-exclui-2 bt-cancelar RECT-18 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-rel fi-desc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-altera 
     LABEL "Alterar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Sair" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-exclui 
     LABEL "Excluir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-exclui-2 
     LABEL "Excluir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-inclui 
     LABEL "Incluir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-inclui-2 
     LABEL "Incluir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON BtnOK-3 AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 5 BY .88
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod-rel AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Codigo do Relatorio" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-desc AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 1.75.

DEFINE BUTTON BtnCancel-2 AUTO-END-KEY DEFAULT 
     LABEL "Cancela" 
     SIZE 11 BY .88
     BGCOLOR 8 .

DEFINE BUTTON BtnOK-2 AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 11 BY .88
     BGCOLOR 8 .

DEFINE VARIABLE fi-cta-dat AS CHARACTER FORMAT "9.9.9.99.999":U INITIAL "00000000" 
     LABEL "Cta Datasul" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cta-fim AS CHARACTER FORMAT "9.9.9.99.999":U INITIAL "00000000" 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cta-ger-i AS CHARACTER FORMAT "9.9.999":U 
     LABEL "Conta Ger CDN" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cta-ini AS CHARACTER FORMAT "9.9.9.99.999":U INITIAL "00000000" 
     LABEL "De" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 1.75.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancela" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE cb-tipo-linha AS CHARACTER FORMAT "X(256)":U INITIAL "Normal" 
     LABEL "Tipo Linha" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Normal","Totalizador","Pular Linha" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tx-dolar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Taxa Dolar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","Mensal","Medio" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-rel-i AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Codigo do Relatorio" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-conta AS CHARACTER FORMAT "X(60)":U 
     LABEL "Descriá∆o" 
     VIEW-AS FILL-IN 
     SIZE 45.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ld-borda AS LOGICAL FORMAT "sim/nao":U INITIAL NO 
     LABEL "Celula com Borda" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-i AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Sequencia" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-3 FOR 
      tt-cta-ger SCROLLING.

DEFINE QUERY br-7 FOR 
      tt-cta-datasul SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-3 C-Win _FREEFORM
  QUERY BR-3 NO-LOCK DISPLAY
      /* FORMAT ">>>9":U WIDTH 17,86 */
      tt-cta-ger.nr-seq        
      tt-cta-ger.cta-ger       
      tt-cta-ger.des-conta-ger LABEL "Descriá∆o"
      tt-cta-ger.tip-taxa-dolar
      tt-cta-ger.ld-borda      
      tt-cta-ger.tipo-linha
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 76 BY 5
         FONT 1.

DEFINE BROWSE br-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-7 C-Win _FREEFORM
  QUERY br-7 DISPLAY
      tt-cta-datasul.cta-ctbl
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30 BY 5.25
         FONT 1
         TITLE "Conta Datasul" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cod-rel AT ROW 1.75 COL 15 COLON-ALIGNED
     fi-desc AT ROW 1.75 COL 22 COLON-ALIGNED NO-LABEL
     BtnOK-3 AT ROW 1.75 COL 74
     BR-3 AT ROW 3.5 COL 3
     bt-inclui AT ROW 8.5 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-altera AT ROW 8.5 COL 13 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-exclui AT ROW 8.5 COL 23 HELP
          "Dispara a execuá∆o do relat¢rio"
     br-7 AT ROW 10 COL 3
     bt-inclui-2 AT ROW 15.25 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-exclui-2 AT ROW 15.25 COL 13 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 16 COL 70 HELP
          "Fechar"
     RECT-18 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.33
         FONT 1
         DEFAULT-BUTTON BtnOK-3.

DEFINE FRAME fr-cta-datasul
     fi-cta-ger-i AT ROW 1.25 COL 12 COLON-ALIGNED
     fi-cta-dat AT ROW 1.25 COL 31 COLON-ALIGNED
     fi-cta-ini AT ROW 3.25 COL 5 COLON-ALIGNED
     fi-cta-fim AT ROW 3.25 COL 25 COLON-ALIGNED
     BtnOK-2 AT ROW 5.25 COL 10
     BtnCancel-2 AT ROW 5.25 COL 25
     RECT-19 AT ROW 2.75 COL 3
     " Intervalo de Contas" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 2.5 COL 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 9.5
         SIZE 45 BY 6.25
         FONT 1
         TITLE "Manutená∆o de Contas Datasul"
         DEFAULT-BUTTON BtnOK-2 CANCEL-BUTTON BtnCancel-2.

DEFINE FRAME fr-digita-cta-ger
     fi-cod-rel-i AT ROW 1.25 COL 14 COLON-ALIGNED
     fi-nr-seq-i AT ROW 1.25 COL 30 COLON-ALIGNED
     fi-cta-ger-i AT ROW 2.25 COL 12 COLON-ALIGNED
          LABEL "Conta Ger CDN" FORMAT "9.9.999":U
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     fi-desc-conta AT ROW 2.25 COL 24.28
     fi-ld-borda AT ROW 3.25 COL 14 COLON-ALIGNED
     cb-tipo-linha AT ROW 3.25 COL 30 COLON-ALIGNED
     cb-tx-dolar AT ROW 3.25 COL 60 COLON-ALIGNED
     BtnOK AT ROW 5.25 COL 26
     BtnCancel AT ROW 5.25 COL 43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3
         SIZE 79 BY 6.5
         FONT 1
         TITLE "Manutenc∆o da Conta Gerencial"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.


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
         TITLE              = "<Cadastro de Parametros>"
         HEIGHT             = 16.38
         WIDTH              = 80.57
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
ASSIGN FRAME fr-cta-datasul:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME fr-digita-cta-ger:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BR-3 fr-digita-cta-ger DEFAULT-FRAME */
/* BROWSE-TAB br-7 fr-cta-datasul DEFAULT-FRAME */
/* SETTINGS FOR FRAME fr-cta-datasul
                                                                        */
/* SETTINGS FOR FILL-IN fi-cta-ger-i IN FRAME fr-cta-datasul
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fr-digita-cta-ger
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FILL-IN fi-cod-rel-i IN FRAME fr-digita-cta-ger
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-conta IN FRAME fr-digita-cta-ger
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-3
/* Query rebuild information for BROWSE BR-3
     _START_FREEFORM
OPEN QUERY br-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BR-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-7
/* Query rebuild information for BROWSE br-7
     _START_FREEFORM
OPEN QUERY br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-7 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Cadastro de Parametros> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Cadastro de Parametros> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-3
&Scoped-define SELF-NAME BR-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-3 C-Win
ON VALUE-CHANGED OF BR-3 IN FRAME DEFAULT-FRAME
DO:
  FOR EACH tt-cta-datasul:
      DELETE tt-cta-datasul.
  END.

  FIND CURRENT tt-cta-ger.

  ASSIGN  cod-rel-aux   = tt-cta-ger.cod-rel 
          nr-seq-aux    = tt-cta-ger.nr-seq
          cta-ger-aux   = tt-cta-ger.cta-ger.

/*
  MESSAGE "1 - " cod-rel-aux  
          nr-seq-aux   
          cta-ger-aux VIEW-AS ALERT-BOX.  
  */

  FOR EACH dsc-param-cta-datasul 
      WHERE dsc-param-cta-datasul.cod-rel = tt-cta-ger.cod-rel 
        AND dsc-param-cta-datasul.nr-seq  = tt-cta-ger.nr-seq NO-LOCK:

      CREATE tt-cta-datasul.
      ASSIGN tt-cta-datasul.cod-rel  = dsc-param-cta-datasul.cod-rel    
             tt-cta-datasul.nr-seq   = dsc-param-cta-datasul.nr-seq     
             tt-cta-datasul.cta-ctbl = dsc-param-cta-datasul.cta-ctbl.  

     
  END.

  OPEN QUERY br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul NO-LOCK
      BY  tt-cta-datasul.cta-ctbl INDEXED-REPOSITION.





END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-altera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-altera C-Win
ON CHOOSE OF bt-altera IN FRAME DEFAULT-FRAME /* Alterar */
DO:
  FIND CURRENT tt-cta-ger NO-LOCK NO-ERROR.
   
   /*
   FOR EACH dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel NO-lock:
      CREATE tt-cta-ger.
      ASSIGN tt-cta-ger.cod-rel = dsc-param-cta-gerencial.cod-rel         
             tt-cta-ger.nr-seq         = dsc-param-cta-gerencial.nr-seq          
             tt-cta-ger.cta-ger        = dsc-param-cta-gerencial.cta-ger         
             tt-cta-ger.des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger   
             tt-cta-ger.tip-taxa-dolar = dsc-param-cta-gerencial.tip-taxa-dolar  
             tt-cta-ger.ld-borda       = dsc-param-cta-gerencial.ld-borda        
             tt-cta-ger.tipo-linha     = dsc-param-cta-gerencial.tipo-linha.     
  END.
  */
   
 
   do  on error undo, return no-apply:
       DO WITH FRAME fr-digita-cta-GER:
          ASSIGN fi-cod-rel-i:SCREEN-VALUE  = INPUT fi-cod-rel.
          ASSIGN fi-cod-rel-i.
          
       END.
       

       ASSIGN fi-nr-seq-i:SCREEN-VALUE   = string(tt-cta-ger.nr-seq)
              fi-cta-ger-i:SCREEN-VALUE  = string(tt-cta-ger.cta-ger)
              fi-desc-conta:SCREEN-VALUE = string(tt-cta-ger.des-conta-ger) 
              fi-ld-borda:SCREEN-VALUE   = string(tt-cta-ger.ld-borda)
              cb-tipo-linha:SCREEN-VALUE = string(tt-cta-ger.tipo-linha).

       IF tt-cta-ger.tip-taxa-dolar = 1 THEN
          ASSIGN cb-tx-dolar:SCREEN-VALUE   = " ".
       ELSE 
       IF tt-cta-ger.tip-taxa-dolar = 2 THEN
           ASSIGN cb-tx-dolar:SCREEN-VALUE  = "Mensal".
       ELSE 
       IF tt-cta-ger.tip-taxa-dolar = 3 THEN
           ASSIGN cb-tx-dolar:SCREEN-VALUE  = "Medio".



       VIEW FRAME fr-digita-cta-ger.


            




     
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME DEFAULT-FRAME /* Sair */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exclui C-Win
ON CHOOSE OF bt-exclui IN FRAME DEFAULT-FRAME /* Excluir */
DO:

   FIND CURRENT tt-cta-ger NO-LOCK NO-ERROR.

   FIND FIRST dsc-param-cta-datasul
        WHERE dsc-param-cta-datasul.cod-rel = tt-cta-ger.cod-rel 
          AND dsc-param-cta-datasul.nr-seq  = tt-cta-ger.nr-seq NO-LOCK NO-ERROR. 
   
   IF AVAIL dsc-param-cta-datasul THEN 
      MESSAGE "Registro possui conta Datasul Relacionada, n∆o pode ser eliminado !!!"
          VIEW-AS ALERT-BOX.
   ELSE DO:
      FIND dsc-param-cta-gerencial
        WHERE dsc-param-cta-gerencial.cod-rel = tt-cta-ger.cod-rel 
          AND dsc-param-cta-gerencial.nr-seq  = tt-cta-ger.nr-seq
          AND dsc-param-cta-gerencial.cta-ger = tt-cta-ger.cta-ger NO-ERROR.

      IF AVAIL dsc-param-cta-gerencial THEN DO:
         MESSAGE "Eliminando Conta Gerencial ..." VIEW-AS ALERT-BOX.
         DELETE dsc-param-cta-gerencial.
      END.

   END.

   /* pega as alteracoes */

   FOR EACH tt-cta-ger:
       DELETE tt-cta-ger.
   END.
   FOR EACH dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel NO-lock:
      CREATE tt-cta-ger.
      ASSIGN tt-cta-ger.cod-rel        = dsc-param-cta-gerencial.cod-rel         
             tt-cta-ger.nr-seq         = dsc-param-cta-gerencial.nr-seq          
             tt-cta-ger.cta-ger        = dsc-param-cta-gerencial.cta-ger         
             tt-cta-ger.des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger   
             tt-cta-ger.tip-taxa-dolar = dsc-param-cta-gerencial.tip-taxa-dolar  
             tt-cta-ger.ld-borda       = dsc-param-cta-gerencial.ld-borda        
             tt-cta-ger.tipo-linha     = dsc-param-cta-gerencial.tipo-linha.   
   END.
  
   OPEN QUERY br-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK .


      
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exclui-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exclui-2 C-Win
ON CHOOSE OF bt-exclui-2 IN FRAME DEFAULT-FRAME /* Excluir */
DO:
  /* do  on error undo, return no-apply: */

      FIND CURRENT tt-cta-datasul NO-LOCK NO-ERROR.

      FIND FIRST dsc-param-cta-datasul
           WHERE dsc-param-cta-datasul.cod-rel     = tt-cta-datasul.cod-rel 
             AND dsc-param-cta-datasul.nr-seq      = tt-cta-datasul.nr-seq 
             AND dsc-param-cta-datasul.cta-ctbl    = tt-cta-datasul.cta-ctbl NO-ERROR. 
   
      IF AVAIL dsc-param-cta-datasul THEN DO:
         MESSAGE "Eliminando Conta Datasul ..." dsc-param-cta-datasul.cta-ctbl
              VIEW-AS ALERT-BOX.
         DELETE dsc-param-cta-datasul.
      END.

      FOR EACH tt-cta-datasul:
      DELETE tt-cta-datasul.
      END. 


  /* limpa browse */
  FIND CURRENT tt-cta-ger.

  FOR EACH tt-cta-datasul:
      DELETE tt-cta-datasul.
  END.

  FOR EACH dsc-param-cta-datasul 
      WHERE dsc-param-cta-datasul.cod-rel = tt-cta-ger.cod-rel 
        AND dsc-param-cta-datasul.nr-seq  = tt-cta-ger.nr-seq NO-LOCK:

      CREATE tt-cta-datasul.
      ASSIGN tt-cta-datasul.cod-rel  = dsc-param-cta-datasul.cod-rel    
             tt-cta-datasul.nr-seq   = dsc-param-cta-datasul.nr-seq     
             tt-cta-datasul.cta-ctbl = dsc-param-cta-datasul.cta-ctbl.  

     
  END.

  OPEN QUERY br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul NO-LOCK
      BY  tt-cta-datasul.cta-ctbl INDEXED-REPOSITION.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inclui C-Win
ON CHOOSE OF bt-inclui IN FRAME DEFAULT-FRAME /* Incluir */
DO:
   do  on error undo, return no-apply:
       DO WITH FRAME fr-digita-cta-GER:
          ASSIGN fi-cod-rel-i:SCREEN-VALUE  = INPUT fi-cod-rel.
          ASSIGN fi-cod-rel-i.
       END.
       VIEW FRAME fr-digita-cta-ger.
     
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inclui-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inclui-2 C-Win
ON CHOOSE OF bt-inclui-2 IN FRAME DEFAULT-FRAME /* Incluir */
DO:
  /* do  on error undo, return no-apply: */
      /* DO WITH FRAME fr-cta-datasul:
          ASSIGN fi-cod-rel-i:SCREEN-VALUE  = INPUT fi-cod-rel.
          ASSIGN fi-cod-rel-i.
       END. */
       VIEW FRAME fr-cta-datasul.
       /*
       ASSIGN fi-cta-dat:SCREEN-VALUE = "00000000"
              fi-cta-ini:SCREEN-VALUE = "00000000"
              fi-cta-fim:SCREEN-VALUE = "00000000".
       */
       ASSIGN fi-cta-ger-i:SCREEN-VALUE = cta-ger-aux .

   /*  
   end. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-digita-cta-ger
&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME fr-digita-cta-ger /* Cancela */
DO:
  HIDE FRAME fr-digita-cta-ger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-cta-datasul
&Scoped-define SELF-NAME BtnCancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel-2 C-Win
ON CHOOSE OF BtnCancel-2 IN FRAME fr-cta-datasul /* Cancela */
DO:
   HIDE FRAME fr-cta-datasul.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-digita-cta-ger
&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME fr-digita-cta-ger /* OK */
DO:
  DO WITH FRAME default-frame:
     ASSIGN fi-cod-rel
            fi-desc.
  END.

  ASSIGN fi-cta-ger-i
         fi-ld-borda
         fi-nr-seq-i
         fi-desc-conta
         fi-ld-borda
         cb-tipo-linha
         cb-tx-dolar.

  /* CONSISTENCIAS */ 
  IF fi-nr-seq-i = 0  THEN DO:
     MESSAGE "Sequencia deve ser diferente de zero !" VIEW-AS ALERT-BOX.
  END.

  IF  fi-nr-seq-i <> 0 THEN DO:

    FIND FIRST dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel-i
        AND dsc-param-cta-gerencial.nr-seq  = fi-nr-seq-i 
        AND dsc-param-cta-gerencial.cta-ger = fi-cta-ger-i NO-ERROR.
    IF NOT AVAIL dsc-param-cta-gerencial  THEN DO:
       CREATE dsc-param-cta-gerencial.
       ASSIGN dsc-param-cta-gerencial.cod-rel        = INPUT fi-cod-rel-i
              dsc-param-cta-gerencial.nr-seq         = INPUT fi-nr-seq-i
              dsc-param-cta-gerencial.cta-ger        = INPUT fi-cta-ger-i
              dsc-param-cta-gerencial.des-conta-ger  = INPUT fi-desc-conta
              dsc-param-cta-gerencial.ld-borda       = INPUT fi-ld-borda   
              dsc-param-cta-gerencial.tipo-linha     = INPUT cb-tipo-linha.

       /* dolar = "" qdo tipo-linha <> normal */
       IF INPUT cb-tipo-linha <> "normal" THEN
          ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 1.
       ELSE DO:
          IF INPUT cb-tx-dolar = "mensal" THEN
             ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 2.
          IF INPUT cb-tx-dolar = "medio" THEN
             ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 3. 
       END.

    END.
    ELSE DO: /* alteracao */
       ASSIGN dsc-param-cta-gerencial.cta-ger        = INPUT fi-cta-ger-i
              dsc-param-cta-gerencial.des-conta-ger  = INPUT fi-desc-conta
              dsc-param-cta-gerencial.ld-borda       = INPUT fi-ld-borda   
              dsc-param-cta-gerencial.tipo-linha     = INPUT cb-tipo-linha.

       /* dolar = "" qdo tipo-linha <> normal */
       IF INPUT cb-tipo-linha <> "normal" THEN
          ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 1.
       ELSE DO:
          IF INPUT cb-tx-dolar = "mensal" THEN
             ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 2.
          IF INPUT cb-tx-dolar = "medio" THEN
             ASSIGN dsc-param-cta-gerencial.tip-taxa-dolar = 3. 
       END.

              
    END.


  
  END.

  /* pega as alteracoes */

   FOR EACH tt-cta-ger:
       DELETE tt-cta-ger.
   END.
   FOR EACH dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel NO-lock:
      CREATE tt-cta-ger.
      ASSIGN tt-cta-ger.cod-rel        = dsc-param-cta-gerencial.cod-rel         
             tt-cta-ger.nr-seq         = dsc-param-cta-gerencial.nr-seq          
             tt-cta-ger.cta-ger        = dsc-param-cta-gerencial.cta-ger         
             tt-cta-ger.des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger   
             tt-cta-ger.tip-taxa-dolar = dsc-param-cta-gerencial.tip-taxa-dolar  
             tt-cta-ger.ld-borda       = dsc-param-cta-gerencial.ld-borda        
             tt-cta-ger.tipo-linha     = dsc-param-cta-gerencial.tipo-linha.   
  END.
  
  HIDE FRAME fr-digita-cta-ger.

  OPEN QUERY br-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK .











END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-cta-datasul
&Scoped-define SELF-NAME BtnOK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK-2 C-Win
ON CHOOSE OF BtnOK-2 IN FRAME fr-cta-datasul /* OK */
DO:

   ASSIGN fi-cta-dat
          fi-cta-ini
          fi-cta-fim. 
  
  /* FIND CURRENT tt-cta-ger. - n∆o estava funcionando */

  IF NOT AVAIL tt-cta-ger THEN DO:
      RUN utp/ut-msgs.p ("show",17006,
                         " Selecione uma conta Gerencial ! ~~ Verifique...").
       HIDE FRAME fr-cta-datasul.
    
      /* APPLY 'entry' TO INPUT fi-cod-rel. */
      RETURN NO-APPLY.

  END.

  
  IF fi-cta-dat <> "00000000"  THEN DO:

     FIND FIRST dsc-param-cta-datasul 
          WHERE dsc-param-cta-datasul.cod-rel  = cod-rel-aux
            AND dsc-param-cta-datasul.nr-seq   = nr-seq-aux
            AND dsc-param-cta-datasul.cta-ctbl = fi-cta-dat NO-ERROR.
     IF AVAIL  dsc-param-cta-datasul THEN DO:
        RUN utp/ut-msgs.p ("show",17006,
                         " Conta Datasul j† cadastrada p/ Conta Gerencial..." +
                          string(nr-seq-aux)  + " !!! "  + "~~ Verifique...").
      APPLY 'entry' TO INPUT fi-cta-dat.
      RETURN NO-APPLY.
       
     END.

     IF NOT AVAIL dsc-param-cta-datasul  THEN DO:
        /* MESSAGE "Criando Conta Datasul ..." INPUT fi-cta-dat 
                + ", para conta gerencial..." + STRING(cta-ger-aux) VIEW-AS ALERT-BOX. */

        FIND cta_ctbl WHERE 
             cta_ctbl.cod_cta_ctbl = INPUT fi-cta-dat NO-LOCK NO-ERROR.
        IF AVAIL cta_ctbl THEN DO:
        
           CREATE dsc-param-cta-datasul.
           ASSIGN dsc-param-cta-datasul.cod-rel  = cod-rel-aux
                  dsc-param-cta-datasul.nr-seq   = nr-seq-aux
                  dsc-param-cta-datasul.cta-ctbl = INPUT fi-cta-dat.
        END.

     END.

  END.

  /* criando sequencia de contas */
  IF fi-cta-ini <> "00000000" THEN DO:
     IF fi-cta-fim = "00000000" OR 
        fi-cta-fim < fi-cta-ini THEN DO:
        RUN utp/ut-msgs.p ("show",17006,
                         " Conta Final deve ser Informada ~~ Atencao, conta final deve ser maior que Inicial...").
        APPLY 'entry' TO INPUT fi-cta-fim.
        RETURN NO-APPLY.

     END.
     /* pesquisando conta a serem incluidas */
     IF fi-cta-ini > "00000000" AND 
        fi-cta-fim > fi-cta-ini THEN DO:

        ASSIGN fi-aux-cta = int(fi-cta-ini).

        DO  i = int(fi-cta-ini) TO int(fi-cta-fim):
                                       
            IF fi-aux-cta > 0 THEN DO:
            
            FIND FIRST dsc-param-cta-datasul 
              WHERE dsc-param-cta-datasul.cod-rel  = cod-rel-aux
                AND dsc-param-cta-datasul.nr-seq   = nr-seq-aux
                AND dsc-param-cta-datasul.cta-ctbl = string(fi-aux-cta) NO-ERROR.
            IF AVAIL  dsc-param-cta-datasul THEN DO:
               MESSAGE " Conta Datasul j† cadastrada p/ Conta Gerencial..." +
                   string(nr-seq-aux)  + " !!! " VIEW-AS ALERT-BOX.
               /*
               RUN utp/ut-msgs.p ("show",17006,
                   " Conta Datasul j† cadastrada p/ Conta Gerencial..." +
                   string(nr-seq-aux)  + " !!! "  + "~~ Verifique...").
               APPLY 'entry' TO INPUT fi-cta-dat.
              RETURN NO-APPLY.*/
            END.

            IF NOT AVAIL dsc-param-cta-datasul  AND string(fi-aux-cta) <> "00000000" THEN DO:
               /*MESSAGE "Criando Sequencia de Conta Datasul..." string(fi-aux-cta) 
                + ", para conta gerencial..." + STRING(cta-ger-aux) VIEW-AS ALERT-BOX. */

               FIND cta_ctbl WHERE 
                    cta_ctbl.cod_cta_ctbl = string(fi-aux-cta) NO-LOCK NO-ERROR.

               IF AVAIL cta_ctbl THEN DO:

                  CREATE dsc-param-cta-datasul.
                  ASSIGN dsc-param-cta-datasul.cod-rel  = cod-rel-aux
                         dsc-param-cta-datasul.nr-seq   = nr-seq-aux
                         dsc-param-cta-datasul.cta-ctbl = string(fi-aux-cta).
               END.
            END.

            END.

            ASSIGN fi-aux-cta = fi-aux-cta + 1 .
            IF fi-aux-cta > int(fi-cta-fim)  THEN
               LEAVE.
     
        END.

     END.
     
  END.

   /* limpa browse */
  FIND CURRENT tt-cta-ger.

  FOR EACH tt-cta-datasul:
      DELETE tt-cta-datasul.
  END.

  FOR EACH dsc-param-cta-datasul 
      WHERE dsc-param-cta-datasul.cod-rel = tt-cta-ger.cod-rel 
        AND dsc-param-cta-datasul.nr-seq  = tt-cta-ger.nr-seq NO-LOCK:

      CREATE tt-cta-datasul.
      ASSIGN tt-cta-datasul.cod-rel  = dsc-param-cta-datasul.cod-rel    
             tt-cta-datasul.nr-seq   = dsc-param-cta-datasul.nr-seq     
             tt-cta-datasul.cta-ctbl = dsc-param-cta-datasul.cta-ctbl.  

     
  END.

  OPEN QUERY br-7 /*{&SELF-NAME}*/ FOR EACH tt-cta-datasul NO-LOCK
      BY  tt-cta-datasul.cta-ctbl INDEXED-REPOSITION.

 
 HIDE FRAME fr-cta-datasul.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BtnOK-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK-3 C-Win
ON CHOOSE OF BtnOK-3 IN FRAME DEFAULT-FRAME /* OK */
DO:
  FIND dsc-param-rel-ctbl WHERE 
       dsc-param-rel-ctbl.cod-rel = fi-cod-rel NO-ERROR.

  IF AVAIL dsc-param-rel-ctbl THEN
     ASSIGN dsc-param-rel.des-rel = fi-desc.
  ELSE DO:
      IF fi-desc <> "" THEN DO:
         MESSAGE "Criando Novo Relat¢rio !!!" VIEW-AS ALERT-BOX.
         CREATE dsc-param-rel.
         ASSIGN dsc-param-rel.cod-rel = fi-cod-rel
                dsc-param-rel.des-rel = fi-desc.
      END.
  END.
  /****
  IF NOT AVAIL dsc-param-rel-ctbl THEN DO:
     MESSAGE "Criando Novo Relat¢rio !!!" VIEW-AS ALERT-BOX.
     CREATE dsc-param-rel.
     ASSIGN dsc-param-rel.cod-rel = fi-cod-rel
            dsc-param-rel.des-rel = fi-desc.
  END.
  ELSE 
     ASSIGN dsc-param-rel.des-rel = fi-desc.
   ********/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-digita-cta-ger
&Scoped-define SELF-NAME cb-tipo-linha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-linha C-Win
ON LEAVE OF cb-tipo-linha IN FRAME fr-digita-cta-ger /* Tipo Linha */
DO:
    ASSIGN cb-tipo-linha.

   IF cb-tipo-linha:SCREEN-VALUE <> "normal" THEN
       ASSIGN cb-tx-dolar:SCREEN-VALUE  = "".



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME fi-cod-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rel C-Win
ON LEAVE OF fi-cod-rel IN FRAME DEFAULT-FRAME /* Codigo do Relatorio */
DO:
  ASSIGN fi-cod-rel = int(fi-cod-rel:SCREEN-VALUE).

  FOR EACH tt-cta-ger:
      DELETE tt-cta-ger.
  END.

  IF fi-cod-rel = 0 THEN do:
     RUN utp/ut-msgs.p ("show",17006,
                         " Real¢rio n∆o pode ser zero ~~ Verifique...").
      APPLY 'entry' TO INPUT fi-cod-rel.
      RETURN NO-APPLY.
  END.

  FIND dsc-param-rel-ctbl 
      WHERE dsc-param-rel-ctbl.cod-rel = INPUT fi-cod-rel NO-ERROR.
  IF AVAIL dsc-param-rel-ctb THEN
     ASSIGN fi-desc:SCREEN-VALUE = dsc-param-rel-ctbl.des-rel
     fi-desc.
  ELSE DO:
      ASSIGN fi-desc:SCREEN-VALUE = "".
      RUN utp/ut-msgs.p ("show",17006,
                         " Relat¢rio Novo !!!   ~~ Entre com a Descriá∆o...").
      APPLY 'entry' TO INPUT fi-desc.
      RETURN NO-APPLY.
  END.
       
  FOR EACH dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel NO-lock:
      CREATE tt-cta-ger.
      ASSIGN tt-cta-ger.cod-rel        = dsc-param-cta-gerencial.cod-rel         
             tt-cta-ger.nr-seq         = dsc-param-cta-gerencial.nr-seq          
             tt-cta-ger.cta-ger        = dsc-param-cta-gerencial.cta-ger         
             tt-cta-ger.des-conta-ger  = dsc-param-cta-gerencial.des-conta-ger   
             tt-cta-ger.tip-taxa-dolar = dsc-param-cta-gerencial.tip-taxa-dolar  
             tt-cta-ger.ld-borda       = dsc-param-cta-gerencial.ld-borda        
             tt-cta-ger.tipo-linha     = dsc-param-cta-gerencial.tipo-linha.   
  END.
  

  OPEN QUERY br-3 /*{&SELF-NAME}*/ FOR EACH tt-cta-ger  NO-LOCK .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-cta-datasul
&Scoped-define SELF-NAME fi-cta-dat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cta-dat C-Win
ON LEAVE OF fi-cta-dat IN FRAME fr-cta-datasul /* Cta Datasul */
DO:
    ASSIGN fi-cta-dat.
    FIND cta_ctbl WHERE 
         cta_ctbl.cod_cta_ctbl = fi-cta-dat NO-LOCK NO-ERROR.

    IF NOT AVAIL cta_ctbl THEN DO:
       MESSAGE " Conta Inv†lida no DATASUL !!!" VIEW-AS ALERT-BOX.
       APPLY 'entry' TO INPUT  fi-cta-dat IN FRAME fr-cta-datasul.
       /* RETURN NO-APPLY. */

    END. 



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME fi-desc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-desc C-Win
ON LEAVE OF fi-desc IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fi-cod-rel
         fi-desc = fi-desc:SCREEN-VALUE.
         /*
         IF fi-desc:SCREEN-VALUE = "" THEN DO:
           RUN utp/ut-msgs.p ("show",17006,
                         " Entre com a descriá∆o do Relat¢rio ~~ Verifique...").
              APPLY 'entry' TO INPUT fi-desc.
              RETURN NO-APPLY.
         END.
           */

         

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-digita-cta-ger
&Scoped-define SELF-NAME fi-nr-seq-i
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-seq-i C-Win
ON LEAVE OF fi-nr-seq-i IN FRAME fr-digita-cta-ger /* Sequencia */
DO:
    ASSIGN fi-nr-seq-i.
    /*
    IF  fi-nr-seq-i <> 0 THEN DO:

    FIND FIRST dsc-param-cta-gerencial 
      WHERE dsc-param-cta-gerencial.cod-rel = fi-cod-rel-i
        AND dsc-param-cta-gerencial.nr-seq =  fi-nr-seq-i NO-ERROR.
    IF AVAIL  dsc-param-cta-gerencial  THEN DO:
      MESSAGE "Sequencia j† cadastrada para esse relatorio" VIEW-AS ALERT-BOX.
      ASSIGN fi-nr-seq-i:SCREEN-VALUE = "".
    END.
    END.
    */
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

HIDE FRAME fr-digita-cta-ger.
HIDE FRAME fr-cta-datasul.

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
  DISPLAY fi-cod-rel fi-desc 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi-cod-rel fi-desc BtnOK-3 BR-3 bt-inclui bt-altera bt-exclui br-7 
         bt-inclui-2 bt-exclui-2 bt-cancelar RECT-18 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-cod-rel-i fi-nr-seq-i fi-cta-ger-i fi-desc-conta fi-ld-borda 
          cb-tipo-linha cb-tx-dolar 
      WITH FRAME fr-digita-cta-ger IN WINDOW C-Win.
  ENABLE fi-nr-seq-i fi-cta-ger-i fi-desc-conta fi-ld-borda cb-tipo-linha 
         cb-tx-dolar BtnOK BtnCancel 
      WITH FRAME fr-digita-cta-ger IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fr-digita-cta-ger}
  DISPLAY fi-cta-ger-i fi-cta-dat fi-cta-ini fi-cta-fim 
      WITH FRAME fr-cta-datasul IN WINDOW C-Win.
  ENABLE fi-cta-dat fi-cta-ini fi-cta-fim BtnOK-2 BtnCancel-2 RECT-19 
      WITH FRAME fr-cta-datasul IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fr-cta-datasul}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inclui-cta-ger C-Win 
PROCEDURE pi-inclui-cta-ger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

