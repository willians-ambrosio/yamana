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

DEF TEMP-TABLE tt-salvage-value
    FIELD tt-num-id-bem-pat         LIKE bem_pat_salvage_value.num_id_bem_pat
    FIELD tt-cod-cenar-ctbl         LIKE bem_pat_salvage_value.cod_cenar_ctbl
    FIELD tt-cod-exerc-ctbl         LIKE bem_pat_salvage_value.cod_exerc_ctbl
    FIELD tt-num-period-ctbl        LIKE bem_pat_salvage_value.num_period_ctbl
    FIELD tt-cod-indic-econ         LIKE bem_pat_salvage_value.cod_indic_econ
    FIELD tt-val-salvage            LIKE bem_pat_salvage_value.val_salvage_value
    FIELD tt-cod-fin-econ           LIKE bem_pat_salvage_value.cod_finalid_econ
    FIELD tt-num-seq-incorp-bem-pat LIKE bem_pat_salvage_value.num_seq_incorp_bem_pat.
   

DEF VAR i AS INT.


DEFINE NEW GLOBAL SHARED VARIABLE wh-rw-bem-pat         AS recid.

DEFINE NEW GLOBAL SHARED VARIABLE wh-rw-incorp-bem-pat         AS recid.


def new global shared var v_rec_indic_econ           as recid no-undo.
def new global shared var v_rec_cenar_ctbl           as recid no-undo.
def new global shared var v_rec_finalid_econ         as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-salvage

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-salvage-value

/* Definitions for BROWSE br-salvage                                    */
&Scoped-define FIELDS-IN-QUERY-br-salvage /* */ tt-cod-cenar-ctbl tt-cod-exerc-ctbl tt-num-period-ctbl tt-cod-indic-econ tt-num-seq-incorp-bem-pat tt-val-salvage   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-salvage   
&Scoped-define SELF-NAME br-salvage
&Scoped-define QUERY-STRING-br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-salvage OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-salvage tt-salvage-value
&Scoped-define FIRST-TABLE-IN-QUERY-br-salvage tt-salvage-value


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-salvage}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-salvage bt-inclui bt-altera bt-exclui ~
bt-cancelar RECT-18 RECT-19 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-emp fi-desc fi-cod-cta-pat ~
fi-desc-cta fi-num-bem-pat fi-num-seq-bem-pat fi-des-bem-pat 

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

DEFINE BUTTON bt-inclui 
     LABEL "Incluir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE VARIABLE fi-cod-cta-pat AS CHARACTER FORMAT "X(18)":U 
     LABEL "Conta Patrim." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-emp AS CHARACTER FORMAT "X(3)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-des-bem-pat AS CHARACTER FORMAT "X(40)":U 
     LABEL "Descriá∆o" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-cta AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-bem-pat AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Nr Bem Pat." 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-seq-bem-pat AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Nr Seq.Bem" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 4.5.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 9.5.

DEFINE BUTTON bt-canc AUTO-GO DEFAULT 
     LABEL "Cancela" 
     SIZE 9 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 9 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnOK-2 AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 9 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnOK-3 AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 9 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE fi-cenar-cont AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cen†rio Ctbl" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-exerc-ctbl AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Exerc.Ctbl" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-econ AS CHARACTER FORMAT "X(10)":U 
     LABEL "Fin.Econ." 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-moeda AS CHARACTER FORMAT "X(8)":U 
     LABEL "Moeda" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-seq-incorp AS INTEGER FORMAT ">>,>>>>>,>>9":U INITIAL 0 
     LABEL "Seq Incorp" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-per-ctbl AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Per°odo Ctbl" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-salvage-value AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Val.Salvage Value" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.57 BY 2.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.57 BY 4.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-salvage FOR 
      tt-salvage-value SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-salvage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-salvage C-Win _FREEFORM
  QUERY br-salvage NO-LOCK DISPLAY
      /* FORMAT ">>>9":U WIDTH 17,86 */
          
      tt-cod-cenar-ctbl      LABEL "  Cen†rio Ctbl  "  
      tt-cod-exerc-ctbl      LABEL "  Exerc.Ctbl  " 
      tt-num-period-ctbl     LABEL "  Per°odo Ctbl  " 
      tt-cod-indic-econ      LABEL "    Moeda     "
          tt-num-seq-incorp-bem-pat
      tt-val-salvage         LABEL " Val.Salvage Value " FORMAT ">,>>>,>>9.9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 68 BY 6.5
         FONT 1 ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cod-emp AT ROW 1.5 COL 11 COLON-ALIGNED
     fi-desc AT ROW 1.5 COL 18 COLON-ALIGNED NO-LABEL
     fi-cod-cta-pat AT ROW 2.5 COL 11 COLON-ALIGNED
     fi-desc-cta AT ROW 2.5 COL 23 COLON-ALIGNED NO-LABEL
     fi-num-bem-pat AT ROW 3.5 COL 11 COLON-ALIGNED
     fi-num-seq-bem-pat AT ROW 3.5 COL 31 COLON-ALIGNED
     fi-des-bem-pat AT ROW 4.5 COL 31 COLON-ALIGNED
     br-salvage AT ROW 6 COL 2
     bt-inclui AT ROW 12.5 COL 2 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-altera AT ROW 12.5 COL 12 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-exclui AT ROW 12.5 COL 22 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14 COL 59 HELP
          "Fechar"
     RECT-18 AT ROW 1.25 COL 1
     RECT-19 AT ROW 5.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.29 BY 15.04
         FONT 1.

DEFINE FRAME fr-salvage-value
     fi-cenar-cont AT ROW 1.25 COL 13 COLON-ALIGNED
     fi-exerc-ctbl AT ROW 1.25 COL 43 COLON-ALIGNED
     fi-per-ctbl AT ROW 2.25 COL 13 COLON-ALIGNED
     fi-moeda AT ROW 2.25 COL 43 COLON-ALIGNED
     fi-salvage-value AT ROW 3.25 COL 13 COLON-ALIGNED
     fi-fin-econ AT ROW 3.25 COL 38 COLON-ALIGNED
     fi-num-seq-incorp AT ROW 4.25 COL 13 COLON-ALIGNED
     BtnOK-3 AT ROW 6 COL 17
     BtnOK-2 AT ROW 6 COL 17
     BtnOK AT ROW 6 COL 17
     bt-canc AT ROW 6 COL 30
     RECT-20 AT ROW 5.5 COL 1
     RECT-21 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 8 ROW 6
         SIZE 56 BY 7.5
         FONT 1
         TITLE "Manutenc∆o de Salvage Value"
         DEFAULT-BUTTON BtnOK.


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
         TITLE              = "<Campo Salvage Value Incorporaá∆o do Bem - ESBEM03.001>"
         HEIGHT             = 14.29
         WIDTH              = 70.43
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
ASSIGN FRAME fr-salvage-value:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB br-salvage fi-des-bem-pat DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fi-cod-cta-pat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-emp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des-bem-pat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-cta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-bem-pat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-seq-bem-pat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fr-salvage-value
   NOT-VISIBLE                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-salvage
/* Query rebuild information for BROWSE br-salvage
     _START_FREEFORM
OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-salvage */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Campo Salvage Value Incorporaá∆o do Bem - ESBEM03.001> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Campo Salvage Value Incorporaá∆o do Bem - ESBEM03.001> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-salvage
&Scoped-define SELF-NAME br-salvage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-salvage C-Win
ON VALUE-CHANGED OF br-salvage IN FRAME DEFAULT-FRAME
DO:                             

  FIND CURRENT tt-salvage-value.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-altera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-altera C-Win
ON CHOOSE OF bt-altera IN FRAME DEFAULT-FRAME /* Alterar */
DO:                                              
  VIEW FRAME fr-salvage-value.
  DO WITH FRAME fr-salvage-value:
 
  FIND CURRENT tt-salvage-value NO-LOCK NO-ERROR.

  IF AVAIL tt-salvage-value THEN DO:
     ASSIGN  fi-cenar-cont     = tt-cod-cenar-ctbl
             fi-exerc-ctbl     = tt-cod-exerc-ctbl  
             fi-per-ctbl       = tt-num-period-ctbl
             fi-moeda          = tt-cod-indic-econ
             fi-salvage-value  = tt-val-salvage
             fi-fin-econ       = tt-cod-fin-econ
             fi-num-seq-incorp = tt-num-seq-incorp-bem-pat.   

     ASSIGN fi-cenar-cont:SENSITIVE = NO
            fi-exerc-ctbl:SENSITIVE = NO
            fi-per-ctbl:SENSITIVE   = NO
            fi-num-seq-incorp:SENSITIVE = NO.

      ASSIGN fi-cenar-cont:SCREEN-VALUE     =    fi-cenar-cont    
             fi-exerc-ctbl:SCREEN-VALUE     =    string(fi-exerc-ctbl)   
             fi-per-ctbl:SCREEN-VALUE       =    string(fi-per-ctbl) 
             fi-moeda:SCREEN-VALUE          =    fi-moeda         
             fi-salvage-value:SCREEN-VALUE  =    string(fi-salvage-value) 
             fi-fin-econ:SCREEN-VALUE       =    fi-fin-econ
             fi-num-seq-incorp:SCREEN-VALUE =    string(fi-num-seq-incorp).


      APPLY 'entry' TO fi-salvage-value /*fi-moeda*/.
     
      
  END.

  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-salvage-value
&Scoped-define SELF-NAME bt-canc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-canc C-Win
ON CHOOSE OF bt-canc IN FRAME fr-salvage-value /* Cancela */
DO:
    
    FOR EACH tt-salvage-value:
        DELETE tt-salvage-value.
    END.

    FOR EACH bem_pat_salvage_value
        WHERE bem_pat_salvage_value.num_id_bem_pat    = bem_pat.num_id_bem_pat 
          AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0 NO-LOCK:
        CREATE tt-salvage-value.
        ASSIGN tt-num-id-bem-pat         = bem_pat_salvage_value.num_id_bem_pat  
               tt-cod-cenar-ctbl         = bem_pat_salvage_value.cod_cenar_ctbl
               tt-cod-exerc-ctbl         = bem_pat_salvage_value.cod_exerc_ctbl
               tt-num-period-ctbl        = bem_pat_salvage_value.num_period_ctbl
               tt-cod-indic-econ         = bem_pat_salvage_value.cod_indic_econ
               tt-val-salvage            = bem_pat_salvage_value.val_salvage_value
               tt-cod-fin-econ           = bem_pat_salvage_value.cod_finalid_econ
               tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.
    END. 
        
    OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .

    HIDE FRAME fr-salvage-value.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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

   
   FIND CURRENT tt-salvage-value NO-ERROR.

   FIND bem_pat_salvage_value
      WHERE bem_pat_salvage_value.num_id_bem_pat         = tt-num-id-bem-pat
        AND bem_pat_salvage_value.cod_cenar_ctbl         = tt-cod-cenar-ctbl  
        AND bem_pat_salvage_value.cod_exerc_ctbl         = tt-cod-exerc-ctbl  
        AND bem_pat_salvage_value.num_period_ctbl        = tt-num-period-ctbl 
        AND bem_pat_salvage_value.num_seq_incorp_bem_pat = tt-num-seq-incorp-bem-pat NO-ERROR.

   IF AVAIL bem_pat_salvage_value THEN DO:
      DELETE bem_pat_salvage_value.
      DELETE tt-salvage-value.
      MESSAGE "Salvage Value Eliminado com Suceso !!!" VIEW-AS ALERT-BOX.

      OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK.

   END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inclui C-Win
ON CHOOSE OF bt-inclui IN FRAME DEFAULT-FRAME /* Incluir */
DO:
   
    do  on error undo, return no-apply:

     
       DO WITH FRAME fr-salvage-value:
          
          ASSIGN fi-cenar-cont:SCREEN-VALUE     = ""
                 fi-exerc-ctbl:SCREEN-VALUE     = ""
                 fi-per-ctbl:SCREEN-VALUE       = ""
                 fi-moeda:SCREEN-VALUE          = ""
                 fi-salvage-value:SCREEN-VALUE  = ""
                 fi-fin-econ:SCREEN-VALUE       = ""
                 fi-num-seq-incorp:SCREEN-VALUE = "".
       END.
          
       VIEW FRAME fr-salvage-value.
       
       FIND   bem_pat_salvage_value 
        WHERE  bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
          AND  bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
          AND  bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
          AND  bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl 
          AND  bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp  NO-ERROR.


       FIND LAST incorp_bem_pat WHERE 
                 incorp_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat NO-LOCK NO-ERROR.

       IF AVAIL bem_pat_salvage_value THEN
          MESSAGE "Salvage Value j† cadastrado !!!" VIEW-AS ALERT-BOX.
       ELSE DO:
          ASSIGN fi-cenar-cont:SENSITIVE = YES
                 fi-exerc-ctbl:SENSITIVE = YES
                 fi-per-ctbl:SENSITIVE   = YES
                 fi-num-seq-incorp:SENSITIVE   = YES.

          ASSIGN fi-cenar-cont:SCREEN-VALUE       = "CONTSOC"
                 fi-moeda:SCREEN-VALUE            = "DOLAR"
                 fi-fin-econ:SCREEN-VALUE         = "DOLAR".
                 /*
                 fi-exerc-ctbl:SCREEN-VALUE       =  string(year(bem_pat.dat_aquis_bem_pat))
                 fi-per-ctbl:SCREEN-VALUE         =  string(MONTH(bem_pat.dat_aquis_bem_pat)). */
          ASSIGN fi-exerc-ctbl:SCREEN-VALUE       =  string(year(incorp_bem_pat.dat_incorp_bem_pat))
                 fi-per-ctbl:SCREEN-VALUE         =  string(MONTH(incorp_bem_pat.dat_incorp_bem_pat)).



          /*   fi-num-seq-incorp:SCREEN-VALUE   =  STRING(bem_pat_salvage_value.num_seq_incorp_bem_pat). */


          APPLY 'entry' TO fi-salvage-value.


         /* APPLY 'entry' TO fi-cenar-cont. */
          
       END.
               
   end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fr-salvage-value
&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME fr-salvage-value /* OK */
DO:
  
    ASSIGN fi-cenar-cont    
           fi-exerc-ctbl    
           fi-per-ctbl      
           fi-moeda         
           fi-salvage-value 
           fi-fin-econ
           fi-num-seq-incorp.    
      

  IF AVAIL bem_pat 
       AND fi-num-seq-incorp > 0 THEN DO:

    FIND  bem_pat_salvage_value 
      WHERE  bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
        AND  bem_pat_salvage_value.cod_cenar_ctbl         = fi-cenar-cont
        AND  bem_pat_salvage_value.cod_exerc_ctbl         = fi-exerc-ctbl
        AND  bem_pat_salvage_value.num_period_ctbl        = fi-per-ctbl 
        AND  bem_pat_salvage_value.num_seq_incorp_bem_pat = fi-num-seq-incorp NO-ERROR.


     IF AVAIL bem_pat_salvage_value THEN DO:

        ASSIGN fi-moeda           = INPUT fi-moeda
               fi-salvage-value   = INPUT fi-salvage-value
               fi-fin-econ        = INPUT fi-fin-econ
               fi-num-seq-incorp  = INPUT fi-num-seq-incorp.

        assign bem_pat_salvage_value.cod_indic_econ    = fi-moeda
               bem_pat_salvage_value.val_salvage_value = fi-salvage-value
               bem_pat_salvage_value.cod_finalid_econ  = fi-fin-econ.

        
        HIDE FRAME fr-salvage-value.

        FOR EACH tt-salvage-value:
            DELETE tt-salvage-value.
        END.

        FOR EACH bem_pat_salvage_value
           WHERE bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat 
             AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0   NO-LOCK:
            CREATE tt-salvage-value.
            ASSIGN tt-num-id-bem-pat         = bem_pat_salvage_value.num_id_bem_pat  
                   tt-cod-cenar-ctbl         = bem_pat_salvage_value.cod_cenar_ctbl
                   tt-cod-exerc-ctbl         = bem_pat_salvage_value.cod_exerc_ctbl
                   tt-num-period-ctbl        = bem_pat_salvage_value.num_period_ctbl
                   tt-cod-indic-econ         = bem_pat_salvage_value.cod_indic_econ
                   tt-val-salvage            = bem_pat_salvage_value.val_salvage_value
                   tt-cod-fin-econ           = bem_pat_salvage_value.cod_finalid_econ
                   tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.
        END. 
        
        OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .
         
     END.

     ELSE DO:
          CREATE bem_pat_salvage_value. 
          ASSIGN bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
                 bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
                 bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
                 bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl
                 bem_pat_salvage_value.cod_indic_econ         = INPUT fi-moeda
                 bem_pat_salvage_value.val_salvage_value      = INPUT fi-salvage-value
                 bem_pat_salvage_value.cod_finalid_econ       = INPUT fi-fin-econ
                 bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp.

          MESSAGE "Salvage Value Criado com Sucesso !!!" VIEW-AS ALERT-BOX.


          FIND LAST incorp_bem_pat WHERE 
                    incorp_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat NO-LOCK NO-ERROR.

         MESSAGE "btnok  " VIEW-AS ALERT-BOX.
          FIND LAST /*first*/ period_ctbl 
              WHERE period_ctbl.cod_cenar_ctbl  = "gerenbvi"
                AND MONTH(dat_inic_period_ctbl) = MONTH(incorp_bem_pat.dat_incorp_bem_pat)
                AND YEAR(dat_inic_period_ctbl)  = year(incorp_bem_pat.dat_incorp_bem_pat) NO-LOCK NO-ERROR.
          
          IF AVAIL period_ctbl THEN do:
              
             CREATE bem_pat_salvage_value. 
             ASSIGN bem_pat_salvage_value.num_id_bem_pat    = bem_pat.num_ID_bem_pat
                    bem_pat_salvage_value.cod_cenar_ctbl    = "GERENBVI"
                    bem_pat_salvage_value.cod_exerc_ctbl    = INT(period_ctbl.cod_exerc_ctbl)
                    bem_pat_salvage_value.num_period_ctbl   = INT(period_ctbl.num_period_ctbl)
                    bem_pat_salvage_value.cod_indic_econ    = "DOLAR"  
                    bem_pat_salvage_value.val_salvage_value = INPUT fi-salvage-value
                    bem_pat_salvage_value.cod_finalid_econ  = "DOLAR"
                    bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp /*bem_pat_salvage_value.num_seq_incorp_bem_pat*/.

             MESSAGE "Salvage Value - GERENVBVI - Criado com Sucesso !!!" VIEW-AS ALERT-BOX.

             /* cida - 11/01/08 */
             HiDE FRAME fr-salvage-value.

             FOR EACH tt-salvage-value:
                 DELETE tt-salvage-value.
             END.

             FOR EACH bem_pat_salvage_value
                WHERE bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat 
                  AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0 NO-LOCK:
                 CREATE tt-salvage-value.
                 ASSIGN tt-num-id-bem-pat    = bem_pat_salvage_value.num_id_bem_pat  
                   tt-cod-cenar-ctbl         = bem_pat_salvage_value.cod_cenar_ctbl
                   tt-cod-exerc-ctbl         = bem_pat_salvage_value.cod_exerc_ctbl
                   tt-num-period-ctbl        = bem_pat_salvage_value.num_period_ctbl
                   tt-cod-indic-econ         = bem_pat_salvage_value.cod_indic_econ
                   tt-val-salvage            = bem_pat_salvage_value.val_salvage_value
                   tt-cod-fin-econ           = bem_pat_salvage_value.cod_finalid_econ
                   tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.
             END. 
        
             OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .
             
          END.

        
          ASSIGN  fi-cenar-cont:SCREEN-VALUE     = "CONTSOC"
                  fi-exerc-ctbl:SCREEN-VALUE     = string(year(bem_pat.dat_aquis_bem_pat))
                  fi-per-ctbl:SCREEN-VALUE       = string(MONTH(bem_pat.dat_aquis_bem_pat))
                  fi-moeda:SCREEN-VALUE          = "DOLAR"
                  fi-salvage-value:SCREEN-VALUE  = "0"
                  fi-fin-econ:SCREEN-VALUE       = "DOLAR"
                  fi-num-seq-incorp:SCREEN-VALUE = "0". 
         

          APPLY 'entry' TO fi-cenar-cont.
     END.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK-2 C-Win
ON CHOOSE OF BtnOK-2 IN FRAME fr-salvage-value /* OK */
DO:
 
 
  IF AVAIL bem_pat 
       AND INPUT fi-num-seq-incorp > 0 THEN DO:

    FIND     bem_pat_salvage_value 
      WHERE  bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_bem_pat
        AND  bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
        AND  bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
        AND  bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl 
        AND  bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp NO-ERROR.


     IF AVAIL bem_pat_salvage_value THEN DO:

        ASSIGN fi-moeda           = INPUT fi-moeda
               fi-salvage-value   = INPUT fi-salvage-value
               fi-fin-econ        = INPUT fi-fin-econ.

        assign bem_pat_salvage_value.cod_indic_econ         = fi-moeda
               bem_pat_salvage_value.val_salvage_value      = fi-salvage-value
               bem_pat_salvage_value.cod_finalid_econ       = fi-fin-econ
               bem_pat_salvage_value.num_seq_incorp_bem_pat = fi-num-seq-incorp.

        MESSAGE "Salvage Value Alterado com Sucesso !!!" VIEW-AS ALERT-BOX.

        HIDE FRAME fr-salvage-value.

        FOR EACH tt-salvage-value:
            DELETE tt-salvage-value.
        END.

        FOR EACH bem_pat_salvage_value
            WHERE bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_bem_pat 
              AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0 NO-LOCK:
            CREATE tt-salvage-value.
            ASSIGN tt-num-id-bem-pat         = bem_pat_salvage_value.num_id_bem_pat  
                   tt-cod-cenar-ctbl         = bem_pat_salvage_value.cod_cenar_ctbl
                   tt-cod-exerc-ctbl         = bem_pat_salvage_value.cod_exerc_ctbl
                   tt-num-period-ctbl        = bem_pat_salvage_value.num_period_ctbl
                   tt-cod-indic-econ         = bem_pat_salvage_value.cod_indic_econ
                   tt-val-salvage            = bem_pat_salvage_value.val_salvage_value
                   tt-cod-fin-econ           = bem_pat_salvage_value.cod_finalid_econ
                   tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.
        END. 
        
        OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .
         
     END.

     ELSE DO:
          CREATE bem_pat_salvage_value. 
          ASSIGN bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
                 bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
                 bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
                 bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl
                 bem_pat_salvage_value.cod_indic_econ         = INPUT fi-moeda
                 bem_pat_salvage_value.val_salvage_value      = INPUT fi-salvage-value
                 bem_pat_salvage_value.cod_finalid_econ       = INPUT fi-fin-econ
                 bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp.

          MESSAGE "Salvage Value Criado com Sucesso !!!" VIEW-AS ALERT-BOX.

          FIND LAST /* FIRST*/ period_ctbl 
              WHERE period_ctbl.cod_cenar_ctbl  = "gerenbvi"
                AND MONTH(dat_inic_period_ctbl) = MONTH(bem_pat.dat_aquis_bem_pat)
                AND YEAR(dat_inic_period_ctbl)  = year(bem_pat.dat_aquis_bem_pat) NO-LOCK NO-ERROR.
          
          IF AVAIL period_ctbl THEN do:
              
             CREATE bem_pat_salvage_value. 
             ASSIGN bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
                    bem_pat_salvage_value.cod_cenar_ctbl         = "GERENBVI"
                    bem_pat_salvage_value.cod_exerc_ctbl         = INT(period_ctbl.cod_exerc_ctbl)
                    bem_pat_salvage_value.num_period_ctbl        = INT(period_ctbl.num_period_ctbl)
                    bem_pat_salvage_value.cod_indic_econ         = "DOLAR"  
                    bem_pat_salvage_value.val_salvage_value      = INPUT fi-salvage-value
                    bem_pat_salvage_value.cod_finalid_econ       = "DOLAR"
                    bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp.

             MESSAGE "Salvage Value - GERENVBVI - Criado com Sucesso !!!" VIEW-AS ALERT-BOX.
          END.


          ASSIGN  fi-cenar-cont:SCREEN-VALUE     = ""
                  fi-exerc-ctbl:SCREEN-VALUE     = ""
                  fi-per-ctbl:SCREEN-VALUE       = "0"
                  fi-moeda:SCREEN-VALUE          = ""
                  fi-salvage-value:SCREEN-VALUE  = "0"
                  fi-fin-econ:SCREEN-VALUE       = ""
                  fi-num-seq-incorp:SCREEN-VALUE = "0".     

          APPLY 'entry' TO fi-cenar-cont.
     END.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK-3 C-Win
ON CHOOSE OF BtnOK-3 IN FRAME fr-salvage-value /* OK */
DO:
  

  IF AVAIL bem_pat 
       AND fi-num-seq-incorp > 0 THEN DO:

    FIND     bem_pat_salvage_value 
      WHERE  bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
        AND  bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
        AND  bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
        AND  bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl 
        AND  bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp NO-ERROR.

   
     IF AVAIL bem_pat_salvage_value THEN DO:

        ASSIGN fi-moeda           = INPUT fi-moeda
               fi-salvage-value   = INPUT fi-salvage-value
               fi-fin-econ        = INPUT fi-fin-econ.

        assign bem_pat_salvage_value.cod_indic_econ         = fi-moeda
               bem_pat_salvage_value.val_salvage_value      = fi-salvage-value
               bem_pat_salvage_value.cod_finalid_econ       = fi-fin-econ
               bem_pat_salvage_value.num_seq_incorp_bem_pat = fi-num-seq-incorp.

        MESSAGE "Salvage Value Alterado com Sucesso !!!" VIEW-AS ALERT-BOX.

        HIDE FRAME fr-salvage-value.

        FOR EACH tt-salvage-value:
            DELETE tt-salvage-value.
        END.

        FOR EACH bem_pat_salvage_value
           WHERE bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat 
             AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0  NO-LOCK:
            CREATE tt-salvage-value.
            ASSIGN tt-num-id-bem-pat         = bem_pat_salvage_value.num_id_bem_pat  
                   tt-cod-cenar-ctbl         = bem_pat_salvage_value.cod_cenar_ctbl
                   tt-cod-exerc-ctbl         = bem_pat_salvage_value.cod_exerc_ctbl
                   tt-num-period-ctbl        = bem_pat_salvage_value.num_period_ctbl
                   tt-cod-indic-econ         = bem_pat_salvage_value.cod_indic_econ
                   tt-val-salvage            = bem_pat_salvage_value.val_salvage_value
                   tt-cod-fin-econ           = bem_pat_salvage_value.cod_finalid_econ
                   tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.
        END. 
        
        OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .
         
     END.

     ELSE DO:
          CREATE bem_pat_salvage_value. 
          ASSIGN bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
                 bem_pat_salvage_value.cod_cenar_ctbl         = INPUT fi-cenar-cont
                 bem_pat_salvage_value.cod_exerc_ctbl         = INPUT fi-exerc-ctbl
                 bem_pat_salvage_value.num_period_ctbl        = INPUT fi-per-ctbl
                 bem_pat_salvage_value.cod_indic_econ         = INPUT fi-moeda
                 bem_pat_salvage_value.val_salvage_value      = INPUT fi-salvage-value
                 bem_pat_salvage_value.cod_finalid_econ       = INPUT fi-fin-econ
                 bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp.

          MESSAGE "Salvage Value Criado com Sucesso !!!" VIEW-AS ALERT-BOX.

          FIND LAST /*FIRST*/ period_ctbl 
              WHERE period_ctbl.cod_cenar_ctbl  = "gerenbvi"
                AND MONTH(dat_inic_period_ctbl) = MONTH(bem_pat.dat_aquis_bem_pat)
                AND YEAR(dat_inic_period_ctbl)  = year(bem_pat.dat_aquis_bem_pat) NO-LOCK NO-ERROR.
          
          IF AVAIL period_ctbl THEN do:
              
             CREATE bem_pat_salvage_value. 
             ASSIGN bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
                    bem_pat_salvage_value.cod_cenar_ctbl         = "GERENBVI"
                    bem_pat_salvage_value.cod_exerc_ctbl         = INT(period_ctbl.cod_exerc_ctbl)
                    bem_pat_salvage_value.num_period_ctbl        = INT(period_ctbl.num_period_ctbl)
                    bem_pat_salvage_value.cod_indic_econ         = "DOLAR"  
                    bem_pat_salvage_value.val_salvage_value      = INPUT fi-salvage-value
                    bem_pat_salvage_value.cod_finalid_econ       = "DOLAR"
                    bem_pat_salvage_value.num_seq_incorp_bem_pat = INPUT fi-num-seq-incorp.

             MESSAGE "Salvage Value - GERENVBVI - Criado com Sucesso !!!" VIEW-AS ALERT-BOX.

          END.   

          ASSIGN  fi-cenar-cont:SCREEN-VALUE      = ""
                  fi-exerc-ctbl:SCREEN-VALUE      = ""
                  fi-per-ctbl:SCREEN-VALUE        = "0"
                  fi-moeda:SCREEN-VALUE           = ""
                  fi-salvage-value:SCREEN-VALUE   = "0"
                  fi-fin-econ:SCREEN-VALUE        = ""
                  fi-num-seq-incorp:SCREEN-VALUE  = "0".     

          APPLY 'entry' TO fi-cenar-cont.
     END.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cenar-cont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cenar-cont C-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cenar-cont IN FRAME fr-salvage-value /* Cen†rio Ctbl */
DO:
  
    find first cenar_ctbl no-lock

          where cenar_ctbl.cod_cenar_ctbl = "Prim†rio" no-error.

     if  avail cenar_ctbl then

         assign v_rec_cenar_ctbl = recid(cenar_ctbl).



     run prgint/utb/utb076aa.p. /*prgint/utb/utb080nc.p.*/

     if  v_rec_cenar_ctbl <> ? then do:

         find first cenar_ctbl no-lock

              where recid(cenar_ctbl) = v_rec_cenar_ctbl no-error.

         assign fi-cenar-cont:screen-value in frame fr-salvage-value = cenar_ctbl.cod_cenar_ctbl.

     end.










END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-econ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-econ C-Win
ON MOUSE-SELECT-DBLCLICK OF fi-fin-econ IN FRAME fr-salvage-value /* Fin.Econ. */
DO:
  find first finalid_econ no-lock            
          where finalid_econ.cod_finalid_econ = "Prim†rio" no-error.

     if  avail finalid_econ  then
         assign v_rec_finalid_econ  = recid(finalid_econ ).

     run prgint/utb/utb077aa.p. /*prgint/utb/utb080nc.p.*/

     if  v_rec_finalid_econ  <> ? then do:

         find first finalid_econ  no-lock
              where recid(finalid_econ ) = v_rec_finalid_econ  no-error.

         assign fi-fin-econ:screen-value in frame fr-salvage-value = finalid_econ.cod_finalid_econ.

     end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-moeda C-Win
ON MOUSE-SELECT-CLICK OF fi-moeda IN FRAME fr-salvage-value /* Moeda */
DO:


    find first indic_econ no-lock

         where indic_econ.cod_indic_econ = "Prim†rio" no-error.

    if  avail indic_econ then

        assign v_rec_indic_econ = recid(indic_econ).

 

    run prgint/utb/utb013aa.p. /*prgint/utb/utb080nc.p.*/

    if  v_rec_indic_econ <> ? then do:

        find first indic_econ no-lock

             where recid(indic_econ) = v_rec_indic_econ no-error.

        assign fi-moeda:screen-value in frame fr-salvage-value = indic_econ.cod_indic_econ.

    end.














END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-seq-incorp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-seq-incorp C-Win
ON LEAVE OF fi-num-seq-incorp IN FRAME fr-salvage-value /* Seq Incorp */
DO:
  ASSIGN fi-num-seq-incorp.

      IF  fi-num-seq-incorp = 0 THEN DO:
          MESSAGE "Sequencia de Incorporaá∆o n∆o pode ser ZERO!" VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-num-seq-incorp.
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

HIDE FRAME fr-salvage-value.

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

  /* FIND FIRST incorp_bem_pat WHERE      
        Recid(incorp_bem_pat) = wh-rw-incorp-bem-pat NO-LOCK NO-ERROR.

   MESSAGE "**no** esac03 " incorp_bem_pat.num_id_bem_pat.

   FIND FIRST bem_pat WHERE      
        bem_pat.num_id_bem_pat  = incorp_bem_pat.num_id_bem_pat NO-LOCK NO-ERROR.
   */

   FIND FIRST bem_pat WHERE      
        recid(bem_pat)  = wh-rw-bem-pat  NO-LOCK NO-ERROR. 

  IF AVAIL bem_pat THEN DO:
   
     ASSIGN fi-cod-emp = bem_pat.cod_empresa.
     FIND ems5.empresa WHERE empresa.cod_empresa = bem_pat.cod_empresa NO-LOCK NO-ERROR.
     IF AVAIL empresa THEN 
        ASSIGN fi-desc = empresa.nom_razao_social.
     
     ASSIGN fi-cod-cta-pat     = bem_pat.cod_cta_pat
            /*fi-desc-cta      = bem_pat. */
            fi-num-bem-pat     = bem_pat.num_bem_pat
            fi-num-seq-bem-pat = bem_pat.num_seq_bem_pat
            fi-des-bem-pat     = bem_pat.des_bem_pat.

    FIND cta_pat 
        WHERE cta_pat.cod_empresa =  bem_pat.cod_empresa 
          AND cta_pat.cod_cta_pat =  bem_pat.cod_cta_pat NO-LOCK NO-ERROR.

    IF AVAIL cta_pat THEN
       ASSIGN fi-desc-cta      = cta_pat.des_cta_pat.

  
    FOR EACH bem_pat_salvage_value
        WHERE bem_pat_salvage_value.num_id_bem_pat    = bem_pat.num_id_bem_pat 
        AND bem_pat_salvage_value.num_seq_incorp_bem_pat > 0 NO-LOCK:
        CREATE tt-salvage-value.
        ASSIGN tt-num-id-bem-pat  = bem_pat_salvage_value.num_id_bem_pat  
               tt-cod-cenar-ctbl  = bem_pat_salvage_value.cod_cenar_ctbl
               tt-cod-exerc-ctbl  = bem_pat_salvage_value.cod_exerc_ctbl
               tt-num-period-ctbl = bem_pat_salvage_value.num_period_ctbl
               tt-cod-indic-econ  = bem_pat_salvage_value.cod_indic_econ
               tt-val-salvage     = bem_pat_salvage_value.val_salvage_value
               tt-cod-fin-econ    = bem_pat_salvage_value.cod_finalid_econ
               tt-num-seq-incorp-bem-pat = bem_pat_salvage_value.num_seq_incorp_bem_pat.

      
    END.

    OPEN QUERY br-salvage /*{&SELF-NAME}*/ FOR EACH tt-salvage-value  NO-LOCK .
      
    
  END.


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
  DISPLAY fi-cod-emp fi-desc fi-cod-cta-pat fi-desc-cta fi-num-bem-pat 
          fi-num-seq-bem-pat fi-des-bem-pat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE br-salvage bt-inclui bt-altera bt-exclui bt-cancelar RECT-18 RECT-19 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-cenar-cont fi-exerc-ctbl fi-per-ctbl fi-moeda fi-salvage-value 
          fi-fin-econ fi-num-seq-incorp 
      WITH FRAME fr-salvage-value IN WINDOW C-Win.
  ENABLE fi-cenar-cont fi-exerc-ctbl fi-per-ctbl fi-moeda fi-salvage-value 
         fi-fin-econ fi-num-seq-incorp BtnOK-3 BtnOK-2 BtnOK bt-canc RECT-20 
         RECT-21 
      WITH FRAME fr-salvage-value IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fr-salvage-value}
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

