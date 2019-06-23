&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
def shared var v_cdn_funcionario_fp1500ymep  as INTEGER no-undo.
def shared var v_prog_chamador_fp1500ymep  as CHARACTER no-undo.
def var v_mes_ano_corrente as CHARACTER NO-UNDO.
DEFINE VARIABLE v_log_corporativo AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cdn_indiv_factor_pag AS INT       NO-UNDO.
DEFINE VARIABLE cdn_indiv_factor_prov AS INT       NO-UNDO.

{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-indiv-factor fi-prov-indiv-factor ~
bt-indiv-factor bt-indiv-factor-2 bt-ok bt-cancela rt-button RECT-2 
&Scoped-Define DISPLAYED-OBJECTS fi-indiv-factor fi-prov-indiv-factor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_busca_hora W-Win 
FUNCTION fn_busca_hora RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-indiv-factor 
     LABEL "Hist¢rico" 
     SIZE 11.72 BY 1.

DEFINE BUTTON bt-indiv-factor-2 
     LABEL "Hist¢rico" 
     SIZE 11.72 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-indiv-factor AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Individual factor (PLR)" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-prov-indiv-factor AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Prov Individual factor (PLR)" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 3.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 48.14 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-indiv-factor AT ROW 1.46 COL 27.57 COLON-ALIGNED WIDGET-ID 12
     fi-prov-indiv-factor AT ROW 2.67 COL 27.57 COLON-ALIGNED WIDGET-ID 18
     bt-indiv-factor AT ROW 1.42 COL 35.86 WIDGET-ID 14
     bt-indiv-factor-2 AT ROW 2.63 COL 35.86 WIDGET-ID 16
     bt-ok AT ROW 4.25 COL 2 HELP
          "Salva e sai" WIDGET-ID 8
     bt-cancela AT ROW 4.25 COL 12.57 HELP
          "Cancela" WIDGET-ID 6
     rt-button AT ROW 4.08 COL 1 WIDGET-ID 4
     RECT-2 AT ROW 1.04 COL 1.14 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 48.14 BY 4.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Indicadores PLR"
         HEIGHT             = 4.58
         WIDTH              = 48.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Indicadores PLR */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Indicadores PLR */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela W-Win
ON CHOOSE OF bt-cancela IN FRAME F-Main /* Cancelar */
DO:
    IF VALID-HANDLE(h-pai) THEN DO:
        ASSIGN h-pai:SENSITIVE = YES.
    END.
    APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-indiv-factor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-indiv-factor W-Win
ON CHOOSE OF bt-indiv-factor IN FRAME F-Main /* Hist¢rico */
DO:
  run prghur/esp/fp1500ymepa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-indiv-factor-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-indiv-factor-2 W-Win
ON CHOOSE OF bt-indiv-factor-2 IN FRAME F-Main /* Hist¢rico */
DO:
  run prghur/esp/fp1500ymepb.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok W-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    RUN pi-valida-folha-calculada.
    IF RETURN-VALUE = "NOK":U THEN DO:
        RUN utp/ut-msgs.p(INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" + "~~" 
                          + "Os campos Individual factor(PLR) e Prov Indiv Factor(PLR) n∆o foram salvos porque a folha do funcion†rio j† est† calculada para o màs " + v_mes_ano_corrente).
        RETURN "NOK":U.
    END.
    ELSE DO:
        DO  ON ERROR UNDO, LEAVE:
            RUN pi-verifica-corporativo.
            RUN pi-cria-func-indiv-factor-pag.
            RUN pi-cria-func-indiv-factor-prov.
            
            IF VALID-HANDLE(h-pai) THEN DO:
                ASSIGN h-pai:SENSITIVE = YES.
            END.

            APPLY 'close' TO THIS-PROCEDURE.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-indiv-factor fi-prov-indiv-factor 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi-indiv-factor fi-prov-indiv-factor bt-indiv-factor bt-indiv-factor-2 
         bt-ok bt-cancela rt-button RECT-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE l-encontrou AS LOGICAL INITIAL NO NO-UNDO.
  
  ASSIGN h-pai = SOURCE-PROCEDURE
         h-pai = h-pai:CURRENT-WINDOW.

  IF NOT VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai = SESSION:FIRST-CHILD
             h-pai = h-pai:NEXT-SIBLING.
  END.

  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = NO.
  END.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF v_prog_chamador_fp1500ymep = 'prghur/fpp/fp1500.w':U THEN DO:
      ASSIGN fi-indiv-factor:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-prov-indiv-factor:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
  END.
  ELSE DO:
      ASSIGN fi-indiv-factor:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             fi-prov-indiv-factor:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.

  FOR EACH tip_func_indiv_factor_pag NO-LOCK
      WHERE tip_func_indiv_factor_pag.cdn_funcionario = v_cdn_funcionario_fp1500ymep 
      BREAK BY tip_func_indiv_factor_pag.dt_inicio:
      IF LAST(tip_func_indiv_factor_pag.dt_inicio) THEN DO:
          ASSIGN fi-indiv-factor:SCREEN-VALUE = String(tip_func_indiv_factor_pag.cdn_indiv_factor)
                 l-encontrou = YES
                 cdn_indiv_factor_pag = tip_func_indiv_factor_pag.cdn_indiv_factor.

      END.
  END.

  IF NOT l-encontrou THEN DO:
      ASSIGN fi-indiv-factor:SCREEN-VALUE = "".
  END.
  ELSE DO:
      ASSIGN l-encontrou = NO.
  END.


  FOR EACH tip_func_indiv_factor_prov NO-LOCK
      WHERE tip_func_indiv_factor_prov.cdn_funcionario = v_cdn_funcionario_fp1500ymep 
      BREAK BY tip_func_indiv_factor_prov.dt_inicio:
      IF LAST(tip_func_indiv_factor_prov.dt_inicio) THEN DO:
          ASSIGN fi-prov-indiv-factor:SCREEN-VALUE = String(tip_func_indiv_factor_prov.cdn_indiv_factor)
                 l-encontrou = YES
                 cdn_indiv_factor_prov = tip_func_indiv_factor_prov.cdn_indiv_factor.
      END.
  END.

  IF NOT l-encontrou THEN DO:
      ASSIGN fi-prov-indiv-factor:SCREEN-VALUE = "".
  END.
  ELSE DO:
      ASSIGN l-encontrou = NO.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-func-indiv-factor-pag W-Win 
PROCEDURE pi-cria-func-indiv-factor-pag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE v_date AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l_existe_indiv_factor AS LOGICAL INITIAL NO NO-UNDO.
    
    ASSIGN v_date = "01/" + v_mes_ano_corrente.
    FIND FIRST tip_func_indiv_factor_pag
         WHERE tip_func_indiv_factor_pag.cdn_funcionario = v_cdn_funcionario_fp1500ymep 
           AND tip_func_indiv_factor_pag.cdn_empresa = funcionario.cdn_empresa
           AND tip_func_indiv_factor_pag.cdn_estab = funcionario.cdn_estab
           AND tip_func_indiv_factor_pag.LOG_corporativo = v_log_corporativo
           AND tip_func_indiv_factor_pag.dt_inicio = date(v_date)
               EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_func_indiv_factor_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
        FOR EACH  tip_indiv_factor
            WHERE tip_indiv_factor.cdn_indiv_factor = int(fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_indiv_factor.cdn_empresa      = funcionario.cdn_empresa 
              AND tip_indiv_factor.cdn_estab = funcionario.cdn_estab
              AND tip_indiv_factor.LOG_corporativo = v_log_corporativo
              AND tip_indiv_factor.dt_inicio       <= date(v_date) BREAK BY tip_indiv_factor.dt_inicio : 
            IF LAST(tip_indiv_factor.dt_inicio) THEN DO:
            
                IF int(fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} )  <> cdn_indiv_factor_pag THEN DO:
                    RUN pi-cria-historico-pag.
                END.

                ASSIGN tip_func_indiv_factor_pag.cdn_funcionario = v_cdn_funcionario_fp1500ymep
                       tip_func_indiv_factor_pag.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
                       tip_func_indiv_factor_pag.indice = tip_indiv_factor.indice
                       tip_func_indiv_factor_pag.cod_usuario = v_cod_usuar_corren
                       tip_func_indiv_factor_pag.dt_inicio = DATE(v_date)
                       tip_func_indiv_factor_pag.dt_termino = 12/31/9999
                       tip_func_indiv_factor_pag.dt_inclusao = TODAY
                       tip_func_indiv_factor_pag.hr_inclusao = fn_busca_hora()
                       l_existe_indiv_factor = YES.
            END.
        END.
        /*N∆o existe nenhum tip_indiv_factor anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + v_date).
            ASSIGN fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_func_indiv_factor_pag.cdn_indiv_factor).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_indiv_factor
            WHERE tip_indiv_factor.cdn_indiv_factor = int(fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_indiv_factor.cdn_empresa      = funcionario.cdn_empresa 
              AND tip_indiv_factor.cdn_estab = funcionario.cdn_estab
              AND tip_indiv_factor.LOG_corporativo = v_log_corporativo
              AND tip_indiv_factor.dt_inicio       <= date(v_date) BREAK BY tip_indiv_factor.dt_inicio : 
            IF LAST(tip_indiv_factor.dt_inicio) THEN DO:
    
                RUN pi-cria-historico-pag.

                CREATE tip_func_indiv_factor_pag.
                ASSIGN tip_func_indiv_factor_pag.cdn_funcionario = v_cdn_funcionario_fp1500ymep
                       tip_func_indiv_factor_pag.cdn_empresa = funcionario.cdn_empresa
                       tip_func_indiv_factor_pag.cdn_estab = funcionario.cdn_estab
                       tip_func_indiv_factor_pag.LOG_corporativo = v_log_corporativo
                       tip_func_indiv_factor_pag.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
                       tip_func_indiv_factor_pag.indice = tip_indiv_factor.indice
                       tip_func_indiv_factor_pag.cod_usuario = v_cod_usuar_corren
                       tip_func_indiv_factor_pag.dt_inicio = DATE(v_date)
                       tip_func_indiv_factor_pag.dt_termino = 12/31/9999
                       tip_func_indiv_factor_pag.dt_inclusao = TODAY
                       tip_func_indiv_factor_pag.hr_inclusao = fn_busca_hora()
                       l_existe_indiv_factor = YES.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + v_date).
            ASSIGN fi-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-func-indiv-factor-prov W-Win 
PROCEDURE pi-cria-func-indiv-factor-prov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE v_date AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l_existe_indiv_factor AS LOGICAL INITIAL NO NO-UNDO.
    
    ASSIGN v_date = "01/" + v_mes_ano_corrente.
    FIND FIRST tip_func_indiv_factor_prov
         WHERE tip_func_indiv_factor_prov.cdn_funcionario = v_cdn_funcionario_fp1500ymep 
           AND tip_func_indiv_factor_prov.cdn_empresa = funcionario.cdn_empresa
           AND tip_func_indiv_factor_prov.cdn_estab = funcionario.cdn_estab
           AND tip_func_indiv_factor_prov.LOG_corporativo = v_log_corporativo
           AND tip_func_indiv_factor_prov.dt_inicio = date(v_date)
               EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_func_indiv_factor_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
        
        FOR EACH  tip_indiv_factor
            WHERE tip_indiv_factor.cdn_indiv_factor = int(fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_indiv_factor.cdn_empresa        = funcionario.cdn_empresa 
              AND tip_indiv_factor.cdn_estab = funcionario.cdn_estab
              AND tip_indiv_factor.LOG_corporativo = v_log_corporativo
              AND tip_indiv_factor.dt_inicio       <= date(v_date) BREAK BY tip_indiv_factor.dt_inicio : 
            IF LAST(tip_indiv_factor.dt_inicio) THEN DO:
               
                IF cdn_indiv_factor_prov <> int(fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME})  THEN DO:
                    RUN pi-cria-historico-prov.
                END.                           

                ASSIGN tip_func_indiv_factor_prov.cdn_funcionario = v_cdn_funcionario_fp1500ymep
                       tip_func_indiv_factor_prov.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
                       tip_func_indiv_factor_prov.indice = tip_indiv_factor.indice
                       tip_func_indiv_factor_prov.cod_usuario = v_cod_usuar_corren
                       tip_func_indiv_factor_prov.dt_inicio = DATE(v_date)
                       tip_func_indiv_factor_prov.dt_termino = 12/31/9999
                       tip_func_indiv_factor_prov.dt_inclusao = TODAY
                       tip_func_indiv_factor_prov.hr_inclusao = fn_busca_hora()
                       l_existe_indiv_factor = YES.
                
                
            END.
        END.
        /*N∆o existe nenhum tip_indiv_factor anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + v_date).
            ASSIGN fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(tip_func_indiv_factor_prov.cdn_indiv_factor).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_indiv_factor
            WHERE tip_indiv_factor.cdn_indiv_factor = int(fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_indiv_factor.cdn_empresa        = funcionario.cdn_empresa 
              AND tip_indiv_factor.cdn_estab = funcionario.cdn_estab
              AND tip_indiv_factor.LOG_corporativo = v_log_corporativo
              AND tip_indiv_factor.dt_inicio       <= date(v_date) BREAK BY tip_indiv_factor.dt_inicio : 
            IF LAST(tip_indiv_factor.dt_inicio) THEN DO:
    
                RUN pi-cria-historico-prov.

                CREATE tip_func_indiv_factor_prov.
                ASSIGN tip_func_indiv_factor_prov.cdn_funcionario = v_cdn_funcionario_fp1500ymep
                       tip_func_indiv_factor_prov.cdn_empresa = funcionario.cdn_empresa
                       tip_func_indiv_factor_prov.cdn_estab = funcionario.cdn_estab
                       tip_func_indiv_factor_prov.LOG_corporativo = v_log_corporativo
                       tip_func_indiv_factor_prov.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
                       tip_func_indiv_factor_prov.indice = tip_indiv_factor.indice
                       tip_func_indiv_factor_prov.cod_usuario = v_cod_usuar_corren
                       tip_func_indiv_factor_prov.dt_inicio = DATE(v_date)
                       tip_func_indiv_factor_prov.dt_termino = 12/31/9999
                       tip_func_indiv_factor_prov.dt_inclusao = TODAY
                       tip_func_indiv_factor_prov.hr_inclusao = fn_busca_hora()
                       l_existe_indiv_factor = YES.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + v_date).
            ASSIGN fi-prov-indiv-factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-historico-pag W-Win 
PROCEDURE pi-cria-historico-pag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_func_indiv_factor_pag.
    ASSIGN tiph_func_indiv_factor_pag.cdn_empresa      = tip_indiv_factor.cdn_empresa
           tiph_func_indiv_factor_pag.cdn_estab        = tip_indiv_factor.cdn_estab
           tiph_func_indiv_factor_pag.LOG_corporativo  = tip_indiv_factor.LOG_corporativo
           tiph_func_indiv_factor_pag.cdn_funcionario  = v_cdn_funcionario_fp1500ymep
           tiph_func_indiv_factor_pag.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
           tiph_func_indiv_factor_pag.indice           = tip_indiv_factor.indice
           tiph_func_indiv_factor_pag.cod_usuario      = v_cod_usuar_corren
           tiph_func_indiv_factor_pag.dt_inicio        = DATE("01/" + v_mes_ano_corrente)
           tiph_func_indiv_factor_pag.dt_termino       = 12/31/9999
           tiph_func_indiv_factor_pag.dt_inclusao      = TODAY
           tiph_func_indiv_factor_pag.hr_inclusao      = fn_busca_hora()
           tiph_func_indiv_factor_pag.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
           v_hr_inclusao = tiph_func_indiv_factor_pag.hr_inclusao.

    FOR LAST  tiph_func_indiv_factor_pag EXCLUSIVE-LOCK
        WHERE tiph_func_indiv_factor_pag.cdn_funcionario = v_cdn_funcionario_fp1500ymep
          AND tiph_func_indiv_factor_pag.hr_inclusao <> v_hr_inclusao 
           BY tiph_func_indiv_factor_pag.dt_inclusao
           BY tiph_func_indiv_factor_pag.hr_inclusao:
       
        IF tiph_func_indiv_factor_pag.dt_inicio = DATE("01/" + v_mes_ano_corrente) THEN
            ASSIGN tiph_func_indiv_factor_pag.dt_termino       = DATE("01/" + v_mes_ano_corrente).
        ELSE 
            ASSIGN tiph_func_indiv_factor_pag.dt_termino       = DATE("01/" + v_mes_ano_corrente) - 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-historico-prov W-Win 
PROCEDURE pi-cria-historico-prov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_func_indiv_factor_prov.
    ASSIGN tiph_func_indiv_factor_prov.cdn_empresa      = tip_indiv_factor.cdn_empresa
           tiph_func_indiv_factor_prov.cdn_estab        = tip_indiv_factor.cdn_estab
           tiph_func_indiv_factor_prov.LOG_corporativo  = tip_indiv_factor.LOG_corporativo
           tiph_func_indiv_factor_prov.cdn_funcionario  = v_cdn_funcionario_fp1500ymep
           tiph_func_indiv_factor_prov.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor
           tiph_func_indiv_factor_prov.indice           = tip_indiv_factor.indice
           tiph_func_indiv_factor_prov.cod_usuario      = v_cod_usuar_corren
           tiph_func_indiv_factor_prov.dt_inicio        = DATE("01/" + v_mes_ano_corrente)
           tiph_func_indiv_factor_prov.dt_termino       = 12/31/9999
           tiph_func_indiv_factor_prov.dt_inclusao      = TODAY
           tiph_func_indiv_factor_prov.hr_inclusao      = fn_busca_hora()
           tiph_func_indiv_factor_prov.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
           v_hr_inclusao = tiph_func_indiv_factor_prov.hr_inclusao.

    FOR LAST  tiph_func_indiv_factor_prov EXCLUSIVE-LOCK
        WHERE tiph_func_indiv_factor_prov.cdn_funcionario = v_cdn_funcionario_fp1500ymep
          AND tiph_func_indiv_factor_prov.hr_inclusao <> v_hr_inclusao 
           BY tiph_func_indiv_factor_prov.dt_inclusao
           BY tiph_func_indiv_factor_prov.hr_inclusao:
       
        IF tiph_func_indiv_factor_prov.dt_inicio = DATE("01/" + v_mes_ano_corrente) THEN
            ASSIGN tiph_func_indiv_factor_prov.dt_termino       = DATE("01/" + v_mes_ano_corrente).
        ELSE 
            ASSIGN tiph_func_indiv_factor_prov.dt_termino       = DATE("01/" + v_mes_ano_corrente) - 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-folha-calculada W-Win 
PROCEDURE pi-valida-folha-calculada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR FIRST funcionario
        WHERE funcionario.cdn_funcionario = v_cdn_funcionario_fp1500ymep NO-LOCK:
        FOR FIRST param_empres_rh
            WHERE param_empres_rh.cdn_empresa = funcionario.cdn_empresa NO-LOCK:
            
            ASSIGN v_mes_ano_corrente = string(PARAM_empres_rh.num_mes_refer_calc_efetd) + '/' + string(PARAM_empres_rh.num_ano_refer_calc_efetd).

            IF CAN-FIND(FIRST movto_calcul_func
                        WHERE movto_calcul_func.cdn_empresa = funcionario.cdn_empresa
                          AND movto_calcul_func.cdn_estab = funcionario.cdn_estab
                          AND movto_calcul_func.cdn_funcionario = funcionario.cdn_funcionario
                          AND movto_calcul_func.num_ano_refer_fp = PARAM_empres_rh.num_ano_refer_calc_efetd
                          AND movto_calcul_func.num_mes_refer_fp = PARAM_empres_rh.num_mes_refer_calc_efetd
                          AND movto_calcul_func.idi_tip_fp = 1
                          AND movto_calcul_func.qti_parc_habilit_calc_fp = 9) THEN DO:
                RETURN "NOK":U.
            END.               
        END.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-corporativo W-Win 
PROCEDURE pi-verifica-corporativo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST funcionario
         WHERE funcionario.cdn_funcionario = v_cdn_funcionario_fp1500ymep NO-LOCK NO-ERROR.
    IF AVAIL funcionario THEN DO:
        IF CAN-FIND (FIRST tip_func_corporativo
                     WHERE tip_func_corporativo.cdn_funcionario = funcionario.cdn_funcionario
                       AND tip_func_corporativo.cdn_empresa = funcionario.cdn_empresa
                       AND tip_func_corporativo.cdn_estab = funcionario.cdn_estab
                       AND tip_func_corporativo.dt_inicio_corp <= Date("01/" + v_mes_ano_corrente)
                       AND tip_func_corporativo.dt_fim_corp >= Date("01/" + v_mes_ano_corrente)) THEN DO:
            ASSIGN v_log_corporativo = YES.
        END.
        ELSE DO:
            ASSIGN v_log_corporativo = NO.
        END.
    END.
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_busca_hora W-Win 
FUNCTION fn_busca_hora RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE horas AS INTEGER     NO-UNDO.
    DEFINE VARIABLE minutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE segundos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horario AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horaInclusao AS CHARACTER   NO-UNDO.

    ASSIGN horario = TIME.
    ASSIGN segundos = horario MOD 60.
    ASSIGN horario = (horario - segundos) / 60.
    ASSIGN minutos = horario MOD 60.           
    ASSIGN horas = (horario - minutos) / 60.

    ASSIGN horaInclusao = IF horas < 10 THEN "0" + STRING(horas) ELSE STRING(horas).
    ASSIGN horaInclusao = horaInclusao + ":" + IF minutos < 10 THEN "0" + STRING(minutos) ELSE STRING(minutos).
    ASSIGN horaInclusao = horaInclusao + ":" + IF segundos < 10 THEN "0" + STRING(segundos) ELSE STRING(segundos).
    
    RETURN horaInclusao.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

