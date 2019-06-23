&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: escc008
** Descricao.............: Finaliza Ordem de Compra / Requisiá∆o
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 08/2014
*****************************************************************************/

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

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

def new global shared var i-ep-codigo-usuario     like mguni.empresa.ep-codigo        no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt_rgf rt_cxcf bt_executar IMAGE-7 IMAGE-8 ~
fi-data fi-nr-contrato-ini fi-nr-contrato-fin rd-lista bt_fechar ~
bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi-data fi-nr-contrato-ini ~
fi-nr-contrato-fin rd-lista 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_executar 
     IMAGE-UP FILE "image/im-run.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Executar" 
     SIZE 4 BY 1.13 TOOLTIP "Executar".

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-data AS DATE FORMAT "99/99/9999":U 
     LABEL "Finalizar Ordens Emitidas antes de" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-contrato-fin AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-contrato-ini AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "N£mero Contrato":R18 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rd-lista AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Lista Ordens / Requisiá‰es", 1,
"ALTERA Ordens para RECEBIDA / Requisiá‰es para FECHADA", 2
     SIZE 53 BY 2.5 NO-UNDO.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt_executar AT ROW 1.17 COL 1.86
     fi-data AT ROW 3.5 COL 27 COLON-ALIGNED
     fi-nr-contrato-ini AT ROW 4.46 COL 27 COLON-ALIGNED HELP
          "N£mero do Contrato da Ordem de Compra"
     fi-nr-contrato-fin AT ROW 4.46 COL 48.43 COLON-ALIGNED HELP
          "N£mero do Contrato da Ordem de Compra" NO-LABEL
     rd-lista AT ROW 5.75 COL 18.72 NO-LABEL
     bt_fechar AT ROW 15.71 COL 3
     bt_cancelar AT ROW 15.71 COL 13.86
     Btn_Help AT ROW 15.71 COL 70.43
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 15.5 COL 1
     IMAGE-7 AT ROW 4.46 COL 41.14
     IMAGE-8 AT ROW 4.46 COL 47.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 1
         CANCEL-BUTTON bt_cancelar.


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
         TITLE              = ".: Finaliza Ordens de Compra / Requisiá∆o - ESCC008 :."
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 274.29
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
/* SETTINGS FOR FRAME f_main
   FRAME-NAME                                                           */
ASSIGN 
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Finaliza Ordens de Compra / Requisiá∆o - ESCC008 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Finaliza Ordens de Compra / Requisiá∆o - ESCC008 :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME f_main /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancelar C-Win
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Cancelar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_executar C-Win
ON CHOOSE OF bt_executar IN FRAME f_main /* Executar */
DO:
    do on error undo, return no-apply:
        run pi_executar.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
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

    {include/i_dbinst.i}
    {include/i_dbtype.i}
    {include/i_fcldef.i}

    {include/i_fclwin.i c-win}
    {include/i_fclfrm.i f_main }

  RUN enable_UI.
  
  run pi_inicio.

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
  DISPLAY fi-data fi-nr-contrato-ini fi-nr-contrato-fin rd-lista 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE rt_rgf rt_cxcf bt_executar IMAGE-7 IMAGE-8 fi-data fi-nr-contrato-ini 
         fi-nr-contrato-fin rd-lista bt_fechar bt_cancelar Btn_Help 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_executar C-Win 
PROCEDURE pi_executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE deSaldo-Contrato AS DECIMAL     NO-UNDO.
    def var ch-Excel            as component-handle                     no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Inicializando").

    OUTPUT TO VALUE (SESSION:TEMP-DIRECTORY + "escc008-OC-" + STRING (i-ep-codigo-usuario) + ".csv") NO-CONVERT.

    PUT UNFORMAT "Conta"
               + ";CCusto"
               + ";Emp"
               + ";Ordem Compra " 
               + ";Item"
               + ";Data Emiss∆o"
               + ";Data Pedido"
               + ";Situaá∆o"
               + ";Contrato"
               + ";Saldo Contrato"
               SKIP(2).

    FOR EACH ordem-compra EXCLUSIVE-LOCK
        WHERE ordem-compra.data-emissao <  INPUT FRAME {&FRAME-NAME} fi-data
        AND   ordem-compra.data-pedido  <  INPUT FRAME {&FRAME-NAME} fi-data
        AND   ordem-compra.nr-contrato  >= INPUT FRAME {&FRAME-NAME} fi-nr-contrato-ini
        AND   ordem-compra.nr-contrato  <= INPUT FRAME {&FRAME-NAME} fi-nr-contrato-fin
        AND  (ordem-compra.situacao     <= 3
        OR    ordem-compra.situacao      = 5):

        RUN pi-acompanhar IN h-acomp (INPUT "Ordens " + STRING (ordem-compra.numero-ordem)).

        FIND FIRST contrato-for NO-LOCK
            WHERE contrato-for.nr-contrato = ordem-compra.nr-contrato NO-ERROR.
        IF AVAILABLE contrato-for THEN DO:
            RUN esp/escnapi001.p (INPUT  contrato-for.nr-contrato,
                                  OUTPUT deSaldo-Contrato).
        END.

        PUT UNFORMAT ordem-compra.ct-codigo 
                   + ";" + ordem-compra.sc-codigo
                   + ";" + STRING (ordem-compra.ep-codigo)
                   + ";" + STRING (ordem-compra.numero-ordem)
                   + ";" ordem-compra.it-codigo 
                   + ";" STRING (ordem-compra.data-emissao,"99/99/9999")
                   + ";" STRING (ordem-compra.data-pedido,"99/99/9999")
                   + ";" STRING (ordem-compra.situacao)
                   + ";" STRING (ordem-compra.nr-contrato)
                   + ";" STRING (deSaldo-Contrato)
                   SKIP.

        IF INPUT FRAME {&FRAME-NAME} rd-lista = 2 THEN DO:
            ASSIGN ordem-compra.situacao = 6.
        END.
    END.

    OUTPUT CLOSE.

    OUTPUT TO VALUE (SESSION:TEMP-DIRECTORY + "escc008-REQ-" + STRING (i-ep-codigo-usuario) + ".csv") NO-CONVERT.
    
    PUT UNFORMAT "Conta"
               + ";CCusto"
               + ";Emp"
               + ";Requisicao"
               + ";Item"
               + ";Data Requisicao"
               + ";Situacao"
               SKIP(2).

    FOR EACH requisicao EXCLUSIVE-LOCK 
        WHERE requisicao.dt-requisicao < INPUT FRAME {&FRAME-NAME} fi-data
        AND   requisicao.situacao       = 1:

        RUN pi-acompanhar IN h-acomp (INPUT "Requisiá‰es " + STRING (requisicao.nr-requisicao)).

        FOR EACH it-requisicao OF requisicao EXCLUSIVE-LOCK
            WHERE it-requisicao.situacao = 1:

            PUT UNFORMAT it-requisicao.ct-codigo
                       + ";" + it-requisicao.sc-codigo
                       + ";" + STRING (it-requisicao.ep-codigo)
                       + ";" + STRING (it-requisicao.nr-requisicao) 
                       + ";" + it-requisicao.it-codigo
                       + ";" + STRING (requisicao.dt-requisicao,"99/99/9999")
                       + ";" + STRING (it-requisicao.situacao)
                       SKIP.

            IF INPUT FRAME {&FRAME-NAME} rd-lista = 2 THEN DO:
                ASSIGN it-requisicao.situacao = 2.
            END.
        END.

        IF INPUT FRAME {&FRAME-NAME} rd-lista = 2 THEN DO:
            ASSIGN requisicao.situacao = 2.
        END.

    END.

    OUTPUT CLOSE.

    run pi-finalizar in h-acomp. 

    Create "Excel.Application" ch-Excel no-error.

    ch-Excel:workbooks:open(SESSION:TEMP-DIRECTORY + "escc008-OC-" + STRING (i-ep-codigo-usuario) + ".csv").
    ch-Excel:visible = TRUE.
    ch-Excel:application:DisplayAlerts = FALSE.
    ch-Excel:sheets:item(1).

    release object ch-Excel.

    Create "Excel.Application" ch-Excel no-error.

    ch-Excel:workbooks:open(SESSION:TEMP-DIRECTORY + "escc008-REQ-" + STRING (i-ep-codigo-usuario) + ".csv").
    ch-Excel:visible = TRUE.
    ch-Excel:application:DisplayAlerts = FALSE.
    ch-Excel:sheets:item(1).

    release object ch-Excel.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN fi-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/01/" + STRING (YEAR(TODAY)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

