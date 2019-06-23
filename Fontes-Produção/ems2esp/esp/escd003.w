&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    ARQUIVO: 
    DESCRI°€O: 
    AUTOR: 
    DATA:
    OBJETIVO: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE BUFFER bf-es-depto     FOR es-depto.
DEFINE BUFFER bf-es-depto-aux FOR es-depto.    


define new global shared variable gsvr-transacao-pai as rowid.
define new global shared variable gsvc-transacao-pai as CHAR.
define new global shared variable gsvc-tipo-mensagem as CHAR.
define new global shared variable gsvc-desc-mensagem as CHAR.
define new global shared variable gsvc-acao-mensagem as CHAR.
define new global shared VARIABLE gsvl-erro-transacao-pai AS LOG.

define new global shared variable gsvi-codigo-old   AS INTEGER FORMAT ">>9". 
define new global shared variable gsvc-desc-sit-old AS CHARACTER FORMAT "x(50)".

/* Local VARIABLE Definitions ---                                       */

def var c-codigo-aux as char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 RECT-24 RECT-25 BtPri BtAnt BtPro ~
BtUlt btn-inc-pai btn-alt-pai btn-cop-pai btn-exclui-pai BtSair 
&Scoped-Define DISPLAYED-OBJECTS i-codigo c-desc-sit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtAnt 
     IMAGE-UP FILE "img/im-pre.bmp":U
     LABEL "" 
     SIZE 5.57 BY 1.58 TOOLTIP "Anterior".

DEFINE BUTTON btn-alt-pai 
     LABEL "Alterar" 
     SIZE 9 BY 1.58 TOOLTIP "Alterar".

DEFINE BUTTON btn-cancel 
     LABEL "Cancelar" 
     SIZE 9 BY 1.58 TOOLTIP "Cancelar".

DEFINE BUTTON btn-cop-pai 
     LABEL "Copiar" 
     SIZE 9 BY 1.58 TOOLTIP "Copiar".

DEFINE BUTTON btn-exclui-pai 
     LABEL "Excluir" 
     SIZE 9 BY 1.58 TOOLTIP "Excluir".

DEFINE BUTTON btn-inc-pai 
     LABEL "Incluir" 
     SIZE 9 BY 1.58 TOOLTIP "Incluir".

DEFINE BUTTON btn-salvar 
     LABEL "Salvar" 
     SIZE 9 BY 1.58 TOOLTIP "Salvar".

DEFINE BUTTON BtPri 
     IMAGE-UP FILE "img/im-fir.bmp":U
     LABEL "" 
     SIZE 5.57 BY 1.58 TOOLTIP "Primeiro".

DEFINE BUTTON BtPro 
     IMAGE-UP FILE "img/im-nex.bmp":U
     LABEL "" 
     SIZE 5.57 BY 1.58 TOOLTIP "Proximo".

DEFINE BUTTON BtSair 
     IMAGE-UP FILE "img/im-exi.bmp":U
     LABEL "" 
     SIZE 5.57 BY 1.58.

DEFINE BUTTON BtUlt 
     IMAGE-UP FILE "img/im-las.bmp":U
     LABEL "" 
     SIZE 5.57 BY 1.58 TOOLTIP "ultimo".

DEFINE VARIABLE c-desc-sit AS CHARACTER FORMAT "x(50)" 
     LABEL "Departamento" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .88 NO-UNDO.

DEFINE VARIABLE i-codigo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Codigo" NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100.14 BY 1.5.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100.14 BY 1.38.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100.14 BY 1.79.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtPri AT ROW 1.25 COL 2.86
     BtAnt AT ROW 1.25 COL 8.14
     BtPro AT ROW 1.25 COL 13.57
     BtUlt AT ROW 1.25 COL 19
     btn-inc-pai AT ROW 1.25 COL 31.86 HELP
          "Incluir nova Situa¯Êo"
     btn-alt-pai AT ROW 1.25 COL 40.86 HELP
          "Alterar Situa¯Êo"
     btn-cop-pai AT ROW 1.25 COL 49.86 HELP
          "Copiar Situa¯Êo"
     btn-exclui-pai AT ROW 1.25 COL 58.86 HELP
          "Excluir Situa¯Êo"
     btn-cancel AT ROW 1.25 COL 68 HELP
          "Cancela Alteracao"
     btn-salvar AT ROW 1.25 COL 77.14 HELP
          "Salvar Alteracao"
     BtSair AT ROW 1.25 COL 95.14
     i-codigo AT ROW 3.25 COL 32.14 COLON-ALIGNED
     c-desc-sit AT ROW 4.79 COL 32.14 COLON-ALIGNED HELP
          "Descricao do Departamento"
     RECT-30 AT ROW 1.13 COL 1.86
     RECT-24 AT ROW 4.5 COL 1.86
     RECT-25 AT ROW 3.04 COL 1.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101 BY 5.46.


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
         TITLE              = "ESCD003 - Cadastro de Departamentos"
         HEIGHT             = 4.96
         WIDTH              = 101.57
         MAX-HEIGHT         = 35.71
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 35.71
         VIRTUAL-WIDTH      = 256
         MAX-BUTTON         = no
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
/* SETTINGS FOR BUTTON btn-cancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn-salvar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-sit IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-codigo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ESCD003 - Cadastro de Departamentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ESCD003 - Cadastro de Departamentos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtAnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtAnt C-Win
ON CHOOSE OF BtAnt IN FRAME DEFAULT-FRAME
DO:
    FIND PREV es-depto NO-LOCK NO-ERROR.
    IF AVAIL es-depto THEN RUN pi-display.
                           ELSE APPLY "CHOOSE" TO BtPri.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-alt-pai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-alt-pai C-Win
ON CHOOSE OF btn-alt-pai IN FRAME DEFAULT-FRAME /* Alterar */
DO:

  ASSIGN 
    gsvc-transacao-pai      = "MOD"
    gsvl-erro-transacao-pai = NO.

  IF AVAIL es-depto THEN DO:
    ASSIGN gsvr-transacao-pai = ROWID(es-depto).
  END.
  RUN pi-habilita.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
 
  ASSIGN gsvc-transacao-pai = "CANCEL".
  RUN pi-display.
  RUN pi-habilita.

  ASSIGN gsvc-transacao-pai = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cop-pai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cop-pai C-Win
ON CHOOSE OF btn-cop-pai IN FRAME DEFAULT-FRAME /* Copiar */
DO:
    ASSIGN gsvc-transacao-pai       = "COP"
           gsvl-erro-transacao-pai  = NO.

    ASSIGN gsvi-codigo-old   = int(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME})  
           gsvc-desc-sit-old = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

      IF CAN-FIND(LAST es-depto) THEN DO:
          FIND LAST es-depto NO-LOCK NO-ERROR.
          IF AVAIL es-depto THEN 
              ASSIGN i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(es-depto.codigo + 1)
                     c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(gsvc-desc-sit-old).
          RUN pi-habilita. 
      END.

/*
    
    IF AVAIL es-depto THEN
        ASSIGN gsvr-transacao-pai = ROWID(es-depto).
    ELSE DO:
        ASSIGN gsvc-tipo-mensagem = "ERRO"
               gsvc-desc-mensagem = "Registro Par³metro n’o encontrado"
               gsvc-acao-mensagem = "Par³metro n’o encontrado para op»’o de Copiar".
            
        RUN esp\suemsg.w.
            
        ASSIGN gsvc-tipo-mensagem = ""
               gsvc-desc-mensagem = ""
               gsvc-acao-mensagem = "".
            
        ASSIGN gsvl-erro-transacao-pai = YES.
    END.
        
    IF gsvl-erro-transacao-pai = NO THEN RUN esp\esnfe002-v01.w.
        
    IF gsvl-erro-transacao-pai = NO THEN do:
        /*CLOSE QUERY br-erros.  */
        /*{&OPEN-QUERY-br-erros}*/

        APPLY "CHOOSE" TO BtPro.
    END.
*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exclui-pai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exclui-pai C-Win
ON CHOOSE OF btn-exclui-pai IN FRAME DEFAULT-FRAME /* Excluir */
DO:

  ASSIGN gsvc-transacao-pai = "DEL".

  FIND FIRST bf-es-depto EXCLUSIVE-LOCK 
    WHERE bf-es-depto.codigo = INT(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF AVAIL bf-es-depto THEN DO:

    MESSAGE "Confirma a exclusÆo do departamento: " c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirma‡Æo de exclusÆo"
      UPDATE w-conf AS LOGICAL.

    IF w-conf THEN DO:
      DELETE bf-es-depto.
      APPLY "CHOOSE" TO BtAnt.
    END.
  END.
  ELSE DO:

  END.
  ASSIGN gsvc-transacao-pai = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-inc-pai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-inc-pai C-Win
ON CHOOSE OF btn-inc-pai IN FRAME DEFAULT-FRAME /* Incluir */
DO:

  ASSIGN gsvi-codigo-old   = int(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME})  
         gsvc-desc-sit-old = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

    ASSIGN gsvc-transacao-pai = "ADD"
           gsvr-transacao-pai = ?.

    IF CAN-FIND(LAST es-depto) THEN DO:
        FIND LAST es-depto NO-LOCK NO-ERROR.
        IF AVAIL es-depto THEN 
            ASSIGN i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(es-depto.codigo + 1)
                   c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
    ELSE DO:
        ASSIGN i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(1)
               c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
        
    RUN pi-habilita.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-salvar C-Win
ON CHOOSE OF btn-salvar IN FRAME DEFAULT-FRAME /* Salvar */
DO:


    /*** Inclusao ***/
    IF gsvc-transacao-pai = "ADD" OR
       gsvc-transacao-pai = "COP"  THEN DO:
               
        IF c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:
        
          FIND FIRST bf-es-depto EXCLUSIVE-LOCK 
            WHERE bf-es-depto.codigo = INT(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
          IF NOT AVAIL bf-es-depto THEN DO:
              FIND FIRST bf-es-depto EXCLUSIVE-LOCK 
                   WHERE bf-es-depto.descricao = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
              IF NOT AVAIL bf-es-depto THEN DO:
                  CREATE bf-es-depto.
                  ASSIGN bf-es-depto.codigo    = INT(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                         bf-es-depto.descricao = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
              END.
              ELSE DO:
                  MESSAGE "Departamento " + c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " j  cadastrado"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Registro j  existente".
        
                  APPLY 'entry' TO c-desc-sit IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.
        
              END.
          END.
          ELSE DO:
              MESSAGE "C¢digo " + i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " j  cadastrado"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Registro j  existente".
        
              APPLY 'entry' TO c-desc-sit IN FRAME {&FRAME-NAME}.
              RETURN NO-APPLY.
        
          END.
        END.
        ELSE DO:    
            MESSAGE "O nome do departamento ‚ obrigat¢rio" SKIP "Por favor, digite o nome do Departamento"
                VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Nome do Departamento obrigat¢rio".
    
            APPLY 'entry' TO c-desc-sit IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.        
    END. 
    /*** Alteracao ***/
    IF gsvc-transacao-pai = "MOD" THEN DO:        
        FIND FIRST bf-es-depto EXCLUSIVE-LOCK 
          WHERE ROWID(bf-es-depto) = gsvr-transacao-pai NO-ERROR.
        IF AVAIL bf-es-depto THEN DO:
            /* Validando se nome depto ja existe */
            FIND FIRST bf-es-depto-aux NO-LOCK 
                 WHERE bf-es-depto-aux.descricao = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
            IF AVAIL bf-es-depto-aux THEN DO:
                MESSAGE "Departamento " + c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " j  cadastrado"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Departamento j  existente".

                APPLY 'entry' TO c-desc-sit IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                ASSIGN bf-es-depto.descricao = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
            END.            
        END.
    END.
    /*** Copia **
    IF gsvc-transacao-pai = "MOD" THEN DO:        
        FIND FIRST bf-es-depto EXCLUSIVE-LOCK 
          WHERE ROWID(bf-es-depto) = gsvr-transacao-pai NO-ERROR.
        IF AVAIL bf-es-depto THEN DO:
            ASSIGN bf-es-depto.descricao = c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        END.
    END.
                 */

    ASSIGN gsvc-transacao-pai = "SAVE".
    /*** Posicionando para navega ***/
    FIND FIRST es-depto WHERE es-depto.codigo = INT(i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAIL es-depto THEN DO:
        RUN pi-display.
        RUN pi-habilita.           
        ASSIGN gsvc-transacao-pai = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtPri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtPri C-Win
ON CHOOSE OF BtPri IN FRAME DEFAULT-FRAME
DO:
    FIND FIRST es-depto NO-LOCK NO-ERROR.
    IF AVAILABLE es-depto THEN RUN pi-display.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtPro C-Win
ON CHOOSE OF BtPro IN FRAME DEFAULT-FRAME
DO:
    FIND NEXT es-depto NO-LOCK NO-ERROR.
    IF AVAILABLE es-depto THEN RUN pi-display.
                               ELSE APPLY "CHOOSE" TO BtUlt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtSair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtSair C-Win
ON CHOOSE OF BtSair IN FRAME DEFAULT-FRAME
DO:
    apply "CLOSE" TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtUlt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtUlt C-Win
ON CHOOSE OF BtUlt IN FRAME DEFAULT-FRAME
DO:
    IF CAN-FIND(FIRST es-depto) THEN DO:

        FIND LAST es-depto NO-LOCK NO-ERROR.
        IF AVAIL es-depto THEN RUN pi-display.

    END.
    ELSE DO:

        ASSIGN gsvc-transacao-pai = "NOK".

        RUN pi-display.
        RUN pi-habilita.
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
        
    APPLY "CHOOSE" TO BtUlt.
        
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
  DISPLAY i-codigo c-desc-sit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-30 RECT-24 RECT-25 BtPri BtAnt BtPro BtUlt btn-inc-pai 
         btn-alt-pai btn-cop-pai btn-exclui-pai BtSair 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display C-Win 
PROCEDURE pi-display :
/*------------------------------------------------------------------------------
  -----------------------------------------------------------------------------*/
          IF AVAIL es-depto THEN DO:
            ASSIGN 
              i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(es-depto.codigo)
              c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(es-depto.descricao).

            ASSIGN 
              c-codigo-aux       = STRING(es-depto.codigo)
              gsvr-transacao-pai = ROWID(es-depto).

          END.
          ELSE DO:
              ASSIGN 
                i-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ?
                c-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
          END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita C-Win 
PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF gsvc-transacao-pai = "ADD" OR gsvc-transacao-pai = "MOD" OR gsvc-transacao-pai = "COP" THEN DO:
        ENABLE c-desc-sit Btn-Cancel Btn-Salvar
            WITH FRAME {&FRAME-NAME}.
    
        DISABLE BtPri BtAnt BtPro BtUlt /*BtBusca*/ BtSair
                btn-inc-pai btn-alt-pai btn-cop-pai btn-exclui-pai
            WITH FRAME {&FRAME-NAME}.
    
        APPLY 'entry' TO INPUT FRAME {&FRAME-NAME} c-desc-sit.
    END.
    IF gsvc-transacao-pai = "SAVE" THEN DO:
        DISABLE c-desc-sit Btn-Cancel Btn-Salvar
            WITH FRAME {&FRAME-NAME}.
    
        ENABLE BtPri BtAnt BtPro BtUlt /*BtBusca*/ BtSair
                btn-inc-pai btn-alt-pai btn-cop-pai btn-exclui-pai
            WITH FRAME {&FRAME-NAME}.
    END.    
    IF gsvc-transacao-pai = "CANCEL" or
       gsvc-transacao-pai = "SAVE" THEN DO:

        ENABLE BtPri BtAnt BtPro BtUlt /*BtBusca*/ BtSair
                btn-inc-pai btn-alt-pai btn-cop-pai btn-exclui-pai
            WITH FRAME {&FRAME-NAME}.

        DISABLE Btn-Cancel Btn-Salvar c-desc-sit
            WITH FRAME {&FRAME-NAME}.        
    END.
    IF gsvc-transacao-pai = "NOK" THEN DO:
        DISABLE BtPri BtAnt BtPro BtUlt /*BtBusca*/
                btn-alt-pai btn-cop-pai btn-exclui-pai  Btn-Cancel Btn-Salvar c-desc-sit
            WITH FRAME {&FRAME-NAME}.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa C-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-navega C-Win 
PROCEDURE pi-navega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-sensitive AS LOGICAL NO-UNDO.

ASSIGN BtPri:SENSITIVE IN FRAME {&FRAME-NAME}   = p-sensitive
       BtAnt:SENSITIVE IN FRAME {&FRAME-NAME}   = p-sensitive
       BtPro:SENSITIVE IN FRAME {&FRAME-NAME}   = p-sensitive
       BtUlt:SENSITIVE IN FRAME {&FRAME-NAME}   = p-sensitive
      /* BtBusca:SENSITIVE IN FRAME {&FRAME-NAME} = p-sensitive */.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

