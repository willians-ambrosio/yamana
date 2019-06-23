&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5esp          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esjd006
** Descricao.............: Cadastro de Exce‡äes para Cria‡Æo de Mapas
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 02/08/2014
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

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_estabelecimento
    as recid
    format ">>>>>>9":U
    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main
&Scoped-define BROWSE-NAME br-excecoes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-cross-ref-regra-mapa estabelecimento

/* Definitions for BROWSE br-excecoes                                   */
&Scoped-define FIELDS-IN-QUERY-br-excecoes ~
es-cross-ref-regra-mapa.cod_empresa es-cross-ref-regra-mapa.cod_estab ~
es-cross-ref-regra-mapa.cod_ccusto_ini ~
es-cross-ref-regra-mapa.cod_ccusto_fin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-excecoes 
&Scoped-define QUERY-STRING-br-excecoes FOR EACH es-cross-ref-regra-mapa NO-LOCK, ~
      EACH estabelecimento OF es-cross-ref-regra-mapa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-excecoes OPEN QUERY br-excecoes FOR EACH es-cross-ref-regra-mapa NO-LOCK, ~
      EACH estabelecimento OF es-cross-ref-regra-mapa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-excecoes es-cross-ref-regra-mapa ~
estabelecimento
&Scoped-define FIRST-TABLE-IN-QUERY-br-excecoes es-cross-ref-regra-mapa
&Scoped-define SECOND-TABLE-IN-QUERY-br-excecoes estabelecimento


/* Definitions for FRAME f_main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f_main ~
    ~{&OPEN-QUERY-br-excecoes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt_rgf rt_cxcf IMAGE-15 IMAGE-16 RECT-13 ~
RECT-14 bt-busca-empresa fi-cod_empresa bt-busca-estab fi-cod_estab bt-add ~
fi-cod_ccusto_ini fi-cod_ccusto_fin br-excecoes bt-del bt_fechar ~
bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_empresa fi-cod_estab ~
fi-cod_ccusto_ini fi-cod_ccusto_fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     LABEL "Adicionar Regra" 
     SIZE 15 BY 1.13 TOOLTIP "Adicionar Regra".

DEFINE BUTTON bt-busca-empresa 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "Busca Empresa" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-busca-estab 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "Busca Estabelecimento" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon/del-au.bmp":U
     LABEL "Elimina Exce‡Æo" 
     SIZE 4 BY 1.13 TOOLTIP "Elimina Exce‡Æo".

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod_ccusto_fin AS CHARACTER FORMAT "x(11)" INITIAL "999999" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-cod_ccusto_ini AS CHARACTER FORMAT "x(11)" INITIAL "100000" 
     LABEL "Exce‡Æo Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-cod_empresa AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_estab AS CHARACTER FORMAT "x(5)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 9.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.86 BY 3.63.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-excecoes FOR 
      es-cross-ref-regra-mapa, 
      estabelecimento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-excecoes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-excecoes C-Win _STRUCTURED
  QUERY br-excecoes NO-LOCK DISPLAY
      es-cross-ref-regra-mapa.cod_empresa FORMAT "x(3)":U
      es-cross-ref-regra-mapa.cod_estab FORMAT "x(5)":U
      es-cross-ref-regra-mapa.cod_ccusto_ini FORMAT "x(11)":U
      es-cross-ref-regra-mapa.cod_ccusto_fin FORMAT "x(11)":U WIDTH 14.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73 BY 8.5
         FONT 1
         TITLE "Exce‡äes para Cria‡Æo de Mapas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-empresa AT ROW 2.92 COL 31.14
     fi-cod_empresa AT ROW 3 COL 17 COLON-ALIGNED HELP
          "C¢digo Empresa"
     bt-busca-estab AT ROW 3.96 COL 31.14
     fi-cod_estab AT ROW 4 COL 17 COLON-ALIGNED HELP
          "C¢digo Estabelecimento"
     bt-add AT ROW 4.75 COL 64
     fi-cod_ccusto_ini AT ROW 5 COL 17 COLON-ALIGNED HELP
          "C¢digo Centro Custo Inicial"
     fi-cod_ccusto_fin AT ROW 5 COL 41.57 COLON-ALIGNED HELP
          "C¢digo Centro Custo Final" NO-LABEL
     br-excecoes AT ROW 6.5 COL 2
     bt-del AT ROW 7.25 COL 76
     bt_fechar AT ROW 15.71 COL 3
     bt_cancelar AT ROW 15.71 COL 13.86
     Btn_Help AT ROW 15.71 COL 70.43
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 15.5 COL 1
     IMAGE-15 AT ROW 4.96 COL 34.72
     IMAGE-16 AT ROW 4.96 COL 40.43
     RECT-13 AT ROW 6.25 COL 1
     RECT-14 AT ROW 2.63 COL 1.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.92
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
         TITLE              = ".: Cadastro de Exce‡äes para Cria‡Æo de Mapas - ESJD006 :."
         HEIGHT             = 15.92
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
/* BROWSE-TAB br-excecoes fi-cod_ccusto_fin f_main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-excecoes
/* Query rebuild information for BROWSE br-excecoes
     _TblList          = "ems5esp.es-cross-ref-regra-mapa,estabelecimento OF ems5esp.es-cross-ref-regra-mapa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = ems5esp.es-cross-ref-regra-mapa.cod_empresa
     _FldNameList[2]   = ems5esp.es-cross-ref-regra-mapa.cod_estab
     _FldNameList[3]   = ems5esp.es-cross-ref-regra-mapa.cod_ccusto_ini
     _FldNameList[4]   > ems5esp.es-cross-ref-regra-mapa.cod_ccusto_fin
"ems5esp.es-cross-ref-regra-mapa.cod_ccusto_fin" ? ? "character" ? ? ? ? ? ? no ? no no "14.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-excecoes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Cadastro de Exce‡äes para Cria‡Æo de Mapas - ESJD006 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Cadastro de Exce‡äes para Cria‡Æo de Mapas - ESJD006 :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add C-Win
ON CHOOSE OF bt-add IN FRAME f_main /* Adicionar Regra */
DO:
    FIND FIRST estabelecimento NO-LOCK
        WHERE estabelecimento.cod_empresa = INPUT FRAME {&FRAME-NAME} fi-cod_empresa
        AND   estabelecimento.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab NO-ERROR.
    IF NOT AVAILABLE estabelecimento THEN DO:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Estabelecimento nÆo encontrado para a Empresa.").
    END.
    ELSE DO:

        IF INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_ini > INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_fin THEN DO:
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Centro de Custo Final menor que o Inicial.").
            RETURN NO-APPLY.
        END.

        FIND FIRST es-cross-ref-regra-mapa NO-LOCK
            WHERE es-cross-ref-regra-mapa.cod_empresa    = estabelecimento.cod_empresa
            AND   es-cross-ref-regra-mapa.cod_estab      = estabelecimento.cod_estab
            AND   es-cross-ref-regra-mapa.cod_ccusto_ini = INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_ini
            AND   es-cross-ref-regra-mapa.cod_ccusto_fin = INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_fin NO-ERROR.
        IF NOT AVAILABLE es-cross-ref-regra-mapa THEN DO:
            CREATE es-cross-ref-regra-mapa.
            ASSIGN es-cross-ref-regra-mapa.cod_empresa    = estabelecimento.cod_empresa
                   es-cross-ref-regra-mapa.cod_estab      = estabelecimento.cod_estab
                   es-cross-ref-regra-mapa.cod_ccusto_ini = INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_ini
                   es-cross-ref-regra-mapa.cod_ccusto_fin = INPUT FRAME {&FRAME-NAME} fi-cod_ccusto_fin.
        END.
        ELSE DO:
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Exce‡Æo j  cadastrada.").
            RETURN NO-APPLY.
        END.
    END.

    CLOSE QUERY br-excecoes.
    OPEN QUERY br-excecoes FOR EACH es-cross-ref-regra-mapa NO-LOCK 
        ,EACH estabelecimento OF es-cross-ref-regra-mapa NO-LOCK 
        INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-empresa C-Win
ON CHOOSE OF bt-busca-empresa IN FRAME f_main /* Busca Empresa */
DO:
    /* fn_generic_zoom */
    if  search("prgint/utb/utb069ka.r") = ? and search("prgint/utb/utb069ka.p") = ? then do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb069ka.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb069ka.p /*prg_sea_empresa*/.
    if  v_rec_empresa <> ?
    then do:
        find ems5.empresa where recid(empresa) = v_rec_empresa no-lock no-error.
        assign fi-cod_empresa:screen-value in frame {&FRAME-NAME} = string(empresa.cod_empresa).

    end /* if */.
    apply "entry" to fi-cod_empresa in frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-estab C-Win
ON CHOOSE OF bt-busca-estab IN FRAME f_main /* Busca Estabelecimento */
DO:
    /* fn_generic_zoom */
    if  search("esp/esjd006z1.r") = ? and search("esp/esjd006z1.p") = ? then do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "esp/esjd006z1.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run esp/esjd006z1.p (Input INPUT FRAME {&FRAME-NAME} fi-cod_empresa) /*prg_sea_estabelecimento*/.

    if  v_rec_estabelecimento <> ?
    then do:
        find estabelecimento where recid(estabelecimento) = v_rec_estabelecimento no-lock no-error.

        assign fi-cod_estab:screen-value in frame {&frame-name} = string(estabelecimento.cod_estab).

    end /* if */.

    apply "entry" to fi-cod_estab in frame {&frame-name}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del C-Win
ON CHOOSE OF bt-del IN FRAME f_main /* Elimina Exce‡Æo */
DO:
    IF AVAILABLE es-cross-ref-regra-mapa THEN DO:
        FIND CURRENT es-cross-ref-regra-mapa EXCLUSIVE-LOCK NO-ERROR.
        DELETE es-cross-ref-regra-mapa.
    END.

    CLOSE QUERY br-excecoes.
    OPEN QUERY br-excecoes FOR EACH es-cross-ref-regra-mapa NO-LOCK 
        ,EACH estabelecimento OF es-cross-ref-regra-mapa NO-LOCK 
        INDEXED-REPOSITION.

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


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-excecoes
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
  DISPLAY fi-cod_empresa fi-cod_estab fi-cod_ccusto_ini fi-cod_ccusto_fin 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE rt_rgf rt_cxcf IMAGE-15 IMAGE-16 RECT-13 RECT-14 bt-busca-empresa 
         fi-cod_empresa bt-busca-estab fi-cod_estab bt-add fi-cod_ccusto_ini 
         fi-cod_ccusto_fin br-excecoes bt-del bt_fechar bt_cancelar Btn_Help 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-arq C-Win 
PROCEDURE pi-abre-arq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def Input param p_cod_dwb_file
        as character
        format "x(40)"
        no-undo.

    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_key_value
        as character
        format "x(8)":U
        no-undo.

    /************************** Variable Definition End *************************/

    get-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value.
    if  v_cod_key_value = ""
    or   v_cod_key_value = ?
    then do:
        assign v_cod_key_value = 'notepad.exe'.
        put-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value no-error.
    end /* if */.

    run winexec (input v_cod_key_value + chr(32) + p_cod_dwb_file, input 1).

    END PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAM prg_name                          AS CHARACTER.
      DEF INPUT  PARAM prg_style                         AS SHORT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

