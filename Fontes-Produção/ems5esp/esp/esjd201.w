&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esjd201
** Descricao.............: Lista Contas JDE X EMS2
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 08/06/2014
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
def buffer ccusto  for ems5.ccusto.
def buffer empresa for ems5.empresa.

def temp-table tt-conta-contab no-undo
    field ep-codigo      like ccusto.cod_empresa
    field cod_empresa    like ccusto.cod_empresa
    field cod_cta_ctbl   like cta_ctbl.cod_cta_ctbl
    field desc-conta     like cta_ctbl.des_tit_ctbl
    field cod_unid_negoc as char format "x(02)"
    field cod_ccusto     like ccusto.cod_ccusto
    field desc-ccusto    like ccusto.des_tit_ctbl
    field id             as char format "x(19)"
    index ix as primary unique id.

/* Parameters Definitions ---                                           */

{esp/esjd200.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

DEF temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf IMAGE-15 ~
IMAGE-16 RECT-12 IMAGE-17 IMAGE-18 fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim ~
fi-data-ini fi-data-fim fi-arquivo bt_fechar bt_executar bt_cancelar ~
Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim ~
fi-data-ini fi-data-fim fi-arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-busca-arq 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Arquivo" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Arquivo".

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

DEFINE VARIABLE fi-arquivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo Saida" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_cta_ctbl-fim AS CHARACTER FORMAT "x(20)" INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-cod_cta_ctbl-ini AS CHARACTER FORMAT "x(20)" INITIAL "00000000" 
     LABEL "Conta Cont bil" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-data-fim AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88.

DEFINE VARIABLE fi-data-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Per¡odo Modifica‡Æo JDE" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.71.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 13.79 COL 71.86
     fi-cod_cta_ctbl-ini AT ROW 4.04 COL 18.29 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil"
     fi-cod_cta_ctbl-fim AT ROW 4.04 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil" NO-LABEL
     fi-data-ini AT ROW 5.04 COL 22 COLON-ALIGNED HELP
          "Per¡odo"
     fi-data-fim AT ROW 5.04 COL 42.86 COLON-ALIGNED HELP
          "Per¡odo" NO-LABEL
     fi-arquivo AT ROW 14 COL 10 COLON-ALIGNED
     bt_fechar AT ROW 15.71 COL 3
     bt_executar AT ROW 1.17 COL 1.86
     bt_cancelar AT ROW 15.71 COL 13.86
     Btn_Help AT ROW 15.71 COL 70.43
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 15.5 COL 1
     IMAGE-15 AT ROW 4 COL 36
     IMAGE-16 AT ROW 4 COL 41.72
     RECT-12 AT ROW 13.5 COL 1
     IMAGE-17 AT ROW 5 COL 36
     IMAGE-18 AT ROW 5 COL 41.72
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
         TITLE              = ".: Lista Contas JDE X EMS2 - ESJD201 :."
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
       bt-busca-arq:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

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
ON END-ERROR OF C-Win /* .: Lista Contas JDE X EMS2 - ESJD201 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Lista Contas JDE X EMS2 - ESJD201 :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-arq C-Win
ON CHOOSE OF bt-busca-arq IN FRAME f_main /* Busca Arquivo */
DO:
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.

    assign input frame {&FRAME-NAME} fi-arquivo.
    SYSTEM-DIALOG GET-FILE fi-arquivo
       FILTERS "*.csv" "*.csv",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "*.csv"
       INITIAL-DIR "spool"
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        display fi-arquivo with frame {&FRAME-NAME}.
    
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
  DISPLAY fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim fi-data-ini fi-data-fim 
          fi-arquivo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf IMAGE-15 IMAGE-16 RECT-12 IMAGE-17 
         IMAGE-18 fi-cod_cta_ctbl-ini fi-cod_cta_ctbl-fim fi-data-ini 
         fi-data-fim fi-arquivo bt_fechar bt_executar bt_cancelar Btn_Help 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-create-tt-conta-contab C-Win 
PROCEDURE pi-create-tt-conta-contab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each cta_ctbl no-lock use-index ctactbl_id
       where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
         and cta_ctbl.dat_inic_valid    <= today
         and cta_ctbl.dat_fim_valid     >= today:
        for each empresa no-lock use-index empresa_id
           where empresa.cod_empresa <> "CAN",
           first estabelecimento no-lock use-index stblcmnt_empresa
           where estabelecimento.cod_empresa = empresa.cod_empresa:
            find first criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                 where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab
                   and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                   and criter_distrib_cta_ctbl.dat_fim_valid     >= today no-error.
            if  not avail criter_distrib_cta_ctbl or
                criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "NÆo Utiliza" then do:
                run pi-cria-tt(input "000000",
                               input "").
            end.
            else do:
                find first plano_ccusto no-lock use-index plnccst_id
                     where plano_ccusto.cod_empresa     = empresa.cod_empresa
                       and plano_ccusto.dat_inic_valid <= today
                       and plano_ccusto.dat_fim_valid  >= today no-error.

                if  criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" then
                    for each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = plano_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
/*                          and ccusto.cod_ccusto       = */
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                else do:
                    for each item_lista_ccusto no-lock use-index itmlstcc_id
                       where item_lista_ccusto.cod_estab               = criter_distrib_cta_ctbl.cod_estab
                         and item_lista_ccusto.cod_mapa_distrib_ccusto = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
                         and item_lista_ccusto.cod_empresa             = criter_distrib_cta_ctbl.cod_empresa
                         and item_lista_ccusto.cod_plano_ccusto        = plano_ccusto.cod_plano_ccusto,
                        each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = item_lista_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                         and ccusto.cod_ccusto       = item_lista_ccusto.cod_ccusto
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                end.
            end.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt C-Win 
PROCEDURE pi-cria-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-ccusto     as char no-undo.
    def input param p-desc-custo as char no-undo.

    def var c-id as char no-undo.

    assign c-id = empresa.cod_empresa + cta_ctbl.cod_cta_ctbl + "00" + p-ccusto.
    find first tt-conta-contab no-lock
         where tt-conta-contab.id = c-id no-error.
    if  not avail tt-conta-contab then do:
        find first trad_org_ext no-lock
             where trad_org_ext.cod_matriz_trad_org_ext = 'GERAL'
               and trad_org_ext.cod_unid_organ          = empresa.cod_empresa no-error.
        if  not avail trad_org_ext then next.

        find first trad_org_ext no-lock
             where trad_org_ext.cod_matriz_trad_org_ext = "EMS2"
               and trad_org_ext.cod_tip_unid_organ      = "998" /* Empresa */
               and trad_org_ext.cod_unid_organ          = empresa.cod_empresa no-error.
        if  not avail trad_org_ext then next.

        create tt-conta-contab.
        assign tt-conta-contab.ep-codigo      = trad_org_ext.cod_unid_organ_ext
               tt-conta-contab.cod_empresa    = empresa.cod_empresa
               tt-conta-contab.cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
               tt-conta-contab.desc-conta     = cta_ctbl.des_tit_ctbl
               tt-conta-contab.cod_unid_negoc = "00"
               tt-conta-contab.cod_ccusto     = p-ccusto
               tt-conta-contab.desc-ccusto    = p-desc-custo
               tt-conta-contab.id             = c-id.
    end.
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
DEF VAR v-Ccusto LIKE ccusto.cod_ccusto NO-UNDO.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Processando JDE X EMS2...").

OUTPUT TO VALUE (INPUT FRAME {&FRAME-NAME} fi-arquivo) NO-CONVERT.

PUT UNFORMATTED "LOG VALIDA€ÇO JDE X EMS2" SKIP(2).

run pi-create-tt-conta-contab.

FOR EACH es-cross-reference-jde NO-LOCK
    WHERE es-cross-reference-jde.Date-Last-MODIFIED >= INPUT FRAME {&FRAME-NAME} fi-data-ini
    AND   es-cross-reference-jde.Date-Last-MODIFIED <= INPUT FRAME {&FRAME-NAME} fi-data-fim
    AND   es-cross-reference-jde.cod_cta_ctbl       >= INPUT FRAME {&FRAME-NAME} fi-cod_cta_ctbl-ini
    AND   es-cross-reference-jde.cod_cta_ctbl       <= INPUT FRAME {&FRAME-NAME} fi-cod_cta_ctbl-fim:

    RUN pi-acompanhar IN h-acomp (INPUT "JDE " + es-cross-reference-jde.cod_cta_ctbl + "-" + es-cross-reference-jde.cod_ccusto).

    FIND FIRST estabelecimento NO-LOCK
        WHERE estabelecimento.cod_estab = es-cross-reference-jde.Legacy-Company-Number NO-ERROR.

    FIND FIRST trad_org_ext NO-LOCK
        WHERE trad_org_ext.cod_matriz_trad_org_ext = 'GERAL'
        AND   trad_org_ext.cod_unid_organ          = estabelecimento.cod_empresa NO-ERROR.

    IF es-cross-reference-jde.cod_ccusto = "" THEN
        ASSIGN v-Ccusto = "000000".
    ELSE
        ASSIGN v-Ccusto = es-cross-reference-jde.cod_ccusto.

    FIND FIRST tt-conta-contab NO-LOCK
         WHERE tt-conta-contab.ep-codigo    = trad_org_ext.cod_unid_organ_ext
           AND tt-conta-contab.cod_cta_ctbl = es-cross-reference-jde.cod_cta_ctbl
           AND tt-conta-contab.cod_ccusto   = v-Ccusto NO-ERROR.
    IF  NOT AVAIL tt-conta-contab THEN
        DISP trad_org_ext.cod_unid_organ_ext     COLUMN-LABEL "Emp"
             es-cross-reference-jde.cod_cta_ctbl COLUMN-LABEL "Conta"
             v-Ccusto                            COLUMN-LABEL "CCusto"
             WITH WIDTH 333.
END.

OUTPUT CLOSE.

run pi-finalizar in h-acomp. 
    
RUN pi-abre-arq (INPUT fi-arquivo:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

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

    assign fi-arquivo:screen-value in frame {&frame-name}  = SESSION:TEMP-DIRECTORY + "esjd201.txt"
           fi-data-ini:screen-value in frame {&frame-name} = STRING ("01/01/1000")
           fi-data-fim:screen-value in frame {&frame-name} = STRING ("31/12/9999").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

