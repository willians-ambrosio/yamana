&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/******************************************************************************************* 
* Copyright (C) 2000 by Progress Software Corporation. All rights
* reserved. Prior versions of this work may contain portions
* contributed by participants of Possenet.
*                                                                    
********************************************************************************************/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def new global shared var v_rec_empresa      as recid format ">>>>>>9" no-undo.
def new global shared var v_rec_unid_organ   as recid format ">>>>>>9" no-undo.
def new global shared var v_rec_cta_ctbl     as recid format ">>>>>>9" no-undo.
def new global shared var v_cod_usuar_corren as char  format "x(12)":U no-undo label "Usu rio Corrente" column-label "Usu rio Corrente".

def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def new shared var v_cod_dwb_file as char format "x(40)":U no-undo label "Arquivo" column-label "Arquivo".

def buffer bf-criter_distrib_cta_ctbl     for criter_distrib_cta_ctbl.
def buffer bf-seq-criter_distrib_cta_ctbl for criter_distrib_cta_ctbl.

/*
if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    message "Programa executÿvel nÆo foi encontrado: prgtec/btb/btb906za.py"
           view-as alert-box error buttons ok.
    stop.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    message "Programa executÿvel nÆo foi encontrado: prgtec/men/men901za.py"
           view-as alert-box error buttons ok.
    return.
end.
else
    run prgtec/men/men901za.py (Input 'esflg004') /*prg_fnc_verify_security*/.
if  return-value = "2014" then do:
    /* Programa a ser executado nÆo ‚ um programa v lido Datasul ! */
    run pi_messages(input "show", input 2014,
                    input substitute("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                     'esflg004')).
    return.
end /* if */.
if  return-value = "2012" then do:
    /* Usu rio sem permissÆo para acessar o programa. */
    run pi_messages(input "show", input 2012,
                    input substitute("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                     'esflg004')).
    return.
end /* if */.
/* End_Include: i_verify_security */
*/

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
&Scoped-Define ENABLED-OBJECTS c-cod_cta_ctbl-ini bt-zoom-cta-ini ~
c-cod_cta_ctbl-fim bt-zoom-cta-fim c-cod_empresa-de bt-zoom-emp-de ~
c-cod_estab-de bt-zoom-est-de c-cod_empresa-para bt-zoom-emp-para ~
c-cod_estab-para bt-zoom-est-para c-saida bt_get_file bt-executar bt-saida 
&Scoped-Define DISPLAYED-OBJECTS c-cod_cta_ctbl-ini c-cod_cta_ctbl-fim ~
c-cod_empresa-de c-cod_estab-de c-cod_empresa-para c-cod_estab-para c-saida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-executar AUTO-GO 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-saida AUTO-END-KEY 
     LABEL "Saida" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-zoom-cta-fim 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Cta Fim" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-cta-ini 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Cta Ini" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-emp-de 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Emp Base" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-emp-para 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Emp Dest" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-est-de 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Est Base" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-est-para 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Zoom Est Dest" 
     SIZE 4 BY .88.

DEFINE BUTTON bt_get_file 
     IMAGE-UP FILE "image/im-sea1":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1":U
     LABEL "Pesquisa Arquivo" 
     SIZE 4 BY .88 TOOLTIP "Pesquisa Arquivo".

DEFINE VARIABLE c-cod_cta_ctbl-fim AS CHARACTER FORMAT "x(20)" INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     LABEL "Conta Cont bil Final" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod_cta_ctbl-ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Conta Cont bil In¡cio" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod_empresa-de AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa Base" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod_empresa-para AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa Destino" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod_estab-de AS CHARACTER FORMAT "x(3)" 
     LABEL "Estab Base" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod_estab-para AS CHARACTER FORMAT "x(3)" 
     LABEL "Estab Destino" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-saida AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo Saida" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod_cta_ctbl-ini AT ROW 1.5 COL 19 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil"
     bt-zoom-cta-ini AT ROW 1.5 COL 41
     c-cod_cta_ctbl-fim AT ROW 1.5 COL 59.72 COLON-ALIGNED HELP
          "C¢digo Conta Cont bil"
     bt-zoom-cta-fim AT ROW 1.5 COL 81.72
     c-cod_empresa-de AT ROW 3 COL 19 COLON-ALIGNED HELP
          "C¢digo Empresa"
     bt-zoom-emp-de AT ROW 3 COL 25.72
     c-cod_estab-de AT ROW 3 COL 59.72 COLON-ALIGNED HELP
          "C¢digo Estabelecimento"
     bt-zoom-est-de AT ROW 3 COL 66.43
     c-cod_empresa-para AT ROW 4.5 COL 19 COLON-ALIGNED HELP
          "C¢digo Empresa"
     bt-zoom-emp-para AT ROW 4.5 COL 25.72
     c-cod_estab-para AT ROW 4.5 COL 59.72 COLON-ALIGNED HELP
          "C¢digo Estabelecimento"
     bt-zoom-est-para AT ROW 4.5 COL 66.43
     c-saida AT ROW 6.5 COL 19 COLON-ALIGNED
     bt_get_file AT ROW 6.5 COL 81
     bt-executar AT ROW 8 COL 1.14
     bt-saida AT ROW 8 COL 16.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.72 BY 8.17
         FONT 1.


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
         TITLE              = ".: Copia Crit‚rios Distribui‡Æo Conta :."
         HEIGHT             = 8.13
         WIDTH              = 84.72
         MAX-HEIGHT         = 28.79
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.79
         VIRTUAL-WIDTH      = 146.14
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
   FRAME-NAME                                                           */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* <insert SmartWindow title> */
DO:
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar W-Win
ON CHOOSE OF bt-executar IN FRAME F-Main /* Executar */
DO:
    run pi-executar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-saida W-Win
ON CHOOSE OF bt-saida IN FRAME F-Main /* Saida */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-cta-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-cta-fim W-Win
ON CHOOSE OF bt-zoom-cta-fim IN FRAME F-Main /* Zoom Cta Fim */
DO:
    FIND FIRST plano_cta_ctbl NO-LOCK 
        WHERE plano_cta_ctbl.cod_plano_cta_ctbl = 'CONTSOC' NO-ERROR.

    ASSIGN v_rec_plano_cta_ctbl = RECID(plano_cta_ctbl).

    assign v_rec_cta_ctbl = ?.
    if  search("prgint/utb/utb080ne.r") = ? and
        search("prgint/utb/utb080ne.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb080ne.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb080ne.p.

    if  v_rec_cta_ctbl <> ? then do:
        find first cta_ctbl no-lock
             where recid(cta_ctbl) = v_rec_cta_ctbl no-error.
        assign c-cod_cta_ctbl-fim:screen-value in frame F-Main = cta_ctbl.cod_cta_ctbl.
    end.
    apply "entry" to c-cod_cta_ctbl-fim in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-cta-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-cta-ini W-Win
ON CHOOSE OF bt-zoom-cta-ini IN FRAME F-Main /* Zoom Cta Ini */
DO:

    FIND FIRST plano_cta_ctbl NO-LOCK 
        WHERE plano_cta_ctbl.cod_plano_cta_ctbl = 'CONTSOC' NO-ERROR.

    ASSIGN v_rec_plano_cta_ctbl = RECID(plano_cta_ctbl).

    assign v_rec_cta_ctbl = ?.
    if  search("prgint/utb/utb080ne.r") = ? and
        search("prgint/utb/utb080ne.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb080ne.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb080ne.p.

    if  v_rec_cta_ctbl <> ? then do:
        find first cta_ctbl no-lock
             where recid(cta_ctbl) = v_rec_cta_ctbl no-error.
        assign c-cod_cta_ctbl-ini:screen-value in frame F-Main = cta_ctbl.cod_cta_ctbl.
    end.
    apply "entry" to c-cod_cta_ctbl-ini in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-emp-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-emp-de W-Win
ON CHOOSE OF bt-zoom-emp-de IN FRAME F-Main /* Zoom Emp Base */
DO:
    assign v_rec_empresa = ?.
    if  search("prgint/utb/utb069ka.r") = ? and
        search("prgint/utb/utb069ka.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb069ka.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb069ka.p.

    if  v_rec_empresa <> ? then do:
        find first emsuni.empresa no-lock
             where recid(empresa) = v_rec_empresa no-error.
        assign c-cod_empresa-de:screen-value in frame F-Main = empresa.cod_empresa.
    end.
    apply "entry" to c-cod_empresa-de in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-emp-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-emp-para W-Win
ON CHOOSE OF bt-zoom-emp-para IN FRAME F-Main /* Zoom Emp Dest */
DO:
    assign v_rec_empresa = ?.
    if  search("prgint/utb/utb069ka.r") = ? and
        search("prgint/utb/utb069ka.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb069ka.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb069ka.p.

    if  v_rec_empresa <> ? then do:
        find first emsuni.empresa no-lock
             where recid(empresa) = v_rec_empresa no-error.
        assign c-cod_empresa-para:screen-value in frame F-Main = empresa.cod_empresa.
    end.
    apply "entry" to c-cod_empresa-para in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-est-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-est-de W-Win
ON CHOOSE OF bt-zoom-est-de IN FRAME F-Main /* Zoom Est Base */
DO:
    assign v_rec_unid_organ = ?.
    if  search("prgint/utb/utb010ka.r") = ? and
        search("prgint/utb/utb010ka.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb010ka.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb010ka.p.

    if  v_rec_unid_organ <> ? then do:
        find first ems5.unid_organ no-lock
             where recid(unid_organ) = v_rec_unid_organ no-error.
        assign c-cod_estab-de:screen-value in frame F-Main = unid_organ.cod_unid_organ.
    end.
    apply "entry" to c-cod_estab-de in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-est-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-est-para W-Win
ON CHOOSE OF bt-zoom-est-para IN FRAME F-Main /* Zoom Est Dest */
DO:
    assign v_rec_unid_organ = ?.
    if  search("prgint/utb/utb010ka.r") = ? and
        search("prgint/utb/utb010ka.p") = ? then do:
        message "Programa execut vel nÆo foi encontrado: prgint/utb/utb010ka.p"
            view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb010ka.p.

    if  v_rec_unid_organ <> ? then do:
        find first ems5.unid_organ no-lock
             where recid(unid_organ) = v_rec_unid_organ no-error.
        assign c-cod_estab-para:screen-value in frame F-Main = unid_organ.cod_unid_organ.
    end.
    apply "entry" to c-cod_estab-para in frame F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_get_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_get_file W-Win
ON CHOOSE OF bt_get_file IN FRAME F-Main /* Pesquisa Arquivo */
DO:
    system-dialog get-file v_cod_dwb_file
        title "Imprimir"
        filters '*.rpt' '*.rpt',
                "*.*"  "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign c-saida:screen-value in frame F-Main = v_cod_dwb_file.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

find first usuar_mestre no-lock
     where usuar_mestre.cod_usuario = v_cod_usuar_corren no-error.
assign c-saida = if  avail usuar_mestre then
                     replace(usuar_mestre.nom_dir_spool,"/","\")
                 else
                     session:temp-directory.

assign c-saida = c-saida + (if  substr(c-saida,length(c-saida),1) <> "\" then "\" else "") +
                 "esflg004.txt".

RUN enable_UI.

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
  DISPLAY c-cod_cta_ctbl-ini c-cod_cta_ctbl-fim c-cod_empresa-de c-cod_estab-de 
          c-cod_empresa-para c-cod_estab-para c-saida 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE c-cod_cta_ctbl-ini bt-zoom-cta-ini c-cod_cta_ctbl-fim bt-zoom-cta-fim 
         c-cod_empresa-de bt-zoom-emp-de c-cod_estab-de bt-zoom-est-de 
         c-cod_empresa-para bt-zoom-emp-para c-cod_estab-para bt-zoom-est-para 
         c-saida bt_get_file bt-executar bt-saida 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar W-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var i-seq   as int    no-undo.
    def var h-acomp as handle no-undo.

    run pi-validate.
    if  return-value = "ADM-ERROR":U then
        return "ADM-ERROR":U.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Processando").

    output to value(c-saida) no-echo no-convert.
    put "Plano Conta;Conta;Estab;Seq;Data Ini;Data Fim;Crit‚rio;Mapa;Observa‡Æo" skip.

    do with frame F-Main:
        for each plano_cta_ctbl no-lock:
            if  not can-find(first criter_distrib_cta_ctbl of plano_cta_ctbl no-lock use-index crtrdsta_id
                             where criter_distrib_cta_ctbl.cod_empresa     = c-cod_empresa-de:input-value
                               and criter_distrib_cta_ctbl.cod_estab       = c-cod_estab-de  :input-value
                               and criter_distrib_cta_ctbl.dat_inic_valid <= today
                               and criter_distrib_cta_ctbl.dat_fim_valid  >= today) then do:
                        put "plano de conta " + plano_cta_ctbl.cod_plano_cta_ctbl + "nao possui Crit‚rio".
                        next.
                    end.

            for each cta_ctbl of plano_cta_ctbl no-lock
               where cta_ctbl.cod_cta_ctbl >= c-cod_cta_ctbl-ini:input-value
                 and cta_ctbl.cod_cta_ctbl <= c-cod_cta_ctbl-fim:input-value:
                if  not can-find(first criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                                 where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                                   and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                                   and criter_distrib_cta_ctbl.cod_estab          = c-cod_estab-de  :input-value
                                   and criter_distrib_cta_ctbl.cod_empresa        = c-cod_empresa-de:input-value
                                   and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                                   and criter_distrib_cta_ctbl.dat_fim_valid     >= today) then do:
                        put "Nao existe Crit‚rio ativo para conta " + cta_ctbl.cod_cta_ctbl + " na Empresa Base".
                        next.
                    end.

                run pi-acompanhar in h-acomp(input "Conta: " + cta_ctbl.cod_cta_ctbl).

                assign i-seq = 0.
                for each criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                   where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                     and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                     and criter_distrib_cta_ctbl.cod_estab          = c-cod_estab-de  :input-value
                     and criter_distrib_cta_ctbl.cod_empresa        = c-cod_empresa-de:input-value
                     and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                     and criter_distrib_cta_ctbl.dat_fim_valid     >= today:
                    if  criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto <> "" and
                        not can-find(first mapa_distrib_ccusto no-lock
                                     where mapa_distrib_ccusto.cod_estab                = c-cod_estab-para:input-value
                                       and mapa_distrib_ccusto.cod_mapa_distrib_ccusto  = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
                                       and mapa_distrib_ccusto.dat_inic_valid          <= today
                                       and mapa_distrib_ccusto.dat_fim_valid           >= today) then do:
                        run pi-log(input "Mapa " + criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto +
                                         " para conta " + criter_distrib_cta_ctbl.cod_cta_ctbl + " inexistente no destino").
                        next.
                    end.

                    if  can-find(first bf-criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                                 where bf-criter_distrib_cta_ctbl.cod_plano_cta_ctbl = criter_distrib_cta_ctbl.cod_plano_cta_ctbl
                                   and bf-criter_distrib_cta_ctbl.cod_cta_ctbl       = criter_distrib_cta_ctbl.cod_cta_ctbl
                                   and bf-criter_distrib_cta_ctbl.cod_estab          = c-cod_estab-para  :input-value
                                   and bf-criter_distrib_cta_ctbl.cod_empresa        = c-cod_empresa-para:input-value
                                   and bf-criter_distrib_cta_ctbl.dat_inic_valid    <= today
                                   and bf-criter_distrib_cta_ctbl.dat_fim_valid     >= today) then do:
                        run pi-log(input "Crit‚rio j  existente").
                        next.
                    end.

                    find first bf-seq-criter_distrib_cta_ctbl no-lock
                         where bf-seq-criter_distrib_cta_ctbl.cod_plano_cta_ctbl = criter_distrib_cta_ctbl.cod_plano_cta_ctbl
                           and bf-seq-criter_distrib_cta_ctbl.cod_cta_ctbl       = criter_distrib_cta_ctbl.cod_cta_ctbl
                           and bf-seq-criter_distrib_cta_ctbl.cod_estab          = c-cod_estab-para  :input-value
                           and bf-seq-criter_distrib_cta_ctbl.cod_empresa        = c-cod_empresa-para:input-value
                           and bf-seq-criter_distrib_cta_ctbl.dat_inic_valid    <= today
                           and bf-seq-criter_distrib_cta_ctbl.dat_fim_valid     >= today no-error.
                    if  avail bf-seq-criter_distrib_cta_ctbl then do:
                        run pi-log(input "J  existe crit‚rio ativo").
                        next.
                    end.
                    else do:
                        find last bf-seq-criter_distrib_cta_ctbl no-lock
                            where bf-seq-criter_distrib_cta_ctbl.cod_plano_cta_ctbl = criter_distrib_cta_ctbl.cod_plano_cta_ctbl
                              and bf-seq-criter_distrib_cta_ctbl.cod_cta_ctbl       = criter_distrib_cta_ctbl.cod_cta_ctbl
                              and bf-seq-criter_distrib_cta_ctbl.cod_estab          = c-cod_estab-para:input-value no-error.
                        if  avail bf-seq-criter_distrib_cta_ctbl then
                            assign i-seq = bf-seq-criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl + 10.
                        else
                            assign i-seq = 10.
                    end.

                    create bf-criter_distrib_cta_ctbl.
                    buffer-copy criter_distrib_cta_ctbl
                         except criter_distrib_cta_ctbl.cod_empresa
                                criter_distrib_cta_ctbl.cod_estab
                                criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl
                                criter_distrib_cta_ctbl.dat_livre_1
                             to bf-criter_distrib_cta_ctbl
                    assign bf-criter_distrib_cta_ctbl.cod_empresa                 = c-cod_empresa-para:input-value
                           bf-criter_distrib_cta_ctbl.cod_estab                   = c-cod_estab-para  :input-value
                           bf-criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl = i-seq
                           bf-criter_distrib_cta_ctbl.dat_livre_1                 = today.

                    run pi-log(input "Crit‚rio criado corretamente").
                end.
            end.
        end.
    end.
    output close.

    run pi-finalizar in h-acomp.  

    RUN pi_show_report_2 (INPUT c-saida).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-log W-Win 
PROCEDURE pi-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-observacao as char no-undo.

    export delimiter ";"
           criter_distrib_cta_ctbl.cod_plano_cta_ctbl
           criter_distrib_cta_ctbl.cod_cta_ctbl
           criter_distrib_cta_ctbl.cod_estab
           criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl
           criter_distrib_cta_ctbl.dat_inic_valid
           criter_distrib_cta_ctbl.dat_fim_valid
           criter_distrib_cta_ctbl.ind_criter_distrib_ccusto
           criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
           p-observacao.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate W-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    do with frame F-Main:
        if  c-cod_cta_ctbl-ini:input-value > c-cod_cta_ctbl-fim:input-value then do:
            message "Conta Cont bil Inicial Maior que Final"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.

        if  not can-find(first emsuni.empresa no-lock
                         where empresa.cod_empresa = c-cod_empresa-de:input-value) then do:
            message "Empresa Base NÆo encontrada"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.

        if  not can-find(first emsuni.empresa no-lock
                         where empresa.cod_empresa = c-cod_empresa-para:input-value) then do:
            message "Empresa Destino NÆo encontrada"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.

        if  not can-find(first estabelecimento no-lock
                         where estabelecimento.cod_empresa = c-cod_empresa-de:input-value
                           and estabelecimento.cod_estab   = c-cod_estab-de  :input-value) then do:
            message "Estabelecimento Base NÆo encontrada"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.

        if  not can-find(first estabelecimento no-lock
                         where estabelecimento.cod_empresa = c-cod_empresa-para:input-value
                           and estabelecimento.cod_estab   = c-cod_estab-para  :input-value) then do:
            message "Estabelecimento Destino NÆo encontrada"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.

        if  c-saida:input-value = "" or
            length(c-saida:input-value) < 8 then do:
            message "Arquivo de Saida Obrigat¢rio"
                view-as alert-box info buttons ok.
            return "ADM-ERROR":U.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_messages W-Win 
PROCEDURE pi_messages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param c_action    as char    no-undo.
    def input param i_msg       as integer no-undo.
    def input param c_param     as char    no-undo.

    def var c_prg_msg           as char    no-undo.

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n’o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
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



PROCEDURE pi_show_report_2:

    /************************ Parameter Definition Begin ************************/

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


END PROCEDURE. /* pi_show_report_2 */
