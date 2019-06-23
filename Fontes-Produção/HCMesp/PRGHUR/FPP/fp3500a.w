&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FP3500A 1.02.04.034 } /*** 010434 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fp3500a MFP}
&ENDIF

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
{prghur/fpp/fp3500tt.i}

def input-output parameter table for tt-param.
def input        parameter v_cdn_estab_ini like rh_estab.cdn_estab no-undo.
def input        parameter v_dat_refer       as date               no-undo.

define var c-progr             as character                        no-undo.
define var v_log_folha_educnal as logical                          no-undo.

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
&Scoped-Define ENABLED-OBJECTS l-imp-carga-horar-sem i-tipo-folha i-parcela ~
i-tipo-formula l-emite-demi l-emite-afast l-emite-ferias r-forma-pgto ~
l-mensal l-horista l-semanal l-quinzenal l-tarefa l-diarista bt-ok ~
fi-tipo-folha bt-cancelar fi-tipo-formula bt-ajuda fi-form-pgto ~
fi_categ_sal l-layout-detalhado RECT-1 RECT-12 RECT-13 RECT-14 RECT-15 ~
RECT-19 
&Scoped-Define DISPLAYED-OBJECTS l-imp-carga-horar-sem i-tipo-folha ~
i-parcela i-tipo-formula l-emite-demi l-emite-afast l-emite-ferias ~
r-forma-pgto l-mensal l-horista l-semanal l-quinzenal l-tarefa l-diarista ~
fi-tipo-folha fi-tipo-formula fi-form-pgto fi_categ_sal l-layout-detalhado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-form-pgto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20.29 BY .63 NO-UNDO.

DEFINE VARIABLE fi-tipo-folha AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE fi-tipo-formula AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE fi_categ_sal AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 18.86 BY .67 NO-UNDO.

DEFINE VARIABLE i-parcela AS INTEGER FORMAT "9":U INITIAL 9 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 2.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-tipo-folha AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Normal", 1,
"Adt Normal", 2,
"13o Sal rio", 3,
"Adt 13o Sal rio", 4
     SIZE 20.86 BY 3.17 NO-UNDO.

DEFINE VARIABLE i-tipo-formula AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Espec¡fico", 1,
"Duplo", 2,
"Individual", 3,
"Individual Moore", 4,
"PDF Duplo", 5,
"PDF Individual", 6
     SIZE 21.14 BY 3.92 NO-UNDO.

DEFINE VARIABLE r-forma-pgto AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Dep¢sito L¡quido", 1,
"Cheque Sal rio", 2,
"Caixa", 3,
"Todas", 4
     SIZE 24.14 BY 3.17.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.14 BY 3.83.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 3.88.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 4.58.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 7.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 14.25.

DEFINE VARIABLE l-diarista AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-emite-afast AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .75 NO-UNDO.

DEFINE VARIABLE l-emite-demi AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .75 NO-UNDO.

DEFINE VARIABLE l-emite-ferias AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .75 NO-UNDO.

DEFINE VARIABLE l-horista AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-imp-carga-horar-sem AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-layout-detalhado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .75 NO-UNDO.

DEFINE VARIABLE l-mensal AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-quinzenal AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-semanal AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-tarefa AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     l-imp-carga-horar-sem AT ROW 14.17 COL 50
     i-tipo-folha AT ROW 1.92 COL 7.57 NO-LABEL
     i-parcela AT ROW 5.83 COL 19.14 COLON-ALIGNED
     i-tipo-formula AT ROW 7.58 COL 7.57 NO-LABEL
     l-emite-demi AT ROW 12.25 COL 5
     l-emite-afast AT ROW 13.25 COL 5
     l-emite-ferias AT ROW 14.25 COL 5
     r-forma-pgto AT ROW 2 COL 45.29 NO-LABEL
     l-mensal AT ROW 7.25 COL 54.14
     l-horista AT ROW 8.25 COL 54.14
     l-semanal AT ROW 9.25 COL 54.14
     l-quinzenal AT ROW 10.25 COL 54.14
     l-tarefa AT ROW 11.25 COL 54.14
     l-diarista AT ROW 12.25 COL 54.14
     bt-ok AT ROW 15.63 COL 3.14
     fi-tipo-folha AT ROW 1.33 COL 3.14 COLON-ALIGNED NO-LABEL
     bt-cancelar AT ROW 15.63 COL 14.14
     fi-tipo-formula AT ROW 6.92 COL 3.14 COLON-ALIGNED NO-LABEL
     bt-ajuda AT ROW 15.63 COL 65.43
     fi-form-pgto AT ROW 1.25 COL 42.72 COLON-ALIGNED NO-LABEL
     fi_categ_sal AT ROW 6.25 COL 51 COLON-ALIGNED NO-LABEL
     l-layout-detalhado AT ROW 12.25 COL 25.29 WIDGET-ID 2
     RECT-1 AT ROW 15.38 COL 2
     RECT-12 AT ROW 1.58 COL 42.72
     RECT-13 AT ROW 1.63 COL 3.72
     RECT-14 AT ROW 7.25 COL 3.72
     RECT-15 AT ROW 6.71 COL 51.86
     RECT-19 AT ROW 1 COL 1.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 15.88
         WIDTH              = 75.57
         MAX-HEIGHT         = 27.96
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.96
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   if input frame {&frame-name} i-tipo-formula = 1 then do:
      c-progr = search("prghur/fpp/fp3500r1.r").
      if c-progr = ? then
         c-progr = search("prghur/fpp/fp3500r1.p").
      if c-progr = ? then do:
         run utp/ut-msgs.p (input "show", input 3457, input "").
         apply 'entry' to i-tipo-formula in frame {&frame-name}.    
         RUN dispatch IN THIS-PROCEDURE ('initialize':U).               
         return no-apply.
      end.
   end.     
  run pi_assign_tt.
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-tipo-formula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tipo-formula w-window
ON VALUE-CHANGED OF i-tipo-formula IN FRAME F-Main
DO:

  if i-tipo-formula:input-value = 3 then
      assign l-layout-detalhado:sensitive = yes.
  else do:
      assign l-layout-detalhado:sensitive = no
             l-layout-detalhado:checked   = no.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
  assign l-mensal:label in frame {&frame-name}    = {database/inpy/i03py029.i 04 01}
         l-horista:label in frame {&frame-name}   = {database/inpy/i03py029.i 04 02}
         l-semanal:label in frame {&frame-name}   = {database/inpy/i03py029.i 04 03}
         l-quinzenal:label in frame {&frame-name} = {database/inpy/i03py029.i 04 04}
         l-tarefa:label in frame {&frame-name}    = {database/inpy/i03py029.i 04 05}
         l-diarista:label in frame {&frame-name}  = {database/inpy/i03py029.i 04 06}.
  {utp/ut-rdset.i i-tipo-folha MFP}
  {utp/ut-rdset.i i-tipo-formula MFP}
  {utp/ut-rdset.i r-forma-pgto MFP}
  {utp/ut-field.i dthrpyc lote_movto_infor_fp num_parc_consdo_movto_efp 1}
  assign i-parcela:label in frame {&FRAME-NAME} = return-value.
  {utp/ut-liter.i Emite_Demitido MFP R}
  assign l-emite-demi:label in frame {&frame-name} = return-value.
  {utp/ut-liter.i Emite_Afastado MFP R}
  assign l-emite-afast:label in frame {&frame-name} = return-value.
  {utp/ut-liter.i Imprime_envelope_para_afastados_e_sem_l¡quido_a_receber MFP R}
  assign l-emite-afast:help in frame {&frame-name} = return-value.
  {utp/ut-liter.i Emite_F‚rias * L}
  assign l-emite-ferias:label in frame {&frame-name} = trim(return-value).
  {utp/ut-liter.i Imprime_envelope_para_funcion rios_em_situa‡Æo_de_f‚rias MFP R}
  assign l-emite-ferias:help in frame {&frame-name} = return-value.
  {utp/ut-liter.i Forma_Pagamento *}
  assign fi-form-pgto = return-value.
  {utp/ut-liter.i Tipo_Formul rio *}
  assign fi-tipo-formula = return-value.
  {utp/ut-liter.i Tipo_Folha *}
  assign fi-tipo-folha = return-value.
  {utp/ut-liter.i Categoria_Salarial *}
  assign fi_categ_sal = return-value.
  {utp/ut-liter.i Imprimir_Carga_Hor ria_Semanal *}
  assign l-imp-carga-horar-sem:label in frame {&frame-name} = return-value.
  {utp/ut-liter.i Layout MFP R}
  assign l-layout-detalhado:label in frame {&frame-name} = return-value.

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY l-imp-carga-horar-sem i-tipo-folha i-parcela i-tipo-formula 
          l-emite-demi l-emite-afast l-emite-ferias r-forma-pgto l-mensal 
          l-horista l-semanal l-quinzenal l-tarefa l-diarista fi-tipo-folha 
          fi-tipo-formula fi-form-pgto fi_categ_sal l-layout-detalhado 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE l-imp-carga-horar-sem i-tipo-folha i-parcela i-tipo-formula 
         l-emite-demi l-emite-afast l-emite-ferias r-forma-pgto l-mensal 
         l-horista l-semanal l-quinzenal l-tarefa l-diarista bt-ok 
         fi-tipo-folha bt-cancelar fi-tipo-formula bt-ajuda fi-form-pgto 
         fi_categ_sal l-layout-detalhado RECT-1 RECT-12 RECT-13 RECT-14 RECT-15 
         RECT-19 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

{utp/ut9000.i "FP3500A" "1.02.04.032"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  find param_folha_educnal no-lock where
       param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
  if avail param_folha_educnal then
      assign v_log_folha_educnal = yes.
  else
      assign v_log_folha_educnal = no.
  /* Code placed here will execute AFTER standard behavior.    */
find first tt-param no-lock no-error.
if avail tt-param then
   assign i-tipo-folha:screen-value in frame {&frame-name} = string(tt-param.i-tipo-folha)
          i-parcela:screen-value in frame {&frame-name} = string(tt-param.i-parcela)
          i-tipo-formula:screen-value in frame {&frame-name} = string(tt-param.i-tipo-formula)
          r-forma-pgto:screen-value in frame {&FRAME-NAME} = string(tt-param.r-forma-pgto)
          l-emite-demi:checked in frame {&FRAME-NAME} = tt-param.l-emite-demi
          l-emite-afast:checked in frame {&FRAME-NAME} = tt-param.l-emite-afast
          l-emite-ferias:checked in frame {&FRAME-NAME} = tt-param.l-emite-ferias
          l-mensal:checked in frame {&FRAME-NAME} = tt-param.l-mensal
          l-horista:checked in frame {&FRAME-NAME} = tt-param.l-horista
          l-semanal:checked in frame {&FRAME-NAME} = tt-param.l-semanal
          l-quinzenal:checked in frame {&FRAME-NAME} = tt-param.l-quinzenal
          l-tarefa:checked in frame {&FRAME-NAME} = tt-param.l-tarefa
          l-diarista:checked in frame {&FRAME-NAME} = tt-param.l-diarista
          l-imp-carga-horar-sem:checked in frame {&FRAME-NAME} = tt-param.l-imp-carga-horar-sem
          l-layout-detalhado:checked in frame {&FRAME-NAME} = tt-param.l-layout-detalhado.

apply "value-changed" to i-tipo-formula in frame {&FRAME-NAME}.

IF v_log_folha_educnal THEN
    ASSIGN l-imp-carga-horar-sem:VISIBLE IN FRAME {&FRAME-NAME} = YES.
ELSE
    ASSIGN l-imp-carga-horar-sem:VISIBLE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_assign_tt w-window 
PROCEDURE pi_assign_tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each tt-param:
    delete tt-param.
end.
create tt-param.
assign tt-param.l-mensal              = input frame {&frame-name} l-mensal
       tt-param.l-horista             = input frame {&frame-name} l-horista
       tt-param.l-semanal             = input frame {&frame-name} l-semanal
       tt-param.l-quinzenal           = input frame {&frame-name} l-quinzenal
       tt-param.l-tarefa              = input frame {&frame-name} l-tarefa
       tt-param.l-diarista            = input frame {&frame-name} l-diarista
       tt-param.l-emite-demi          = input frame {&frame-name} l-emite-demi
       tt-param.i-tipo-folha          = input frame {&frame-name} i-tipo-folha
       tt-param.i-parcela             = input frame {&frame-name} i-parcela
       tt-param.i-tipo-formula        = input frame {&frame-name} i-tipo-formula
       tt-param.r-forma-pgto          = input frame {&frame-name} r-forma-pgto
       tt-param.l-emite-afast         = input frame {&frame-name} l-emite-afast
       tt-param.l-emite-ferias        = input frame {&frame-name} l-emite-ferias
       tt-param.l-imp-carga-horar-sem = input frame {&frame-name} l-imp-carga-horar-sem
       tt-param.l-layout-detalhado    = input frame {&frame-name} l-layout-detalhado.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

