&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
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
define variable l-ok            as logical      no-undo.
define variable c-mes-ano       as character    no-undo.
define variable c-competencia   as character    no-undo.
define variable c-desc-licenc   as character    no-undo.

def buffer bf-sdo_bem_pat for sdo_bem_pat.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-par

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-4 IMAGE-1 IMAGE-2 RECT-5 ~
RECT-6 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 ~
IMAGE-11 IMAGE-12 fi-empresa-ini fi-empresa-fim fi-data-trans-ini ~
fi-data-trans-fim fi-fornecedor-ini fi-fornecedor-fim rs-destino ~
fi-conta-ini fi-conta-fim bt-arquivo fi-ccusto-ini fi-ccusto-fim rs-padrao ~
fi-licenca-ini fi-licenca-fim tg-bem-baixado bt_fecha bt_imprime bt_cancela 
&Scoped-Define DISPLAYED-OBJECTS fi-empresa-ini fi-empresa-fim ~
fi-data-trans-ini fi-data-trans-fim fi-fornecedor-ini fi-fornecedor-fim ~
rs-destino fi-conta-ini fi-conta-fim c-arquivo-entrada fi-ccusto-ini ~
fi-ccusto-fim rs-padrao fi-licenca-ini fi-licenca-fim tg-bem-baixado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt_cancela 
     LABEL "Cancela" 
     SIZE 12 BY 1.

DEFINE BUTTON bt_fecha 
     LABEL "Fecha" 
     SIZE 12 BY 1.

DEFINE BUTTON bt_imprime 
     LABEL "Imprime" 
     SIZE 12 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ccusto-fim AS CHARACTER FORMAT "X(8)":U INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ccusto-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-conta-fim AS CHARACTER FORMAT "X(8)":U INITIAL "999999999" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-conta-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Conta Patrim" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-data-trans-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-data-trans-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fornecedor-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fornecedor-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-licenca-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-licenca-ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "N£mero OI" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-destino AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Terminal", 1,
"Arquivo", 2
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE rs-padrao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Padr∆o EMS", 1,
"Padr∆o CSV", 2
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 7.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 1.5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 1.75.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 5.67.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 3.75.

DEFINE VARIABLE tg-bem-baixado AS LOGICAL INITIAL no 
     LABEL "Considera Bens j† Baixados ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-par
     fi-empresa-ini AT ROW 2 COL 17 COLON-ALIGNED WIDGET-ID 78
     fi-empresa-fim AT ROW 2 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fi-data-trans-ini AT ROW 2 COL 68 COLON-ALIGNED WIDGET-ID 28
     fi-data-trans-fim AT ROW 2 COL 87 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fi-fornecedor-ini AT ROW 3 COL 17 COLON-ALIGNED WIDGET-ID 68
     fi-fornecedor-fim AT ROW 3 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     rs-destino AT ROW 3.92 COL 62 NO-LABEL WIDGET-ID 52
     fi-conta-ini AT ROW 4 COL 17 COLON-ALIGNED WIDGET-ID 86
     fi-conta-fim AT ROW 4 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     bt-arquivo AT ROW 4.88 COL 95 WIDGET-ID 58 NO-TAB-STOP 
     c-arquivo-entrada AT ROW 4.92 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fi-ccusto-ini AT ROW 5 COL 17 COLON-ALIGNED WIDGET-ID 94
     fi-ccusto-fim AT ROW 5 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     rs-padrao AT ROW 5.92 COL 62 NO-LABEL WIDGET-ID 60
     fi-licenca-ini AT ROW 6 COL 17 COLON-ALIGNED WIDGET-ID 102
     fi-licenca-fim AT ROW 6 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     tg-bem-baixado AT ROW 7.25 COL 3 WIDGET-ID 14
     bt_fecha AT ROW 8.67 COL 3 WIDGET-ID 6
     bt_imprime AT ROW 8.67 COL 16 WIDGET-ID 8
     bt_cancela AT ROW 8.67 COL 29 WIDGET-ID 10
     "Per°odo" VIEW-AS TEXT
          SIZE 7.29 BY .54 AT ROW 1.33 COL 61.72 WIDGET-ID 20
     "Destino" VIEW-AS TEXT
          SIZE 7.29 BY .54 AT ROW 3.25 COL 61.72 WIDGET-ID 50
     "ParÉmetros" VIEW-AS TEXT
          SIZE 12.29 BY .54 AT ROW 1.33 COL 3.72 WIDGET-ID 12
     RECT-1 AT ROW 1.25 COL 2 WIDGET-ID 2
     RECT-2 AT ROW 8.5 COL 2 WIDGET-ID 4
     RECT-4 AT ROW 1.5 COL 61 WIDGET-ID 18
     IMAGE-1 AT ROW 2 COL 81 WIDGET-ID 24
     IMAGE-2 AT ROW 2 COL 85 WIDGET-ID 26
     RECT-5 AT ROW 1.5 COL 3 WIDGET-ID 34
     RECT-6 AT ROW 3.42 COL 61 WIDGET-ID 48
     IMAGE-3 AT ROW 3 COL 34 WIDGET-ID 70
     IMAGE-4 AT ROW 3 COL 38 WIDGET-ID 72
     IMAGE-5 AT ROW 2 COL 34 WIDGET-ID 80
     IMAGE-6 AT ROW 2 COL 38 WIDGET-ID 82
     IMAGE-7 AT ROW 4 COL 34 WIDGET-ID 88
     IMAGE-8 AT ROW 4 COL 38 WIDGET-ID 90
     IMAGE-9 AT ROW 5 COL 34 WIDGET-ID 96
     IMAGE-10 AT ROW 5 COL 38 WIDGET-ID 98
     IMAGE-11 AT ROW 6 COL 34 WIDGET-ID 106
     IMAGE-12 AT ROW 6 COL 38 WIDGET-ID 104
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.57 BY 9.38
         FONT 1 WIDGET-ID 100.


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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Extrai Licenáa de Uso"
         HEIGHT             = 9.38
         WIDTH              = 101.57
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 101.57
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 101.57
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
/* SETTINGS FOR FRAME f-pg-par
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN c-arquivo-entrada IN FRAME f-pg-par
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Extrai Licenáa de Uso */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Extrai Licenáa de Uso */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo W-Win
ON CHOOSE OF bt-arquivo IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancela W-Win
ON CHOOSE OF bt_cancela IN FRAME f-pg-par /* Cancela */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fecha W-Win
ON CHOOSE OF bt_fecha IN FRAME f-pg-par /* Fecha */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_imprime W-Win
ON CHOOSE OF bt_imprime IN FRAME f-pg-par /* Imprime */
DO:
    run pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino W-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-par
DO:
    if rs-destino:input-value in frame {&frame-name} = 1 then
        assign c-arquivo-entrada:sensitive    in frame {&frame-name} = no
               bt-arquivo     :sensitive    in frame {&frame-name} = no
               c-arquivo-entrada:screen-value in frame {&frame-name} = ''.
    else if rs-destino:input-value in frame {&frame-name} = 2 then do:
        assign c-arquivo-entrada:sensitive    in frame {&frame-name} = yes
               bt-arquivo     :sensitive    in frame {&frame-name} = yes.

        if rs-padrao:input-value in frame {&frame-name} = 1 then
            assign c-arquivo-entrada:screen-value in frame {&frame-name} = session:temp-directory + 'esacr001.lst'.
        else
            assign c-arquivo-entrada:screen-value in frame {&frame-name} = session:temp-directory + 'esacr001.csv'.

    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-padrao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-padrao W-Win
ON VALUE-CHANGED OF rs-padrao IN FRAME f-pg-par
DO:
    apply 'value-changed' to rs-destino.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

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
  DISPLAY fi-empresa-ini fi-empresa-fim fi-data-trans-ini fi-data-trans-fim 
          fi-fornecedor-ini fi-fornecedor-fim rs-destino fi-conta-ini 
          fi-conta-fim c-arquivo-entrada fi-ccusto-ini fi-ccusto-fim rs-padrao 
          fi-licenca-ini fi-licenca-fim tg-bem-baixado 
      WITH FRAME f-pg-par IN WINDOW W-Win.
  ENABLE RECT-1 RECT-2 RECT-4 IMAGE-1 IMAGE-2 RECT-5 RECT-6 IMAGE-3 IMAGE-4 
         IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 
         fi-empresa-ini fi-empresa-fim fi-data-trans-ini fi-data-trans-fim 
         fi-fornecedor-ini fi-fornecedor-fim rs-destino fi-conta-ini 
         fi-conta-fim bt-arquivo fi-ccusto-ini fi-ccusto-fim rs-padrao 
         fi-licenca-ini fi-licenca-fim tg-bem-baixado bt_fecha bt_imprime 
         bt_cancela 
      WITH FRAME f-pg-par IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign fi-data-trans-ini:screen-value in frame {&frame-name} = string(today - day(today) + 1)
         fi-data-trans-fim:screen-value in frame {&frame-name} = string(today).

  apply 'value-changed' to rs-destino.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime W-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first ems5.empresa
     where ems5.empresa.cod_empresa = v_cod_empres_usuar
           no-lock no-error.

assign c-mes-ano        = "Janeiro,Fevereiro,Marco,Abril,Maio,Junho,Julho,Agosto,Setembro,Outubro,Novembro,Dezembro"
       c-competencia    = entry(month(fi-data-trans-ini:input-value in frame f-pg-par),c-mes-ano) + '/' + string(year(fi-data-trans-ini:input-value in frame f-pg-par)) + 
                          ' ate ' + 
                          entry(month(fi-data-trans-fim:input-value in frame f-pg-par),c-mes-ano) + '/' + string(year(fi-data-trans-fim:input-value in frame f-pg-par)).

if rs-destino:input-value in frame f-pg-par = 1 then do:
    if rs-padrao:input-value in frame f-pg-par = 1 then
        assign c-arquivo-entrada = session:temp-directory + 'extrai-licenca.lst'.
    else
        assign c-arquivo-entrada = session:temp-directory + 'extrai-licenca.csv'.
end.
else
    assign c-arquivo-entrada = c-arquivo-entrada:input-value in frame f-pg-par.

output to value(c-arquivo-entrada) NO-CONVERT.

if rs-padrao:input-value in frame f-pg-par = 2 then do:
    put "Empresa"       ";" ems5.empresa.nom_abrev format "x(40)" skip
        "Relatorio"     ";" "Aquisicoes do Ativo Permanente" skip
        "Competencia"   ";" c-competencia format "x(40)" skip
        "Empresa;Conta Pat;Descriá∆o Conta;Bem;Seq;Descriá∆o Bem;Data Aquisiá∆o;Centro Custo;Vlr R$;Vlr USD;Numero OI;Descriá∆o OI" skip.
end.
else do:
    put fill('-',180) format 'x(180)' skip
        "Aquisicoes do Ativo Permanente" at 50 skip
        fill('-',180) format 'x(180)' skip.

    put "Empresa.........: " ems5.empresa.nom_abrev format "x(40)" skip
        "Relatorio.......: " "Aquisicoes do Ativo Permanente" skip
        "Competencia.....: " c-competencia format "x(40)" skip(2).

end.

for each bem_pat 
   where bem_pat.cod_empresa         >= fi-empresa-ini    :input-value in frame f-pg-par
     and bem_pat.cod_empresa         <= fi-empresa-fim    :input-value in frame f-pg-par
     and bem_pat.cod_cta_pat         >= fi-conta-ini      :input-value in frame f-pg-par
     and bem_pat.cod_cta_pat         <= fi-conta-fim      :input-value in frame f-pg-par
     and bem_pat.cdn_fornecedor      >= fi-fornecedor-ini :input-value in frame f-pg-par
     and bem_pat.cdn_fornecedor      <= fi-fornecedor-fim :input-value in frame f-pg-par
     and bem_pat.dat_aquis_bem_pat   >= fi-data-trans-ini :input-value in frame f-pg-par
     and bem_pat.dat_aquis_bem_pat   <= fi-data-trans-fim :input-value in frame f-pg-par
     and bem_pat.cod_ccusto_respons  >= fi-ccusto-ini     :input-value in frame f-pg-par
     and bem_pat.cod_ccusto_respons  <= fi-ccusto-fim     :input-value in frame f-pg-par
     and bem_pat.cod_licenc_uso      >= fi-licenca-ini    :input-value in frame f-pg-par
     and bem_pat.cod_licenc_uso      <= fi-licenca-fim    :input-value in frame f-pg-par
         NO-LOCK ,
   first cta_pat of bem_pat 
         no-lock :

    if can-find(first movto_bem_pat of bem_pat no-lock
                where movto_bem_pat.ind_trans_calc_bem_pat   = "Baixa"
                  /*and movto_bem_pat.ind_orig_calc_bem_pat   <> "Transferància" */
                   and movto_bem_pat.log_estorn_movto_bem_pat = no) and not tg-bem-baixado:checked in frame f-pg-par then next.

    find first sdo_bem_pat no-lock use-index sdbmpt_id
         where sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
         /*  and sdo_bem_pat.num_seq_incorp_bem_pat = 0 */
           and sdo_bem_pat.cod_cenar_ctbl         = "GERENBVI"
           and sdo_bem_pat.cod_finalid_econ       = "Corrente" no-error.
    if  not avail sdo_bem_pat then next.

    find first bf-sdo_bem_pat no-lock use-index sdbmpt_id
         where bf-sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
          /* and bf-sdo_bem_pat.num_seq_incorp_bem_pat = 0 */
           and bf-sdo_bem_pat.cod_cenar_ctbl         = "GERENBVI"
           and bf-sdo_bem_pat.cod_finalid_econ       = "Dolar" no-error.
    if  not avail bf-sdo_bem_pat then next.

    find first licenc_uso of bem_pat no-lock no-error.
    if avail licenc_uso then
        assign c-desc-licenc = licenc_uso.des_restric_uso
               c-desc-licenc = replace(c-desc-licenc,chr(13),' ')
               c-desc-licenc = replace(c-desc-licenc,chr(10),' ').

    if rs-padrao:input-value in frame f-pg-par = 2 then
        put bem_pat.cod_empresa         ";"
            bem_pat.cod_cta_pat         ";"
            cta_pat.des_cta_pat         ";"
            bem_pat.num_bem_pat         ";"
            bem_pat.num_seq_bem_pat     ";"
            bem_pat.des_bem_pat         ";"
            bem_pat.dat_aquis_bem_pat   ";"
            bem_pat.cod_ccusto_resp     ";"
            sdo_bem_pat.val_original    ";"
            bf-sdo_bem_pat.val_original ";"
            bem_pat.cod_licenc_uso 
            skip.
    else
        disp bem_pat.cod_empresa
             bem_pat.cod_cta_pat
             cta_pat.des_cta_pat
             bem_pat.num_bem_pat
             bem_pat.num_seq_bem_pat
             bem_pat.des_bem_pat
             bem_pat.dat_aquis_bem_pat
             bem_pat.cod_ccusto_resp
             sdo_bem_pat.val_original
             bf-sdo_bem_pat.val_original
             bem_pat.cod_licenc_uso
             with stream-io width 300.
end.

output close.

if rs-destino:input-value in frame f-pg-par = 1 then
    run winexec (input os-getenv("windir") + "\notepad.exe" + chr(32) + c-arquivo-entrada, input 1).
else
    message 'Arquivo Gerado com Sucesso no Diret¢rio ' c-arquivo-entrada
        view-as alert-box info buttons ok .

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

