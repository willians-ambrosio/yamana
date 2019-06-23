&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999B 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
 def input        parameter v-row-parent as rowid         no-undo. 
 

/* Local Variable Definitions ---                                       */


/*** Variaveis usadas internamente pelo estilo, favor nao elimina-las   */

/*** Fim das variaveis utilizadas no estilo */

DEF TEMP-TABLE ttRegistro
    FIELD cod_produt_financ LIKE es_produt_fin_adm.cod_produt_financ
    FIELD cod_admdra_apf    LIKE es_produt_fin_adm.cod_admdra_apf 
    FIELD descricao         LIKE admdra_apf.nom_razao_social     
  INDEX idx0  IS PRIMARY UNIQUE cod_produt_financ cod_admdra_apf.

DEF BUFFER bfttRegistro FOR ttRegistro.

DEF BUFFER bf_es_produt_financ FOR es_produt_financ.

DEFINE VARIABLE icont AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-forma
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES admdra_apf ttRegistro

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 admdra_apf.cod_admdra_apf ~
admdra_apf.nom_razao_social 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH admdra_apf NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH admdra_apf NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 admdra_apf
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 admdra_apf


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 ttRegistro.cod_adm ttRegistro.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH ttRegistro
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH ttRegistro.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 ttRegistro
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 ttRegistro


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button BROWSE-2 BROWSE-3 bt-add bt-addall ~
bt-backall bt-back bt-ajuda bt-ok bt-save bt-cancela 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "prgfin/image/add_button.png":U
     LABEL "Button 2" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-addall 
     IMAGE-UP FILE "prgfin/image/add_all_button.png":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-back 
     IMAGE-UP FILE "prgfin/image/del_button.png":U
     LABEL "Button 5" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-backall 
     IMAGE-UP FILE "prgfin/image/del_all_button.png":U
     LABEL "Button 4" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-save AUTO-GO 
     LABEL "&Salvar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 71 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      admdra_apf SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      ttRegistro SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-cadsim _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      admdra_apf.cod_admdra_apf FORMAT "x(5)":U
      admdra_apf.nom_razao_social FORMAT "x(60)":U WIDTH 12.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 30 BY 10.5 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 w-cadsim _FREEFORM
  QUERY BROWSE-3 DISPLAY
      ttRegistro.cod_adm
 ttRegistro.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 30 BY 10.5 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BROWSE-2 AT ROW 1.5 COL 3 HELP
          "Use o CTRL para selecionar mais de uma linha." WIDGET-ID 200
     BROWSE-3 AT ROW 1.5 COL 41 WIDGET-ID 300
     bt-add AT ROW 3.33 COL 35 WIDGET-ID 2
     bt-addall AT ROW 4.54 COL 35 WIDGET-ID 4
     bt-backall AT ROW 5.71 COL 35 WIDGET-ID 6
     bt-back AT ROW 6.88 COL 35 WIDGET-ID 8
     bt-ajuda AT ROW 13.46 COL 60.14
     bt-ok AT ROW 13.5 COL 2 HELP
          "Salva e sai"
     bt-save AT ROW 13.5 COL 12.43 HELP
          "Salva e cria novo"
     bt-cancela AT ROW 13.5 COL 23 HELP
          "Cancela"
     rt-button AT ROW 13.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.29 BY 13.83 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-forma
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Inclui/Modifica - Programa Conflito"
         HEIGHT             = 13.83
         WIDTH              = 71.29
         MAX-HEIGHT         = 28
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-incsim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   L-To-R                                                               */
/* BROWSE-TAB BROWSE-2 rt-button f-cad */
/* BROWSE-TAB BROWSE-3 BROWSE-2 f-cad */
ASSIGN 
       bt-ajuda:HIDDEN IN FRAME f-cad           = TRUE
       bt-ajuda:POPUP-MENU IN FRAME f-cad       = MENU POPUP-MENU-bt-ajuda:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "mgesp.admdra_apf"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = mgesp.admdra_apf.cod_admdra_apf
     _FldNameList[2]   > mgesp.admdra_apf.nom_razao_social
"admdra_apf.nom_razao_social" ? ? "character" ? ? ? ? ? ? no ? no no "12.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttRegistro.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-cad
/* Query rebuild information for FRAME f-cad
     _Query            is NOT OPENED
*/  /* FRAME f-cad */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Inclui/Modifica - Programa Conflito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Inclui/Modifica - Programa Conflito */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-cadsim
ON CHOOSE OF bt-add IN FRAME f-cad /* Button 2 */
DO:
    GET CURRENT browse-2.

    RUN pi-registro (INPUT admdra_apf.cod_admdra_apf).

    OPEN QUERY browse-3 FOR EACH ttRegistro.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-addall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-addall w-cadsim
ON CHOOSE OF bt-addall IN FRAME f-cad /* Button 3 */
DO:
  
   DO icont = 1 TO browse-2:NUM-SELECTED-ROWS:

       browse-2:FETCH-SELECTED-ROW(icont).

       RUN pi-registro (INPUT admdra_apf.cod_admdra_apf).

   END.

   OPEN QUERY browse-3 FOR EACH ttRegistro.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-cadsim
ON CHOOSE OF bt-ajuda IN FRAME f-cad /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-back w-cadsim
ON CHOOSE OF bt-back IN FRAME f-cad /* Button 5 */
DO:
    GET CURRENT browse-3.

    RUN pi-exclui (INPUT ROWID(ttRegistro)).

    OPEN QUERY browse-3 FOR EACH ttRegistro.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-backall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-backall w-cadsim
ON CHOOSE OF bt-backall IN FRAME f-cad /* Button 4 */
DO:
  DO icont = 1 TO browse-3:NUM-SELECTED-ROWS:

       browse-3:FETCH-SELECTED-ROW(icont).

       RUN pi-exclui (INPUT ROWID(ttRegistro)).

   END.

   OPEN QUERY browse-3 FOR EACH ttRegistro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-cadsim
ON CHOOSE OF bt-cancela IN FRAME f-cad /* Cancelar */
DO:
{include/cancefil.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-cadsim
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
  RUN pi-salvar.

  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-save w-cadsim
ON CHOOSE OF bt-save IN FRAME f-cad /* Salvar */
DO:
  RUN pi-salvar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsim  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
  THEN DELETE WIDGET w-cadsim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsim  _DEFAULT-ENABLE
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
  ENABLE rt-button BROWSE-2 BROWSE-3 bt-add bt-addall bt-backall bt-back 
         bt-ajuda bt-ok bt-save bt-cancela 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "ESMEN012B" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN pi-carrega-dados.

 
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados w-cadsim 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttRegistro.
    
    FIND bf_es_produt_financ NO-LOCK WHERE 
         ROWID(bf_es_produt_financ) = v-row-parent NO-ERROR.
    IF NOT AVAIL bf_es_produt_financ THEN RETURN.
    
    FOR EACH es_produt_fin_adm NO-LOCK WHERE 
             es_produt_fin_adm.cod_produt_financ = bf_es_produt_financ.cod_produt_financ:
    
        FIND admdra_apf NO-LOCK WHERE 
             admdra_apf.cod_admdra_apf = es_produt_fin_adm.cod_admdra_apf NO-ERROR.
    
        CREATE ttRegistro.
        BUFFER-COPY es_produt_fin_adm TO ttRegistro
            ASSIGN ttRegistro.descricao = admdra_apf.nom_razao_social WHEN AVAIL admdra_apf.
    
    END.
    
    {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exclui w-cadsim 
PROCEDURE pi-exclui :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipr-rowtt AS ROWID       NO-UNDO.

FIND bfttRegistro WHERE
     ROWID(bfttRegistro) = ipr-rowtt NO-ERROR.
IF AVAIL bfttRegistro
     THEN DELETE bfttRegistro.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-registro w-cadsim 
PROCEDURE pi-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipc_adm LIKE admdra_apf.cod_admdra_apf NO-UNDO.

FIND ttRegistro 
    WHERE ttRegistro.cod_produt_financ = bf_es_produt_financ.cod_produt_financ
      AND ttRegistro.cod_admdra_apf    = ipc_adm NO-ERROR.
IF NOT AVAIL ttRegistro THEN DO:

    CREATE ttRegistro.
    ASSIGN ttRegistro.cod_produt_financ = bf_es_produt_financ.cod_produt_financ   
           ttRegistro.cod_admdra_apf    = ipc_adm
           ttRegistro.descricao         = admdra_apf.nom_razao_social.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salvar w-cadsim 
PROCEDURE pi-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttRegistro:

     FIND es_produt_fin_adm NO-LOCK 
         WHERE es_produt_fin_adm.cod_produt_financ = ttRegistro.cod_produt_financ
           AND es_produt_fin_adm.cod_admdra_apf    = ttRegistro.cod_admdra_apf NO-ERROR.
     IF AVAIL es_produt_fin_adm THEN NEXT.
    
     CREATE es_produt_fin_adm.
     BUFFER-COPY ttRegistro TO es_produt_fin_adm.
END.

FOR EACH es_produt_fin_adm WHERE 
         es_produt_fin_adm.cod_produt_financ = bf_es_produt_financ.cod_produt_financ:
    
    FIND ttRegistro 
    WHERE ttRegistro.cod_produt_financ = es_produt_fin_adm.cod_produt_financ
      AND ttRegistro.cod_admdra_apf    = es_produt_fin_adm.cod_admdra_apf NO-ERROR.
    IF NOT AVAIL ttRegistro
         THEN DELETE es_produt_fin_adm.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

