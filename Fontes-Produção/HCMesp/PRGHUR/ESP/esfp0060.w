&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFP0060 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VAR c-arq-digita AS CHARACTER NO-UNDO.
DEFINE VAR l-ok AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD log_exclui        AS LOGICAL
    FIELD cdn_sit_afast_orig LIKE sit_afast.cdn_sit_afast_func
    FIELD des_sit_afast_orig LIKE sit_afast.des_sit_afast_func
    FIELD cdn_sit_afast_dest LIKE sit_afast.cdn_sit_afast_func
    FIELD des_sit_afast_dest LIKE sit_afast.des_sit_afast_func.

DEFINE VARIABLE v_han_acomp AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.log_exclui tt-digita.cdn_sit_afast_orig tt-digita.des_sit_afast_orig tt-digita.cdn_sit_afast_dest tt-digita.des_sit_afast_dest   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.cdn_sit_afast_orig tt-digita.cdn_sit_afast_dest   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&SELF-NAME} FOR EACH tt-digita NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-executar bt-recuperar br-digita ~
bt-inserir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-executar 
     IMAGE-UP FILE "image/im-import.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.25.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-livre _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.log_exclui  FORMAT 'Sim/NÆo'
tt-digita.cdn_sit_afast_orig COLUMN-LABEL 'Sit Orig'
tt-digita.des_sit_afast_orig
tt-digita.cdn_sit_afast_dest COLUMN-LABEL 'Sit Dest'
tt-digita.des_sit_afast_dest
ENABLE
    tt-digita.cdn_sit_afast_orig
tt-digita.cdn_sit_afast_dest
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 9.75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-executar AT ROW 1.08 COL 1.43 WIDGET-ID 2
     bt-salvar AT ROW 1.25 COL 42.72 WIDGET-ID 10
     bt-recuperar AT ROW 1.25 COL 57.72 WIDGET-ID 8
     br-digita AT ROW 2.5 COL 1 WIDGET-ID 200
     bt-inserir AT ROW 12.25 COL 1 WIDGET-ID 14
     bt-alterar AT ROW 12.25 COL 16 WIDGET-ID 12
     bt-retirar AT ROW 12.25 COL 31 WIDGET-ID 16
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 12.29 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 12.38
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-digita bt-recuperar f-cad */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-digita NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-livre
ON MOUSE-SELECT-DBLCLICK OF br-digita IN FRAME f-cad
DO:
    IF AVAIL tt-digita THEN
        ASSIGN tt-digita.log_exclui:SCREEN-VALUE IN BROWSE br-digita = string(NOT tt-digita.log_exclui)
        INPUT BROWSE br-digita tt-digita.log_exclui.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-livre
ON ROW-ENTRY OF br-digita IN FRAME f-cad
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
   if  br-digita:new-row in frame {&FRAME-NAME} then do:
       assign tt-digita.log_exclui:screen-value in browse br-digita = 'No'.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-livre
ON ROW-LEAVE OF br-digita IN FRAME f-cad
DO:
    if br-digita:NEW-ROW in frame {&FRAME-NAME} then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.log_exclui
               input browse br-digita tt-digita.cdn_sit_afast_orig
               input browse br-digita tt-digita.cdn_sit_afast_dest.

        br-digita:CREATE-RESULT-LIST-ENTRY() in frame {&FRAME-NAME}.

    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.log_exclui
               input browse br-digita tt-digita.cdn_sit_afast_orig
               input browse br-digita tt-digita.cdn_sit_afast_dest.
        
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-livre
ON CHOOSE OF bt-alterar IN FRAME f-cad /* Alterar */
DO:
   apply 'entry' to tt-digita.cdn_Sit_afast_orig in browse br-digita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Button 1 */
DO:

    FOR EACH tt-digita NO-LOCK:

        IF NOT CAN-FIND(FIRST sit_afast NO-LOCK WHERE
                              sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig) THEN DO:

            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 56,
                              INPUT 'Situa‡Æo de afastamento').
            REPOSITION br-digita TO ROWID(rowid(tt-digita)).
            APPLY 'ENTRY':U TO tt-digita.cdn_sit_afast_orig IN BROWSE br-digita.
            RETURN NO-APPLY.

        END.

        IF NOT CAN-FIND(FIRST sit_afast NO-LOCK WHERE
                              sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest) THEN DO:

            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 56,
                              INPUT 'Situa‡Æo de afastamento').
            REPOSITION br-digita TO ROWID(rowid(tt-digita)).
            APPLY 'ENTRY':U TO tt-digita.cdn_sit_afast_dest IN BROWSE br-digita.
            RETURN NO-APPLY.

        END.

        RUN pi-de-para.
    
        IF tt-digita.LOG_exclui THEN
            RUN pi-delete.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-livre
ON CHOOSE OF bt-inserir IN FRAME f-cad /* Inserir */
DO:
    assign bt-alterar:SENSITIVE in frame {&FRAME-NAME} = YES
           bt-retirar:SENSITIVE in frame {&FRAME-NAME} = yes
           bt-salvar:SENSITIVE in frame {&FRAME-NAME}  = yes.
    
    if num-results("br-digita") > 0 then
        br-digita:INSERT-ROW("after") in frame {&FRAME-NAME}.
    else do transaction:
        create tt-digita.
        ASSIGN tt-digita.log_exclui = FALSE.
        
        open query br-digita for each tt-digita.
        
        apply "entry" to tt-digita.cdn_sit_afast_orig in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-livre
ON CHOOSE OF bt-recuperar IN FRAME f-cad /* Recuperar */
DO:

SYSTEM-DIALOG GET-FILE c-arq-digita
   FILTERS "*.dig" "*.dig",
           "*.*" "*.*"
   DEFAULT-EXTENSION "*.dig"
   MUST-EXIST
   USE-FILENAME
   UPDATE l-ok.
if l-ok then do:
    for each tt-digita:
        delete tt-digita.
    end.
    input from value(c-arq-digita) no-echo.
    repeat:             
        create tt-digita.
        import tt-digita.
    end.    
    input close. 
    
    delete tt-digita.
    
    open query br-digita for each tt-digita.
    
    if num-results("br-digita":U) > 0 then 
        assign bt-alterar:SENSITIVE in frame {&FRAME-NAME} = YES
               bt-retirar:SENSITIVE in frame {&FRAME-NAME} = yes
               bt-salvar:SENSITIVE in frame {&FRAME-NAME}  = yes.
    else
        assign bt-alterar:SENSITIVE in frame {&FRAME-NAME} = no
               bt-retirar:SENSITIVE in frame {&FRAME-NAME} = no
               bt-salvar:SENSITIVE in frame {&FRAME-NAME}  = no.
end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-livre
ON CHOOSE OF bt-retirar IN FRAME f-cad /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame {&FRAME-NAME} then.
    end.
    
    if num-results("br-digita") = 0 then
        assign bt-alterar:SENSITIVE in frame {&FRAME-NAME} = no
               bt-retirar:SENSITIVE in frame {&FRAME-NAME} = no
               bt-salvar:SENSITIVE in frame {&FRAME-NAME}  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-livre
ON CHOOSE OF bt-salvar IN FRAME f-cad /* Salvar */
DO:
/*****************************************************************
**
** I-RPSVD.I - Bot’o Salvar Digita»’o             
**
*****************************************************************/

define var r-tt-digita as rowid no-undo.



SYSTEM-DIALOG GET-FILE c-arq-digita
   FILTERS "*.dig" "*.dig",
           "*.*" "*.*"
   ASK-OVERWRITE 
   DEFAULT-EXTENSION "*.dig"
   SAVE-AS             
   CREATE-TEST-FILE
   USE-FILENAME
   UPDATE l-ok.

if avail tt-digita then assign r-tt-digita = rowid(tt-digita).

if l-ok then do:
    output to value(c-arq-digita).
    for each tt-digita:
        export tt-digita.
    end.
    output close. 
    
    reposition br-digita to rowid(r-tt-digita) no-error.
end.

/* i-rpsvd.i */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

ON LEAVE OF tt-digita.cdn_sit_afast_orig IN BROWSE br-digita DO:

    FIND FIRST sit_afast NO-LOCK WHERE
               sit_afast.cdn_sit_afast_func = INPUT BROWSE br-digita tt-digita.cdn_sit_afast_orig NO-ERROR.
    IF AVAIL sit_afast THEN
        ASSIGN tt-digita.des_sit_afast_orig:SCREEN-VALUE IN BROWSE br-digita = sit_afast.des_sit_afast_func.
    ELSE
        ASSIGN tt-digita.des_sit_afast_orig:SCREEN-VALUE IN BROWSE br-digita = ''.

END.

ON LEAVE OF tt-digita.cdn_sit_afast_dest IN BROWSE br-digita DO:

    FIND FIRST sit_afast NO-LOCK WHERE
               sit_afast.cdn_sit_afast_func = INPUT BROWSE br-digita tt-digita.cdn_sit_afast_dest NO-ERROR.
    IF AVAIL sit_afast THEN
        ASSIGN tt-digita.des_sit_afast_dest:SCREEN-VALUE IN BROWSE br-digita = sit_afast.des_sit_afast_func.
    ELSE
        ASSIGN tt-digita.des_sit_afast_dest:SCREEN-VALUE IN BROWSE br-digita = ''.


END.

ON F5, MOUSE-SELECT-DBLCLICK OF tt-digita.cdn_sit_afast_orig IN BROWSE br-digita DO:

    IF AVAIL tt-digita THEN DO:

        {include/zoomvar.i &prog-zoom=object/sopy/zoom/z01py169.w
                           &campo=tt-digita.cdn_sit_afast_orig
                           &campozoom=cdn_sit_afast_func
                           &campo2=tt-digita.des_sit_afast_orig
                           &campozoom2=des_sit_afast_func                       
                           &BROWSE=br-digita}
                           
    END.

END.

ON F5, MOUSE-SELECT-DBLCLICK OF tt-digita.cdn_sit_afast_dest IN BROWSE br-digita DO:

    IF AVAIL tt-digita THEN DO:

        {include/zoomvar.i &prog-zoom=object/sopy/zoom/z01py169.w
                           &campo=tt-digita.cdn_sit_afast_dest
                           &campozoom=cdn_sit_afast_func
                           &campo2=tt-digita.des_sit_afast_dest
                           &campozoom2=des_sit_afast_func                       
                           &BROWSE=br-digita}
                           
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-executar:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-executar bt-recuperar br-digita bt-inserir 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESFP0060" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-de-para w-livre 
PROCEDURE pi-de-para :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN utp/ut-acomp.p PERSISTENT SET v_han_acomp.
    run pi-inicializar in v_han_acomp('Processando').

    disable triggers for load of afast_pacien.

    RUN pi-acompanhar IN v_han_acomp('afast_pacien').
    
    for each afast_pacien exclusive-lock where
             afast_pacien.cdn_sit_afast = tt-digita.cdn_sit_afast_orig:
    
        assign afast_pacien.cdn_sit_afast = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('benefic_calc_sit').

    disable triggers for load of benefic_calc_sit.
    
    for each benefic_calc_sit exclusive-lock where
             benefic_calc_sit.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign benefic_calc_sit.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('conver_carg_turno_trab').
    
    disable triggers for load of conver_carg_turno_trab.
    
    for each conver_carg_turno_trab exclusive-lock where
             conver_carg_turno_trab.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign conver_carg_turno_trab.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('efp_par_marcac_ptoelet').
    
    disable triggers for load of efp_par_marcac_ptoelet.
    
    for each efp_par_marcac_ptoelet exclusive-lock where
             efp_par_marcac_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign efp_par_marcac_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('entrev_desligto_func').
    
    disable triggers for load of entrev_desligto_func.
    
    for each entrev_desligto_func exclusive-lock where
             entrev_desligto_func.cdn_sit_afast = tt-digita.cdn_sit_afast_orig:
    
        assign entrev_desligto_func.cdn_sit_afast = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('entrev_desligto_func').
    
    disable triggers for load of entrev_desligto_func.
    
    for each entrev_desligto_func exclusive-lock where
             entrev_desligto_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign entrev_desligto_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('estabil_func').
    
    disable triggers for load of estabil_func.
    
    for each estabil_func exclusive-lock where
             estabil_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign estabil_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('excec_func_afastdo_ppr').
    
    disable triggers for load of excec_func_afastdo_ppr.
    
    for each excec_func_afastdo_ppr exclusive-lock where
             excec_func_afastdo_ppr.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign excec_func_afastdo_ppr.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('habilit_rescis').
    
    disable triggers for load of habilit_rescis.
    
    for each habilit_rescis exclusive-lock where
             habilit_rescis.cdn_sit_rescis_contratual = tt-digita.cdn_sit_afast_orig:
    
        assign habilit_rescis.cdn_sit_rescis_contratual = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('integr_pend_dados_rescis').
    
    disable triggers for load of integr_pend_dados_rescis.
    
    for each integr_pend_dados_rescis exclusive-lock where
             integr_pend_dados_rescis.cdn_sit_rescis_contratual = tt-digita.cdn_sit_afast_orig:
    
        assign integr_pend_dados_rescis.cdn_sit_rescis_contratual = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('integr_pend_dados_sit').
    
    disable triggers for load of integr_pend_dados_sit.
    
    for each integr_pend_dados_sit exclusive-lock where
             integr_pend_dados_sit.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign integr_pend_dados_sit.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('movimen_quadro_desligto').
    
    disable triggers for load of movimen_quadro_desligto.
    
    for each movimen_quadro_desligto exclusive-lock where
             movimen_quadro_desligto.cdn_sit_afast = tt-digita.cdn_sit_afast_orig:
    
        assign movimen_quadro_desligto.cdn_sit_afast = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('movimen_quadro_pessoal_afast').
    
    disable triggers for load of movimen_quadro_pessoal_afast.
    
    for each movimen_quadro_pessoal_afast exclusive-lock where
             movimen_quadro_pessoal_afast.cdn_sit_afast = tt-digita.cdn_sit_afast_orig:
    
        assign movimen_quadro_pessoal_afast.cdn_sit_afast = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('movimen_quadro_pessoal_trans').
    
    disable triggers for load of movimen_quadro_pessoal_trans.
    
    for each movimen_quadro_pessoal_trans exclusive-lock where
             movimen_quadro_pessoal_trans.cdn_sit_afast = tt-digita.cdn_sit_afast_orig:
    
        assign movimen_quadro_pessoal_trans.cdn_sit_afast = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('par_marcac_ptoelet').
    
    disable triggers for load of par_marcac_ptoelet.
    
    for each par_marcac_ptoelet exclusive-lock where
             par_marcac_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign par_marcac_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('ptoelet_par_marcac').
    
    disable triggers for load of ptoelet_par_marcac.
    
    for each ptoelet_par_marcac exclusive-lock where
             ptoelet_par_marcac.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign ptoelet_par_marcac.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('serv_sit_afast').
    
    disable triggers for load of serv_sit_afast.
    
    for each serv_sit_afast exclusive-lock where
             serv_sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign serv_sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sit_afast_control_aces').
    
    disable triggers for load of sit_afast_control_aces.
    
    for each sit_afast_control_aces exclusive-lock where
             sit_afast_control_aces.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_afast_control_aces.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sit_afast_func').
    
    disable triggers for load of sit_afast_func.
    
    for each sit_afast_func exclusive-lock where
             sit_afast_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_afast_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sit_afast_medic').
    
    disable triggers for load of sit_afast_medic.
    
    for each sit_afast_medic exclusive-lock where
             sit_afast_medic.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_afast_medic.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sit_afast_motiv').
    
    disable triggers for load of sit_afast_motiv.
    
    for each sit_afast_motiv exclusive-lock where
             sit_afast_motiv.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_afast_motiv.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.
    
    RUN pi-acompanhar IN v_han_acomp('sit_benefic').

    disable triggers for load of sit_benefic.
    
    for each sit_benefic exclusive-lock where
             sit_benefic.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_benefic.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sit_espcif_ptoelet').
    
    disable triggers for load of sit_espcif_ptoelet.
    
    for each sit_espcif_ptoelet exclusive-lock where
             sit_espcif_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sit_espcif_ptoelet.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-acompanhar IN v_han_acomp('sped_sit_afast_func').
    
    disable triggers for load of sped_sit_afast_func.
    
    for each sped_sit_afast_func exclusive-lock where
             sped_sit_afast_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sped_sit_afast_func.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.
    
    RUN pi-acompanhar IN v_han_acomp('turma_trein').

    disable triggers for load of turma_trein.
    
    for each turma_trein exclusive-lock where
             turma_trein.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign turma_trein.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.
    
    RUN pi-acompanhar IN v_han_acomp('sped_sit_afast').

    disable triggers for load of sped_sit_afast.
    
    for each sped_sit_afast exclusive-lock where
             sped_sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:
    
        assign sped_sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_dest.
    
    end.

    RUN pi-finalizar IN v_han_acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-delete w-livre 
PROCEDURE pi-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH sit_afast EXCLUSIVE-LOCK WHERE
             sit_afast.cdn_sit_afast_func = tt-digita.cdn_sit_afast_orig:

        DELETE sit_afast.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

