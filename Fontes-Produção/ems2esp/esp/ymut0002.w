&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMUT0002 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-emitente LIKE emitente
       FIELDS r-rowid AS ROWID
       FIELDS selecionado AS CHARACTER FORMAT "x".

DEFINE VARIABLE h-acomp AS HANDLE  NO-UNDO.
DEFINE VARIABLE i-cont  AS INTEGER NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-ini LIKE emitente.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-fim LIKE emitente.cod-emitente NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE gr-emitente AS ROWID NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE l-cliente          AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-fornecedor       AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-ambos            AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-pessoa-fisica    AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-pessoa-juridica  AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-estrangeiro      AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-trading          AS LOGICAL NO-UNDO.




{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-emitente

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-emitente.selecionado tt-emitente.cod-emitente tt-emitente.nome-abrev ENTRY (tt-emitente.identif , "Cliente,Fornecedor,Ambos") ENTRY (tt-emitente.natureza , "Pessoa Fisica,Pessoa Juridica,Estrangeiro,Trading") ENTRY (tt-emitente.idi-tributac-pis , "Tributado,Isento,Outros") ENTRY (tt-emitente.idi-tributac-cofins , "Tributado,Isento,Outros")   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-emitente
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-emitente.
&Scoped-define TABLES-IN-QUERY-br-table tt-emitente
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-emitente


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button rt-button-2 br-table bt-preenche ~
bt-limpa bt-preenche-todos bt-limpa-todos bt-filtro bt-confirma 

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
       SUB-MENU  mi-programa    LABEL "&YMUT0002"     
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/im-ok.bmp":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.29 TOOLTIP "Atualiza".

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Filtro" 
     SIZE 5.14 BY 1.29 TOOLTIP "Filtro".

DEFINE BUTTON bt-limpa 
     IMAGE-UP FILE "image/ii-fillin.bmp":U
     LABEL "Limpa" 
     SIZE 5.14 BY 1.29 TOOLTIP "Desmarca o emitente".

DEFINE BUTTON bt-limpa-todos 
     IMAGE-UP FILE "image/ii-nenhum.bmp":U
     LABEL "Limpa Todos" 
     SIZE 5.14 BY 1.29 TOOLTIP "Desmarca todos os emitentes".

DEFINE BUTTON bt-preenche 
     IMAGE-UP FILE "image/im-fillin.bmp":U
     LABEL "Preenche" 
     SIZE 5.14 BY 1.29 TOOLTIP "Marca o emitente".

DEFINE BUTTON bt-preenche-todos 
     IMAGE-UP FILE "image/im-todos.bmp":U
     LABEL "Preenche Todos" 
     SIZE 5.14 BY 1.29 TOOLTIP "Marca todos os emitentes".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE RECTANGLE rt-button-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 11.75
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-emitente SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table w-livre _FREEFORM
  QUERY br-table DISPLAY
      tt-emitente.selecionado COLUMN-LABEL "*" WIDTH 1
      tt-emitente.cod-emitente     

 tt-emitente.nome-abrev            
 ENTRY (tt-emitente.identif             , "Cliente,Fornecedor,Ambos")    FORMAT "x(15)" COLUMN-LABEL "Identif"  WIDTH 10 
 ENTRY (tt-emitente.natureza            , "Pessoa Fisica,Pessoa Juridica,Estrangeiro,Trading")   FORMAT "x(15)"      COLUMN-LABEL "Natureza" WIDTH 15
 ENTRY (tt-emitente.idi-tributac-pis    , "Tributado,Isento,Outros")     FORMAT "x(15)"   COLUMN-LABEL "Tributaá∆o PIS"     WIDTH 15
 ENTRY (tt-emitente.idi-tributac-cofins , "Tributado,Isento,Outros")     FORMAT "x(15)"   COLUMN-LABEL "Tributaá∆o COFINS"  WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81.29 BY 10.25 FIT-LAST-COLUMN TOOLTIP "Duplo clique para abrir o programa de consulta".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     br-table AT ROW 3.5 COL 2.72 WIDGET-ID 200
     bt-preenche AT ROW 3.5 COL 85 WIDGET-ID 4
     bt-limpa AT ROW 5 COL 85 WIDGET-ID 6
     bt-preenche-todos AT ROW 6.5 COL 85 WIDGET-ID 12
     bt-limpa-todos AT ROW 8 COL 85 WIDGET-ID 10
     bt-filtro AT ROW 9.5 COL 85 WIDGET-ID 16
     bt-confirma AT ROW 12.5 COL 85 WIDGET-ID 18
     rt-button AT ROW 1 COL 1
     rt-button-2 AT ROW 2.75 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o de Tributaá∆o PIS/COFINS emitentes"
         HEIGHT             = 13.83
         WIDTH              = 90.29
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90.29
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90.29
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
/* BROWSE-TAB br-table rt-button-2 f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-emitente.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Manutená∆o de Tributaá∆o PIS/COFINS emitentes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Manutená∆o de Tributaá∆o PIS/COFINS emitentes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table w-livre
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME f-cad
DO:
  ASSIGN gr-emitente = ?.

  IF AVAILABLE(tt-emitente) THEN DO:
      FIND emitente
           WHERE ROWID(emitente) = tt-emitente.r-rowid
           NO-LOCK NO-ERROR.
      IF AVAILABLE(emitente) THEN DO:
         ASSIGN gr-emitente =  ROWID(emitente).

         IF emitente.identif <> 1 THEN
             RUN cdp/cd0401.w.
         ELSE
             RUN cdp/cd0704.w.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-livre
ON CHOOSE OF bt-confirma IN FRAME f-cad /* Button 1 */
DO:
   RUN utp/ut-msgs.p (INPUT "show",
                      INPUT 27100,
                      INPUT "A T E N Ä « O !~~" + 
                            "Os dados ser∆o gravados e n∆o poder∆o ser alterados" + CHR(13) + CHR(13) + 
                            "Confirma?").

   IF RETURN-VALUE = "yes" THEN DO:
      RUN pi-atualiza.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-livre
ON CHOOSE OF bt-filtro IN FRAME f-cad /* Filtro */
DO:
  RUN esp/ymut0002a.w.

  EMPTY TEMP-TABLE tt-emitente.

  RUN pi-gera-tabela.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-limpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpa w-livre
ON CHOOSE OF bt-limpa IN FRAME f-cad /* Limpa */
DO:
  ASSIGN tt-emitente.selecionado = "".

  ASSIGN tt-emitente.selecionado:SCREEN-VALUE IN BROWSE BR-TABLE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-limpa-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpa-todos w-livre
ON CHOOSE OF bt-limpa-todos IN FRAME f-cad /* Limpa Todos */
DO:
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Atualizando Browse").

  ASSIGN i-cont = 0.

  FOR EACH tt-emitente
           EXCLUSIVE-LOCK:
     ASSIGN i-cont = i-cont + 1.

     RUN pi-acompanhar IN h-acomp (INPUT "Desmarcando..." + STRING(i-cont)).
          
     ASSIGN tt-emitente.selecionado = "".
  END.

  RUN pi-finalizar IN h-acomp.

  {&OPEN-BROWSERS-IN-QUERY-f-cad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-preenche
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-preenche w-livre
ON CHOOSE OF bt-preenche IN FRAME f-cad /* Preenche */
DO:
  ASSIGN tt-emitente.selecionado = "*".

  ASSIGN tt-emitente.selecionado:SCREEN-VALUE IN BROWSE BR-TABLE = "*".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-preenche-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-preenche-todos w-livre
ON CHOOSE OF bt-preenche-todos IN FRAME f-cad /* Preenche Todos */
DO:
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Atualizando Browse").

  ASSIGN i-cont = 0.

  FOR EACH tt-emitente
           EXCLUSIVE-LOCK:
     ASSIGN i-cont = i-cont + 1.

     RUN pi-acompanhar IN h-acomp (INPUT "Marcando..." + STRING(i-cont)).
          
     ASSIGN tt-emitente.selecionado = "*".
  END.

  RUN pi-finalizar IN h-acomp.

  {&OPEN-BROWSERS-IN-QUERY-f-cad}
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
ON MENU-DROP OF MENU mi-programa /* YMUT0002 */
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
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             br-table:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  ENABLE rt-button rt-button-2 br-table bt-preenche bt-limpa bt-preenche-todos 
         bt-limpa-todos bt-filtro bt-confirma 
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

  {utp/ut9000.i "YMUT0002" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

 
  ASSIGN cod-emitente-ini = 0
         cod-emitente-fim = 999999999.


  ASSIGN l-cliente         = NO
         l-fornecedor      = YES
         l-ambos           = YES
         l-pessoa-fisica   = YES
         l-pessoa-juridica = YES
         l-estrangeiro     = YES
         l-trading         = NO.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pi-gera-tabela. 


  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza w-livre 
PROCEDURE pi-atualiza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE c-chave AS CHARACTER NO-UNDO FORMAT "x(100)".

   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   RUN pi-inicializar IN h-acomp (INPUT "Atualizando Browse").

   ASSIGN i-cont = 0.

   blk:
   FOR EACH tt-emitente
            WHERE tt-emitente.selecionado = "*"
            EXCLUSIVE-LOCK:
      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando dados..." + STRING(i-cont)).

      FIND emitente 
           WHERE ROWID(emitente) = tt-emitente.r-rowid
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(emitente) THEN DO:
         CASE emitente.natureza:
              WHEN 1 THEN ASSIGN emitente.idi-tributac-pis    = 2
                                 emitente.idi-tributac-cofins = 2.
              WHEN 2 THEN ASSIGN emitente.idi-tributac-pis    = 1
                                 emitente.idi-tributac-cofins = 1.
              WHEN 3 THEN ASSIGN emitente.idi-tributac-pis    = 2
                                 emitente.idi-tributac-cofins = 2.
              WHEN 4 THEN ASSIGN emitente.idi-tributac-pis    = 2
                                 emitente.idi-tributac-cofins = 2.
         END CASE.

         DELETE tt-emitente.
      END.
   END.

   {&OPEN-BROWSERS-IN-QUERY-f-cad}

   RUN pi-finalizar IN h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-tabela w-livre 
PROCEDURE pi-gera-tabela :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   RUN pi-inicializar IN h-acomp (INPUT "Atualizando Browse").

   ASSIGN i-cont = 0.

   EMPTY TEMP-TABLE tt-emitente.

   blk:
   FOR EACH emitente
            WHERE emitente.cod-emitente >= cod-emitente-ini AND 
                  emitente.cod-emitente <= cod-emitente-fim
            NO-LOCK:

      CASE emitente.identif:
           WHEN 1 THEN DO:
              IF l-cliente = NO THEN NEXT blk.
           END.
           WHEN 2 THEN DO:
              IF l-fornecedor = NO THEN NEXT blk.
           END.
           WHEN 3 THEN DO:
              IF l-ambos = NO THEN NEXT blk.
           END.
      END CASE.

      CASE emitente.natureza:
           WHEN 1 THEN DO:
              IF l-pessoa-fisica = NO THEN NEXT blk.
           END.
           WHEN 2 THEN DO:
              IF l-pessoa-juridica = NO THEN NEXT blk.
           END.
           WHEN 3 THEN DO:
              IF l-estrangeiro = NO THEN NEXT blk.
           END.
           WHEN 4 THEN DO:
              IF l-trading = NO THEN NEXT blk.
           END.
      END CASE.

      CASE emitente.natureza:
           WHEN 1 OR WHEN 3 OR WHEN 4 THEN DO:
              IF (emitente.idi-tributac-pis    = 2 AND emitente.idi-tributac-cofins = 2) OR
                 (emitente.idi-tributac-pis    = 0 AND emitente.idi-tributac-cofins = 0) THEN 
                  NEXT blk.
           END.
           WHEN 2 THEN DO:
               IF (emitente.idi-tributac-pis    = 1 AND emitente.idi-tributac-cofins = 1) THEN 
                   NEXT blk.
           
           END.
      END CASE.

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando dados..." + STRING(i-cont)).

      CREATE tt-emitente.
      BUFFER-COPY emitente TO tt-emitente.
      ASSIGN tt-emitente.r-rowid = ROWID(emitente).

      IF tt-emitente.idi-tributac-pis    = 0 THEN ASSIGN tt-emitente.idi-tributac-pis    = 2.
      IF tt-emitente.idi-tributac-cofins = 0 THEN ASSIGN tt-emitente.idi-tributac-cofins = 2.
   END.

   RUN pi-finalizar IN h-acomp.

   {&OPEN-BROWSERS-IN-QUERY-f-cad}
END.

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
  {src/adm/template/snd-list.i "tt-emitente"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
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

