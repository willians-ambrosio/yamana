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
{include/i-prgvrs.i YMUT0001 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-es-item-doc-est-natoper LIKE es-item-doc-est-natoper
       FIELDS r-rowid AS ROWID.

DEFINE VARIABLE h-acomp AS HANDLE  NO-UNDO.
DEFINE VARIABLE i-cont  AS INTEGER NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE ep-codigo-ini    LIKE es-item-doc-est-natoper.ep-codigo    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE ep-codigo-fim    LIKE es-item-doc-est-natoper.ep-codigo    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE serie-docto-ini  LIKE es-item-doc-est-natoper.serie-docto  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE serie-docto-fim  LIKE es-item-doc-est-natoper.serie-docto  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nro-docto-ini    LIKE es-item-doc-est-natoper.nro-docto    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nro-docto-fim    LIKE es-item-doc-est-natoper.nro-docto    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-ini LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-fim LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-emissao-ini   LIKE docum-est.dt-emissao                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-emissao-fim   LIKE docum-est.dt-emissao                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-trans-ini     LIKE docum-est.dt-trans                   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-trans-fim     LIKE docum-est.dt-trans                   NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-es-item-doc-est-natoper

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-es-item-doc-est-natoper.serie-docto tt-es-item-doc-est-natoper.nro-docto tt-es-item-doc-est-natoper.cod-emitente tt-es-item-doc-est-natoper.nat-operacao tt-es-item-doc-est-natoper.nat-operacao-orig tt-es-item-doc-est-natoper.cod-cfop-saida tt-es-item-doc-est-natoper.cod-beneficio-icms tt-es-item-doc-est-natoper.classe tt-es-item-doc-est-natoper.ep-codigo tt-es-item-doc-est-natoper.sequencia tt-es-item-doc-est-natoper.it-codigo tt-es-item-doc-est-natoper.cod-model-nf-eletro tt-es-item-doc-est-natoper.cod-beneficio-piscof tt-es-item-doc-est-natoper.emit-crt tt-es-item-doc-est-natoper.imp-CSOSN tt-es-item-doc-est-natoper.imp-pCredSN tt-es-item-doc-est-natoper.imp-vCredICMSSN tt-es-item-doc-est-natoper.aliquota-icm tt-es-item-doc-est-natoper.sequencia-it-doc-fisc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table tt-es-item-doc-est-natoper.classe   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-table tt-es-item-doc-est-natoper
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-table tt-es-item-doc-est-natoper
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-es-item-doc-est-natoper
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-es-item-doc-est-natoper.
&Scoped-define TABLES-IN-QUERY-br-table tt-es-item-doc-est-natoper
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-es-item-doc-est-natoper


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
       SUB-MENU  mi-programa    LABEL "&YMUT0001"     
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
     SIZE 5.14 BY 1.29 TOOLTIP "Limpa o CFA do Item".

DEFINE BUTTON bt-limpa-todos 
     IMAGE-UP FILE "image/ii-nenhum.bmp":U
     LABEL "Limpa Todos" 
     SIZE 5.14 BY 1.29 TOOLTIP "Limpa todos CFA do Item".

DEFINE BUTTON bt-preenche 
     IMAGE-UP FILE "image/im-fillin.bmp":U
     LABEL "Preenche" 
     SIZE 5.14 BY 1.29 TOOLTIP "Preenche com o CFA do Item".

DEFINE BUTTON bt-preenche-todos 
     IMAGE-UP FILE "image/im-todos.bmp":U
     LABEL "Preenche Todos" 
     SIZE 5.14 BY 1.29 TOOLTIP "Preenche todos com o CFA do Item".

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
      tt-es-item-doc-est-natoper SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table w-livre _FREEFORM
  QUERY br-table DISPLAY
      tt-es-item-doc-est-natoper.serie-docto          
 tt-es-item-doc-est-natoper.nro-docto            
 tt-es-item-doc-est-natoper.cod-emitente         
 tt-es-item-doc-est-natoper.nat-operacao         
 tt-es-item-doc-est-natoper.nat-operacao-orig    
 tt-es-item-doc-est-natoper.cod-cfop-saida       
 tt-es-item-doc-est-natoper.cod-beneficio-icms   
 tt-es-item-doc-est-natoper.classe   WIDTH 10            
 tt-es-item-doc-est-natoper.ep-codigo            
 tt-es-item-doc-est-natoper.sequencia            
 tt-es-item-doc-est-natoper.it-codigo            
 tt-es-item-doc-est-natoper.cod-model-nf-eletro  
 tt-es-item-doc-est-natoper.cod-beneficio-piscof 
 tt-es-item-doc-est-natoper.emit-crt             
 tt-es-item-doc-est-natoper.imp-CSOSN            
 tt-es-item-doc-est-natoper.imp-pCredSN          
 tt-es-item-doc-est-natoper.imp-vCredICMSSN      
 tt-es-item-doc-est-natoper.aliquota-icm         
 tt-es-item-doc-est-natoper.sequencia-it-doc-fisc
     ENABLE
          tt-es-item-doc-est-natoper.classe
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81.29 BY 10.25 FIT-LAST-COLUMN TOOLTIP "Duplo clique para preencher/limpar o CFA do Item".


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
         TITLE              = "Manutená∆o de CFA do Item do Documento de Estoque"
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-item-doc-est-natoper.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Manutená∆o de CFA do Item do Documento de Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Manutená∆o de CFA do Item do Documento de Estoque */
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
  IF AVAILABLE(tt-es-item-doc-est-natoper) THEN DO:
     IF tt-es-item-doc-est-natoper.classe:SCREEN-VALUE IN BROWSE br-table = "" THEN
        FOR FIRST ext-item-cfa FIELDS(classe)
                   WHERE ext-item-cfa.it-codigo = tt-es-item-doc-est-natoper.it-codigo
                     AND ext-item-cfa.ep-codigo = tt-es-item-doc-est-natoper.ep-codigo 
                   NO-LOCK:
            ASSIGN tt-es-item-doc-est-natoper.classe:SCREEN-VALUE IN BROWSE br-table = ext-item-cfa.classe.

            ASSIGN tt-es-item-doc-est-natoper.classe = ext-item-cfa.classe.
        END.   
     ELSE 
        ASSIGN tt-es-item-doc-est-natoper.classe:SCREEN-VALUE IN BROWSE br-table = ""
               tt-es-item-doc-est-natoper.classe = "".
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
  RUN esp/YMUT0001a.w.

  EMPTY TEMP-TABLE tt-es-item-doc-est-natoper.

  RUN pi-gera-tabela.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-limpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpa w-livre
ON CHOOSE OF bt-limpa IN FRAME f-cad /* Limpa */
DO:
   IF AVAILABLE(tt-es-item-doc-est-natoper) THEN DO:
      ASSIGN tt-es-item-doc-est-natoper.classe:SCREEN-VALUE IN BROWSE br-table = "".

      ASSIGN tt-es-item-doc-est-natoper.classe = "".
   END.
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

  FOR EACH tt-es-item-doc-est-natoper
           EXCLUSIVE-LOCK:
     ASSIGN i-cont = i-cont + 1.

     RUN pi-acompanhar IN h-acomp (INPUT "Limpando..." + STRING(i-cont)).
          
     ASSIGN tt-es-item-doc-est-natoper.classe = "".
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
  IF AVAILABLE(tt-es-item-doc-est-natoper) THEN DO:
     FOR FIRST ext-item-cfa FIELDS(classe)
                WHERE ext-item-cfa.it-codigo = tt-es-item-doc-est-natoper.it-codigo
                  AND ext-item-cfa.ep-codigo = tt-es-item-doc-est-natoper.ep-codigo 
                NO-LOCK:
         ASSIGN tt-es-item-doc-est-natoper.classe:SCREEN-VALUE IN BROWSE br-table = ext-item-cfa.classe.

         ASSIGN tt-es-item-doc-est-natoper.classe = ext-item-cfa.classe.
     END.   
  END.
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

   blk:
   FOR EACH tt-es-item-doc-est-natoper
            EXCLUSIVE-LOCK:
      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp (INPUT "Preenchendo..." + STRING(i-cont)).

      FOR FIRST ext-item-cfa FIELDS(classe)
                 WHERE ext-item-cfa.it-codigo = tt-es-item-doc-est-natoper.it-codigo
                   AND ext-item-cfa.ep-codigo = tt-es-item-doc-est-natoper.ep-codigo 
                 NO-LOCK:
         ASSIGN tt-es-item-doc-est-natoper.classe = ext-item-cfa.classe.
      END.   
   END.

  RUN pi-finalizar IN h-acomp.

  {&OPEN-BROWSERS-IN-QUERY-f-cad}
END.




   DO:
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Atualizando Browse").

  ASSIGN i-cont = 0.

  FOR EACH tt-es-item-doc-est-natoper
           EXCLUSIVE-LOCK:
     ASSIGN i-cont = i-cont + 1.

     RUN pi-acompanhar IN h-acomp (INPUT "Limpando..." + STRING(i-cont)).
          
     ASSIGN tt-es-item-doc-est-natoper.classe = "".
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
ON MENU-DROP OF MENU mi-programa /* YMUT0001 */
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

  {utp/ut9000.i "YMUT0001" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

 
  ASSIGN ep-codigo-ini    = i-ep-codigo-usuario
         ep-codigo-fim    = i-ep-codigo-usuario
         serie-docto-ini  = ""
         serie-docto-fim  = "zzzzz"
         nro-docto-ini    = ""
         nro-docto-fim    = "zzzzzzzzzzzzzzzz"
         cod-emitente-ini = 0
         cod-emitente-fim = 999999999
         dt-emissao-ini   = 01/01/1900
         dt-emissao-fim   = 12/31/2999
         dt-trans-ini     = 01/01/1900
         dt-trans-fim     = 12/31/9999.



  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN pi-gera-tabela.*/


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
   FOR EACH tt-es-item-doc-est-natoper
            WHERE tt-es-item-doc-est-natoper.classe <> ""
            NO-LOCK:
      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp (INPUT "Atualizando dados..." + STRING(i-cont)).

      FIND es-item-doc-est-natoper
           WHERE ROWID(es-item-doc-est-natoper) = tt-es-item-doc-est-natoper.r-rowid
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(es-item-doc-est-natoper) THEN DO:
         ASSIGN c-chave = es-item-doc-est-natoper.ep-codigo            + "|" +
                          es-item-doc-est-natoper.serie-docto          + "|" +
                          es-item-doc-est-natoper.nro-docto            + "|" +
                          STRING(es-item-doc-est-natoper.cod-emitente) + "|" +
                          es-item-doc-est-natoper.nat-operacao         + "|" +
                          STRING(es-item-doc-est-natoper.sequencia).

         FIND FIRST ext-audit-trail
              WHERE ext-audit-trail.tabela     = "es-item-doc-est-natoper"  AND
                    ext-audit-trail.campo      = "classe"                   AND
                    ext-audit-trail.chave      = c-chave                    AND
                    ext-audit-trail.data       = TODAY                      AND
                    ext-audit-trail.hora       = STRING(TIME,"HH:MM:SS")    AND
                    ext-audit-trail.usuario    = c-seg-usuario                   
              NO-LOCK NO-ERROR.
         IF AVAILABLE(ext-audit-trail) THEN
            PAUSE 2 BEFORE-HIDE NO-MESSAGE.

         CREATE ext-audit-trail.
         ASSIGN ext-audit-trail.tabela     = "es-item-doc-est-natoper"
                ext-audit-trail.campo      = "classe"
                ext-audit-trail.chave      = c-chave                         
                ext-audit-trail.valor-ant  = es-item-doc-est-natoper.classe
                ext-audit-trail.valor-atu  = tt-es-item-doc-est-natoper.classe
                ext-audit-trail.data       = TODAY
                ext-audit-trail.hora       = STRING(TIME,"HH:MM:SS")
                ext-audit-trail.usuario    = c-seg-usuario.

         ASSIGN es-item-doc-est-natoper.classe = tt-es-item-doc-est-natoper.classe.

         DELETE tt-es-item-doc-est-natoper.
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

   EMPTY TEMP-TABLE tt-es-item-doc-est-natoper.

   blk:
   FOR EACH es-item-doc-est-natoper
            WHERE es-item-doc-est-natoper.ep-codigo    >= ep-codigo-ini    AND
                  es-item-doc-est-natoper.ep-codigo    <= ep-codigo-fim    AND
                  es-item-doc-est-natoper.serie-docto  >= serie-docto-ini  AND
                  es-item-doc-est-natoper.serie-docto  <= serie-docto-fim  AND
                  es-item-doc-est-natoper.nro-docto    >= nro-docto-ini    AND 
                  es-item-doc-est-natoper.nro-docto    <= nro-docto-fim    AND 
                  es-item-doc-est-natoper.cod-emitente >= cod-emitente-ini AND 
                  es-item-doc-est-natoper.cod-emitente <= cod-emitente-fim AND 
                  es-item-doc-est-natoper.classe        = ""
            NO-LOCK:
      IF es-item-doc-est-natoper.nat-operacao = "1999" OR
         es-item-doc-est-natoper.nat-operacao = "2999" OR
         es-item-doc-est-natoper.nat-operacao = "3999" THEN
         NEXT blk.

      FIND docum-est OF es-item-doc-est-natoper
           WHERE docum-est.dt-emissao >= dt-emissao-ini AND
                 docum-est.dt-emissao <= dt-emissao-fim AND
                 docum-est.dt-trans   >= dt-trans-ini   AND
                 docum-est.dt-trans   <= dt-trans-fim
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(docum-est) THEN
         NEXT blk.

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp (INPUT "Gerando dados..." + STRING(i-cont)).

      CREATE tt-es-item-doc-est-natoper.
      BUFFER-COPY es-item-doc-est-natoper TO tt-es-item-doc-est-natoper.
      ASSIGN tt-es-item-doc-est-natoper.r-rowid = ROWID(es-item-doc-est-natoper).
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
  {src/adm/template/snd-list.i "tt-es-item-doc-est-natoper"}

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

