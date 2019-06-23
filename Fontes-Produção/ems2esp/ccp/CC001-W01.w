&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emsfnd           PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-forma 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/****************************************************************************************** 
**             Programa: CC001-W01.w
**            Autor: Felipe Vieira
**       Fornecedor: DKP
**            Data: 13/04/2018
**  Change/Chamado: XXXXXXX
**        Objetivo: Cadastrar grupos Autorizados para Narrativa
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: tt-param
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include/i-prgvrs.i CC001-W01 1.00.00.000}



/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i CC001-W01 CCP}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var v-row-1 as rowid no-undo.
def var v-row-2 as rowid no-undo.

DEF BUFFER buf-esp_grp_usuar FOR es_grp_usuar.

DEFINE TEMP-TABLE tt-grp_usuar NO-UNDO
    FIELD cod_grp_usuar    AS CHAR
    FIELD idi_dtsul_instan AS INT
    FIELD idi_dtsul        AS INT 
    FIELD des_grp_usuar    AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-forma
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME B-es_grp_usuar

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es_grp_usuar grp_usuar tt-grp_usuar

/* Definitions for BROWSE B-es_grp_usuar                                */
&Scoped-define FIELDS-IN-QUERY-B-es_grp_usuar es_grp_usuar.cod_grp_usuar grp_usuar.des_grp_usuar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-es_grp_usuar   
&Scoped-define SELF-NAME B-es_grp_usuar
&Scoped-define QUERY-STRING-B-es_grp_usuar FOR EACH es_grp_usuar, ~
                                   EACH grp_usuar WHERE grp_usuar.cod_grp_usuar = es_grp_usuar.cod_grp_usuar
&Scoped-define OPEN-QUERY-B-es_grp_usuar OPEN QUERY {&SELF-NAME} FOR EACH es_grp_usuar, ~
                                   EACH grp_usuar WHERE grp_usuar.cod_grp_usuar = es_grp_usuar.cod_grp_usuar.
&Scoped-define TABLES-IN-QUERY-B-es_grp_usuar es_grp_usuar grp_usuar
&Scoped-define FIRST-TABLE-IN-QUERY-B-es_grp_usuar es_grp_usuar
&Scoped-define SECOND-TABLE-IN-QUERY-B-es_grp_usuar grp_usuar


/* Definitions for BROWSE B-grp_usuar                                   */
&Scoped-define FIELDS-IN-QUERY-B-grp_usuar tt-grp_usuar.cod_grp_usuar tt-grp_usuar.des_grp_usuar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-grp_usuar   
&Scoped-define SELF-NAME B-grp_usuar
&Scoped-define OPEN-QUERY-B-grp_usuar RUN AtualizarB-grp_usuar. OPEN QUERY {&SELF-NAME} FOR EACH tt-grp_usuar.
&Scoped-define TABLES-IN-QUERY-B-grp_usuar tt-grp_usuar
&Scoped-define FIRST-TABLE-IN-QUERY-B-grp_usuar tt-grp_usuar


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-B-es_grp_usuar}~
    ~{&OPEN-QUERY-B-grp_usuar}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 rt-button B-grp_usuar B-es_grp_usuar ~
bt-add bt-del 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-forma AS WIDGET-HANDLE NO-UNDO.

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
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 15.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-es_grp_usuar FOR 
      es_grp_usuar, 
      grp_usuar SCROLLING.

DEFINE QUERY B-grp_usuar FOR 
      tt-grp_usuar SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-es_grp_usuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-es_grp_usuar w-forma _FREEFORM
  QUERY B-es_grp_usuar DISPLAY
      es_grp_usuar.cod_grp_usuar  FORMAT "X(3)"  COLUMN-LABEL "Grupo"
grp_usuar.des_grp_usuar  FORMAT "X(32)" COLUMN-LABEL "Descri‡Æo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 13.25 FIT-LAST-COLUMN.

DEFINE BROWSE B-grp_usuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-grp_usuar w-forma _FREEFORM
  QUERY B-grp_usuar DISPLAY
      tt-grp_usuar.cod_grp_usuar  FORMAT "X(3)"  COLUMN-LABEL "Grupo"
tt-grp_usuar.des_grp_usuar  FORMAT "X(32)" COLUMN-LABEL "Descri‡Æo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 13.25 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     B-grp_usuar AT ROW 4.13 COL 3.72 WIDGET-ID 200
     B-es_grp_usuar AT ROW 4.13 COL 52 WIDGET-ID 300
     bt-add AT ROW 8.42 COL 42.43
     bt-del AT ROW 11.08 COL 42.43
     "Grupos Liberados" VIEW-AS TEXT
          SIZE 20 BY .88 AT ROW 3.13 COL 61.57 WIDGET-ID 6
     "Grupos Disponiveis" VIEW-AS TEXT
          SIZE 20 BY .88 AT ROW 3.08 COL 12 WIDGET-ID 4
     RECT-22 AT ROW 2.88 COL 2 WIDGET-ID 2
     rt-button AT ROW 1.25 COL 1 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17 WIDGET-ID 100.


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
  CREATE WINDOW w-forma ASSIGN
         HIDDEN             = YES
         TITLE              = "Cadastro Grupos Autorizados Narrativas"
         HEIGHT             = 16.92
         WIDTH              = 90.43
         MAX-HEIGHT         = 28.33
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 28.33
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-forma 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
/*
{include/w-consim.i}
*/ 

{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-forma
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB B-grp_usuar rt-button f-cad */
/* BROWSE-TAB B-es_grp_usuar B-grp_usuar f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
THEN w-forma:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-es_grp_usuar
/* Query rebuild information for BROWSE B-es_grp_usuar
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH es_grp_usuar,
                            EACH grp_usuar WHERE grp_usuar.cod_grp_usuar = es_grp_usuar.cod_grp_usuar.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE B-es_grp_usuar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-grp_usuar
/* Query rebuild information for BROWSE B-grp_usuar
     _START_FREEFORM
RUN AtualizarB-grp_usuar.
OPEN QUERY {&SELF-NAME} FOR EACH tt-grp_usuar.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE B-grp_usuar */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON END-ERROR OF w-forma /* Cadastro Grupos Autorizados Narrativas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON WINDOW-CLOSE OF w-forma /* Cadastro Grupos Autorizados Narrativas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-forma
ON CHOOSE OF bt-add IN FRAME f-cad
DO:
     
    IF B-grp_usuar:FETCH-SELECTED-ROW(1) THEN DO: /*Verifica se foi selecionado um campos do browse*/
        
      IF AVAIL(tt-grp_usuar) THEN DO: /*Verifica se existe um registro*/
          CREATE es_grp_usuar. /*Cria um registro na tabela temporaria*/
          ASSIGN es_grp_usuar.cod_grp_usuar     = tt-grp_usuar.cod_grp_usuar.
                /*
                 es_grp_usuar.idi_dtsul_instan  = tt-grp_usuar.idi_dtsul_instan
                 es_grp_usuar.idi_dtsul         = tt-grp_usuar.idi_dtsul
                 es_grp_usuar.des_grp_usuar     = tt-grp_usuar.des_grp_usuar.
                 */

          B-grp_usuar:DELETE-SELECTED-ROWS().    
          {&OPEN-QUERY-B-es_grp_usuar}
      END.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-forma
ON CHOOSE OF bt-del IN FRAME f-cad
DO:
      IF B-es_grp_usuar:FETCH-SELECTED-ROW(1) THEN DO: /*Verifica se foi selecionado um campos do browse*/
        
      IF AVAIL(es_grp_usuar) THEN DO: /*Verifica se existe um registro*/
 
        FIND FIRST buf-esp_grp_usuar EXCLUSIVE-LOCK 
            WHERE ROWID(buf-esp_grp_usuar) = ROWID(es_grp_usuar).

        IF AVAIL(buf-esp_grp_usuar) THEN DELETE buf-esp_grp_usuar. /*deleta o registro na tabela especi*/

       
          B-es_grp_usuar:DELETE-SELECTED-ROWS().    
          {&OPEN-QUERY-B-grp_usuar}
      END.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-forma
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-forma
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-forma
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-forma
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-forma
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-forma
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-es_grp_usuar
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-forma 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-forma  _ADM-CREATE-OBJECTS
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
       RUN set-position IN h_p-exihel ( 1.38 , 73.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             B-grp_usuar:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-forma  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AtualizarB-grp_usuar w-forma 
PROCEDURE AtualizarB-grp_usuar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-grp_usuar.

FOR EACH grp_usuar NO-LOCK 
    WHERE NOT CAN-FIND(es_grp_usuar WHERE es_grp_usuar.cod_grp_usuar    = grp_usuar.cod_grp_usuar
                                  /*    AND es_grp_usuar.idi_dtsul_instan = grp_usuar.idi_dtsul_instan
                                      AND es_grp_usuar.idi_dtsul        = grp_usuar.idi_dtsul */
                       ):

    CREATE tt-grp_usuar.
    ASSIGN  tt-grp_usuar.cod_grp_usuar       = grp_usuar.cod_grp_usuar    
            tt-grp_usuar.idi_dtsul_instan    = grp_usuar.idi_dtsul_instan 
            tt-grp_usuar.idi_dtsul           = grp_usuar.idi_dtsul 
            tt-grp_usuar.des_grp_usuar       = grp_usuar.des_grp_usuar.
                                          


END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-forma  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
  THEN DELETE WIDGET w-forma.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-forma  _DEFAULT-ENABLE
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
  ENABLE RECT-22 rt-button B-grp_usuar B-es_grp_usuar bt-add bt-del 
      WITH FRAME f-cad IN WINDOW w-forma.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-forma.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-forma 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "CC001-W01" "Cadastro Grupos Autorizados Narrativas" "1.00.00.000"} 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-forma  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-grp_usuar"}
  {src/adm/template/snd-list.i "es_grp_usuar"}
  {src/adm/template/snd-list.i "grp_usuar"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-forma 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

