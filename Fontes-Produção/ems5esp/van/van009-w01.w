&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i VAN009-W01 5.01.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-seq  AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-ok  AS LOGICAL     NO-UNDO.
DEFINE STREAM sData.

DEF TEMP-TABLE tt-dados
    FIELD iseq      AS INT
    FIELD banco     AS CHAR
    FIELD ag        AS CHAR
    FIELD conta     AS CHAR
    FIELD nom_fav   AS CHAR
    FIELD nom_func  AS CHAR
    FIELD cod_func  AS INT
    FIELD num_id    AS INT
    FIELD cod_estab AS CHAR 
    FIELD docum     AS CHAR 
    FIELD ag-dv     AS CHAR 
    FIELD dvconta   AS CHAR
    FIELD vlr_bco   AS DEC
    FIELD vlr_rh    AS DEC
    FIELD cod_ocorr AS CHAR
    FIELD tip_ocor  AS CHAR
    FIELD desc_rej  AS CHAR.

DEFINE VARIABLE h-van006 AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(100)"
    FIELD lSearch   AS LOGICAL.

{van\van006.i} /* Procedures para conexao de bancos */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-dados

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt-dados.cod_estab tt-dados.nom_fav tt-dados.cod_func tt-dados.vlr_bco tt-dados.vlr_rh tt-dados.cod_ocorr tt-dados.tip_ocor tt-dados.desc_rej   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt-dados
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY browse-3 FOR EACH tt-dados.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt-dados
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt-dados


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-ok rd_tipo BROWSE-3 
&Scoped-Define DISPLAYED-OBJECTS d_tot_bco d_dif d_tot_folha rd_tipo 

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
DEFINE BUTTON bt-ok 
     IMAGE-UP FILE "ems2/image/application.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1.13 TOOLTIP "Busca arquivos de retorno".

DEFINE VARIABLE d_dif AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Diferenáa" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE d_tot_bco AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor total Retorno Banco" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE d_tot_folha AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total Folha" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE rd_tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Rejeitados", 2
     SIZE 24 BY .75 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt-dados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 w-livre _FREEFORM
  QUERY BROWSE-3 DISPLAY
      tt-dados.cod_estab column-label "Est"         format "X(03)"              width 5
  tt-dados.nom_fav   column-label "Nome"        format "X(30)"              width 30
  tt-dados.cod_func  column-label "Matricula"   format ">>>>>>>9"           width 10
  tt-dados.vlr_bco   column-label "Valor Banco" format ">>,>>>,>>9.99"      width 14
  tt-dados.vlr_rh    column-label "Valor RH"    format ">>,>>>,>>9.99"      width 14
  tt-dados.cod_ocorr COLUMN-LABEL "Cod.Ocor"    FORMAT "X(03)"              WIDTH 4
  tt-dados.tip_ocor  COLUMN-LABEL "TpOcor"      FORMAT "X(03)"              WIDTH 5
  tt-dados.desc_rej  COLUMN-LABEL "Motivo"      FORMAT "X(30)"              WIDTH 22
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 9.29 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-ok AT ROW 1.17 COL 2 WIDGET-ID 8
     d_tot_bco AT ROW 3 COL 27 COLON-ALIGNED WIDGET-ID 2
     d_dif AT ROW 3 COL 64 COLON-ALIGNED WIDGET-ID 6
     d_tot_folha AT ROW 4 COL 27 COLON-ALIGNED WIDGET-ID 4
     rd_tipo AT ROW 4.08 COL 66 NO-LABEL WIDGET-ID 12
     BROWSE-3 AT ROW 5.42 COL 3 WIDGET-ID 200
     "Visualizar:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 4.13 COL 58 WIDGET-ID 16
     rt-button AT ROW 1 COL 1
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 14.08
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
/* BROWSE-TAB BROWSE-3 rd_tipo f-cad */
/* SETTINGS FOR FILL-IN d_dif IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d_tot_bco IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d_tot_folha IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY browse-3 FOR EACH tt-dados.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
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


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 w-livre
ON ROW-DISPLAY OF BROWSE-3 IN FRAME f-cad
DO:
  IF tt-dados.vlr_bco <> tt-dados.vlr_rh OR tt-dados.tip_ocor = "Pagamento Rejeitado"
      THEN ASSIGN tt-dados.cod_estab:fgcolor in browse browse-3 = 12 
                  tt-dados.nom_fav  :fgcolor in browse browse-3 = 12
                  tt-dados.cod_func :fgcolor in browse browse-3 = 12 
                  tt-dados.vlr_bco  :fgcolor in browse browse-3 = 12 
                  tt-dados.vlr_rh   :fgcolor in browse browse-3 = 12
                  tt-dados.cod_ocor :fgcolor in browse browse-3 = 12
                  tt-dados.tip_ocor :fgcolor in browse browse-3 = 12
                  tt-dados.desc_rej :fgcolor in browse browse-3 = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-livre
ON CHOOSE OF bt-ok IN FRAME f-cad /* Button 2 */
DO:
    /* Conecta bancos do 5 */
    DEFINE VARIABLE c_conect AS CHARACTER   NO-UNDO.

    FIND FIRST param_conex_base_dados WHERE
               param_conex_base_dados.cdn_base_dados_conex = 02 AND 
               param_conex_base_dados.nom_fisic_base_dados = 'ems5' NO-ERROR.
    IF NOT AVAIL PARAM_conex_base_dados THEN RETURN "Parametro de conex∆o com o EMS5 n∆o encontrado!".
            
    ASSIGN c_conect =   " -db " + param_conex_base_dados.nom_fisic_base_dados 
                      + " -ld " + param_conex_base_dados.nom_logic_base_dados
                      + " -H "  + param_conex_base_dados.nom_maquina
                      + " -S "  + param_conex_base_dados.des_serv_conex 
                      + " -N "  + param_conex_base_dados.des_protoc_conex_base_dados.
    
    IF NOT CONNECTED(param_conex_base_dados.nom_logic_base_dados) 
        THEN CONNECT VALUE(c_conect).
    
    IF NOT CONNECTED(param_conex_base_dados.nom_fisic_base_dados) THEN DO:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "Banco Movimento n∆o Conectado~~N∆o ser† poss°vel mostrar as informaá‰es relativas a conta e centro de custo ").
        RETURN "NOK":U.
    END.    

    SESSION:SET-WAIT-STATE("General").

   /* Gera a baixa do t°tulo no Contas a Pagar */
    RUN van\van006.p PERSISTENT SET h-van006 "hcm".
    RUN pi-listamsg IN h-van006.

   
    SESSION:SET-WAIT-STATE("").

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


&Scoped-define SELF-NAME rd_tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_tipo w-livre
ON VALUE-CHANGED OF rd_tipo IN FRAME f-cad
DO:
  IF SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" 
      THEN OPEN QUERY browse-3 FOR EACH tt-dados BY tt-dados.nom_fav.
  ELSE OPEN QUERY browse-3 FOR EACH tt-dados WHERE 
                        tt-dados.tip_ocor = "Pagamento Rejeitado" BY tt-dados.nom_fav.

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
             bt-ok:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY d_tot_bco d_dif d_tot_folha rd_tipo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button bt-ok rd_tipo BROWSE-3 
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

  {utp/ut9000.i "VAN009-W01" "5.01.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-carga.

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carga w-livre 
PROCEDURE pi-carga :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST es_param_folha NO-LOCK 
    WHERE es_param_folha.cdn_empresa = v_cod_empres_usuar NO-ERROR.
IF NOT AVAIL es_param_folha THEN DO:

    MESSAGE "N∆o foi poss°vel encontrar parÉmetro de diret¢rio para retorno de folha!" SKIP
            "Verificar o cadastro van010-w01" SKIP
            "Entre em contato com a Sustentaá∆o!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN ERROR.
END.

ASSIGN d_tot_folha = 0
       d_tot_bco   = 0
       d_dif       = 0.

RUN van\van001a.p (INPUT es_param_folha.dir_retorno,
                   OUTPUT TABLE tt-listFiles,
                   OUTPUT l-ok).

/* Monta a Lista de todos os arquivos na pasta */

FOR EACH tt-listFiles:
    
    IF SEARCH(tt-listFiles.cFile) = ? THEN NEXT.

    FILE-INFO:FILE-NAME = tt-listFiles.cFile.

    /* Despresa os diretor¢rios */
    IF FILE-INFO:FILE-TYPE = "DRW" THEN NEXT.

    INPUT STREAM sData FROM VALUE(FILE-INFO:FILE-NAME).

    REPEAT:

        IMPORT STREAM sData UNFORMATTED cLinha.
        ASSIGN i-seq = i-seq + 1.
        
        IF INT(SUBSTR(cLinha,8,1)) = 3 AND SUBSTR(cLinha,14,1) = "A"  THEN DO:

            FIND FIRST ocor_bcia_bco NO-LOCK 
                WHERE ocor_bcia_bco.cod_banco               = SUBSTR(cLinha,1,3) 
                  AND ocor_bcia_bco.cod_modul_dtsul         = "APB"
                  AND ocor_bcia_bco.ind_ocor_bcia_remes_ret = "Retorno" /* Retorno */
                  AND ocor_bcia_bco.cod_ocor_bcia_bco       = TRIM(SUBSTR(cLinha,231,10)) NO-ERROR.

            FIND motiv_rej_bco NO-LOCK 
                WHERE motiv_rej_bco.cod_banco                   = SUBSTR(cLinha,1,3)
                  AND motiv_rej_bco.cdn_motiv_rej_cobr_especial = INT(SUBSTR(cLinha,231,10)) NO-ERROR.

             FIND motiv_rej_cobr_especial NO-LOCK OF motiv_rej_bco NO-ERROR.

             MESSAGE AVAIL OCOR_BCIA_BCO SKIP
                     AVAIL motiv_rej_bco SKIP
                     AVAIL motiv_rej_cobr_especial
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
            CREATE tt-dados.
            ASSIGN /*tt-dados.clinha  = cLinha*/
                   tt-dados.iseq     = i-seq
                   tt-dados.banco    = SUBSTR(cLinha,21,3)  /* 21 a 23 Banco */
                   tt-dados.ag       = SUBSTR(cLinha,24,5)  /* 24 a  28 Ag */
                   tt-dados.ag-dv    = SUBSTR(cLinha,29,1)  /* 29 a 29 Dig */
                   tt-dados.conta    = SUBSTR(cLinha,30,12) /* 30 a 41 Conta */
                   tt-dados.dvconta  = SUBSTR(cLinha,42,1)  /* 42 a 42 DV conta */
                   tt-dados.nom_fav  = SUBSTR(cLinha,44,30) /* 44 a 73 - Nome Favorec */
                   tt-dados.docum    = SUBSTR(cLinha,74,20) /* 74 a 93 - docum */
                   tt-dados.cod_estab = SUBSTR(cLinha,83,3)
                   tt-dados.cod_func  = INT(SUBSTR(cLinha,74,9))
                   tt-dados.num_id    = INT(SUBSTR(cLinha,86,8))
                   tt-dados.vlr_bco   = DEC(SUBSTR(cLinha,120,15)) / 100 /* 120 a 134 - Valor */ 
                   tt-dados.cod_ocorr = SUBSTR(cLinha,231,10)
                   tt-dados.tip_ocor  = ocor_bcia_bco.ind_tip_ocor_bcia WHEN AVAIL ocor_bcia_bco
                   tt-dados.desc_rej  = motiv_rej_cobr_especial.des_motiv_rej_cobr_especial WHEN AVAIL motiv_rej_cobr_especial.
                  
        END.       
        /* Trailler do Lote */
         IF INT(SUBSTR(cLinha,8,1)) = 5 THEN 
             ASSIGN d_tot_bco =  d_tot_bco + DEC(SUBSTR(cLinha,24,18)) / 100.
    END.
    INPUT STREAM sData CLOSE.
END. 

INPUT CLOSE.

FOR EACH tt-dados BREAK BY  tt-dados.num_id:

    IF FIRST-OF(tt-dados.num_id) THEN DO:

        FIND tit_pagto NO-LOCK WHERE
             tit_pagto.cdn_empresa    = v_cod_empres_usuar  AND 
             tit_pagto.num_tit_pagto  = tt-dados.num_id NO-ERROR.
        IF AVAIL tit_pagto
             THEN ASSIGN d_tot_folha = d_tot_folha + tit_pagto.val_tit_pagto.
    END.

    FIND FIRST histor_tit_pagto OF tit_pagto NO-LOCK 
          WHERE histor_tit_pagto.cdn_funcionario = tt-dados.cod_func NO-ERROR.
    IF AVAIL histor_tit_pagto
         THEN ASSIGN tt-dados.vlr_rh = histor_tit_pagto.val_tit_pagto. 

END.

ASSIGN d_tot_folha:screen-value in frame {&frame-name} = string(d_tot_folha) 
       d_tot_bco  :screen-value in frame {&frame-name} = string(d_tot_bco)
       d_dif      :screen-value in frame {&frame-name} = string(d_tot_bco - d_tot_folha).

OPEN QUERY browse-3 FOR EACH tt-dados BY tt-dados.nom_fav.

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
  {src/adm/template/snd-list.i "tt-dados"}

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

