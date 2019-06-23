&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESFAS900 11.5.12.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESFAS900 FAS}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&global-define programa ESFAS900

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{include/i-rpvar.i}
def var c-destino                    as character format "x(15)":U.

/* find emscad.empresa                                */
/*     where empresa.cod_empresa = v_cdn_empres_usuar */
/*     no-lock no-error.                              */
find first param-global no-lock no-error.

{utp/ut-liter.i ATIVO_FIXO * }
assign c-sistema = return-value.
{utp/ut-liter.i AJUSTE_CRONOGRAMA_CALCULO_BEM * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "11.5.12":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 3}.

def var h-acomp         as handle no-undo.    
DEFINE TEMP-TABLE tt-cronograma NO-UNDO
    FIELD cod_empresa     LIKE bem_pat.cod_empresa
    FIELD cod_cta_pat     LIKE bem_pat.cod_cta_pat
    FIELD num_bem_pat     LIKE bem_pat.num_bem_pat
    FIELD num_seq_bem_pat LIKE bem_pat.num_seq_bem_pat
    FIELD des_bem_pat     LIKE bem_pat.des_bem_pat
    FIELD num_id_bem_pat  LIKE bem_pat.num_id_bem_pat
    FIELD dt_ini          AS DATE FORMAT "99/99/9999" COLUMN-LABEL "Data Ini"
    FIELD dt_fim          AS DATE FORMAT "99/99/9999" COLUMN-LABEL "Data final"
    FIELD situacao        AS CHARACTER FORMAT "x(60)" COLUMN-LABEL "Situa‡Æo"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-8 RECT-7 c-arquivo-entrada ~
bt-arquivo-entrada bt-arquivo c-arquivo bt-import 
&Scoped-Define DISPLAYED-OBJECTS c-arquivo-entrada c-arquivo 

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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-import 
     LABEL "&Importar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 51 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     c-arquivo-entrada AT ROW 4.04 COL 4.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 4
     bt-arquivo-entrada AT ROW 4.04 COL 44.14 HELP
          "Escolha do nome do arquivo" WIDGET-ID 2
     bt-arquivo AT ROW 7.42 COL 44.14 HELP
          "Escolha do nome do arquivo" WIDGET-ID 14
     c-arquivo AT ROW 7.46 COL 4.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 16
     bt-import AT ROW 10.13 COL 34 WIDGET-ID 10
     "Resultado" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 5.75 COL 4 WIDGET-ID 22
     "Arquivo de Entrada" VIEW-AS TEXT
          SIZE 16.72 BY .67 AT ROW 3.04 COL 4.29 WIDGET-ID 12
     rt-button AT ROW 1 COL 1
     RECT-8 AT ROW 3.33 COL 3 WIDGET-ID 6
     RECT-7 AT ROW 6.04 COL 3 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre Template
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
         HEIGHT             = 10.42
         WIDTH              = 51.14
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
{include/i-rpcab.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-livre
ON CHOOSE OF bt-arquivo IN FRAME f-cad
DO:
    def var c-arq-conv  as char no-undo.
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.

    assign c-arq-conv = replace(input frame f-cad c-arquivo, "/", CHR(92)).

        SYSTEM-DIALOG GET-FILE c-arq-conv
           FILTERS "*.txt" "*.txt",
                   "*.*" "*.*"
           ASK-OVERWRITE 
           DEFAULT-EXTENSION "lst"
           INITIAL-DIR "spool" 
           SAVE-AS
           USE-FILENAME
           UPDATE l-ok.

    if  l-ok = yes then do:
        assign c-arquivo = replace(c-arq-conv, CHR(92), "/"). 
        display c-arquivo with frame f-cad.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada w-livre
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-cad
DO:
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.
    {include/i-imarq.i c-arquivo-entrada f-cad "'*.csv' '*.csv'" }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-import w-livre
ON CHOOSE OF bt-import IN FRAME f-cad /* Importar */
DO:
  RUN pi-importar.
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
ON MENU-DROP OF MENU mi-programa /* Arquivo */
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
       RUN set-position IN h_p-exihel ( 1.17 , 34.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-arquivo-entrada:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY c-arquivo-entrada c-arquivo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-8 RECT-7 c-arquivo-entrada bt-arquivo-entrada 
         bt-arquivo c-arquivo bt-import 
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

  {utp/ut9000.i "ESFAS900" "11.5.12.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

        assign c-arquivo = replace(SESSION:TEMP-DIRECTORY + "esfas900.txt", CHR(92), "/"). 
        display c-arquivo with frame f-cad.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importar w-livre 
PROCEDURE pi-importar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

disable triggers for load of cronog_calc_pat.
disable triggers for DUMP of cronog_calc_pat.

ASSIGN INPUT FRAME f-cad c-arquivo-entrada c-arquivo.

assign file-info:file-name = input frame f-cad c-arquivo-entrada.
IF file-info:pathname = ? THEN DO:
    run utp/ut-msgs.p (input "show":U,
                       input 326,
                       input c-arquivo-entrada).                               
    apply 'entry':U to c-arquivo-entrada in frame f-cad.                
    return error.
end. 

EMPTY TEMP-TABLE tt-cronograma.
run utp/ut-acomp.p persistent set h-acomp.  

run pi-inicializar in h-acomp (input "Importando Cronograma de C lculo...":U). 

run pi-acompanhar in h-acomp (input "Importando planilha...":U).
INPUT FROM VALUE(c-arquivo-entrada) NO-CONVERT.
REPEAT:
    CREATE tt-cronograma.
    IMPORT DELIMITER ";"
           tt-cronograma.cod_empresa     
           tt-cronograma.cod_cta_pat     
           tt-cronograma.num_bem_pat     
           tt-cronograma.num_seq_bem_pat 
           tt-cronograma.des_bem_pat     
           tt-cronograma.dt_ini        
           tt-cronograma.dt_fim        
        NO-ERROR.
END.
INPUT CLOSE.

FOR EACH tt-cronograma
    WHERE tt-cronograma.cod_emp = "" OR
          tt-cronograma.cod_emp BEGINS  "Emp":
    DELETE tt-cronograma.
END.

run pi-acompanhar in h-acomp (input "Procurando bens...":U).
FOR EACH tt-cronograma:
    ASSIGN tt-cronograma.cod_cta_pat = REPLACE(tt-cronograma.cod_cta_pat,".","").
    FOR FIRST bem_pat FIELDS(num_id_bem_pat des_bem_pat)
        WHERE bem_pat.cod_empresa      = tt-cronograma.cod_empresa     
          AND bem_pat.cod_cta_pat      = tt-cronograma.cod_cta_pat     
          AND bem_pat.num_bem_pat      = tt-cronograma.num_bem_pat     
          AND bem_pat.num_seq_bem_pat  = tt-cronograma.num_seq_bem_pat NO-LOCK: END.
    IF AVAIL bem_pat THEN
        ASSIGN tt-cronograma.num_id_bem_pat = bem_pat.num_id_bem_pat
               tt-cronograma.des_bem_pat    = bem_pat.des_bem_pat.
    ELSE
        ASSIGN tt-cronograma.situacao = "Bem NÆo Encontrado".
END.

run pi-acompanhar in h-acomp (input "Criando cronograma...":U).

FOR EACH tt-cronograma
    WHERE tt-cronograma.situacao       = "":

    run pi-acompanhar in h-acomp (input "Bem ":U +
                                        STRING(tt-cronograma.cod_cta_pat) +
                                        "-" +
                                        STRING(tt-cronograma.num_bem_pat)).

    FOR EACH param_calc_cta
        WHERE param_calc_cta.cod_empresa = tt-cronograma.cod_empresa
          AND param_calc_cta.cod_cta_pat = tt-cronograma.cod_cta_pat
          AND (param_calc_cta.cod_tip_calc = "DP" OR
               param_calc_cta.cod_tip_calc = "AM" ) NO-LOCK:

        FIND FIRST cronog_calc_pat NO-LOCK
            WHERE cronog_calc_pat.num_id_bem_pat  = tt-cronograma.num_id_bem_pat
            AND   cronog_calc_pat.cod_tip_calc    = param_calc_cta.cod_tip_calc
            AND   cronog_calc_pat.cod_cenar_ctbl  = param_calc_cta.cod_cenar_ctbl
            AND   cronog_calc_pat.dat_inic_parada = tt-cronograma.dt_ini NO-ERROR.
        IF NOT AVAILABLE cronog_calc_pat THEN DO:
            CREATE cronog_calc_pat.
            ASSIGN cronog_calc_pat.num_id_bem_pat  = tt-cronograma.num_id_bem_pat
                   cronog_calc_pat.cod_tip_calc    = param_calc_cta.cod_tip_calc
                   cronog_calc_pat.cod_cenar_ctbl  = param_calc_cta.cod_cenar_ctbl
                   cronog_calc_pat.dat_inic_parada = tt-cronograma.dt_ini
                   cronog_calc_pat.dat_fim_parada  = tt-cronograma.dt_fim.
            ASSIGN tt-cronograma.situacao = tt-cronograma.situacao + "Tipo calc " + QUOTER(param_calc_cta.cod_tip_calc) + " criado. ".
        END.
        ELSE
            ASSIGN tt-cronograma.situacao = tt-cronograma.situacao + "Tipo calc " + QUOTER(param_calc_cta.cod_tip_calc) + " j  existente. ".

    END. /* param_calc_cta */

    IF tt-cronograma.situacao = "" THEN
        ASSIGN tt-cronograma.situacao = "Parƒmetro nÆo encontrado para conta.".


END. /* EACH tt-cronograma */

run pi-acompanhar in h-acomp (input "Exportando Resultado...":U).

output to value(c-arquivo) 
       paged page-size 64
       convert target "iso8859-1".
view frame f-cabec.
view frame f-rodape.    

    FOR EACH tt-cronograma:
        DISP tt-cronograma
            WITH WIDTH 300 SCROLLABLE STREAM-IO.
    END.

run pi-finalizar in h-acomp.
{include/i-rpclo.i}
OS-COMMAND NO-WAIT VALUE(c-arquivo).

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

