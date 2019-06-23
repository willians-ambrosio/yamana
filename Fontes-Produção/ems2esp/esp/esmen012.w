&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadpaifilho-ambos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadpaifilho-ambos 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var p-table as rowid.

DEF BUFFER bf-prog1 FOR prog_dtsul.
DEF BUFFER bf-prog2 FOR prog_dtsul.

DEF VAR vi-tipo AS INT NO-UNDO.
DEF VAR vc-arq-entrada AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-importa
    FIELD linha AS INT
    FIELD cod_prog_dtsul_base LIKE prog_dtsul.cod_prog_dtsul
    FIELD cod_prog_dtsul      LIKE prog_dtsul.cod_prog_dtsul
    FIELD observacao          AS CHAR
    FIELD razao               AS CHAR
    FIELD msg AS CHAR
    FIELD erro AS LOG
    INDEX tt-importa IS PRIMARY
    linha
    INDEX tt-erro
    erro
    linha.

DEF VAR ii AS INT NO-UNDO.

def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-paiamb
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-excel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadpaifilho-ambos AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&�ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&�ltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V� para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-incluir     LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM mi-copiar      LABEL "C&opiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM mi-alterar     LABEL "A&lterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM mi-eliminar    LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat�rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  mi-ajuda       LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_esmen012-b01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_esmen012-q01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_esmen012-v01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cadpai AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/toolbar/im-excel.bmp":U
     LABEL "bt-excel" 
     SIZE 4 BY 1.13 TOOLTIP "Envia ao Excel todo o cadastro de Conflito de Acessos.".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-excel AT ROW 1.21 COL 52.29 WIDGET-ID 40
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.13 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-paiamb
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadpaifilho-ambos ASSIGN
         HIDDEN             = YES
         TITLE              = "Manuten��o - Matriz de Conflito de Acesso"
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 21.21
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.21
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadpaifilho-ambos 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-paiamb.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadpaifilho-ambos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-ambos)
THEN w-cadpaifilho-ambos:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadpaifilho-ambos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-ambos w-cadpaifilho-ambos
ON END-ERROR OF w-cadpaifilho-ambos /* Manuten��o - Matriz de Conflito de Acesso */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadpaifilho-ambos w-cadpaifilho-ambos
ON WINDOW-CLOSE OF w-cadpaifilho-ambos /* Manuten��o - Matriz de Conflito de Acesso */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-cadpaifilho-ambos
ON CHOOSE OF bt-excel IN FRAME f-cad /* bt-excel */
DO:
  
    RUN esp/esmen012-g02.w (OUTPUT vi-tipo,
                            OUTPUT vc-arq-entrada).

    IF vi-tipo = 0 THEN
        RETURN NO-APPLY.

    IF vi-tipo = 1 THEN DO:
        
        MESSAGE "Esta op��o ir� gerar todo o cadastro de Conflito de Acesso no Excel." SKIP(1)
            "Confirma execu��o ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOG.
    
        IF NOT l-confirma THEN
            RETURN NO-APPLY.
    
        RUN pi-excel.

    END.

    ELSE DO:

        IF SEARCH(vc-arq-entrada) = ? THEN DO:

            MESSAGE "Arquivo de entrada n�o localizado !"
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        MESSAGE "Confirma a importa��o ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-importar AS LOG.

        IF NOT l-importar THEN
            RETURN NO-APPLY. 

        RUN pi-csv.
        
        /* Dispatch standard ADM method.                             */
        RUN adm-open-query IN h_esmen012-q01.
        RUN adm-display-fields IN h_esmen012-q01.


    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-alterar w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadpai.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadpaifilho-ambos
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-copiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-copiar w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
  RUN pi-copiar IN h_p-cadpai.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-eliminar w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadpai.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat�rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-incluir w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadpai.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr�ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-ultimo /* �ltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadpaifilho-ambos
ON CHOOSE OF MENU-ITEM mi-va-para /* V� para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadpaifilho-ambos 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadpaifilho-ambos  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.17 , 1.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-cadpai.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadpai ).
       RUN set-position IN h_p-cadpai ( 1.17 , 26.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esmen012-v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_esmen012-v01 ).
       RUN set-position IN h_esmen012-v01 ( 2.63 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.50 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'P Conflito' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 6.50 , 1.43 ) NO-ERROR.
       RUN set-size IN h_folder ( 11.63 , 89.29 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esmen012-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?,
                     ProgAtributo = ,
                     ProgIncMod = esp/esmen012b.w':U ,
             OUTPUT h_esmen012-b01 ).
       RUN set-position IN h_esmen012-b01 ( 7.79 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 10.04 , 85.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esp/esmen012-q01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = esp/esmen012-z01.w,
                     ProgVaPara = esp/esmen012-g01.w,
                     ProgIncMod = esp/esmen012a.w,
                     Implantar = no':U ,
             OUTPUT h_esmen012-q01 ).
       RUN set-position IN h_esmen012-q01 ( 1.00 , 65.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_esmen012-v01. */
       RUN add-link IN adm-broker-hdl ( h_esmen012-q01 , 'Record':U , h_esmen012-v01 ).
       RUN add-link IN adm-broker-hdl ( h_p-cadpai , 'TableIO':U , h_esmen012-v01 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to BrowserCadastro2 h_esmen012-b01. */
       RUN add-link IN adm-broker-hdl ( h_esmen012-q01 , 'Record':U , h_esmen012-b01 ).

       /* Links to SmartQuery h_esmen012-q01. */
       RUN add-link IN adm-broker-hdl ( h_p-cadpai , 'State':U , h_esmen012-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_esmen012-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_esmen012-q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_esmen012-q01 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-excel:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadpai ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-cadpai , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_esmen012-v01 ,
             bt-excel:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_esmen012-v01 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_esmen012-b01 ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadpaifilho-ambos  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadpaifilho-ambos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-ambos)
  THEN DELETE WIDGET w-cadpaifilho-ambos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadpaifilho-ambos  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-excel 
      WITH FRAME f-cad IN WINDOW w-cadpaifilho-ambos.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadpaifilho-ambos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadpaifilho-ambos 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadpaifilho-ambos 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadpaifilho-ambos 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "ESMEN012" "1.00.00.000"}
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-csv w-cadpaifilho-ambos 
PROCEDURE pi-csv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var h-acomp           as handle    no-undo.

DEFINE VARIABLE curDir AS CHARACTER.
DEFINE VARIABLE l-erro AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-erro-base AS LOGICAL     NO-UNDO.

FILE-INFO:FILE-NAME = ".".
curDir = FILE-INFO:FULL-PATHNAME.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Controle de Acesso Conflitante").

run pi-acompanhar in h-acomp (input "Aguarde...").

INPUT FROM VALUE(vc-arq-entrada).

EMPTY TEMP-TABLE tt-importa.
ASSIGN ii = 1.

IMPORT ^ ^ .

REPEAT:

    PROCESS EVENTS.

    ASSIGN ii = ii + 1.

    RUN pi-acompanhar IN h-acomp ("Importando linha: " + STRING(ii)).

    CREATE tt-importa.
    IMPORT DELIMITER ";" 
           tt-importa.cod_prog_dtsul_base
           tt-importa.cod_prog_dtsul
           tt-importa.observacao
           tt-importa.razao.

    ASSIGN tt-importa.linha = ii.
END.

INPUT CLOSE.

FOR EACH tt-importa
    WHERE  tt-importa.linha > 0
    BREAK BY tt-importa.cod_prog_dtsul_base.

    RUN pi-acompanhar IN h-acomp ("Validando/Criando: " + TRIM(tt-importa.cod_prog_dtsul_base)).

    ASSIGN l-erro = NO.

    IF FIRST-OF(tt-importa.cod_prog_dtsul_base) THEN DO:

        ASSIGN l-erro-base = NO
               l-erro      = NO.
        /* Valida se o programa existe */
        IF NOT CAN-FIND(FIRST prog_dtsul NO-LOCK 
                            WHERE prog_dtsul.cod_prog_dtsul = tt-importa.cod_prog_dtsul_base)
        THEN DO: 
            ASSIGN tt-importa.erro = YES
                   tt-importa.msg  = "Programa Conflito Base n�o existe no TOTVS."
                   l-erro-base     = YES.
        END.
        ELSE DO:
            IF NOT CAN-FIND(FIRST ctrl-conflito NO-LOCK                                             
                              WHERE ctrl-conflito.cod_prog_dtsul_base = tt-importa.cod_prog_dtsul_base)
                THEN DO:
                CREATE ctrl-conflito.
                ASSIGN ctrl-conflito.cod_prog_dtsul_base = tt-importa.cod_prog_dtsul_base
                       ctrl-conflito.dat_ult_alter       = TODAY
                       ctrl-conflito.hor_ult_alter       = TIME
                       ctrl-conflito.usuar_ult_alter     = v_cod_usuar_corren.
            END.
        END.
    END.

    IF NOT l-erro-base THEN DO:
        IF CAN-FIND(FIRST prog_dtsul NO-LOCK 
                            WHERE prog_dtsul.cod_prog_dtsul = tt-importa.cod_prog_dtsul)
        THEN DO: 

            IF NOT CAN-FIND(FIRST ctrl-conflito-it WHERE                                             
                                  ctrl-conflito-it.cod_prog_dtsul_base = tt-importa.cod_prog_dtsul_base AND 
                                  ctrl-conflito-it.cod_prog_dtsul      = tt-importa.cod_prog_dtsul) THEN DO:

                CREATE ctrl-conflito-it.
                ASSIGN ctrl-conflito-it.cod_prog_dtsul_base = tt-importa.cod_prog_dtsul_base
                       ctrl-conflito-it.cod_prog_dtsul      = tt-importa.cod_prog_dtsul
                       ctrl-conflito-it.dat_ult_alter       = TODAY
                       ctrl-conflito-it.hor_ult_alter       = TIME
                       ctrl-conflito-it.usuar_ult_alter     = v_cod_usuar_corren
                       ctrl-conflito-it.observacao          = tt-importa.observacao
                       ctrl-conflito-it.razao               = tt-importa.razao.
            END.
            ELSE ASSIGN tt-importa.erro = YES
                        tt-importa.msg  = "Conflito j� cadastrado!"
                        l-erro          = YES.  
        END.
        ELSE ASSIGN tt-importa.erro = YES
                    tt-importa.msg  = "Programa Conflito n�o existe no TOTVS."
                    l-erro          = YES. 

    END.
    ELSE ASSIGN tt-importa.erro = YES
                tt-importa.msg  = "Programa Conflito Base n�o existe no TOTVS.".

    IF NOT l-erro-base AND NOT l-erro THEN
    ASSIGN tt-importa.msg = "Importado com sucesso."
           tt-importa.erro = NO.
END.

OUTPUT TO value(SESSION:TEMP-DIRECTORY + "ESMEN012-log" + STRING(TIME) + ".csv") NO-CONVERT.

PUT UNFORMATTED
    "Linha;Programa Conflito Base;Programa Conflito;Erro;Mensagem" SKIP.

FOR EACH tt-importa:

    PUT UNFORMATTED
        tt-importa.linha ";"
        tt-importa.cod_prog_dtsul_base ";"
        tt-importa.cod_prog_dtsul ";".

    IF tt-importa.erro THEN
    PUT UNFORMATTED 
        "Sim" ";".
    ELSE
    PUT UNFORMATTED
        "N�o" ";".

    PUT UNFORMATTED
        tt-importa.msg ";" SKIP.
END.

OUTPUT CLOSE.

run pi-finalizar in h-acomp.

DO:

  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "Excel.Application" excelAppl. 
  EXCELAPPL:WORKBOOKS:OPEN(SESSION:TEMP-DIRECTORY + "ESMEN012-log.csv") NO-ERROR.
  excelAppl:Visible=true.

  RELEASE OBJECT excelAppl.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel w-cadpaifilho-ambos 
PROCEDURE pi-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var h-acomp           as handle    no-undo.

DEFINE VARIABLE curDir AS CHARACTER.
FILE-INFO:FILE-NAME = ".".
curDir = FILE-INFO:FULL-PATHNAME.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Controle de Acesso Conflitante").

run pi-acompanhar in h-acomp (input "Aguarde...").

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "ESMEN012" + STRING(TIME) + ".csv") NO-CONVERT.

PUT UNFORMATTED
    "Programa Conflito Base;Descri��o Programa Base;Nome Externo Programa Base;Programa Conflito;Descri��o Programa Conflito;Nome Externo Programa Conflito;Observa��o;Raz�o" SKIP.

FOR EACH ctrl-conflito-it NO-LOCK:

    FIND FIRST bf-prog1 WHERE
        bf-prog1.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul_base NO-LOCK NO-ERROR.

    FIND FIRST bf-prog2 WHERE
        bf-prog2.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul NO-LOCK NO-ERROR.

    PUT UNFORMATTED trim(ctrl-conflito-it.cod_prog_dtsul_base) ";"
        trim(bf-prog1.des_prog_dtsul             ) ";"
        trim(bf-prog1.nom_prog_ext               ) ";"
        trim(ctrl-conflito-it.cod_prog_dtsul     ) ";"
        trim(bf-prog2.des_prog_dtsul             ) ";"
        trim(bf-prog2.nom_prog_ext               ) ";"
        TRIM(ctrl-conflito-it.observacao) ";"
        TRIM(ctrl-conflito-it.razao)  SKIP.

END.

FOR EACH ctrl-conflito NO-LOCK:

    FIND FIRST ctrl-conflito-it OF ctrl-conflito NO-LOCK NO-ERROR.

    IF AVAIL ctrl-conflito-it THEN
        NEXT.

    FIND FIRST bf-prog1 WHERE
        bf-prog1.cod_prog_dtsul = ctrl-conflito.cod_prog_dtsul_base NO-LOCK NO-ERROR.

    PUT UNFORMATTED trim(ctrl-conflito.cod_prog_dtsul_base) ";"
        trim(bf-prog1.des_prog_dtsul             ) ";"
        trim(bf-prog1.nom_prog_ext               ) ";"
         ";"
         ";"
         ";" SKIP.

END.


OUTPUT CLOSE.

run pi-finalizar in h-acomp.

DO:

  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "Excel.Application" excelAppl. 
  EXCELAPPL:WORKBOOKS:OPEN(SESSION:TEMP-DIRECTORY + "ESMEN012.csv") NO-ERROR.
  excelAppl:Visible=true.

  RELEASE OBJECT excelAppl.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadpaifilho-ambos  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-paiamb, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadpaifilho-ambos 
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

