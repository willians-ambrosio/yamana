&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESCE002 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


{cep\ceapi001.i}
{cdp\cd0666.i}


FIND FIRST param-estoq NO-LOCK NO-ERROR.
DEF STREAM s-lista.

DEF VAR l-erro AS LOG.

DEF VAR l-gerou AS LOG.

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
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 RECT-2 RECT-3 c-estab-ini ~
c-estab-fim i-ge-ini i-ge-fim c-fm-codigo-ini c-fm-codigo-fim ~
c-it-codigo-ini c-it-codigo-fim c-local-ini c-local-fim c-depos-ini ~
c-depos-fim d-data bt-executar 
&Scoped-Define DISPLAYED-OBJECTS c-estab-ini c-estab-fim i-ge-ini i-ge-fim ~
c-fm-codigo-ini c-fm-codigo-fim c-it-codigo-ini c-it-codigo-fim c-local-ini ~
c-local-fim c-depos-ini c-depos-fim d-data 

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
       MENU-ITEM mi-imprimir    LABEL "&Relat½rios"    ACCELERATOR "CTRL-P"
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
DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE c-depos-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-depos-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Deposito" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-local-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-local-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Localizacao" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE d-data AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-ini AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo Estoq" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.43 BY 6.17.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.43 BY 4.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.43 BY 1.92.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     c-estab-ini AT ROW 3.13 COL 19 COLON-ALIGNED WIDGET-ID 4
     c-estab-fim AT ROW 3.13 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     i-ge-ini AT ROW 4.04 COL 19 COLON-ALIGNED WIDGET-ID 8
     i-ge-fim AT ROW 4.04 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-fm-codigo-ini AT ROW 4.96 COL 19 COLON-ALIGNED WIDGET-ID 16
     c-fm-codigo-fim AT ROW 4.96 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-it-codigo-ini AT ROW 5.88 COL 19 COLON-ALIGNED WIDGET-ID 22
     c-it-codigo-fim AT ROW 5.88 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     c-local-ini AT ROW 6.79 COL 19 COLON-ALIGNED WIDGET-ID 66
     c-local-fim AT ROW 6.79 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     c-depos-ini AT ROW 7.71 COL 19 COLON-ALIGNED WIDGET-ID 74
     c-depos-fim AT ROW 7.71 COL 40.86 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     d-data AT ROW 9.75 COL 19 COLON-ALIGNED WIDGET-ID 42
     bt-executar AT ROW 14.33 COL 20.86 WIDGET-ID 2
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 6.79 COL 39.14 WIDGET-ID 68
          FONT 1
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 3.13 COL 39.14 WIDGET-ID 12
          FONT 1
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 4.96 COL 39.14 WIDGET-ID 20
          FONT 1
     "Transa‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 9 COL 2.57 WIDGET-ID 52
          FGCOLOR 9 
     "Sele‡Æo" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 2.5 COL 2.57 WIDGET-ID 36
          FGCOLOR 9 
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 5.88 COL 39.14 WIDGET-ID 26
          FONT 1
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 7.71 COL 39.14 WIDGET-ID 80
          FONT 1
     "Aten‡Æo ! Programa dever  ser executado ap¢s o calculo do m‚dio com a data do dia 01 do mˆs" VIEW-AS TEXT
          SIZE 67 BY .54 AT ROW 11.75 COL 7 WIDGET-ID 82
     "|<>|" VIEW-AS TEXT
          SIZE 3 BY .88 AT ROW 4.04 COL 39.14 WIDGET-ID 14
          FONT 1
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.75 COL 1.57 WIDGET-ID 34
     RECT-2 AT ROW 9.25 COL 1.57 WIDGET-ID 50
     RECT-3 AT ROW 13.96 COL 1.57 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.21
         FONT 1 WIDGET-ID 100.


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
         TITLE              = "Gera Arquivo com ACTïs para Acerto"
         HEIGHT             = 15.08
         WIDTH              = 79.57
         MAX-HEIGHT         = 20.04
         MAX-WIDTH          = 97.57
         VIRTUAL-HEIGHT     = 20.04
         VIRTUAL-WIDTH      = 97.57
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Gera Arquivo com ACTïs para Acerto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Gera Arquivo com ACTïs para Acerto */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Executar */
DO:
  RUN pi-validar.

  IF l-erro = NO THEN
  RUN pi-montar-tt.

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
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat½rios */
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
       RUN set-position IN h_p-exihel ( 1.13 , 63.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-estab-ini:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY c-estab-ini c-estab-fim i-ge-ini i-ge-fim c-fm-codigo-ini 
          c-fm-codigo-fim c-it-codigo-ini c-it-codigo-fim c-local-ini 
          c-local-fim c-depos-ini c-depos-fim d-data 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 RECT-2 RECT-3 c-estab-ini c-estab-fim i-ge-ini 
         i-ge-fim c-fm-codigo-ini c-fm-codigo-fim c-it-codigo-ini 
         c-it-codigo-fim c-local-ini c-local-fim c-depos-ini c-depos-fim d-data 
         bt-executar 
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

  {utp/ut9000.i "ESCE002" "2.06.00.000"}

 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN d-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).


  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-montar-tt w-livre 
PROCEDURE pi-montar-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

OUTPUT STREAM s-lista TO VALUE(SESSION:TEMP-DIRECTORY + "movto" + 
                                STRING(DAY  (INPUT FRAME {&FRAME-NAME} d-data),'99') +
                                STRING(MONTH(INPUT FRAME {&FRAME-NAME} d-data),'99') +
                                SUBSTR(STRING(YEAR (INPUT FRAME {&FRAME-NAME} d-data),'9999'),3,2) + ".txt").

ASSIGN l-gerou = NO.
FOR EACH ITEM WHERE
         ITEM.ge-codigo >= INPUT FRAME {&FRAME-NAME} i-ge-ini AND
         ITEM.ge-codigo <= INPUT FRAME {&FRAME-NAME} i-ge-fim AND
         ITEM.fm-codigo >= INPUT FRAME {&FRAME-NAME} c-fm-codigo-ini AND
         ITEM.fm-codigo <= INPUT FRAME {&FRAME-NAME} c-fm-codigo-fim AND
         ITEM.it-codigo >= INPUT FRAME {&FRAME-NAME} c-it-codigo-ini AND
         ITEM.it-codigo <= INPUT FRAME {&FRAME-NAME} c-it-codigo-fim NO-LOCK:

    FOR EACH movto-estoq WHERE
             movto-estoq.cod-estabel >= INPUT FRAME {&FRAME-NAME} c-estab-ini AND
             movto-estoq.cod-estabel <= INPUT FRAME {&FRAME-NAME} c-estab-fim AND
             movto-estoq.cod-localiz >= INPUT FRAME {&FRAME-NAME} c-local-ini AND
             movto-estoq.cod-localiz <= INPUT FRAME {&FRAME-NAME} c-local-fim AND
             movto-estoq.it-codigo    = ITEM.it-codigo AND 
             movto-estoq.cod-depos   >= INPUT FRAME {&FRAME-NAME} c-depos-ini AND
             movto-estoq.cod-depos   <= INPUT FRAME {&FRAME-NAME} c-depos-fim AND 
             movto-estoq.dt-trans     = INPUT FRAME {&FRAME-NAME} d-data AND 
             movto-estoq.esp-docto    = 2:

        ASSIGN l-gerou = YES.

        PUT STREAM s-lista 
             movto-estoq.it-codigo ";"
             movto-estoq.cod-refer ";"
             movto-estoq.cod-estabel ";"
             movto-estoq.cod-depos   ";"
             movto-estoq.lote        ";"
             movto-estoq.cod-localiz ";"
         int(movto-estoq.tipo-trans) ";"
             movto-estoq.un ";"
             movto-estoq.nro-docto ";"
             movto-estoq.cod-emit  ";"
             movto-estoq.nat-oper  ";"
             movto-estoq.conta-contab ";"
             movto-estoq.quantidade  ";"
             movto-estoq.valor-mat-m[1] ";"
             movto-estoq.valor-mat-m[2] ";"
             movto-estoq.valor-mat-m[3] ";"
             movto-estoq.valor-mob-m[1] ";"
             movto-estoq.valor-mob-m[2] ";"
             movto-estoq.valor-mob-m[3] ";"
             movto-estoq.valor-ggf-m[1] ";"
             movto-estoq.valor-ggf-m[2] ";"
             movto-estoq.valor-ggf-m[3] ";"
            SKIP.

                                    
    END. /* FOR EACH movto-estoq WHERE */

END. /* FOR EACH ITEM WHERE */
    
IF l-gerou THEN
    MESSAGE 'Arquivo Gerado em: ' SESSION:TEMP-DIRECTORY + "movto" + 
                                STRING(DAY  (INPUT FRAME {&FRAME-NAME} d-data),'99') +
                                STRING(MONTH(INPUT FRAME {&FRAME-NAME} d-data),'99') +
                                SUBSTR(STRING(YEAR (INPUT FRAME {&FRAME-NAME} d-data),'9999'),3,2) + ".txt" SKIP
            'Favor Verificar !!!' 


        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
    MESSAGE 'NÆo foram encontradas informacoes para geracao, favor verificar !'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

 OUTPUT STREAM s-lista CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validar w-livre 
PROCEDURE pi-validar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

l-erro = NO.
/* VALIDAR SELECAO ***********************************************************************/

IF INPUT FRAME {&FRAME-NAME} c-estab-fim < INPUT FRAME {&FRAME-NAME} c-estab-ini THEN DO:
    MESSAGE 'Estabelecimento final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-estab-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

IF INPUT FRAME {&FRAME-NAME} i-ge-fim < INPUT FRAME {&FRAME-NAME} i-ge-ini THEN DO:
    MESSAGE 'Grupo de estoque final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO i-ge-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

IF INPUT FRAME {&FRAME-NAME} c-fm-codigo-fim < INPUT FRAME {&FRAME-NAME} c-fm-codigo-ini THEN DO:
    MESSAGE 'Familia de materiais final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-fm-codigo-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

IF INPUT FRAME {&FRAME-NAME} c-it-codigo-fim < INPUT FRAME {&FRAME-NAME} c-it-codigo-ini THEN DO:
    MESSAGE 'Item final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-it-codigo-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

IF INPUT FRAME {&FRAME-NAME} c-local-fim < INPUT FRAME {&FRAME-NAME} c-local-ini THEN DO:
    MESSAGE 'Localizacao final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-local-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

IF INPUT FRAME {&FRAME-NAME} c-depos-fim < INPUT FRAME {&FRAME-NAME} c-depos-ini THEN DO:
    MESSAGE 'Deposito final menor que inicial!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-depos-ini IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.

/* VALIDAR TRANSACAO ***********************************************************************/
/*
IF INPUT FRAME {&FRAME-NAME} c-docto = '' THEN DO:
    MESSAGE 'Documento deve ser informado!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-docto IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.
*/
IF param-estoq.mensal-ate >= INPUT FRAME {&FRAME-NAME} d-data THEN DO:
    MESSAGE 'Data informada menor que data do œltimo per­odo calculado!' SKIP
        'Medio calculado at² ' STRING(param-estoq.mensal-ate,'99/99/9999')
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO d-data IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

