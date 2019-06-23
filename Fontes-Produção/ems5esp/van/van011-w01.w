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
{include/i-prgvrs.i VAN011-W01 5.06.99.999}

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

DEF TEMP-TABLE tt-arquivos
    FIELD cFile AS CHAR
    FIELD dTotal AS DEC
    FIELD cConta AS CHAR
    FIELD Id     AS CHAR
    FIELD Proc   AS CHAR
    FIELD lok    AS LOG.

DEF BUFFER b-tt-arquivos FOR tt-arquivos.

DEFINE VARIABLE c-Dir AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cLinha                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-ok         AS LOGICAL     NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR cTipoOrigem AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR r_row_webserver    AS ROWID NO-UNDO.
DEFINE NEW SHARED VAR r_row_param_dir    AS ROWID NO-UNDO.
DEFINE VARIABLE cConta                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-van006  AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFormaPg  AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "X(100)"
    FIELD lSearch   AS LOGICAL.

DEFINE VARIABLE d-data AS DATE        NO-UNDO.
DEFINE VARIABLE c-time AS CHARACTER   NO-UNDO.

FORM tt-arquivos.cFile  FORMAT "x(50)" COLUMN-LABEL "Arquivo"                       
     tt-arquivos.dTotal FORMAT "->>>,>>>,>>>,>>9.99" COLUMN-LABEL "Vlr Tot Arquivo" 
    WITH FRAME f-dados NO-BOX DOWN STREAM-IO WIDTH 80.
           

{include/i-rpvar.i}

assign c-titulo-relat = "Log Pagamentos enviados para Van".

assign c-programa    = "VAN011-W01"
       c-versao      = "5.06"  
       c-revisao     = "00.021".


{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-arquivos

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-arquivos.cFile tt-arquivos.dTotal tt-arquivos.lok   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-arquivos
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-arquivos.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-arquivos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-arquivos


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-del BUTTON-3 bt-ARQ ~
c-arquivo-entrada rs-file BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS c-arquivo-entrada rs-file dValor dValor-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BROWSE-2 

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
DEFINE BUTTON bt-ARQ 
     IMAGE-UP FILE "adeicon/cnfginfo.bmp":U
     LABEL "bt del 2" 
     SIZE 5 BY 1.13 TOOLTIP "Elimina linha selecionada".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.13 TOOLTIP "Elimina linha selecionada".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "image/process_icon.jpg":U
     LABEL "Button 3" 
     SIZE 5 BY 1.13 TOOLTIP "Envia para a Van de Pagamentos".

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dValor AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor arquivo" 
     VIEW-AS FILL-IN 
     SIZE 28 BY .88 NO-UNDO.

DEFINE VARIABLE dValor-2 AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Geral" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE rs-file AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Arquivo", 1,
"Diret¢rio", 2
     SIZE 20 BY .75 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-arquivos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-arquivos.cFile     COLUMN-LABEL "Arquivo" FORMAT "X(50)" WIDTH 50
      tt-arquivos.dTotal    COLUMN-LABEL "TOTAL"   FORMAT "->>>,>>>,>>>,>>9.99"  WIDTH 20
      tt-arquivos.lok       COLUMN-LABEL "Proc"    FORMAT "Sim/N∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 9.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-del AT ROW 1.25 COL 2 WIDGET-ID 30
     BUTTON-3 AT ROW 1.25 COL 64 WIDGET-ID 42
     bt-ARQ AT ROW 2.75 COL 45.14 WIDGET-ID 44
     c-arquivo-entrada AT ROW 2.92 COL 5 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 18
     rs-file AT ROW 3.08 COL 66.14 NO-LABEL WIDGET-ID 36
     dValor AT ROW 4 COL 19 COLON-ALIGNED WIDGET-ID 24
     dValor-2 AT ROW 4 COL 65 COLON-ALIGNED WIDGET-ID 34
     BROWSE-2 AT ROW 5.08 COL 2.14 WIDGET-ID 200
     "Tipo:" VIEW-AS TEXT
          SIZE 5 BY .67 AT ROW 3.04 COL 60.86 WIDGET-ID 40
     rt-button AT ROW 1.08 COL 1.14
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
         HEIGHT             = 14.13
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
/* BROWSE-TAB BROWSE-2 dValor-2 f-cad */
/* SETTINGS FOR BROWSE BROWSE-2 IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN dValor IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       dValor:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN dValor-2 IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       dValor-2:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-arquivos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON DELETE-CHARACTER OF BROWSE-2 IN FRAME f-cad
DO:
 APPLY "choose" TO bt-del.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ARQ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ARQ w-livre
ON CHOOSE OF bt-ARQ IN FRAME f-cad /* bt del 2 */
DO:
  ASSIGN c-dir = "".

 IF INPUT FRAME {&FRAME-NAME} rs-file = 1 THEN DO:
    
      SYSTEM-DIALOG GET-FILE c-dir
           INITIAL-DIR SESSION:TEMP-DIRECTORY
           TITLE "Arquivo remessa" UPDATE l-ok.

    END.
  ELSE DO:

      SYSTEM-DIALOG GET-DIR c-dir
           INITIAL-DIR SESSION:TEMP-DIRECTORY
           TITLE "Diret¢rio arquivos remessa" UPDATE l-ok.
  END.

  IF NOT l-ok THEN RETURN.

  ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir. 

  APPLY "leave" TO c-arquivo-entrada.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-livre
ON CHOOSE OF bt-del IN FRAME f-cad /* Button 1 */
DO:
   GET CURRENT browse-2.

   ASSIGN dvalor-2 = dvalor-2 - tt-arquivos.dTotal.

   DELETE tt-arquivos.

   browse-2:REFRESH() IN FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 w-livre
ON CHOOSE OF BUTTON-3 IN FRAME f-cad /* Button 3 */
DO:
  RUN pi-van.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arquivo-entrada w-livre
ON LEAVE OF c-arquivo-entrada IN FRAME f-cad
DO:
   ASSIGN c-dir = INPUT FRAME {&FRAME-NAME} c-arquivo-entrada.
       
   FILE-INFO:FILE-NAME = c-dir.

   /* Importa todos os arquivos da pasta */
   IF FILE-INFO:FILE-TYPE = "DRW" THEN DO:

      EMPTY TEMP-TABLE tt-listFiles.

      RUN van\van001a.p (INPUT c-dir,  /* Diret¢rio de arquivos pendentes */
                         OUTPUT TABLE tt-listFiles,
                         OUTPUT l-ok).
      IF NOT l-ok THEN NEXT.

      FOR EACH tt-listFiles:

          RUN pi-importa-arquivo (INPUT tt-listFiles.cFile).

      END.
   END.
   ELSE DO: 
      IF SEARCH(FILE-INFO:FILE-NAME) = ? AND FILE-INFO:FILE-NAME <> "" THEN DO:

          MESSAGE "Arquivo informado n∆o Ç v†lido!" SKIP
                 "Por favor verifique o caminho e nome DO arquivo."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
            RETURN NO-APPLY.
      END.
      ELSE RUN pi-importa-arquivo (INPUT FILE-INFO:FILE-NAME).
   END.
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
             bt-del:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY c-arquivo-entrada rs-file dValor dValor-2 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button bt-del BUTTON-3 bt-ARQ c-arquivo-entrada rs-file BROWSE-2 
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
  RUN pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "VAN011-W01" "5.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN c-dir = SESSION:TEMP-DIRECTORY.

  RUN pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importa-arquivo w-livre 
PROCEDURE pi-importa-arquivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER c-arquivo AS CHARACTER   NO-UNDO.

 INPUT FROM VALUE(c-arquivo).
 REPEAT:

      IMPORT UNFORMATTED clinha.

      IF SUBSTR(cLinha,8,1) <> "5" AND SUBSTR(cLinha,8,1) <> "0" AND SUBSTR(cLinha,8,1) <> "1" THEN NEXT.
 
      IF SUBSTR(cLinha,8,1) = "5" THEN 
        ASSIGN dvalor = DEC(SUBSTR(cLinha,24,18)) / 100.

       IF SUBSTR(cLinha,8,1) = "1" THEN 
           ASSIGN cFormaPg = SUBSTR(cLinha,12,2).

      IF SUBSTR(cLinha,8,1) = "0" THEN 
        ASSIGN cconta = SUBSTR(cLinha,59,12).
 END.
 INPUT CLOSE.

      
  IF CAN-FIND(FIRST b-tt-arquivos WHERE
                   b-tt-arquivos.cConta <> cConta) THEN DO:

     MESSAGE "N∆o poder† ser enviado ao mesmo tempo arquivos de contas diferentes!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN ERROR.
  END.

  IF NOT CAN-FIND(FIRST tt-arquivos WHERE 
                        tt-arquivos.cFile = c-arquivo) THEN DO:
     CREATE tt-arquivos.
     ASSIGN tt-arquivos.cFile   = c-arquivo 
            tt-arquivos.dTotal  = dvalor
            tt-arquivos.cConta  = cConta
            dvalor-2            = dvalor-2 + dvalor.  

     IF ((INT(cFormaPg) >= 11 AND 
        INT(cFormaPg) <= 19) OR
        (INT(cFormaPg) >= 21 AND 
        INT(cFormaPg) <= 29))
         THEN ASSIGN tt-arquivos.Id    = "03"
                     tt-arquivos.Proc = "EnviaRemessaTributo".
     ELSE ASSIGN tt-arquivos.Id   = "01"            
                 tt-arquivos.Proc = "EnviaRemessa". 
 END.
 ELSE DO:

     MESSAGE "Arquivo" c-arquivo " j† foi informado!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN ERROR.
 END.
                            
  DISP dvalor dvalor-2 WITH FRAME {&frame-name}.
    
  {&OPEN-QUERY-{&LIST-1}}   
 
  ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         dvalor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-van w-livre 
PROCEDURE pi-van :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
FORM
 cConta     LABEL "Conta Corrente" FORMAT "x(12)"         SPACE(10)             
 dValor-2   LABEL "Total de Pagamentos"  FORMAT "->>>,>>>,>>>,>>9.99"   
 d-data     LABEL "Data" FORMAT "99/99/9999"                           
 c-time     LABEL "Hora" FORMAT "x(08)"                  
 usuar_mestre.nom_usuario FORMAT "x(30)" LABEL "Usu†rio"    SKIP(01)                
WITH FRAME f-tot SIDE-LABELS NO-BOX DOWN STREAM-IO WIDTH 120.   
    
    FIND FIRST usuar_mestre NO-LOCK WHERE
               usuar_mestre.cod_usuario = v_cod_usuar_corren NO-ERROR.

     ASSIGN dValor-2 = 0.
       
    FOR EACH b-tt-arquivos BREAK BY b-tt-arquivos.Id:

       IF FIRST-OF(b-tt-arquivos.Id)
            THEN DO:

           EMPTY TEMP-TABLE tt-listFiles.

           FIND FIRST es_param_webserver NO-LOCK 
            WHERE es_param_webserver.identificador = b-tt-arquivos.Id
              AND es_param_webserver.nomeserv      = b-tt-arquivos.Proc NO-ERROR.
           IF NOT AVAIL es_param_webserver THEN DO:
                       
                   MESSAGE "N∆o foi poss°vel identificar parametro para Web SERVER" SKIP
                           "Identificador:" b-tt-arquivos.Id SKIP
                           "Processo:" b-tt-arquivos.Proc SKIP
                           "Entre em contato com TI para verificar o cadastro van001-w01"  SKIP
                           "Arquivo de pagamento escritural n∆o ser† enviado para a Van."
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
                   RETURN ERROR.
           END.
        
           FIND FIRST es_param_van_dir NO-LOCK 
                WHERE es_param_van_dir.identificador     = b-tt-arquivos.Id  
                AND   es_param_van_dir.nomeserv          = b-tt-arquivos.Proc
                AND INT(es_param_van_dir.cod_cta_corren) = INT(cConta) NO-ERROR.
           IF NOT AVAIL es_param_van_dir THEN DO:
            
              MESSAGE "N∆o foi poss°vel identificar parametro para Web SERVER DIR." SKIP
                           "Identificador:" b-tt-arquivos.Id  SKIP
                           "Processo:" b-tt-arquivos.Proc SKIP
                           "Entre em contato com TI para verificar o cadastro van001-w01"  SKIP
                           "Arquivo de pagamento escritural n∆o ser† enviado para a Van."
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN ERROR.
           END.

           ASSIGN r_row_webserver = ROWID(es_param_webserver)
                  r_row_param_dir = ROWID(es_param_van_dir).  
                
           FOR EACH tt-arquivos WHERE
                    tt-arquivos.id      = b-tt-arquivos.id
                AND tt-arquivos.Proc    = b-tt-arquivos.Proc
                AND tt-arquivos.cConta  = b-tt-arquivos.cConta:

               ASSIGN dValor-2 = dValor-2 + tt-arquivos.dTotal
                      tt-arquivos.lok = YES.
               CREATE tt-listFiles.
               ASSIGN tt-listFiles.cFile = tt-arquivos.cFile.
           END.
                                                                
           RUN van\van006.p PERSISTENT SET h-van006.
                                                                
           RUN pi-remessa IN h-van006 (INPUT TABLE tt-listFiles).
           IF RETURN-VALUE <> "OK" THEN DO:

                MESSAGE RETURN-VALUE SKIP
                        "N∆o foi poss°vel enviar os arquivos!" SKIP
                        "Verifique o log de erros no e-mail."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                RETURN.
           END.
           ELSE DO:

                MESSAGE "Processamento conclu°do!" SKIP
                        "Id: "     b-tt-arquivos.Id    SKIP
                        "Serviáo:" b-tt-arquivos.Proc
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
           END.
       END.
   END.

   ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "hist_pg_" +  b-tt-arquivos.Id + STRING(TODAY,"99999999") + STRING(TIME,"999999") + ".txt"  
          d-data = TODAY
          c-time = STRING(TIME,"hh:mm:ss").

   OUTPUT TO VALUE(c-arquivo) NO-CONVERT.

   view frame f-cabec.
  
   DISP cConta  
         dValor-2 
         d-data
         c-time
         usuar_mestre.nom_usuario
        WITH FRAME f-tot.

   FOR EACH tt-arquivos 
          WHERE tt-arquivos.lok = YES:

        DISP tt-arquivos.cFile 
             tt-arquivos.dTotal
            WITH FRAME f-dados DOWN.
        DOWN WITH FRAME f-dados.
   END.

   PUT SKIP(01).
   view frame f-rodape. 

   OUTPUT CLOSE.

   RUN prgfin\apya599.p (INPUT c-arquivo).
   
   EMPTY TEMP-TABLE tt-arquivos.
            
   browse-2:REFRESH() IN FRAME {&FRAME-NAME}.
   ASSIGN dvalor-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 

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
  {src/adm/template/snd-list.i "tt-arquivos"}

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

