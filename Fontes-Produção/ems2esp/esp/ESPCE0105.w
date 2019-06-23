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
def buffer localizacao for ems2cademp.localizacao.

{include/i-prgvrs.i ESPCE0105 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR arquivo AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-registro NO-UNDO
    FIELD cod-estabel LIKE localizacao.cod-estabel        
    FIELD cod-depos   LIKE localizacao.cod-depos  
    FIELD cod-localiz LIKE localizacao.cod-localiz    
    FIELD descricao   LIKE localizacao.descricao
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD sequencia   AS INT FORMAT "99999".  

DEF VAR h-acomp     AS HANDLE       NO-UNDO.
DEF VAR cont        AS INT  INIT 1  NO-UNDO.
DEF VAR c-validacao AS CHAR         NO-UNDO.
DEF VAR l-converte  AS LOG  INIT NO NO-UNDO.
DEF VAR l-erro      AS LOG          NO-UNDO.
DEF VAR extensoes   AS CHAR         NO-UNDO.
DEF VAR dir-padrao  AS CHAR         NO-UNDO.
DEF VAR log-erro    AS CHAR         NO-UNDO.

DEF STREAM s-erro.

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
&Scoped-Define ENABLED-OBJECTS rt-button c-arquivo bt-arquivo bt-importar 
&Scoped-Define DISPLAYED-OBJECTS c-arquivo 

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
       SUB-MENU  mi-programa    LABEL "&Carga Localizacao"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image/im-sea":U
     IMAGE-INSENSITIVE FILE "image/ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-importar 
     LABEL "Importar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE c-arquivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     c-arquivo AT ROW 5.25 COL 13 COLON-ALIGNED WIDGET-ID 2
     bt-arquivo AT ROW 5.25 COL 78 HELP
          "Escolha do nome do arquivo" WIDGET-ID 4
     bt-importar AT ROW 10 COL 39 WIDGET-ID 6
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
    SYSTEM-DIALOG GET-FILE  c-arquivo
        TITLE "Localizar..."
        FILTERS "Todos os arquivos do Excel (*.xl; *.xlsx; *.xlsm; *.xlsb; *.xlam; *.xltx; *.xltm; *.xls; *.xlt; *.htm; *.html; *.mht; *.mhtml; *.xml; *.xla; *.xlm; *.xlw; *.odc; *.uxdc; *.ods)" "*.xl; *.xlsx; *.xlsm; *.xlsb; *.xlam; *.xltx; *.xltm; *.xls; *.xlt; *.htm; *.html; *.mht; *.mhtml; *.xml; *.xla; *.xlm; *.xlw; *.odc; *.uxdc; *.ods", "CSV (*.csv)" "*.csv"
        MUST-EXIST
        INITIAL-DIR dir-padrao
        USE-FILENAME.

    DISPLAY c-arquivo
       WITH FRAME {&FRAME-NAME}.

    ASSIGN arquivo = c-arquivo:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-importar w-livre
ON CHOOSE OF bt-importar IN FRAME f-cad /* Importar */
DO:
    ASSIGN extensoes  = ".xl,lsx,lsm,lsb,lam,ltx,ltm,xls,xlt,htm,tml,mht,tml,xml,xla,xlm,xlw,odc,xdc,ods,csv"
           dir-padrao = SESSION:TEMP-DIRECTORY
           log-erro   = dir-padrao + "log-carga-localizacao.txt".

  IF SEARCH(INPUT FRAME {&FRAME-NAME} c-arquivo) <> ? THEN DO:
/*
      IF LOOKUP(SUBSTR(arquivo,LENGTH(arquivo) - 2, 3, "CHARACTER"), extensoes) = 0 THEN DO:
          MESSAGE "Extens∆o de arquivo inv†lida!" SKIP
                  "Favor selecionar um arquivo do tipo Excel."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.

          APPLY "entry":U TO c-arquivo IN FRAME {&FRAME-NAME}.
          RETURN NO-APPLY.
      END.
*/

      RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
      RUN pi-inicializar IN h-acomp(INPUT "Carga de dados").

      OUTPUT STREAM s-erro TO VALUE(log-erro).

      PUT STREAM s-erro "Um erro ocorreu durante a importacao." SKIP 
                        "O(s) seguinte(s) registro(s) nao puderam ser importado(s):" SKIP(2)
                        "LINHA ESTABELEC DEPOSITO LOCALIZACAO ITEM             MOTIVO".
      ASSIGN l-erro = NO.

      IF SUBSTR(arquivo, LENGTH(arquivo) - 2, 3) <> "csv" THEN
          RUN pi-converte.

      RUN pi-importar.
      
      IF l-converte AND 
         SEARCH(arquivo) <> ? THEN
          OS-DELETE VALUE (arquivo).

      RUN pi-finalizar IN h-acomp.
      OUTPUT STREAM s-erro CLOSE.

      IF l-erro THEN
          DOS SILENT START VALUE(log-erro).
      ELSE DO:
          OUTPUT STREAM s-erro TO VALUE(log-erro).
          PUT STREAM s-erro "Importacao completada com sucesso!".
          OUTPUT STREAM s-erro CLOSE.

          DOS SILENT START VALUE(log-erro).
      END.
      
      APPLY "close":U TO THIS-PROCEDURE.
  END.
  ELSE DO:
      MESSAGE "Arquivo n∆o encontrado!"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.

      APPLY "entry":U TO c-arquivo IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arquivo w-livre
ON LEAVE OF c-arquivo IN FRAME f-cad /* Arquivo */
DO:
  ASSIGN arquivo = c-arquivo:SCREEN-VALUE.
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
ON MENU-DROP OF MENU mi-programa /* Carga Localizacao */
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
             c-arquivo:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY c-arquivo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button c-arquivo bt-arquivo bt-importar 
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

  {utp/ut9000.i "Carga de Localizacao" "ESPCE0105" "2.06.00.000"} 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-converte w-livre 
PROCEDURE pi-converte :
/*------------------------------------------------------------------------------
  Purpose: Converte um arquivo do Excel em CSV (.csv)    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR excelappl   AS COM-HANDLE NO-UNDO.
    DEF VAR ChWorkSheet AS COM-HANDLE NO-UNDO.

    RUN pi-acompanhar IN h-acomp(INPUT "Convertendo arquivo de entrada").

    MESSAGE "Log de Erro: " arquivo
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE "excel.application" excelappl.
    excelappl:workbooks:ADD(arquivo).
    excelappl:worksheets:ITEM(1):SELECT.
    chWorkSheet = excelappl:Sheets:Item(1).
    excelappl:VISIBLE = FALSE.

    excelappl:APPLICATION:DisplayAlerts = FALSE.
    excelappl:Workbooks:Item(1):SaveAs(dir-padrao + "temp-conv.csv",6,,,,,).
    excelappl:APPLICATION:QUIT().

    RELEASE OBJECT excelappl.
    RELEASE OBJECT chWorksheet.

    ASSIGN arquivo    = dir-padrao + "temp-conv.csv"
           l-converte = YES.
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
    FOR EACH tt-registro:
        DELETE tt-registro.
    END.

    INPUT FROM VALUE (arquivo).
    REPEAT WHILE TRUE:
       CREATE tt-registro.
       IMPORT DELIMITER ";" tt-registro EXCEPT tt-registro.sequencia.

        ASSIGN tt-registro.cod-estabel = trim(tt-registro.cod-estabel)
               tt-registro.cod-depos   = trim(tt-registro.cod-depos)
               tt-registro.cod-localiz = trim(tt-registro.cod-localiz)
               tt-registro.descricao   = trim(tt-registro.descricao)
               tt-registro.it-codigo   = trim(tt-registro.it-codigo).

       IF tt-registro.cod-estabel = "" OR
          tt-registro.cod-estabel = ? THEN
           LEAVE.

       RUN pi-acompanhar IN h-acomp(INPUT "Lendo entrada" + STRING(tt-registro.cod-localiz)).

       ASSIGN tt-registro.sequencia = cont
              cont = cont + 1.
    END.
    INPUT CLOSE.

    FOR EACH tt-registro NO-LOCK
       WHERE tt-registro.sequencia = 0
          OR tt-registro.sequencia = 1:
        DELETE tt-registro.
    END.

    FOR EACH tt-registro:
        RUN pi-acompanhar IN h-acomp(INPUT "Armazenando dados" + STRING(tt-registro.cod-localiz)).

        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = tt-registro.cod-estabel NO-ERROR.
        IF NOT AVAIL estabelec THEN DO:
            PUT STREAM s-erro SKIP tt-registro.sequencia
                              SPACE(1) tt-registro.cod-estabel
                              SPACE(7) tt-registro.cod-depos
                              SPACE(6) tt-registro.cod-localiz
                              SPACE(2) tt-registro.it-codigo
                              SPACE "Estabelecimento nao cadastrado.".

            ASSIGN l-erro = YES.
            NEXT.
        END.

        FIND FIRST deposito NO-LOCK
             WHERE deposito.cod-depos = tt-registro.cod-depos NO-ERROR.
        IF NOT AVAIL deposito THEN DO:
            PUT STREAM s-erro SKIP tt-registro.sequencia
                              SPACE(1) tt-registro.cod-estabel
                              SPACE(7) tt-registro.cod-depos
                              SPACE(6) tt-registro.cod-localiz
                              SPACE(2) tt-registro.it-codigo
                              SPACE "Deposito nao cadastrado.".

            ASSIGN l-erro = YES.
            NEXT.
        END.

        IF tt-registro.it-codigo <> "" THEN DO:
            FIND FIRST ITEM NO-LOCK
                 WHERE ITEM.it-codigo = tt-registro.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN DO:
                PUT STREAM s-erro SKIP tt-registro.sequencia
                                  SPACE(1) tt-registro.cod-estabel
                                  SPACE(7) tt-registro.cod-depos
                                  SPACE(6) tt-registro.cod-localiz
                                  SPACE(2) tt-registro.it-codigo
                                  SPACE "Item nao cadastrado.".

                ASSIGN l-erro = YES.
                NEXT.
            END.

            FIND FIRST item-uni-estab EXCLUSIVE-LOCK
                 WHERE item-uni-estab.it-codigo   = tt-registro.it-codigo
                   AND item-uni-estab.cod-estabel = tt-registro.cod-estabel NO-ERROR.
            IF NOT AVAIL item-uni-estab THEN DO:
                PUT STREAM s-erro SKIP tt-registro.sequencia
                                  SPACE(1) tt-registro.cod-estabel
                                  SPACE(7) tt-registro.cod-depos
                                  SPACE(6) tt-registro.cod-localiz
                                  SPACE(2) tt-registro.it-codigo
                                  SPACE "Itens x Estabelecimento inexistente.".

                ASSIGN l-erro = YES.
                NEXT.
            END.
            ELSE
                ASSIGN item-uni-estab.cod-localiz = tt-registro.cod-localiz.
        END.

        FIND FIRST localizacao NO-LOCK
             WHERE localizacao.cod-estabel = tt-registro.cod-estabel
               AND localizacao.cod-depos   = tt-registro.cod-depos
               AND localizacao.cod-localiz = tt-registro.cod-localiz NO-ERROR.
        IF NOT AVAIL localizacao THEN DO:
            CREATE localizacao.
            ASSIGN localizacao.cod-estabel = tt-registro.cod-estabel
                   localizacao.cod-depos   = tt-registro.cod-depos
                   localizacao.cod-localiz = tt-registro.cod-localiz
                   localizacao.descricao   = tt-registro.descricao.
        END.
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

