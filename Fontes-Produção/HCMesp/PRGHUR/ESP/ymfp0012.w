&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          dthrpmg          PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt_tip_func_corporativo NO-UNDO LIKE tip_func_corporativo
FIELD nom_pessoa_fisic LIKE funcionario.nom_pessoa_fisic.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{utp/ut-glob.i}
DEFINE VARIABLE wh-pesquisa AS HANDLE      NO-UNDO.

DEFINE VARIABLE state AS CHARACTER INITIAL "create" NO-UNDO.
DEFINE VARIABLE v_mes_ano_corrente AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_tip_func_corporativo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_tip_func_corporativo

/* Definitions for BROWSE br_tip_func_corporativo                       */
&Scoped-define FIELDS-IN-QUERY-br_tip_func_corporativo ~
tt_tip_func_corporativo.cdn_empresa tt_tip_func_corporativo.cdn_estab ~
tt_tip_func_corporativo.cdn_funcionario tt_tip_func_corporativo.nom_pessoa_fisic ~
tt_tip_func_corporativo.dt_inicio_corp tt_tip_func_corporativo.dt_fim_corp 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_tip_func_corporativo 
&Scoped-define QUERY-STRING-br_tip_func_corporativo FOR EACH tt_tip_func_corporativo NO-LOCK ~
    BY tt_tip_func_corporativo.cdn_empresa ~
       BY tt_tip_func_corporativo.cdn_estab ~
        BY tt_tip_func_corporativo.cdn_funcionario INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_tip_func_corporativo OPEN QUERY br_tip_func_corporativo FOR EACH tt_tip_func_corporativo NO-LOCK ~
    BY tt_tip_func_corporativo.cdn_empresa ~
       BY tt_tip_func_corporativo.cdn_estab ~
        BY tt_tip_func_corporativo.cdn_funcionario INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_tip_func_corporativo ~
tt_tip_func_corporativo
&Scoped-define FIRST-TABLE-IN-QUERY-br_tip_func_corporativo tt_tip_func_corporativo


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_tip_func_corporativo}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-empresa fi-estab fi-matricula ~
fi-dt-inicio-corp fi-dt-fim-corp bt-save bt-cancel bt-modificar bt-excluir ~
br_tip_func_corporativo bt-sair rt-button RECT-18 
&Scoped-Define DISPLAYED-OBJECTS fi-empresa fi-estab fi-matricula ~
fi-dt-inicio-corp fi-dt-fim-corp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel 
     IMAGE-UP FILE "image/im-can":U
     IMAGE-INSENSITIVE FILE "image/ii-can":U
     LABEL "" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-excluir 
     LABEL "Excluir" 
     SIZE 14 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "Modificar" 
     SIZE 14 BY 1.

DEFINE BUTTON bt-sair 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exi" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar Resultado"
     FONT 4.

DEFINE BUTTON bt-save 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE fi-dt-fim-corp AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Data Fim" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-inicio-corp AS DATE FORMAT "99/99/9999":U 
     LABEL "Data In¡cio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(3)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-matricula AS INTEGER FORMAT "zzzzzzz9":U INITIAL 0 
     LABEL "Matr¡cula" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 3.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_tip_func_corporativo FOR 
      tt_tip_func_corporativo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_tip_func_corporativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_tip_func_corporativo W-Win _STRUCTURED
  QUERY br_tip_func_corporativo NO-LOCK DISPLAY
      tt_tip_func_corporativo.cdn_empresa FORMAT "x(3)":U
      tt_tip_func_corporativo.cdn_estab FORMAT "x(5)":U
      tt_tip_func_corporativo.cdn_funcionario FORMAT "zzzzzzzz9":U
      tt_tip_func_corporativo.nom_pessoa_fisic FORMAT "x(40)":U
      tt_tip_func_corporativo.dt_inicio_corp FORMAT "99/99/9999":U
      tt_tip_func_corporativo.dt_fim_corp FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 13 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-empresa AT ROW 3 COL 11.72 COLON-ALIGNED WIDGET-ID 30
     fi-estab AT ROW 3 COL 37.86 COLON-ALIGNED WIDGET-ID 32
     fi-matricula AT ROW 3 COL 58.72 COLON-ALIGNED WIDGET-ID 34
     fi-dt-inicio-corp AT ROW 4.5 COL 11.72 COLON-ALIGNED WIDGET-ID 36
     fi-dt-fim-corp AT ROW 4.5 COL 38 COLON-ALIGNED WIDGET-ID 38
     bt-save AT ROW 4.25 COL 66.29 WIDGET-ID 40
     bt-cancel AT ROW 4.25 COL 71 WIDGET-ID 42
     bt-modificar AT ROW 19.17 COL 1.57 WIDGET-ID 46
     bt-excluir AT ROW 19.17 COL 16 WIDGET-ID 48
     br_tip_func_corporativo AT ROW 6 COL 1.72 WIDGET-ID 200
     bt-sair AT ROW 1.33 COL 75.57 WIDGET-ID 10
     rt-button AT ROW 1.21 COL 1.57 WIDGET-ID 2
     RECT-18 AT ROW 2.71 COL 1.72 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 19.5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt_tip_func_corporativo T "?" NO-UNDO dthrpmg tip_func_corporativo
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Manuten‡Æo Funcion rios Corporativos"
         HEIGHT             = 19.42
         WIDTH              = 80
         MAX-HEIGHT         = 23.17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 23.17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br_tip_func_corporativo bt-excluir F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_tip_func_corporativo
/* Query rebuild information for BROWSE br_tip_func_corporativo
     _TblList          = "Temp-Tables.tt_tip_func_corporativo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt_tip_func_corporativo.cdn_empresa|yes,Temp-Tables.tt_tip_func_corporativo.cdn_estab|yes,Temp-Tables.tt_tip_func_corporativo.cdn_funcionario|yes"
     _FldNameList[1]   = Temp-Tables.tt_tip_func_corporativo.cdn_empresa
     _FldNameList[2]   = Temp-Tables.tt_tip_func_corporativo.cdn_estab
     _FldNameList[3]   = Temp-Tables.tt_tip_func_corporativo.cdn_funcionario
     _FldNameList[4]   = Temp-Tables.tt_tip_func_corporativo.dt_inicio_corp
     _FldNameList[5]   = Temp-Tables.tt_tip_func_corporativo.dt_fim_corp
     _Query            is OPENED
*/  /* BROWSE br_tip_func_corporativo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Manuten‡Æo Funcion rios Corporativos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Manuten‡Æo Funcion rios Corporativos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_tip_func_corporativo
&Scoped-define SELF-NAME br_tip_func_corporativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_tip_func_corporativo W-Win
ON MOUSE-SELECT-DBLCLICK OF br_tip_func_corporativo IN FRAME F-Main
DO:
  APPLY "CHOOSE" TO bt-modificar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel W-Win
ON CHOOSE OF bt-cancel IN FRAME F-Main
DO:
  RUN pi-limpar-campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excluir W-Win
ON CHOOSE OF bt-excluir IN FRAME F-Main /* Excluir */
DO:
  run utp/ut-msgs.p (input "show":U, input 3051, input "Deseja excluir o registro?").
    if return-value = "yes" then do:
        RUN pi-valida-folha-calculada (INPUT tt_tip_func_corporativo.cdn_funcionario).
        IF RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p(INPUT "show", INPUT 17006, INPUT "NÆo foi poss¡vel excluir este funcion rio" + "~~" 
                              + "NÆo foi poss¡vel excluir o relacionamento do funcion rio corporativo porque a folha do funcion rio j  est  calculada para o mˆs " + v_mes_ano_corrente).
            RETURN "NOK":U.
        END.
        ELSE DO:
            FOR FIRST tip_func_corporativo
                WHERE tip_func_corporativo.cdn_estab = tt_tip_func_corporativo.cdn_estab
                  AND tip_func_corporativo.cdn_empresa = tt_tip_func_corporativo.cdn_empresa
                  AND tip_func_corporativo.cdn_funcionario = tt_tip_func_corporativo.cdn_funcionario
                  AND tip_func_corporativo.dt_inicio_corp = tt_tip_func_corporativo.dt_inicio_corp EXCLUSIVE-LOCK:
                DELETE tip_func_corporativo.
                DELETE tt_tip_func_corporativo.
            END.
            {&OPEN-QUERY-br_tip_func_corporativo}
            RUN pi-limpar-campos.
        END.                 
    END.                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar W-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-modificar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair W-Win
ON CHOOSE OF bt-sair IN FRAME F-Main /* Exi */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-save W-Win
ON CHOOSE OF bt-save IN FRAME F-Main
DO:
  RUN pi-validate.
  IF RETURN-VALUE = "OK" THEN DO:
      RUN pi-salvar.
      RUN pi-carrega-dados.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa W-Win
ON F5 OF fi-empresa IN FRAME F-Main /* Empresa */
DO:
  {include/zoomvar.i &prog-zoom=object/sopy/zoom/z04py197.w
                       &campo=fi-empresa
                       &campozoom=ep-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-empresa IN FRAME F-Main /* Empresa */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-estab W-Win
ON F5 OF fi-estab IN FRAME F-Main /* Estabelecimento */
DO:
  assign l-implanta = NO.

  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z02py298.w"
                     &campo=fi-estab
                     &campozoom=cdn_estab
                     &parametros="run pi-seta-inicial in wh-pesquisa (input frame {&frame-name} fi-empresa)".}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-estab W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-estab IN FRAME F-Main /* Estabelecimento */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-matricula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matricula W-Win
ON F5 OF fi-matricula IN FRAME F-Main /* Matr¡cula */
DO:
    {include/zoomvar.i &prog-zoom="object/sopy/zoom/z03py085.w"
                       &campo=fi-matricula
                       &campozoom=cdn_funcionario
                       &parametros="run pi-seta-inicial in wh-pesquisa 
                                    (input fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} ,
                                     input frame {&FRAME-NAME} fi-estab)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matricula W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-matricula IN FRAME F-Main /* Matr¡cula */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
fi-estab:load-mouse-pointer("image/lupa.cur").
fi-empresa:load-mouse-pointer("image/lupa.cur").
fi-matricula:load-mouse-pointer("image/lupa.cur").
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-empresa fi-estab fi-matricula fi-dt-inicio-corp fi-dt-fim-corp 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi-empresa fi-estab fi-matricula fi-dt-inicio-corp fi-dt-fim-corp 
         bt-save bt-cancel bt-modificar bt-excluir br_tip_func_corporativo 
         bt-sair rt-button RECT-18 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-carrega-dados.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados W-Win 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt_tip_func_corporativo.

FOR EACH tip_func_corporativo:
    CREATE tt_tip_func_corporativo.
    BUFFER-COPY tip_func_corporativo TO tt_tip_func_corporativo.
    FIND FIRST funcionario OF tip_func_corporativo NO-LOCK NO-ERROR.
    IF AVAIL funcionario THEN
        ASSIGN tt_tip_func_corporativo.nom_pessoa_fisic = funcionario.nom_pessoa_fisic.
END.

{&OPEN-QUERY-br_tip_func_corporativo}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpar-campos W-Win 
PROCEDURE pi-limpar-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
       fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""           
       fi-matricula:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""     
       fi-dt-inicio-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
       fi-dt-fim-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "31/12/9999"
       state = "create".

ASSIGN fi-estab:SENSITIVE IN FRAME {&FRAME-NAME} = YES
       fi-empresa:SENSITIVE IN FRAME {&FRAME-NAME} = YES
       fi-matricula:SENSITIVE IN FRAME {&FRAME-NAME} = YES
       fi-dt-inicio-corp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
       fi-dt-fim-corp:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-modificar W-Win 
PROCEDURE pi-modificar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN state = "update".

ASSIGN fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt_tip_func_corporativo.cdn_estab
       fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt_tip_func_corporativo.cdn_empresa
       fi-matricula:SCREEN-VALUE IN FRAME {&FRAME-NAME} = String(tt_tip_func_corporativo.cdn_funcionario)
       fi-dt-inicio-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = String(tt_tip_func_corporativo.dt_inicio_corp) 
       fi-dt-fim-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = String(tt_tip_func_corporativo.dt_fim_corp).     

ASSIGN fi-estab:SENSITIVE IN FRAME {&FRAME-NAME} = NO
       fi-empresa:SENSITIVE IN FRAME {&FRAME-NAME} = NO
       fi-matricula:SENSITIVE IN FRAME {&FRAME-NAME} = NO
       fi-dt-inicio-corp:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

FIND FIRST tip_func_corporativo
     WHERE tip_func_corporativo.cdn_estab = tt_tip_func_corporativo.cdn_estab 
       and tip_func_corporativo.cdn_empresa = tt_tip_func_corporativo.cdn_empresa
       and tip_func_corporativo.cdn_funcionario = tt_tip_func_corporativo.cdn_funcionario
       AND tip_func_corporativo.dt_inicio_corp = tt_tip_func_corporativo.dt_inicio_corp
       and tip_func_corporativo.dt_fim_corp = tt_tip_func_corporativo.dt_fim_corp EXCLUSIVE-LOCK NO-ERROR.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salvar W-Win 
PROCEDURE pi-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF state = "create" THEN DO:
    CREATE tip_func_corporativo.
    ASSIGN tip_func_corporativo.cdn_estab = INPUT FRAME {&FRAME-NAME} fi-estab
           tip_func_corporativo.cdn_empresa = INPUT FRAME {&FRAME-NAME} fi-empresa
           tip_func_corporativo.cdn_funcionario = INPUT FRAME {&FRAME-NAME} fi-matricula
           tip_func_corporativo.dt_inicio_corp = INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp
           tip_func_corporativo.dt_fim_corp = INPUT FRAME {&FRAME-NAME} fi-dt-fim-corp.
END.
ELSE DO: /*update*/
    IF AVAIL tip_func_corporativo THEN DO:
        ASSIGN tip_func_corporativo.cdn_estab = INPUT FRAME {&FRAME-NAME} fi-estab
               tip_func_corporativo.cdn_empresa = INPUT FRAME {&FRAME-NAME} fi-empresa
               tip_func_corporativo.cdn_funcionario = INPUT FRAME {&FRAME-NAME} fi-matricula
               tip_func_corporativo.dt_inicio_corp = INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp
               tip_func_corporativo.dt_fim_corp = INPUT FRAME {&FRAME-NAME} fi-dt-fim-corp.
    END.
END.

RUN pi-limpar-campos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-folha-calculada W-Win 
PROCEDURE pi-valida-folha-calculada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_cdn_funcionario AS INTEGER NO-UNDO.

    FOR FIRST funcionario
        WHERE funcionario.cdn_funcionario = p_cdn_funcionario NO-LOCK:
        FOR FIRST param_empres_rh
            WHERE param_empres_rh.cdn_empresa = funcionario.cdn_empresa NO-LOCK:
            
            ASSIGN v_mes_ano_corrente = string(PARAM_empres_rh.num_mes_refer_calc_efetd) + '/' + string(PARAM_empres_rh.num_ano_refer_calc_efetd).

            IF CAN-FIND(FIRST movto_calcul_func
                        WHERE movto_calcul_func.cdn_empresa = funcionario.cdn_empresa
                          AND movto_calcul_func.cdn_estab = funcionario.cdn_estab
                          AND movto_calcul_func.cdn_funcionario = funcionario.cdn_funcionario
                          AND movto_calcul_func.num_ano_refer_fp = PARAM_empres_rh.num_ano_refer_calc_efetd
                          AND movto_calcul_func.num_mes_refer_fp = PARAM_empres_rh.num_mes_refer_calc_efetd
                          AND movto_calcul_func.idi_tip_fp = 1
                          AND movto_calcul_func.qti_parc_habilit_calc_fp = 9) THEN DO:
                RETURN "NOK":U.
            END.               
        END.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate W-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF NOT CAN-FIND(FIRST empresa
                WHERE empresa.ep-codigo = fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
    RUN utp/ut-msgs.p (INPUT "Show", INPUT 2, INPUT "Empresa").
    APPLY "ENTRY":U TO fi-empresa IN FRAME {&FRAME-NAME}.
    RETURN "NOK":U.
END.

IF NOT CAN-FIND(FIRST rh_estab
                WHERE rh_estab.cdn_estab = fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
    RUN utp/ut-msgs.p (INPUT "Show", INPUT 2, INPUT "Estabelecimento").
    
    APPLY "ENTRY":U TO fi-estab IN FRAME {&FRAME-NAME}.

    RETURN "NOK":U.
END.

IF NOT CAN-FIND(FIRST rh_estab
                WHERE rh_estab.cdn_estab = fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  AND rh_estab.cdn_empresa = fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) THEN DO:
    run utp/ut-msgs.p (input "show":U, input 17006, INPUT  "Estabelecimento nÆo vinculado … empresa"  
                           + "~~" + "O estabelecimento " + fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                           + " nÆo possui vinculo com a empresa " + fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    APPLY "ENTRY":U TO fi-estab IN FRAME {&FRAME-NAME}.
        
    RETURN "NOK":U.
END.

IF NOT CAN-FIND(FIRST funcionario
                WHERE funcionario.cdn_funcionario = int(fi-matricula:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
    RUN utp/ut-msgs.p (INPUT "Show", INPUT 2, INPUT "Funcion rio").

    APPLY "ENTRY":U TO fi-matricula IN FRAME {&FRAME-NAME}.

    RETURN "NOK":U.
END.

IF NOT CAN-FIND(FIRST funcionario
                WHERE funcionario.cdn_funcionario = int(fi-matricula:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                  AND funcionario.cdn_estab = fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                  AND funcionario.cdn_empresa = fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) THEN DO:
    run utp/ut-msgs.p (input "show":U, input 17006, INPUT  "Funcion rio nÆo vinculado … Estabelecimento e Empresa"  
                           + "~~" + "O funcion rio de matr¡cula " + fi-matricula:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                           + " nÆo possui vinculo com a empresa " + fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                           + " e com o estabelecimento " + fi-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    
    APPLY "ENTRY":U TO fi-matricula IN FRAME {&FRAME-NAME}.

    RETURN "NOK":U.
END.

IF fi-dt-inicio-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
   OR trim(fi-dt-inicio-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = "/  /" THEN DO:
    RUN utp/ut-msgs.p (INPUT "Show", INPUT 17006, INPUT "Data inicial inv lida." + "~~" + "Informe uma data inicial.").

    APPLY "ENTRY":U TO fi-dt-inicio-corp IN FRAME {&FRAME-NAME}.

    RETURN "NOK":U.
END.

IF DATE(fi-dt-inicio-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > date(fi-dt-fim-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
    RUN utp/ut-msgs.p (INPUT "Show", INPUT 17006, INPUT "Faixa de datas inv lida." + "~~" + "A Data In¡cio deve ser menor do que a Data Fim").

    APPLY "ENTRY":U TO fi-dt-inicio-corp IN FRAME {&FRAME-NAME}.

    RETURN "NOK":U.
END.

IF state = "create" THEN DO:
    FIND FIRST tip_func_corporativo
         WHERE tip_func_corporativo.cdn_estab = INPUT FRAME {&FRAME-NAME} fi-estab               
           and tip_func_corporativo.cdn_empresa = INPUT FRAME {&FRAME-NAME} fi-empresa           
           and tip_func_corporativo.cdn_funcionario = INPUT FRAME {&FRAME-NAME} fi-matricula
           and tip_func_corporativo.dt_inicio_corp = INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp EXCLUSIVE-LOCK NO-ERROR.      
    IF AVAIL tip_func_corporativo THEN DO:
        RUN utp/ut-msgs.p (INPUT "Show", INPUT 1, INPUT "Funcion rio Corporativo").
    RETURN "NOK":U.
    END.


    FIND FIRST tip_func_corporativo
         WHERE tip_func_corporativo.cdn_estab = INPUT FRAME {&FRAME-NAME} fi-estab               
           and tip_func_corporativo.cdn_empresa = INPUT FRAME {&FRAME-NAME} fi-empresa           
           and tip_func_corporativo.cdn_funcionario = INPUT FRAME {&FRAME-NAME} fi-matricula

           AND ((tip_func_corporativo.dt_inicio_corp <= INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp
                AND tip_func_corporativo.dt_fim_corp >= INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp) 
                OR
                (tip_func_corporativo.dt_inicio_corp <= INPUT FRAME {&FRAME-NAME} fi-dt-fim-corp
                AND tip_func_corporativo.dt_fim_corp >= INPUT FRAME {&FRAME-NAME} fi-dt-fim-corp)
                OR 
                (tip_func_corporativo.dt_inicio_corp >= INPUT FRAME {&FRAME-NAME} fi-dt-inicio-corp
                AND tip_func_corporativo.dt_fim_corp <= INPUT FRAME {&FRAME-NAME} fi-dt-fim-corp)) EXCLUSIVE-LOCK NO-ERROR.      
    IF AVAIL tip_func_corporativo THEN DO:
        RUN utp/ut-msgs.p (INPUT "Show", INPUT 17006, INPUT "Per¡odo incorreto" + "~~" 
                           + "Verifique as datas inicial e final. O per¡odo informado sobrepäe outro registro do mesmo funcion rio" ).
        RETURN "NOK":U.
    END.
END.


RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt_tip_func_corporativo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

