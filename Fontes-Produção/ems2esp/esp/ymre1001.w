&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMRE1001 2.06.00.001}

/* Chamada a include do gerenciador de licen�as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m�dulo>:  Informar qual o m�dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m�dulo>}
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

DEFINE INPUT  PARAMETER serie-docto  like docum-est.serie-docto.  
DEFINE INPUT  PARAMETER nro-docto    like docum-est.nro-docto.   
DEFINE INPUT  PARAMETER cod-emitente like docum-est.cod-emitente.
DEFINE INPUT  PARAMETER nat-operacao like docum-est.nat-operacao.

DEFINE BUFFER bes-item-doc-est-natoper FOR es-item-doc-est-natoper.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 cfop-orig modelo-cfe ~
emit-crt aliq-icms benef-icms benef-pis-cof bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-nro-docto i-cod-emitente c-serie-docto ~
c-nat-operacao cfop-orig modelo-cfe emit-crt aliq-icms benef-icms ~
benef-pis-cof 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE emit-crt AS CHARACTER FORMAT "X(256)":U INITIAL "Trib Normal" 
     LABEL "Tribut." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Trib Simples","Trib Normal","RED. ICMS" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE aliq-icms AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "Aliq. ICMS" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-operacao AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nat. Operacao" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nro-docto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nro. Docto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-docto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serie Docto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE cfop-orig AS CHARACTER FORMAT "X(10)":U 
     LABEL "CFOP Orig." 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Cod. Emitente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE modelo-cfe AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N�o Aplicavel", 1,
"NFe", 2,
"CTe", 3
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 3.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 7.

DEFINE VARIABLE benef-icms AS LOGICAL INITIAL no 
     LABEL "Benef. ICMS" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE benef-pis-cof AS LOGICAL INITIAL no 
     LABEL "Benef. PIS/COF" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-nro-docto AT ROW 1.75 COL 18.57 COLON-ALIGNED WIDGET-ID 2
     i-cod-emitente AT ROW 1.75 COL 51.72 COLON-ALIGNED WIDGET-ID 6
     c-serie-docto AT ROW 2.75 COL 18.57 COLON-ALIGNED WIDGET-ID 4
     c-nat-operacao AT ROW 2.75 COL 51.72 COLON-ALIGNED WIDGET-ID 8
     cfop-orig AT ROW 5.25 COL 18.43 COLON-ALIGNED WIDGET-ID 16
     modelo-cfe AT ROW 6.25 COL 21 NO-LABEL WIDGET-ID 20
     emit-crt AT ROW 7.21 COL 18.43 COLON-ALIGNED WIDGET-ID 26
     aliq-icms AT ROW 8.42 COL 18.43 COLON-ALIGNED WIDGET-ID 28
     benef-icms AT ROW 9.5 COL 20 WIDGET-ID 30
     benef-pis-cof AT ROW 10.5 COL 20 WIDGET-ID 32
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 14
     bt-ajuda AT ROW 12.21 COL 69
     "Modelo NFe/CTe:" VIEW-AS TEXT
          SIZE 16.14 BY .67 AT ROW 6.33 COL 4.86 WIDGET-ID 24
     "Chave Nota:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 1 COL 2.57 WIDGET-ID 12
     RECT-1 AT ROW 12 COL 2
     RECT-2 AT ROW 1.25 COL 2 WIDGET-ID 10
     RECT-3 AT ROW 4.75 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Altera��o CFOP Origem"
         HEIGHT             = 12.54
         WIDTH              = 80
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN c-nat-operacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nro-docto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie-docto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-cod-emitente IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Altera��o CFOP Origem */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Altera��o CFOP Origem */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    FOR FIRST es-natoper-rec 
        WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK:
    END.

    FOR EACH item-doc-est NO-LOCK
       WHERE item-doc-est.serie-docto  = serie-docto         
         AND item-doc-est.nro-docto    = nro-docto           
         AND item-doc-est.cod-emitente = cod-emitente
         AND item-doc-est.nat-operacao = nat-operacao:


        FIND FIRST es-item-doc-est-natoper NO-LOCK
             WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
               AND es-item-doc-est-natoper.serie-docto  = item-doc-est.serie-docto
               AND es-item-doc-est-natoper.nro-docto    = item-doc-est.nro-docto
               AND es-item-doc-est-natoper.cod-emitente = item-doc-est.cod-emitente  
               AND es-item-doc-est-natoper.nat-operacao = item-doc-est.nat-operacao
               AND es-item-doc-est-natoper.sequencia    = item-doc-est.sequencia NO-ERROR.
        IF AVAIL(es-item-doc-est-natoper) THEN DO:
           FOR FIRST bes-item-doc-est-natoper
               WHERE bes-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario               
                 AND bes-item-doc-est-natoper.serie-docto  = item-doc-est.serie-docto         
                 AND bes-item-doc-est-natoper.nro-docto    = item-doc-est.nro-docto           
                 AND bes-item-doc-est-natoper.cod-emitente = item-doc-est.cod-emitente        
                 AND bes-item-doc-est-natoper.nat-operacao = item-doc-est.nat-operacao        
                 AND bes-item-doc-est-natoper.sequencia    = item-doc-est.sequencia:
    
                    ASSIGN bes-item-doc-est-natoper.cod-cfop-saida       = cfop-orig:SCREEN-VALUE
                           bes-item-doc-est-natoper.cod-model-nf-eletro  = IF modelo-cfe:SCREEN-VALUE = "1" THEN "" ELSE IF modelo-cfe:SCREEN-VALUE = "2" THEN "55" ELSE "57"
                           bes-item-doc-est-natoper.emit-crt             = IF emit-crt:SCREEN-VALUE = "Trib Simples" THEN 1 ELSE IF emit-crt:SCREEN-VALUE = "Trib Normal" THEN 3 ELSE 4
                           bes-item-doc-est-natoper.aliquota-icm         = DEC(aliq-icms:SCREEN-VALUE)
                           bes-item-doc-est-natoper.cod-beneficio-icms   = IF benef-icms:CHECKED    THEN es-natoper-rec.cod-benef-icms-rec       ELSE 0
                           bes-item-doc-est-natoper.cod-beneficio-piscof = IF benef-pis-cof:CHECKED THEN es-natoper-rec.cod-benef-pis-cofins-rec ELSE 0 .
    
            END.
        END.
        ELSE DO:


            CREATE bes-item-doc-est-natoper.
            ASSIGN bes-item-doc-est-natoper.ep-codigo            = i-ep-codigo-usuario       
                   bes-item-doc-est-natoper.serie-docto          = item-doc-est.serie-docto 
                   bes-item-doc-est-natoper.nro-docto            = item-doc-est.nro-docto   
                   bes-item-doc-est-natoper.cod-emitente         = item-doc-est.cod-emitente
                   bes-item-doc-est-natoper.nat-operacao         = item-doc-est.nat-operacao
                   bes-item-doc-est-natoper.sequencia            = item-doc-est.sequencia
                   bes-item-doc-est-natoper.it-codigo            = item-doc-est.it-codigo
                   bes-item-doc-est-natoper.cod-cfop-saida       = cfop-orig:SCREEN-VALUE
                   bes-item-doc-est-natoper.cod-model-nf-eletro  = IF modelo-cfe:SCREEN-VALUE = "1" THEN "" ELSE IF modelo-cfe:SCREEN-VALUE = "2" THEN "55" ELSE "57"
                   bes-item-doc-est-natoper.emit-crt             = IF emit-crt:SCREEN-VALUE = "Trib Simples" THEN 1 ELSE IF emit-crt:SCREEN-VALUE = "Trib Normal" THEN 3 ELSE 4
                   bes-item-doc-est-natoper.aliquota-icm         = DEC(aliq-icms:SCREEN-VALUE)
                   bes-item-doc-est-natoper.cod-beneficio-icms   = IF benef-icms:CHECKED    THEN es-natoper-rec.cod-benef-icms-rec       ELSE 0
                   bes-item-doc-est-natoper.cod-beneficio-piscof = IF benef-pis-cof:CHECKED THEN es-natoper-rec.cod-benef-pis-cofins-rec ELSE 0 .

        END.

    END.

  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}


    ASSIGN c-serie-docto:SCREEN-VALUE  = serie-docto 
           c-nro-docto:SCREEN-VALUE    = nro-docto   
           i-cod-emitente:SCREEN-VALUE = string(cod-emitente)
           c-nat-operacao:SCREEN-VALUE = nat-operacao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY c-nro-docto i-cod-emitente c-serie-docto c-nat-operacao cfop-orig 
          modelo-cfe emit-crt aliq-icms benef-icms benef-pis-cof 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 RECT-3 cfop-orig modelo-cfe emit-crt aliq-icms 
         benef-icms benef-pis-cof bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "YMRE1001" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN c-serie-docto:SCREEN-VALUE  = serie-docto 
         c-nro-docto:SCREEN-VALUE    = nro-docto   
         i-cod-emitente:SCREEN-VALUE = string(cod-emitente)
         c-nat-operacao:SCREEN-VALUE = nat-operacao.

  FIND FIRST es-item-doc-est-natoper NO-LOCK
       WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
         AND es-item-doc-est-natoper.serie-docto  = serie-docto
         AND es-item-doc-est-natoper.nro-docto    = nro-docto
         AND es-item-doc-est-natoper.cod-emitente = cod-emitente  
         AND es-item-doc-est-natoper.nat-operacao = nat-operacao NO-ERROR.
  IF AVAIL(es-item-doc-est-natoper) THEN DO:

      IF es-item-doc-est-natoper.cod-model-nf-eletro = "" THEN
          ASSIGN modelo-cfe:SCREEN-VALUE = "1".
      ELSE IF es-item-doc-est-natoper.cod-model-nf-eletro = "55" THEN
          ASSIGN modelo-cfe:SCREEN-VALUE = "2".
      ELSE 
          ASSIGN modelo-cfe:SCREEN-VALUE = "3".

      IF es-item-doc-est-natoper.emit-crt = 1 THEN
          ASSIGN emit-crt:SCREEN-VALUE = "Trib Simples".
      ELSE IF es-item-doc-est-natoper.emit-crt = 3 THEN
          ASSIGN emit-crt:SCREEN-VALUE = "Trib Normal".
      ELSE
          ASSIGN emit-crt:SCREEN-VALUE = "RED. ICMS".

      ASSIGN aliq-icms:SCREEN-VALUE  = string(es-item-doc-est-natoper.aliquota-icm).

      IF es-item-doc-est-natoper.cod-beneficio-icms > 0 THEN
          ASSIGN benef-icms:CHECKED = TRUE.

      IF es-item-doc-est-natoper.cod-beneficio-piscof > 0 THEN
          ASSIGN benef-pis-cof:CHECKED = TRUE.











      /* N�o Aplic�vel,,
         NFe,55,
         CTe,57'
         
         Trib Simples,1,
         Trib Normal,3,
         Red. ICMS,4'
         */

  END.
  ELSE DO:

  END.
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

