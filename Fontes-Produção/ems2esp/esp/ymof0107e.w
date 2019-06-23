&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-window


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-es-movto-ext-item-cfa NO-UNDO LIKE es-movto-ext-item-cfa.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
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



DEFINE INPUT PARAMETER p-item        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-empresa     AS CHARACTER   NO-UNDO.
/* DEFINE INPUT PARAMETER p-cod-estabel AS CHARACTER   NO-UNDO. */

DEF BUFFER b-es-movto-ext-item-cfa FOR es-movto-ext-item-cfa.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-5

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-es-movto-ext-item-cfa

/* Definitions for BROWSE BROWSE-5                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-5 ~
tt-es-movto-ext-item-cfa.pergunta-literal ~
tt-es-movto-ext-item-cfa.resposta-literal 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-5 
&Scoped-define QUERY-STRING-BROWSE-5 FOR EACH tt-es-movto-ext-item-cfa NO-LOCK ~
    BY tt-es-movto-ext-item-cfa.nr-seq INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-5 OPEN QUERY BROWSE-5 FOR EACH tt-es-movto-ext-item-cfa NO-LOCK ~
    BY tt-es-movto-ext-item-cfa.nr-seq INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-5 tt-es-movto-ext-item-cfa
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-5 tt-es-movto-ext-item-cfa


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-5}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 f-item ~
f-desc-item BROWSE-5 f-cfa BUTTON-7 BUTTON-8 bt-ok bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS f-item f-desc-item f-responsavel f-data ~
f-hora FILL-IN-10 f-cfa 

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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "ems2/adeicon/pvfirst.bmp":U
     LABEL "Button 7" 
     SIZE 4.72 BY 1.13.

DEFINE BUTTON BUTTON-8 
     IMAGE-UP FILE "ems2/adeicon/pvlast.bmp":U
     LABEL "Button 8" 
     SIZE 5 BY 1.13.

DEFINE VARIABLE f-cfa AS CHARACTER FORMAT "X(256)":U 
     LABEL "CFA" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE f-data AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE f-hora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE f-responsavel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Responsavel" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Seq" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 87 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 3.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 1.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-5 FOR 
      tt-es-movto-ext-item-cfa SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-5 w-window _STRUCTURED
  QUERY BROWSE-5 NO-LOCK DISPLAY
      tt-es-movto-ext-item-cfa.pergunta-literal COLUMN-LABEL "Pergunta"
            WIDTH 36.43
      tt-es-movto-ext-item-cfa.resposta-literal COLUMN-LABEL "Resposta"
            WIDTH 48.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 89 BY 8.06 ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     f-item AT ROW 1.25 COL 13.43 COLON-ALIGNED WIDGET-ID 10
     f-desc-item AT ROW 1.25 COL 27.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     f-responsavel AT ROW 2.54 COL 13.43 COLON-ALIGNED WIDGET-ID 22
     f-data AT ROW 2.54 COL 35 COLON-ALIGNED WIDGET-ID 24
     f-hora AT ROW 2.54 COL 62 COLON-ALIGNED WIDGET-ID 26
     BROWSE-5 AT ROW 4.29 COL 1.29 WIDGET-ID 200
     FILL-IN-10 AT ROW 13.08 COL 14 COLON-ALIGNED WIDGET-ID 38
     f-cfa AT ROW 13.13 COL 55 COLON-ALIGNED WIDGET-ID 44
     BUTTON-7 AT ROW 13.17 COL 34.14 WIDGET-ID 30
     BUTTON-8 AT ROW 13.17 COL 39.72 WIDGET-ID 32
     bt-ok AT ROW 14.83 COL 3.14
     bt-ajuda AT ROW 14.83 COL 69.14
     RECT-1 AT ROW 14.63 COL 2.14
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 28
     RECT-3 AT ROW 12.96 COL 31.72 WIDGET-ID 34
     RECT-4 AT ROW 12.75 COL 1.72 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.72 BY 15.17 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-es-movto-ext-item-cfa T "?" NO-UNDO mgesp es-movto-ext-item-cfa
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 15.08
         WIDTH              = 89.72
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
/* BROWSE-TAB BROWSE-5 f-hora F-Main */
ASSIGN 
       f-cfa:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN f-data IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       f-desc-item:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN f-hora IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       f-item:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN f-responsavel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       f-responsavel:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-5
/* Query rebuild information for BROWSE BROWSE-5
     _TblList          = "Temp-Tables.tt-es-movto-ext-item-cfa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-es-movto-ext-item-cfa.nr-seq|yes"
     _FldNameList[1]   > Temp-Tables.tt-es-movto-ext-item-cfa.pergunta-literal
"tt-es-movto-ext-item-cfa.pergunta-literal" "Pergunta" ? "character" ? ? ? ? ? ? no ? no no "36.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-es-movto-ext-item-cfa.resposta-literal
"tt-es-movto-ext-item-cfa.resposta-literal" "Resposta" ? "character" ? ? ? ? ? ? no ? no no "48.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-5 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-5
&Scoped-define SELF-NAME BROWSE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-5 w-window
ON ROW-DISPLAY OF BROWSE-5 IN FRAME F-Main
DO:
  f-item:SCREEN-VALUE     = trim(tt-es-movto-ext-item-cfa.it-codigo).
/*    f-estab:SCREEN-VALUE = trim(tt-es-movto-ext-item-cfa.cod-estabel). */
   f-responsavel:SCREEN-VALUE = tt-es-movto-ext-item-cfa.usuario.
   f-data:SCREEN-VALUE = string(tt-es-movto-ext-item-cfa.dt-movto,"99/99/9999").
   f-hora:SCREEN-VALUE =tt-es-movto-ext-item-cfa.hr-movto.
   FILL-IN-10:SCREEN-VALUE =string(tt-es-movto-ext-item-cfa.identificador).
   f-cfa:SCREEN-VALUE =tt-es-movto-ext-item-cfa.classe. 

   APPLY "leave" TO f-item.

/*    APPLY "leave" TO f-estab. */
  
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 w-window
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Button 7 */
DO:
  RUN pi-prev-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 w-window
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Button 8 */
DO:
  RUN pi-next-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-item w-window
ON LEAVE OF f-item IN FRAME F-Main /* Item */
DO:
/*   FIND estabelec                                                                */
/*      WHERE estabelec.cod-estabel = INPUT FRAME F-Main f-estab NO-LOCK NO-ERROR. */
/*  IF AVAIL estabelec THEN ASSIGN F-DESC-ESTB:SCREEN-VALUE = estabelec.nome.      */
    
    

  FIND ITEM  WHERE ITEM.it-codigo = INPUT FRAME F-Main f-item NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN  ASSIGN f-desc-item:SCREEN-VALUE = ITEM.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  DISPLAY f-item f-desc-item f-responsavel f-data f-hora FILL-IN-10 f-cfa 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 f-item f-desc-item BROWSE-5 f-cfa BUTTON-7 
         BUTTON-8 bt-ok bt-ajuda 
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
  
  {utp/ut9000.i "YMOF0107E" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

RUN pi-carrega-browser.

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browser w-window 
PROCEDURE pi-carrega-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST es-movto-ext-item-cfa WHERE trim(es-movto-ext-item-cfa.it-codigo)   = p-item
                                  AND   trim(es-movto-ext-item-cfa.ep-codigo)   = p-empresa
/*                                   AND   trim(es-movto-ext-item-cfa.cod-estabel) = p-cod-estabel */
                                  USE-INDEX pk-ident  NO-LOCK NO-ERROR.
     IF AVAIL es-movto-ext-item-cfa THEN DO:

         FOR EACH b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = es-movto-ext-item-cfa.it-codigo 
                                          AND   b-es-movto-ext-item-cfa.ep-codigo     = es-movto-ext-item-cfa.ep-codigo   
                                          AND   b-es-movto-ext-item-cfa.identificador = es-movto-ext-item-cfa.identificador NO-LOCK :

                       CREATE tt-es-movto-ext-item-cfa.
                       BUFFER-COPY b-es-movto-ext-item-cfa TO tt-es-movto-ext-item-cfa.
                       ASSIGN tt-es-movto-ext-item-cfa.resposta = REPLACE(tt-es-movto-ext-item-cfa.resposta,"|"," => ").


         END.
       
  END.





    

    
  {&OPEN-QUERY-{&BROWSE-NAME}}



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-next-movto w-window 
PROCEDURE pi-next-movto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-item AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-empresa1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-estab1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-ident AS INTEGER     NO-UNDO.


c-item     = tt-es-movto-ext-item-cfa.it-codigo.
c-empresa1 = tt-es-movto-ext-item-cfa.ep-codigo.
i-ident    = tt-es-movto-ext-item-cfa.identificador.





 FIND FIRST  b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item              
                                     AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1          
                                     AND   b-es-movto-ext-item-cfa.identificador = i-ident + 1 NO-LOCK NO-ERROR.


 IF NOT AVAIL b-es-movto-ext-item-cfa THEN DO:

       FIND FIRST  b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item                      
                                           AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1                     
                                           AND   b-es-movto-ext-item-cfa.identificador = i-ident + 2 NO-LOCK NO-ERROR.  


       IF NOT AVAIL b-es-movto-ext-item-cfa THEN DO: 
           
           
           RETURN.
       END.
       ELSE ASSIGN i-ident = i-ident + 2.
 

 END.
 ELSE ASSIGN i-ident = i-ident + 1.

EMPTY temp-table tt-es-movto-ext-item-cfa.








BROWSE-5:REFRESH() IN FRAME F-Main.




         FOR EACH b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item 
                                          AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1   
                                          AND   b-es-movto-ext-item-cfa.identificador = i-ident NO-LOCK :

                       CREATE tt-es-movto-ext-item-cfa.
                       BUFFER-COPY b-es-movto-ext-item-cfa TO tt-es-movto-ext-item-cfa.
                       ASSIGN tt-es-movto-ext-item-cfa.resposta = REPLACE(tt-es-movto-ext-item-cfa.resposta,"|"," => ").

         END.
   


  {&OPEN-QUERY-{&BROWSE-NAME}}



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-prev-movto w-window 
PROCEDURE pi-prev-movto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-item AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-empresa1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-estab1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-ident AS INTEGER     NO-UNDO.


c-item     = tt-es-movto-ext-item-cfa.it-codigo.
c-empresa1 = tt-es-movto-ext-item-cfa.ep-codigo.
/* c-estab1   = tt-es-movto-ext-item-cfa.cod-estabel. */
i-ident    = tt-es-movto-ext-item-cfa.identificador.





 FIND FIRST  b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item              
                                     AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1          
                                     AND   b-es-movto-ext-item-cfa.identificador = i-ident - 1 NO-LOCK NO-ERROR.


 IF NOT AVAIL b-es-movto-ext-item-cfa THEN DO:

       FIND FIRST  b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item                      
                                           AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1                     
                                           AND   b-es-movto-ext-item-cfa.identificador = i-ident - 2 NO-LOCK NO-ERROR.  


       IF NOT AVAIL b-es-movto-ext-item-cfa THEN DO: 
           
           
           RETURN.
       END.
       ELSE ASSIGN i-ident = i-ident - 2.
 

 END.
 ELSE ASSIGN i-ident = i-ident - 1.

EMPTY temp-table tt-es-movto-ext-item-cfa.








BROWSE-5:REFRESH() IN FRAME F-Main.




         FOR EACH b-es-movto-ext-item-cfa WHERE b-es-movto-ext-item-cfa.it-codigo     = c-item 
                                          AND   b-es-movto-ext-item-cfa.ep-codigo     = c-empresa1   
                                          AND   b-es-movto-ext-item-cfa.identificador = i-ident NO-LOCK :

                       CREATE tt-es-movto-ext-item-cfa.
                       BUFFER-COPY b-es-movto-ext-item-cfa TO tt-es-movto-ext-item-cfa.
                       ASSIGN tt-es-movto-ext-item-cfa.resposta = REPLACE(tt-es-movto-ext-item-cfa.resposta,"|"," => ").

         END.
   


  {&OPEN-QUERY-{&BROWSE-NAME}}

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-es-movto-ext-item-cfa"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

