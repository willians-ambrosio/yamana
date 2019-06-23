&Scoped-define WINDOW-NAME C-Win
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE TEMP-TABLE tt-saida NO-UNDO
    FIELD      tt-cdn-empresa         LIKE config-var-operac-regra-cond.cdn-empresa
    FIELD      tt-cod-estab           LIKE config-var-operac-regra-cond.cod-estab 
    FIELD      tt-cod-tip-operac      LIKE config-var-operac-regra-cond.cod-tip-operac
    FIELD      tt-cdn-regra-operac    LIKE config-var-operac-regra-cond.cdn-regra-operac
    FIELD      tt-cdn-seq-regra       LIKE config-var-operac-regra-cond.cdn-seq-regra
    FIELD      tt-cod-variavel        LIKE config-var-operac-regra-cond.cod-variavel   
    FIELD      tt-cdn-cond-regra      LIKE config-var-operac-regra-cond.cdn-cond-regra
    FIELD      tt-cod-val-var         LIKE config-var-operac-regra-cond.cod-val-var
    FIELD      tt-cdn-operador-regra  LIKE config-var-operac-regra-cond.cdn-operador-regra
    FIELD      tt-cod-campo-ret       LIKE config-var-operac-regra-ret.cod-campo-ret
    FIELD      tt-cod-val-ret         LIKE config-var-operac-regra-ret.cod-val-ret.


DEF VAR emp    LIKE config-var-operac-regra-cond.cdn-empresa.
DEF VAR est    LIKE config-var-operac-regra-cond.cod-estab.
DEF VAR tp-op  LIKE config-var-operac-regra-cond.cod-tip-operac.
DEF VAR regrai LIKE config-var-operac-regra-cond.cdn-regra-operac.
DEF VAR regraf LIKE config-var-operac-regra-cond.cdn-regra-operac.

DEFINE VARIABLE c-dir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-cancel AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.



/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 c-ini-empresa c-fim-empresa ~
c-ini-estabel c-fim-estabel cb-tp-oper c-ini-regra c-fim-regra bt-executar ~
BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS c-ini-empresa c-fim-empresa c-ini-estabel ~
c-fim-estabel cb-tp-oper c-ini-regra c-fim-regra 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE cb-tp-oper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Opera‡Æo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE c-fim-empresa AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-estabel AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-regra AS DECIMAL FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-empresa AS CHARACTER FORMAT "X(3)":U INITIAL ""
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-estabel AS CHARACTER FORMAT "X(5)":U 
     LABEL "Establecimento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-regra AS DECIMAL FORMAT ">>>>9":U INITIAL 0 
     LABEL "Regra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 6.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     c-ini-empresa AT ROW 3.5 COL 21.57 COLON-ALIGNED WIDGET-ID 2
     c-fim-empresa AT ROW 3.5 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     c-ini-estabel AT ROW 4.75 COL 21.57 COLON-ALIGNED WIDGET-ID 12
     c-fim-estabel AT ROW 4.75 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cb-tp-oper AT ROW 6 COL 21.57 COLON-ALIGNED WIDGET-ID 18
     c-ini-regra AT ROW 7.42 COL 21.57 COLON-ALIGNED WIDGET-ID 22
     c-fim-regra AT ROW 7.42 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     bt-executar AT ROW 9.75 COL 24 WIDGET-ID 30
     BtnCancel AT ROW 9.75 COL 43 WIDGET-ID 34
     "<<" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 7.46 COL 39.72 WIDGET-ID 24
     "<<" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 3.54 COL 39.72 WIDGET-ID 4
     ">>" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 3.54 COL 43.57 WIDGET-ID 6
     "                                                     Listagem de Regra" VIEW-AS TEXT
          SIZE 73 BY 1.75 AT ROW 1.25 COL 5 WIDGET-ID 28
          BGCOLOR 8 
     ">>" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 7.46 COL 43.57 WIDGET-ID 26
     "<<" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 4.79 COL 39.72 WIDGET-ID 14
     ">>" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 4.79 COL 43.57 WIDGET-ID 16
     RECT-1 AT ROW 3 COL 5 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 10.46
         CANCEL-BUTTON BtnCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */

/* *************************  Create Window  ************************** */

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 10.38
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.


 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.


ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.


&Scoped-define SELF-NAME bt-executar
ON CHOOSE OF bt-executar IN FRAME DEFAULT-FRAME /* Executar */
DO:
  RUN pi-executar.
END.

&Scoped-define SELF-NAME bt-executar
ON CHOOSE OF btncancel IN FRAME DEFAULT-FRAME /* cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

&UNDEFINE SELF-NAME



/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

FOR EACH config-tip-operac NO-LOCK:

    ASSIGN i-cont = i-cont + 1.
    cb-tp-oper:ADD-LAST(config-tip-operac.cod-tip-operac).    

  END.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.


END.




/* **********************  Internal Procedures  *********************** */

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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

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
  DISPLAY c-ini-empresa c-fim-empresa c-ini-estabel c-fim-estabel cb-tp-oper 
          c-ini-regra c-fim-regra 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 c-ini-empresa c-fim-empresa c-ini-estabel c-fim-estabel 
         cb-tp-oper c-ini-regra c-fim-regra bt-executar BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE ini-empresa AS CHARACTER NO-UNDO.
DEFINE VARIABLE fim-empresa AS CHARACTER no-undo.
DEFINE VARIABLE ini-estabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE fim-estabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tp-oper AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ini-regra AS CHARACTER   NO-UNDO.
DEFINE VARIABLE fim-regra AS CHARACTER   NO-UNDO.

ASSIGN ini-empresa = c-ini-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       fim-empresa = c-fim-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       ini-estabel = c-ini-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       fim-estabel = c-fim-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       tp-oper     = cb-tp-oper:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       ini-regra   = c-ini-regra:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       fim-regra   = c-fim-regra:SCREEN-VALUE IN FRAME {&FRAME-NAME}.


ASSIGN c-dir = SESSION:TEMP-DIRECTORY + "log-real-engine.txt".

DISP "Exportando Dados..." WITH FRAME f-a VIEW-AS DIALOG-BOX COLOR red/white.

/* OUTPUT TO value(SESSION:TEMP-DIRECTORY + "realengine.txt"). */
OUTPUT TO VALUE(c-dir).

EXPORT DELIMITER ";" " Empresa " " Estabel " " Tp Operac " " Regra " " Seq " " Variavel " " Condicao " " Valor " " Operador " " Retorno ".

FOR EACH config-var-operac-regra-cond NO-LOCK 
     WHERE config-var-operac-regra-cond.cdn-empresa    >= ini-empresa
       AND config-var-operac-regra-cond.cdn-empresa    <= fim-empresa
       AND config-var-operac-regra-cond.cod-estab      >= ini-estabel
       AND config-var-operac-regra-cond.cod-estab      <= fim-estabel
       AND config-var-operac-regra-cond.cod-tip-operac  = tp-oper
       AND config-var-operac-regra-cond.cdn-regra-operac >= dec(ini-regra)
       AND config-var-operac-regra-cond.cdn-regra-operac <= dec(fim-regra)  BREAK BY config-var-operac-regra-cond.cdn-regra-operac:

         
         IF LAST-OF (config-var-operac-regra-cond.cdn-regra-operac) THEN DO:

          
          FIND FIRST  config-var-operac-regra-ret WHERE 
          config-var-operac-regra-cond.cdn-empresa      = config-var-operac-regra-ret.cdn-empresa AND
          config-var-operac-regra-cond.cod-estab        = config-var-operac-regra-ret.cod-estab   AND
          config-var-operac-regra-cond.cdn-regra-operac = config-var-operac-regra-ret.cdn-regra-operac AND
          config-var-operac-regra-cond.cod-tip-operac   = config-var-operac-regra-ret.cod-tip-operac NO-LOCK NO-ERROR.

          IF AVAIL config-var-operac-regra-ret THEN DO:

            
            CREATE tt-saida.
            ASSIGN  tt-cdn-empresa         = config-var-operac-regra-cond.cdn-empresa
                    tt-cod-estab           = config-var-operac-regra-cond.cod-estab 
                    tt-cod-tip-operac      = config-var-operac-regra-cond.cod-tip-operac
                    tt-cdn-regra-operac    = config-var-operac-regra-cond.cdn-regra-operac
                    tt-cdn-seq-regra       = config-var-operac-regra-cond.cdn-seq-regra
                    tt-cod-variavel        = config-var-operac-regra-cond.cod-variavel   
                    tt-cdn-cond-regra      = config-var-operac-regra-cond.cdn-cond-regra
                    tt-cod-val-var         = config-var-operac-regra-cond.cod-val-var
                    tt-cdn-operador-regra  = config-var-operac-regra-cond.cdn-operador-regra
                    tt-cod-campo-ret       = config-var-operac-regra-ret.cod-campo-ret
                    tt-cod-val-ret         = config-var-operac-regra-ret.cod-val-ret.
          
          END.

         END.
           ELSE DO:
           
            CREATE tt-saida.
            ASSIGN  tt-cdn-empresa         = config-var-operac-regra-cond.cdn-empresa
                    tt-cod-estab           = config-var-operac-regra-cond.cod-estab 
                    tt-cod-tip-operac      = config-var-operac-regra-cond.cod-tip-operac
                    tt-cdn-regra-operac    = config-var-operac-regra-cond.cdn-regra-operac
                    tt-cdn-seq-regra       = config-var-operac-regra-cond.cdn-seq-regra
                    tt-cod-variavel        = config-var-operac-regra-cond.cod-variavel   
                    tt-cdn-cond-regra      = config-var-operac-regra-cond.cdn-cond-regra
                    tt-cod-val-var         = config-var-operac-regra-cond.cod-val-var
                    tt-cdn-operador-regra  = config-var-operac-regra-cond.cdn-operador-regra
                    tt-cod-campo-ret       = ""
                    tt-cod-val-ret         = "".
                  
           END.
      
    END.

 
    FOR EACH tt-saida.

        DEF VAR aux AS CHAR.
        DEF VAR aux2 AS CHAR.

        IF tt-saida.tt-cdn-operador-regra <> 0 THEN aux = {ininc/i02in810.i 04 tt-saida.tt-cdn-operador-regra}.
                                                  ELSE aux = "".


        IF tt-saida.tt-cdn-cond-regra <> 0 THEN aux2 = {ininc/i01in810.i 04 tt-saida.tt-cdn-cond-regra}.
                                                  ELSE aux2 = "".


        EXPORT DELIMITER ";"
               tt-saida.tt-cdn-empresa 
               tt-saida.tt-cod-estab
               tt-saida.tt-cod-tip-operac 
               tt-saida.tt-cdn-regra-operac
               tt-saida.tt-cdn-seq-regra
               tt-saida.tt-cod-variavel
               aux2
               tt-saida.tt-cod-val-var
               aux
               tt-saida.tt-cod-campo-ret
               tt-saida.tt-cod-val-ret.
    END.
HIDE FRAME f-a.
DOS SILENT notepad value(c-dir).



END PROCEDURE.


