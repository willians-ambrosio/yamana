&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CR0706AA 2.00.00.000}  /*** 010000 ***/

/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input params:
      <none>

  Output params:
      <none>

  Author: 

  Created: 
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
/* Miniflexibiliza‡Æo */  
  {include/i_dbvers.i}


def var l-ok as logical no-undo.
DEF VAR i AS DATE NO-UNDO.
DEF VAR d-da-atraso-ant AS DATE NO-UNDO.

def input-output param i-conv-moeda         like titulo.mo-codigo    no-undo.
def input-output param da-data-sel          as date                  no-undo.
def input-output param c-cod-esp-inicial    like titulo.cod-esp      no-undo.
def input-output param c-cod-esp-final      like titulo.cod-esp      no-undo.
def input-output param d-dt-emissao-inicial like titulo.dt-emissao   no-undo.
def input-output param d-dt-emissao-final   like titulo.dt-emissao   no-undo.
def input-output param d-dt-vencto-inicial  like titulo.dt-vencimen  no-undo.
def input-output param d-dt-vencto-final    like titulo.dt-vencimen  no-undo.
def input-output param i-cod-rep-inicial    like titulo.cod-rep      no-undo.
def input-output param i-cod-rep-final      like titulo.cod-rep      no-undo.
def input-output param i-cod-por-inicial    like titulo.cod-port     no-undo.
def input-output param i-cod-por-final      like titulo.cod-port     no-undo.
def input-output param i-titulo             as integer               no-undo.
def input-output param i-valor              as integer               no-undo.
def input-output param l-vencidos           as logical               no-undo.
def input-output param l-a-vencer           as logical               no-undo.
def input-output param i-dias-vencimento    as integer format ">>9"  no-undo.
def input-output param l-juros              as logical               no-undo.
def input-output param i-op-juros           as integer               no-undo.
def input-output param de-juros             as dec                   no-undo.
def input-output param l-canc               as log                   no-undo.
def input-output param da-data-base         as date                  no-undo.
DEF INPUT-OUTPUT PARAM da-data-atraso       AS DATE                  NO-UNDO.

def var da-vcto-inf-ini                     as date                  no-undo.
def var da-vcto-inf-fim                     as date                  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-10 IMAGE-9 RECT-4 RECT-2 RECT-10 ~
rt-buttom fi-cod-esp-inicial fi-cod-esp-final fi-dt-emissao-inicial ~
fi-dt-emissao-final da-vencto-ini da-vencto-fim i-rep-ini i-rep-fim ~
i-portador-ini i-portador-fim rs-titulo da-atraso tb-juros rs-juros bt-ok ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-esp-inicial fi-cod-esp-final ~
fi-dt-emissao-inicial fi-dt-emissao-final da-vencto-ini da-vencto-fim ~
i-rep-ini i-rep-fim i-portador-ini i-portador-fim rs-titulo tb-vencido ~
tb-a-vencer da-atraso fi-dias-vencimento tb-juros rs-juros de-perc-juros ~
data-base 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE da-atraso AS DATE FORMAT "99/99/9999":U INITIAL today     
     LABEL "Data Atraso" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE da-vencto-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE da-vencto-ini AS DATE FORMAT "99/99/9999":U INITIAL &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE data-base AS DATE FORMAT "99/99/9999":U INITIAL &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE de-perc-juros AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-esp-final AS CHARACTER FORMAT "!!" INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-esp-inicial AS CHARACTER FORMAT "!!" INITIAL "AA" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dias-vencimento AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Margem Dias" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-final AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-inicial AS DATE FORMAT "99/99/9999" INITIAL &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-portador-fim AS INTEGER FORMAT ">>,>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-portador-ini AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-rep-fim AS INTEGER FORMAT ">>,>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-rep-ini AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image~\ii-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image~\ii-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image~\ii-las":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image~\ii-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image~\ii-las":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image~\ii-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image~\ii-las":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image~\ii-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image~\ii-las":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image~\ii-las":U
     SIZE 2.86 BY .88.

DEFINE VARIABLE rs-juros AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2,
"3", 3
     SIZE 39.43 BY 1.17 NO-UNDO.

DEFINE VARIABLE rs-titulo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2
     SIZE 16.72 BY 2.29 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.44 BY 3.83.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 24.43 BY 3.83.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34.86 BY 3.83.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 5.58.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 59 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tb-a-vencer AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.72 BY .88 NO-UNDO.

DEFINE VARIABLE tb-juros AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.86 BY .88 NO-UNDO.

DEFINE VARIABLE tb-vencido AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.72 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-cod-esp-inicial AT ROW 1.46 COL 20.86 COLON-ALIGNED
     fi-cod-esp-final AT ROW 1.46 COL 42.43 COLON-ALIGNED NO-LABEL
     fi-dt-emissao-inicial AT ROW 2.46 COL 20.86 COLON-ALIGNED
     fi-dt-emissao-final AT ROW 2.46 COL 42.43 COLON-ALIGNED NO-LABEL
     da-vencto-ini AT ROW 3.46 COL 20.86 COLON-ALIGNED
     da-vencto-fim AT ROW 3.46 COL 42.43 COLON-ALIGNED NO-LABEL
     i-rep-ini AT ROW 4.46 COL 20.86 COLON-ALIGNED
     i-rep-fim AT ROW 4.46 COL 42.43 COLON-ALIGNED NO-LABEL
     i-portador-ini AT ROW 5.46 COL 20.86 COLON-ALIGNED
     i-portador-fim AT ROW 5.46 COL 42.43 COLON-ALIGNED NO-LABEL
     rs-titulo AT ROW 7.25 COL 3 NO-LABEL
     tb-vencido AT ROW 7.25 COL 27
     tb-a-vencer AT ROW 8.25 COL 27
     da-atraso AT ROW 7.25 COL 45 COLON-ALIGNED 
     fi-dias-vencimento AT ROW 8.25 COL 45 COLON-ALIGNED
     tb-juros AT ROW 11.08 COL 2.57
     rs-juros AT ROW 12.25 COL 2.57 NO-LABEL
     de-perc-juros AT ROW 13.54 COL 10.14 COLON-ALIGNED
     data-base AT ROW 13.54 COL 27.72 COLON-ALIGNED
     bt-ok AT ROW 15.13 COL 2.57
     bt-cancela AT ROW 15.13 COL 14.14
     bt-ajuda AT ROW 15.13 COL 49.14
     RECT-9 AT ROW 1.13 COL 1
     IMAGE-1 AT ROW 1.46 COL 36.57
     IMAGE-2 AT ROW 1.5 COL 41.29
     IMAGE-3 AT ROW 2.46 COL 36.57
     IMAGE-4 AT ROW 2.5 COL 41.29
     IMAGE-5 AT ROW 3.54 COL 36.57
     IMAGE-6 AT ROW 3.58 COL 41.29
     IMAGE-7 AT ROW 4.54 COL 36.57
     IMAGE-8 AT ROW 4.58 COL 41.29
     IMAGE-10 AT ROW 5.54 COL 36.57
     IMAGE-9 AT ROW 5.54 COL 41.29
     RECT-4 AT ROW 6.88 COL 26
     RECT-2 AT ROW 6.88 COL 1.57
     RECT-10 AT ROW 10.88 COL 1.57
     rt-buttom AT ROW 14.83 COL 1.14
     SPACE(0.86) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Filtro T¡tulos da Matriz/Cliente"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN data-base IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-perc-juros IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dias-vencimento IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb-a-vencer IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb-vencido IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Filtro T¡tulos da Matriz/Cliente */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  assign l-canc = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:

     if  input frame {&FRAME-NAME} fi-cod-esp-final <
         input frame {&FRAME-NAME} fi-cod-esp-inicial then do:
         run utp/ut-msgs.p (input "show",
                            input 252,
                            input "").
         apply 'entry':u to fi-cod-esp-inicial in frame {&FRAME-NAME}.
         return no-apply.
     end.

     if  input frame {&FRAME-NAME} fi-dt-emissao-final <
         input frame {&FRAME-NAME} fi-dt-emissao-inicial then do:
         run utp/ut-msgs.p (input "show",
                            input 252,
                            input "").
         apply 'entry':u to fi-dt-emissao-inicial in frame {&FRAME-NAME}.
         return no-apply.
     end.

     if  input frame {&FRAME-NAME} da-vencto-fim <
         input frame {&FRAME-NAME} da-vencto-ini then do:
         run utp/ut-msgs.p (input "show",
                            input 252,
                            input "").
         apply 'entry':u to da-vencto-ini in frame {&FRAME-NAME}.
         return no-apply.
     end.

     if  input frame {&FRAME-NAME} i-rep-fim <
         input frame {&FRAME-NAME} i-rep-ini then do:
         run utp/ut-msgs.p (input "show",
                            input 252,
                            input "").
         apply 'entry':u to i-rep-ini in frame {&FRAME-NAME}.
         return no-apply.
     end.

     if  input frame {&FRAME-NAME} i-portador-fim <
         input frame {&FRAME-NAME} i-portador-ini then do:
         run utp/ut-msgs.p (input "show",
                            input 252,
                            input "").
         apply 'entry':u to i-portador-ini in frame {&FRAME-NAME}.
         return no-apply.
     end. 

     assign input frame {&frame-name} fi-cod-esp-inicial 
                                      fi-cod-esp-final 
                                      fi-dt-emissao-inicial 
                                      fi-dt-emissao-final
                                      da-vencto-ini
                                      da-vencto-fim
                                      i-rep-ini
                                      i-rep-fim
                                      i-portador-ini
                                      i-portador-fim
                                      data-base
                                      rs-titulo 
                                      rs-juros 
                                      tb-vencido 
                                      tb-a-vencer
                                      tb-juros 
                                      fi-dias-vencimento
                                      de-perc-juros
                                      da-atraso.


     assign c-cod-esp-inicial    = fi-cod-esp-inicial
            c-cod-esp-final      = fi-cod-esp-final
            d-dt-emissao-inicial = fi-dt-emissao-inicial
            d-dt-emissao-final   = fi-dt-emissao-final
            d-dt-vencto-inicial  = da-vencto-ini
            d-dt-vencto-final    = da-vencto-fim
            i-cod-rep-inicial    = i-rep-ini
            i-cod-rep-final      = i-rep-fim
            i-cod-por-inicial    = i-portador-ini
            i-cod-por-final      = i-portador-fim
            da-data-base         = data-base
            da-data-atraso       = da-atraso
            i-titulo             = rs-titulo
            l-vencidos           = tb-vencido
            l-a-vencer           = tb-a-vencer
            i-dias-vencimento    = fi-dias-vencimento 
            l-juros              = tb-juros
            i-op-juros           = rs-juros
            de-juros             = de-perc-juros
            l-canc               = no. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME da-atraso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-atraso D-Dialog
ON LEAVE OF da-atraso IN FRAME D-Dialog /* Data Atraso */
DO:
   assign da-atraso = input frame {&FRAME-NAME} da-atraso.
   IF tb-a-vencer:screen-value in frame {&frame-name} = "yes" THEN DO: 
      IF da-atraso <> d-da-atraso-ant THEN  
         ASSIGN da-vcto-inf-ini = da-atraso
                da-vencto-ini:screen-value = string(da-atraso,"99/99/9999") 
                d-da-atraso-ant = da-atraso.
   END.
   ELSE
       IF tb-vencido:screen-value in frame {&frame-name} = "yes" THEN DO:
          IF da-atraso <> d-da-atraso-ant THEN DO:
             ASSIGN da-vcto-inf-fim = da-atraso 
                    da-vencto-fim:screen-value = string(da-atraso,"99/99/9999") 
                    d-da-atraso-ant = da-atraso.
          END.
       END.     
END.


&Scoped-define SELF-NAME da-vencto-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-vencto-fim D-Dialog
ON LEAVE OF da-vencto-fim IN FRAME D-Dialog
DO:
   assign da-vcto-inf-fim = input frame {&FRAME-NAME} da-vencto-fim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME da-vencto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-vencto-ini D-Dialog
ON LEAVE OF da-vencto-ini IN FRAME D-Dialog
DO:
   assign da-vcto-inf-ini = input frame {&FRAME-NAME} da-vencto-ini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dias-vencimento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dias-vencimento D-Dialog
ON LEAVE OF fi-dias-vencimento IN FRAME D-Dialog /* Margem Dias */
DO:
   RUN pi-habilita.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-juros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-juros D-Dialog
ON VALUE-CHANGED OF rs-juros IN FRAME D-Dialog
DO:

    if  input frame {&FRAME-NAME} rs-juros = 3 then do:
        assign de-perc-juros:sensitive in frame {&FRAME-NAME} = yes.
    end.
    else do:
        assign de-perc-juros:sensitive in frame {&FRAME-NAME} = no.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-titulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-titulo D-Dialog
ON VALUE-CHANGED OF rs-titulo IN FRAME D-Dialog
DO:

    assign input frame {&frame-name} rs-titulo.

    if  rs-titulo = 1 then do:
        assign tb-vencido:sensitive            in frame {&frame-name} = no
               tb-vencido:screen-value         in frame {&frame-name} = string(no)               
               tb-a-vencer:sensitive           in frame {&frame-name} = no    
               tb-a-vencer:screen-value        in frame {&frame-name} = string(no)               
               fi-dias-vencimento:sensitive    in frame {&frame-name} = no
               da-vencto-ini:sensitive         in frame {&frame-name} = yes    
               da-vencto-fim:sensitive         in frame {&frame-name} = yes    
               da-vencto-ini:screen-value      in frame {&frame-name} = string(da-vcto-inf-ini,"99/99/9999")
               da-vencto-fim:screen-value      in frame {&frame-name} = string(da-vcto-inf-fim,"99/99/9999")
               fi-dias-vencimento:screen-value in frame {&frame-name} = "0".
    end.   
    else do:
        assign tb-vencido:sensitive         in frame {&frame-name} = yes
               tb-a-vencer:sensitive        in frame {&frame-name} = yes
               fi-dias-vencimento:sensitive in frame {&frame-name} = yes.

         run pi-habilita.         
    end.     

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-a-vencer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-a-vencer D-Dialog
ON VALUE-CHANGED OF tb-a-vencer IN FRAME D-Dialog
DO:

    if tb-a-vencer:screen-value in frame {&frame-name} = "yes" then 
       ASSIGN da-vcto-inf-ini = input frame {&FRAME-NAME} da-atraso 
              da-vencto-ini:screen-value  in frame {&frame-name} = string(da-vcto-inf-ini,"99/99/9999")       
              tb-vencido:screen-value in frame {&frame-name} = string(no).
    ELSE
       ASSIGN da-vencto-ini:sensitive         in frame {&frame-name} = YES   
              da-vencto-fim:sensitive         in frame {&frame-name} = YES.  

    assign input frame {&frame-name} tb-a-vencer
           input frame {&FRAME-NAME} tb-vencido.

    run pi-habilita. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-juros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-juros D-Dialog
ON VALUE-CHANGED OF tb-juros IN FRAME D-Dialog
DO:

    if  input frame {&FRAME-NAME} tb-juros = yes then do:
        assign rs-juros:sensitive  in frame {&FRAME-NAME} = yes
               data-base:sensitive in frame {&FRAME-NAME} = yes.
    end.
    else do:
        assign rs-juros:screen-value      in frame {&FRAME-NAME} = string(1)
               de-perc-juros:screen-value in frame {&FRAME-NAME} = string(0)
               rs-juros:sensitive         in frame {&FRAME-NAME} = no
               de-perc-juros:sensitive    in frame {&FRAME-NAME} = no
               data-base:sensitive        in frame {&FRAME-NAME} = no.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-vencido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-vencido D-Dialog
ON VALUE-CHANGED OF tb-vencido IN FRAME D-Dialog
DO:

    if  tb-vencido:screen-value in frame {&frame-name} = "yes" then 
        assign tb-a-vencer:screen-value in frame {&frame-name} = string(no)  
               da-vencto-fim:screen-value = STRING(da-atraso) 
               da-vcto-inf-fim = input frame {&FRAME-NAME} da-vencto-fim.                       
    
    assign input frame {&frame-name} tb-vencido
           input frame {&FRAME-NAME} tb-a-vencer.

    run pi-habilita. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{utp/ut-field.i mgadm titulo cod-esp 1}.
assign fi-cod-esp-inicial:label in frame {&FRAME-NAME} = return-value.

{utp/ut-field.i mgadm titulo dt-emissao 1}.
assign fi-dt-emissao-inicial:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i %_Juros_Mˆs * L}
assign de-perc-juros:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Dias * L}
assign fi-dias-vencimento:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Considera_juros_p/_t¡tulos_atrasados? * R}.
assign tb-juros:label in frame {&frame-name} = trim(return-value).

{utp/ut-liter.i Vencidos * R}.
assign tb-vencido:label in frame {&FRAME-NAME} = return-value. 

{utp/ut-liter.i ·_Vencer * R}.
assign tb-a-vencer:label in frame {&FRAME-NAME} = return-value. 

{utp/ut-field.i mgadm titulo dt-vencimen 1}.
assign da-vencto-ini:label in frame {&FRAME-NAME} = return-value. 

{utp/ut-field.i mgadm titulo cod-rep 1}.
assign i-rep-ini:label in frame {&FRAME-NAME} = return-value. 

{utp/ut-liter.i Portador * L}
assign i-portador-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Data_Base * L}
assign data-base:label in frame {&FRAME-NAME} = trim(return-value).


{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ASSIGN d-da-atraso-ant = da-atraso.

  DISPLAY fi-cod-esp-inicial fi-cod-esp-final fi-dt-emissao-inicial 
          fi-dt-emissao-final da-vencto-ini da-vencto-fim i-rep-ini i-rep-fim 
          i-portador-ini i-portador-fim rs-titulo tb-vencido tb-a-vencer 
          da-atraso fi-dias-vencimento tb-juros rs-juros de-perc-juros 
          data-base 
      WITH FRAME D-Dialog.
  ENABLE RECT-9 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-10 IMAGE-9 RECT-4 RECT-2 RECT-10 rt-buttom fi-cod-esp-inicial 
         fi-cod-esp-final fi-dt-emissao-inicial fi-dt-emissao-final 
         da-vencto-ini da-vencto-fim i-rep-ini i-rep-fim i-portador-ini 
         i-portador-fim rs-titulo da-atraso tb-juros rs-juros bt-ok bt-cancela 
         bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

{utp/ut9000.i "CR0706AA" "2.00.00.000"}
  do  with frame {&FRAME-NAME}:
        {utp/ut-liter.i Todos_os_T¡tulos * R}.
      assign l-ok = rs-titulo:replace( return-value , 1 , "1" ). 
      {utp/ut-liter.i Somente_em_Aberto * R}.
      assign l-ok = rs-titulo:replace( return-value , 2 , "2" ). 

      {utp/ut-liter.i T¡tulo * R}.
      assign l-ok = rs-juros:replace( return-value , 1 , "1" ). 
      {utp/ut-liter.i Parƒmetro * R}.
      assign l-ok = rs-juros:replace( return-value , 2 , "2" ). 
      {utp/ut-liter.i Informa * R}.
      assign l-ok = rs-juros:replace( return-value , 3 , "3" ). 

      assign i-conv-moeda = 0
             da-data-sel  = today
             i-valor      = 2.

  end.
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view D-Dialog 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  assign fi-cod-esp-inicial:screen-value    in frame {&FRAME-NAME} = string(c-cod-esp-inicial)
         fi-cod-esp-final:screen-value      in frame {&FRAME-NAME} = string(c-cod-esp-final)
         fi-dt-emissao-inicial:screen-value in frame {&FRAME-NAME} = string(d-dt-emissao-inicial)
         fi-dt-emissao-final:screen-value   in frame {&FRAME-NAME} = string(d-dt-emissao-final)
         da-vencto-ini:screen-value         in frame {&FRAME-NAME} = string(d-dt-vencto-inicial)
         da-vencto-fim:screen-value         in frame {&FRAME-NAME} = string(d-dt-vencto-final)
         i-rep-ini:screen-value             in frame {&FRAME-NAME} = string(i-cod-rep-inicial)
         i-rep-fim:screen-value             in frame {&FRAME-NAME} = string(i-cod-rep-final)
         i-portador-ini:screen-value        in frame {&FRAME-NAME} = string(i-cod-por-inicial)
         i-portador-fim:screen-value        in frame {&FRAME-NAME} = string(i-cod-por-final)
         data-base:screen-value             in frame {&FRAME-NAME} = string(da-data-base)
         rs-titulo:screen-value             in frame {&FRAME-NAME} = string(i-titulo)
         tb-vencido:screen-value            in frame {&FRAME-NAME} = string(l-vencidos)
         tb-a-vencer:screen-value           in frame {&FRAME-NAME} = string(l-a-vencer)
         fi-dias-vencimento:screen-value    in frame {&FRAME-NAME} = string(i-dias-vencimento)
         tb-juros:screen-value              in frame {&FRAME-NAME} = string(l-juros)
         rs-juros:screen-value              in frame {&FRAME-NAME} = string(i-op-juros)
         de-perc-juros:screen-value         in frame {&FRAME-NAME} = string(de-juros)
         da-atraso:SCREEN-VALUE             IN FRAME {&FRAME-NAME} = string(da-data-atraso).  
  
  assign da-vcto-inf-ini = d-dt-vencto-inicial
         da-vcto-inf-fim = d-dt-vencto-final.
  
  apply 'value-changed':U to tb-juros      in frame {&FRAME-NAME}.
  apply 'value-changed':U to rs-juros      in frame {&FRAME-NAME}.
  apply 'value-changed':U to rs-titulo     in frame {&FRAME-NAME}.
  apply 'value-changed':U to tb-vencido    in frame {&FRAME-NAME}.
  apply 'value-changed':U to tb-a-vencer   in frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita D-Dialog 
PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  with frame {&FRAME-NAME}:

    if  tb-vencido:screen-value  = "no" and
        tb-a-vencer:screen-value = "no" then do:
        assign fi-dias-vencimento:screen-value  = string(0)
               fi-dias-vencimento:sensitive     = no.
    end.
    else  
        assign fi-dias-vencimento:sensitive     = yes.   
    
    IF tb-vencido:SCREEN-VALUE = "yes" THEN
       ASSIGN da-vcto-inf-fim = input frame {&FRAME-NAME} da-atraso
              da-vencto-ini:SCREEN-VALUE IN FRAME {&frame-name} = &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF                 
              da-vencto-fim:screen-value = 
                     string(da-vcto-inf-fim - int(fi-dias-vencimento:screen-value),"99/99/9999")
              da-vcto-inf-fim = da-vencto-fim.  
     
    ELSE
        IF tb-a-vencer:SCREEN-VALUE = "yes" THEN 
            ASSIGN da-vencto-fim:screen-value = 
                        string(da-vcto-inf-ini + int(fi-dias-vencimento:screen-value),"99/99/9999")  
                   da-vcto-inf-fim = da-vencto-fim.    
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

