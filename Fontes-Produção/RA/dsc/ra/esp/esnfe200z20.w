&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
  Altera‡Æo: 20/05/2014 - L‚a Campos - 
             Find do emitente passa a ser pelo nome-abrev e nÆo mais
             pelo cgc (Devido ao cadastro de inativo com cnpj duplicado)
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
{dsc\ra\include\variaveis-nfe-receb.i}
DEFINE INPUT  PARAMETER p_cod_estabel AS CHAR                 NO-UNDO.
DEFINE INPUT  PARAMETER p_nome_abrev  AS CHAR                 NO-UNDO.
DEFINE INPUT  PARAMETER p_origem      AS INTE                 NO-UNDO.
DEFINE INPUT  PARAMETER p_it_codigo   AS CHAR  FORMAT "x(16)" NO-UNDO.
DEFINE OUTPUT PARAMETER p_rw_ordem    AS ROWID                NO-UNDO.
DEFINE OUTPUT PARAMETER pi-num-parc   AS INT                  NO-UNDO.

/* Local Variable Definitions ---  
                                     */


DEFINE TEMP-TABLE tt-pedido-ordem NO-UNDO
     FIELD num-pedido       LIKE pedido-compr.num-pedido
     FIELD numero-ordem     LIKE ordem-compra.numero-ordem
     FIELD parcela          LIKE prazo-compra.parcela
     FIELD it-codigo        LIKE ordem-compra.it-codigo
     FIELD cod-estabel      LIKE pedido-compr.cod-estabel
     FIELD dt-entrega       AS   DATE FORMAT "99/99/9999"
     FIELD preco-fornec     LIKE ordem-compra.preco-fornec
     FIELD desc-item        LIKE ITEM.desc-item
     FIELD cod-comprado     LIKE ordem-compra.cod-comprado
     FIELD cod-cond-pag     LIKE ordem-compra.cod-cond-pag
     FIELD nr-contrato      LIKE ordem-compra.nr-contrato
     FIELD rw-ordem         AS   ROWID
     INDEX xx dt-entrega numero-ordem.

DEF VAR de-qtd-rec-forn     LIKE ordem-compra.preco-fornec NO-UNDO.
DEF VAR de-qtd-receber      LIKE ordem-compra.preco-fornec NO-UNDO.
DEF VAR de-indice-aux       AS   DECI                      NO-UNDO.
DEF VAR da-data-entrega     AS   DATE                      NO-UNDO.
DEF VAR i-quant             AS   INT                       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-ordem-compra

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pedido-ordem

/* Definitions for BROWSE br-ordem-compra                               */
&Scoped-define FIELDS-IN-QUERY-br-ordem-compra tt-pedido-ordem.numero-ordem tt-pedido-ordem.parcela tt-pedido-ordem.it-codigo tt-pedido-ordem.num-pedido tt-pedido-ordem.cod-estabel tt-pedido-ordem.dt-entrega tt-pedido-ordem.preco-fornec tt-pedido-ordem.desc-item tt-pedido-ordem.cod-comprado tt-pedido-ordem.cod-cond-pag tt-pedido-ordem.nr-contrato   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ordem-compra   
&Scoped-define SELF-NAME br-ordem-compra
&Scoped-define QUERY-STRING-br-ordem-compra FOR EACH tt-pedido-ordem NO-LOCK             /*EACH prazo-compra WHERE prazo-compra.numero-ordem = tt-pedido-ordem.numero-ordem                                 AND prazo-compra.it-codigo    = tt-pedido-ordem.it-codigo                                 AND prazo-compra.situacao     = 2 /* --- Confirmada --- */                                 AND (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0                                 NO-LOCK BY prazo-compra.numero-ordem                                         BY prazo-compra.parcela     */ INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-ordem-compra OPEN QUERY {&SELF-NAME}     FOR EACH tt-pedido-ordem NO-LOCK             /*EACH prazo-compra WHERE prazo-compra.numero-ordem = tt-pedido-ordem.numero-ordem                                 AND prazo-compra.it-codigo    = tt-pedido-ordem.it-codigo                                 AND prazo-compra.situacao     = 2 /* --- Confirmada --- */                                 AND (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0                                 NO-LOCK BY prazo-compra.numero-ordem                                         BY prazo-compra.parcela     */ INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-ordem-compra tt-pedido-ordem
&Scoped-define FIRST-TABLE-IN-QUERY-br-ordem-compra tt-pedido-ordem


/* Definitions for BROWSE br-pedido-compr                               */
&Scoped-define FIELDS-IN-QUERY-br-pedido-compr tt-pedido-ordem.num-pedido tt-pedido-ordem.numero-ordem tt-pedido-ordem.parcela tt-pedido-ordem.it-codigo tt-pedido-ordem.cod-estabel tt-pedido-ordem.dt-entrega tt-pedido-ordem.preco-fornec tt-pedido-ordem.desc-item tt-pedido-ordem.cod-comprado tt-pedido-ordem.cod-cond-pag tt-pedido-ordem.nr-contrato   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedido-compr   
&Scoped-define SELF-NAME br-pedido-compr
&Scoped-define QUERY-STRING-br-pedido-compr FOR EACH tt-pedido-ordem NO-LOCK         BY tt-pedido-ordem.dt-entrega INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-pedido-compr OPEN QUERY {&SELF-NAME}     FOR EACH tt-pedido-ordem NO-LOCK         BY tt-pedido-ordem.dt-entrega INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-pedido-compr tt-pedido-ordem
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedido-compr tt-pedido-ordem


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-ordem-compra}~
    ~{&OPEN-QUERY-br-pedido-compr}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-desc-item fi-i-cod-emitente-ini ~
fi-i-cod-emitente-fim fi-i-num-pedido-ini fi-i-numero-ordem-ini ~
fi-i-numero-ordem-fim fi-i-num-pedido-fim fi-c-it-codigo-ini ~
fi-c-it-codigo-fim bt-confirma br-ordem-compra br-pedido-compr btn-ok ~
btn-cancelar RECT-41 RECT-42 IMAGE-22 IMAGE-23 IMAGE-21 RECT-34 IMAGE-18 ~
RECT-36 
&Scoped-Define DISPLAYED-OBJECTS ed-desc-item fi-c-cod-estabel ~
fi-c-nome-estab fi-i-cod-emitente-ini fi-i-cod-emitente-fim ~
fi-i-num-pedido-ini fi-i-numero-ordem-ini fi-i-numero-ordem-fim ~
fi-i-num-pedido-fim fi-c-it-codigo-ini fi-c-it-codigo-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON btn-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "dsc\ra\img\cancelar.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.13 TOOLTIP "Cancelar".

DEFINE BUTTON btn-ok AUTO-END-KEY 
     IMAGE-UP FILE "dsc\ra\img\salvar.bmp":U
     LABEL "Ok" 
     SIZE 10 BY 1.13 TOOLTIP "Confirmar".

DEFINE VARIABLE ed-desc-item AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 84 BY 2.25 NO-UNDO.

DEFINE VARIABLE fi-c-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-nome-estab AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 58.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-num-pedido-fim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-num-pedido-ini AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-numero-ordem-fim AS INTEGER FORMAT "999999,99":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-numero-ordem-ini AS INTEGER FORMAT "zzzzz999":U INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-18
     FILENAME "dsc\ra\img\dsc.bmp":U
     SIZE 4.86 BY 1.42.

DEFINE IMAGE IMAGE-21
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.72 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.72 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.72 BY .88.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 85 BY 1.92
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 8.5.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 3.5.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 1.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ordem-compra FOR 
      tt-pedido-ordem SCROLLING.

DEFINE QUERY br-pedido-compr FOR 
      tt-pedido-ordem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ordem-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ordem-compra D-Dialog _FREEFORM
  QUERY br-ordem-compra NO-LOCK DISPLAY
      tt-pedido-ordem.numero-ordem                                                                                     COLUMN-LABEL "Ordem"            WIDTH 12
tt-pedido-ordem.parcela                                                                                          COLUMN-LABEL "Par"            WIDTH 02
tt-pedido-ordem.it-codigo                                                                                        COLUMN-LABEL "Item"
tt-pedido-ordem.num-pedido                                                                                       COLUMN-LABEL "Pedido"
tt-pedido-ordem.cod-estabel                                                                                      COLUMN-LABEL "Estab"
tt-pedido-ordem.dt-entrega                                                                                       COLUMN-LABEL "Dt Entrega"       WIDTH 11
tt-pedido-ordem.preco-fornec                                                                                     COLUMN-LABEL "Preco Forn"
tt-pedido-ordem.desc-item                                                                                        COLUMN-LABEL "Descricao Item"
tt-pedido-ordem.cod-comprado                                                                                     COLUMN-LABEL "Comprador"
tt-pedido-ordem.cod-cond-pag                                                                                     COLUMN-LABEL "Condicao"
tt-pedido-ordem.nr-contrato                                                                                      COLUMN-LABEL "Contrato"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 83.43 BY 8.04
         FONT 1
         TITLE "Ordem Compra" FIT-LAST-COLUMN.

DEFINE BROWSE br-pedido-compr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedido-compr D-Dialog _FREEFORM
  QUERY br-pedido-compr NO-LOCK DISPLAY
      tt-pedido-ordem.num-pedido                                                                                       COLUMN-LABEL "Pedido"           
tt-pedido-ordem.numero-ordem                                                                                     COLUMN-LABEL "Ordem"            WIDTH 08
tt-pedido-ordem.parcela                                                                                          COLUMN-LABEL "Pa"            WIDTH 02
tt-pedido-ordem.it-codigo                                                                                        COLUMN-LABEL "Item"             WIDTH 15
tt-pedido-ordem.cod-estabel                                                                                      COLUMN-LABEL "Estab"
tt-pedido-ordem.dt-entrega                                                                                       COLUMN-LABEL "Dt Entrega"       WIDTH 11
tt-pedido-ordem.preco-fornec                                                                                     COLUMN-LABEL "Preco Forn"
tt-pedido-ordem.desc-item                                                                                        COLUMN-LABEL "Descricao Item"
tt-pedido-ordem.cod-comprado                                                                                     COLUMN-LABEL "Comprador"
tt-pedido-ordem.cod-cond-pag                                                                                     COLUMN-LABEL "Condicao"
tt-pedido-ordem.nr-contrato                                                                                      COLUMN-LABEL "Contrato"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 83.43 BY 7.92
         FONT 1
         TITLE "Pedido Compra" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     ed-desc-item AT ROW 14.75 COL 1.86 NO-LABEL WIDGET-ID 224
     fi-c-cod-estabel AT ROW 1.38 COL 19.72 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 196
     fi-c-nome-estab AT ROW 1.38 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 200
     fi-i-cod-emitente-ini AT ROW 2.79 COL 28 COLON-ALIGNED WIDGET-ID 194
     fi-i-cod-emitente-fim AT ROW 2.79 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 220
     fi-i-num-pedido-ini AT ROW 3.79 COL 28 COLON-ALIGNED WIDGET-ID 160
     fi-i-numero-ordem-ini AT ROW 3.79 COL 28 COLON-ALIGNED WIDGET-ID 210
     fi-i-numero-ordem-fim AT ROW 3.79 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     fi-i-num-pedido-fim AT ROW 3.79 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     fi-c-it-codigo-ini AT ROW 4.79 COL 22 COLON-ALIGNED WIDGET-ID 214
     fi-c-it-codigo-fim AT ROW 4.79 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     bt-confirma AT ROW 4.63 COL 79.72 WIDGET-ID 182
     br-ordem-compra AT ROW 6.29 COL 2.29 WIDGET-ID 300
     br-pedido-compr AT ROW 6.42 COL 2.29 WIDGET-ID 200
     btn-ok AT ROW 17.92 COL 22.29 WIDGET-ID 152
     btn-cancelar AT ROW 17.92 COL 55.57 WIDGET-ID 150
     RECT-41 AT ROW 2.5 COL 1.57 WIDGET-ID 190
     RECT-42 AT ROW 1.13 COL 1.57 WIDGET-ID 192
     IMAGE-22 AT ROW 4.79 COL 42.29 WIDGET-ID 216
     IMAGE-23 AT ROW 2.79 COL 42.29 WIDGET-ID 222
     IMAGE-21 AT ROW 3.79 COL 42.29 WIDGET-ID 166
     RECT-34 AT ROW 17.46 COL 1.57 WIDGET-ID 156
     IMAGE-18 AT ROW 17.75 COL 80.72 WIDGET-ID 154
     RECT-36 AT ROW 6.13 COL 1.57 WIDGET-ID 158
     SPACE(0.28) SKIP(4.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "esnfe200z11 - Pesquisa Pedido/Ordem de Compra" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-ordem-compra bt-confirma D-Dialog */
/* BROWSE-TAB br-pedido-compr br-ordem-compra D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       ed-desc-item:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN fi-c-cod-estabel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-nome-estab IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ordem-compra
/* Query rebuild information for BROWSE br-ordem-compra
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH tt-pedido-ordem NO-LOCK
            /*EACH prazo-compra WHERE prazo-compra.numero-ordem = tt-pedido-ordem.numero-ordem
                                AND prazo-compra.it-codigo    = tt-pedido-ordem.it-codigo
                                AND prazo-compra.situacao     = 2 /* --- Confirmada --- */
                                AND (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0
                                NO-LOCK BY prazo-compra.numero-ordem
                                        BY prazo-compra.parcela     */ INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-ordem-compra */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedido-compr
/* Query rebuild information for BROWSE br-pedido-compr
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH tt-pedido-ordem NO-LOCK
        BY tt-pedido-ordem.dt-entrega INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedido-compr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* esnfe200z11 - Pesquisa Pedido/Ordem de Compra */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ordem-compra
&Scoped-define SELF-NAME br-ordem-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ordem-compra D-Dialog
ON MOUSE-SELECT-CLICK OF br-ordem-compra IN FRAME D-Dialog /* Ordem Compra */
DO:

   ASSIGN pi-num-parc = prazo-compra.parcela. 

   FIND FIRST ITEM NO-LOCK
       WHERE ITEM.it-codigo = tt-pedido-ordem.it-codigo NO-ERROR.

   IF AVAIL ITEM THEN DO:
            
       ASSIGN ed-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item + " - " + ITEM.narrativa.

   END.
   ELSE DO:

       
       ASSIGN ed-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


   END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ordem-compra D-Dialog
ON MOUSE-SELECT-DBLCLICK OF br-ordem-compra IN FRAME D-Dialog /* Ordem Compra */
DO:
    APPLY "CHOOSE" TO btn-ok IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ordem-compra D-Dialog
ON VALUE-CHANGED OF br-ordem-compra IN FRAME D-Dialog /* Ordem Compra */
DO:
  APPLY 'MOUSE-SELECT-CLICK' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedido-compr
&Scoped-define SELF-NAME br-pedido-compr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedido-compr D-Dialog
ON MOUSE-SELECT-CLICK OF br-pedido-compr IN FRAME D-Dialog /* Pedido Compra */
DO:

   ASSIGN pi-num-parc = 0. 
   
   FIND FIRST ITEM NO-LOCK
       WHERE ITEM.it-codigo = tt-pedido-ordem.it-codigo NO-ERROR.

   IF AVAIL ITEM THEN DO:
            
       ASSIGN ed-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item + " - " + ITEM.narrativa.

   END.
   ELSE DO:

       
       ASSIGN ed-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedido-compr D-Dialog
ON MOUSE-SELECT-DBLCLICK OF br-pedido-compr IN FRAME D-Dialog /* Pedido Compra */
DO:
    
    APPLY "CHOOSE" TO btn-ok IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedido-compr D-Dialog
ON ROW-ENTRY OF br-pedido-compr IN FRAME D-Dialog /* Pedido Compra */
DO:
  MESSAGE "ROW ENTRY"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedido-compr D-Dialog
ON VALUE-CHANGED OF br-pedido-compr IN FRAME D-Dialog /* Pedido Compra */
DO:
  APPLY 'MOUSE-SELECT-CLICK' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma D-Dialog
ON CHOOSE OF bt-confirma IN FRAME D-Dialog /* Button 1 */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        EMPTY TEMP-TABLE tt-pedido-ordem.
            
        FOR EACH pedido-compr WHERE 
                 pedido-compr.cod-emitente >= INPUT fi-i-cod-emitente-ini AND 
                 pedido-compr.cod-emitente <= INPUT fi-i-cod-emitente-fim AND 
                 pedido-compr.cod-estabel   = INPUT fi-c-cod-estabel      NO-LOCK,
            EACH ordem-compra WHERE 
                 ordem-compra.num-pedido = pedido-compr.num-pedido AND 
                 ordem-compra.situacao   = 2                       NO-LOCK: /* --- 2 - Confirmada --- */

            IF p_origem = 1 THEN
                IF ordem-compra.num-pedido < INPUT fi-i-num-pedido-ini OR
                   ordem-compra.num-pedido > INPUT fi-i-num-pedido-fim THEN NEXT.               
                   
            IF p_origem = 2 THEN
                IF ordem-compra.numero-ordem < INPUT fi-i-numero-ordem-ini OR
                   ordem-compra.numero-ordem > INPUT fi-i-numero-ordem-fim THEN NEXT.
    
            IF ordem-compra.it-codigo < INPUT fi-c-it-codigo-ini OR
               ordem-compra.it-codigo > INPUT fi-c-it-codigo-fim THEN NEXT.
            
            ASSIGN da-data-entrega = ?
                   de-qtd-receber  = 0
                   de-indice-aux   = 0.        
            
            FOR EACH prazo-compra NO-LOCK
                 WHERE prazo-compra.numero-ordem                       = ordem-compra.numero-ordem  
                 AND   prazo-compra.situacao                           = 2 /* --- Confirmada --- */ 
                 AND   (prazo-compra.quant-saldo - prazo-compra.dec-1) > 0     
                BREAK BY prazo-compra.parcela:
            
                FIND FIRST ITEM no-lock
                    WHERE item.it-codigo = ordem-compra.it-codigo  NO-ERROR.
                    
                FIND FIRST item-fornec no-lock
                    WHERE item-fornec.it-codigo    = ordem-compra.it-codigo    
                    AND   item-fornec.cod-emitente = pedido-compr.cod-emitente NO-ERROR.
            
                CREATE tt-pedido-ordem.
    
                ASSIGN tt-pedido-ordem.num-pedido      = pedido-compr.num-pedido
                       tt-pedido-ordem.numero-ordem    = ordem-compra.numero-ordem
                       tt-pedido-ordem.parcela         = prazo-compra.parcela
                       tt-pedido-ordem.it-codigo       = ordem-compra.it-codigo
                       tt-pedido-ordem.cod-estabel     = pedido-compr.cod-estabel
                       tt-pedido-ordem.dt-entrega      = prazo-compra.data-entrega
                       tt-pedido-ordem.preco-forn      = ordem-compra.preco-fornec
                       tt-pedido-ordem.desc-item       = IF AVAIL item THEN ITEM.desc-item ELSE ""
                       tt-pedido-ordem.cod-comprado    = ordem-compra.cod-comprado
                       tt-pedido-ordem.cod-cond-pag    = ordem-compra.cod-cond-pag
                       tt-pedido-ordem.nr-contrato     = ordem-compra.nr-contrato
                       tt-pedido-ordem.rw-ordem        = ROWID(ordem-compra).
            END. /* FOR EACH prazo-compra FIELDS..... */
        END. /* FOR EACH pedido-compr WHERE.... */
        IF p_origem = 1 THEN {&open-query-br-pedido-compr}
        IF p_origem = 2 THEN {&open-query-br-ordem-compra}
    END. /* DO WITH FRAME {&FRAME-NAME}..... */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancelar D-Dialog
ON CHOOSE OF btn-cancelar IN FRAME D-Dialog /* Cancelar */
DO:
    ASSIGN p_rw_ordem = ?.
              
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok D-Dialog
ON CHOOSE OF btn-ok IN FRAME D-Dialog /* Ok */
DO:
    IF AVAIL tt-pedido-ordem THEN 
        ASSIGN p_rw_ordem  = tt-pedido-ordem.rw-ordem
               pi-num-parc = tt-pedido-ordem.parcela.
        
    ELSE ASSIGN p_rw_ordem = ?.

    APPLY "GO" TO FRAME {&FRAME-NAME}.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-cod-emitente-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-cod-emitente-fim D-Dialog
ON ENTRY OF fi-i-cod-emitente-fim IN FRAME D-Dialog
DO:
    RUN pi-desabilita.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ordem-compra
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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
  DISPLAY ed-desc-item fi-c-cod-estabel fi-c-nome-estab fi-i-cod-emitente-ini 
          fi-i-cod-emitente-fim fi-i-num-pedido-ini fi-i-numero-ordem-ini 
          fi-i-numero-ordem-fim fi-i-num-pedido-fim fi-c-it-codigo-ini 
          fi-c-it-codigo-fim 
      WITH FRAME D-Dialog.
  ENABLE ed-desc-item fi-i-cod-emitente-ini fi-i-cod-emitente-fim 
         fi-i-num-pedido-ini fi-i-numero-ordem-ini fi-i-numero-ordem-fim 
         fi-i-num-pedido-fim fi-c-it-codigo-ini fi-c-it-codigo-fim bt-confirma 
         br-ordem-compra br-pedido-compr btn-ok btn-cancelar RECT-41 RECT-42 
         IMAGE-22 IMAGE-23 IMAGE-21 RECT-34 IMAGE-18 RECT-36 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    
    FIND emitente WHERE 
         emitente.nome-abrev = p_nome_abrev NO-LOCK NO-ERROR.

    FIND FIRST estabelec WHERE 
               estabelec.cod-estabel = p_cod_estabel NO-LOCK NO-ERROR.

    ASSIGN fi-i-cod-emitente-ini = emitente.cod-emitente
           fi-i-cod-emitente-fim = emitente.cod-emitente
           fi-c-cod-estabel      = IF AVAIL estabelec THEN estabelec.cod-estabel ELSE ""
           fi-c-nome-estab       = IF AVAIL estabelec THEN estabelec.nome ELSE "".

    IF TRIM(p_it_codigo) <> "" THEN
        ASSIGN fi-c-it-codigo-ini = p_it_codigo
               fi-c-it-codigo-fim = p_it_codigo.
    ELSE
        ASSIGN fi-c-it-codigo-ini = ""
               fi-c-it-codigo-fim = "ZZZZZZZZZZZZZZZZ".
    
    
    /* Dispatch standard ADM method.                             */

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    RUN pi-desabilita.

    FIND FIRST param-re no-lock
        WHERE param-re.usuario = c-seg-usuario NO-ERROR. 
    IF param-re.rec-out-for = YES THEN DO:
        ASSIGN fi-i-cod-emitente-ini:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               fi-i-cod-emitente-fim:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE DO:
        ASSIGN fi-i-cod-emitente-ini:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               fi-i-cod-emitente-fim:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    END.


    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita D-Dialog 
PROCEDURE pi-desabilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF p_origem = 1 THEN
        ASSIGN br-ordem-compra:HIDDEN IN FRAME {&FRAME-NAME}       = YES
               fi-i-numero-ordem-ini:HIDDEN IN FRAME {&FRAME-NAME} = YES
               fi-i-numero-ordem-fim:HIDDEN IN FRAME {&FRAME-NAME} = YES
               br-pedido-compr:HIDDEN IN FRAME {&FRAME-NAME}       = NO
               fi-i-num-pedido-ini:HIDDEN IN FRAME {&FRAME-NAME}   = NO
               fi-i-num-pedido-fim:HIDDEN IN FRAME {&FRAME-NAME}   = NO.
            
    IF p_origem = 2 THEN
        ASSIGN br-pedido-compr:HIDDEN IN FRAME {&FRAME-NAME}       = YES
               fi-i-num-pedido-ini:HIDDEN IN FRAME {&FRAME-NAME}   = YES
               fi-i-num-pedido-fim:HIDDEN IN FRAME {&FRAME-NAME}   = YES
               br-ordem-compra:HIDDEN IN FRAME {&FRAME-NAME}       = NO
               fi-i-numero-ordem-ini:HIDDEN IN FRAME {&FRAME-NAME} = NO
               fi-i-numero-ordem-fim:HIDDEN IN FRAME {&FRAME-NAME} = NO.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-pedido-ordem"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-saldo-ordem D-Dialog 
FUNCTION fc-saldo-ordem RETURNS DECIMAL
  ( p-numero-ordem AS INT , p-parcela AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

