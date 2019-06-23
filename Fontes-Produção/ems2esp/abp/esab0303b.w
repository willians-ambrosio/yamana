&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAB0303B 2.00.00.000}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESAB0303B
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Master/Detail

&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    0
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 btSave ~
                              cItem deQuant
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER p-action    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-num-docto AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER p-it-codigo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-quant     AS DECIMAL   NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

DEF VAR d-disponivel-disp AS DECIMAL FORMAT "->>>>>>,>>9.9999"    LABEL "Dispon°vel" NO-UNDO.
DEF VAR d-saldo-calculado AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999" LABEL "Quantidade na Data".
DEF VAR qt-disponivel     AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999"                    NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-saldo-zerado AS LOGICAL INITIAL NO NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-dt-saldo LIKE movto-estoq.dt-trans NO-UNDO.
DEFINE VARIABLE d-tot-disponivel AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE d-tot-saldo-calc AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE dt-saldo AS DATE FORMAT "99/99/9999":U NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar RECT-1 RECT-2 iNum-docto cItem ~
cDesc deQuant btOK btSave btCancel btHelp2 
&Scoped-Define DISPLAYED-OBJECTS iNum-docto cItem cDesc deQuant 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-data wWindow 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btSave 
     LABEL "Salvar" 
     SIZE 10 BY 1.

DEFINE VARIABLE cDesc AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE cItem AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE deQuant AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE iNum-docto AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.46.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.08.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     iNum-docto AT ROW 1.79 COL 13 COLON-ALIGNED
     cItem AT ROW 4 COL 13 COLON-ALIGNED
     cDesc AT ROW 4 COL 30 COLON-ALIGNED NO-LABEL
     deQuant AT ROW 5 COL 13 COLON-ALIGNED
     btOK AT ROW 6.71 COL 2
     btSave AT ROW 6.71 COL 13
     btCancel AT ROW 6.71 COL 24
     btHelp2 AT ROW 6.71 COL 80
     rtToolBar AT ROW 6.5 COL 1
     RECT-1 AT ROW 1.04 COL 1
     RECT-2 AT ROW 3.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 7.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 6.96
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN validateRecord IN THIS-PROCEDURE.
    IF RETURN-VALUE = "OK":U THEN DO:
        RUN piGrava IN THIS-PROCEDURE.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wWindow
ON CHOOSE OF btSave IN FRAME fpage0 /* Salvar */
DO:
    RUN validateRecord IN THIS-PROCEDURE.
    IF RETURN-VALUE = "OK":U THEN DO:
        RUN piGrava IN THIS-PROCEDURE.
        ASSIGN cItem:SCREEN-VALUE IN FRAME fPage0   = "":U
               deQuant:SCREEN-VALUE IN FRAME fPage0 = "0,00":U.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItem wWindow
ON F5 OF cItem IN FRAME fpage0 /* Item */
DO:
   assign l-implanta = yes.
  {include/zoomvar.i  &prog-zoom="inzoom\z01in172.w"
                      &campo=cItem
                      &campozoom=it-codigo
                      &campo2=cDesc
                      &campozoom2=desc-item}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItem wWindow
ON LEAVE OF cItem IN FRAME fpage0 /* Item */
DO:
    FOR FIRST ITEM
        WHERE ITEM.it-codigo = cItem:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
    END.
    ASSIGN cDesc = IF AVAIL ITEM THEN ITEM.desc-item ELSE "":U.

    DISPLAY cDesc WITH FRAME fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItem wWindow
ON MOUSE-SELECT-DBLCLICK OF cItem IN FRAME fpage0 /* Item */
DO:
  APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
cItem:LOAD-MOUSE-POINTER("image\lupa.cur":U) IN FRAME fPage0.

{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN iNum-docto:SCREEN-VALUE IN FRAME fPage0 = STRING(p-num-docto)
           cItem:SCREEN-VALUE      IN FRAME fPage0 = p-it-codigo
           deQuant:SCREEN-VALUE    IN FRAME fPage0 = STRING(p-quant).

    IF p-action = "ADD" THEN
        ASSIGN cItem:SENSITIVE IN FRAME fPage0 = YES.
    ELSE
        ASSIGN cItem:SENSITIVE IN FRAME fPage0 = NO.

    APPLY "LEAVE":U TO cItem IN FRAME fPage0.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava wWindow 
PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR FIRST ITEM
        WHERE ITEM.it-codigo = cItem:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
        FOR EACH saldo-estoq OF ITEM NO-LOCK:
            ASSIGN d-saldo-calculado = fn-qtd-data().
            IF d-saldo-calculado = 0 AND NOT g-saldo-zerado THEN NEXT.
    
            ASSIGN  qt-disponivel = saldo-estoq.qtidade-atu  -
                                    saldo-estoq.qt-alocada   -
                                    saldo-estoq.qt-aloc-prod -
                                    saldo-estoq.qt-aloc-ped.
    
            ASSIGN d-disponivel-disp = d-disponivel-disp + qt-disponivel.        
        END.
    END.

    IF p-action = "ADD":U THEN DO:
        CREATE mab-evento-mat.
        ASSIGN mab-evento-mat.num-docto  = p-num-docto
               mab-evento-mat.descricao  = cDesc:SCREEN-VALUE IN FRAME fPage0
               mab-evento-mat.it-codigo  = cItem:SCREEN-VALUE IN FRAME fPage0               
               mab-evento-mat.quantidade = DECIMAL(deQuant:SCREEN-VALUE IN FRAME fPage0)
               mab-evento-mat.saldo = IF DECIMAL(deQuant:SCREEN-VALUE IN FRAME fPage0) < d-disponivel-disp THEN YES ELSE NO.
               
    END.
    ELSE DO:
        FOR FIRST mab-evento-mat
            WHERE mab-evento-mat.num-docto = p-num-docto
            AND   mab-evento-mat.it-codigo = cItem:SCREEN-VALUE IN FRAME fPage0 EXCLUSIVE-LOCK:
            ASSIGN mab-evento-mat.quantidade = DECIMAL(deQuant:SCREEN-VALUE IN FRAME fPage0)
                   mab-evento-mat.saldo      = IF DECIMAL(deQuant:SCREEN-VALUE IN FRAME fPage0) < d-disponivel-disp THEN YES ELSE NO.
        END.
    END.

    /*
    O campo MAB-EVENTO-MAT.saldo Ç l¢gico e dever† realizar a seguinte validaá∆o:
                Se MAB-EVENTO-MAT.quantidade < Total Dispon°vel (CE0830)
                        MAB-EVENTO-MAT.saldo = Yes.
Se MAB-EVENTO-MAT.quantidade > Total Dispon°vel (CE0830)
                        MAB-EVENTO-MAT.saldo = No

*/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateRecord wWindow 
PROCEDURE validateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF p-action = "ADD":U THEN DO:
        /** Chave Duplicada **/
        IF CAN-FIND (FIRST mab-evento-mat
                     WHERE mab-evento-mat.num-docto = INT(iNum-docto:SCREEN-VALUE IN FRAME fPage0)
                     AND   mab-evento-mat.it-codigo = cItem:SCREEN-VALUE IN FRAME fPage0) THEN DO:
                RUN utp/ut-msgs.p (INPUT "SHOW":U,
                                   INPUT 1,
                                   INPUT "Item":U).
            RETURN "NOK".
        END.
    END.

    {utp/ut-liter.i "Item"}
    /** Item n∆o cadastrada **/
    IF NOT CAN-FIND(FIRST ITEM
                    WHERE ITEM.it-codigo = cItem:SCREEN-VALUE IN FRAME fPage0 NO-LOCK) THEN DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 56,
                           INPUT RETURN-VALUE).
        RETURN "NOK".
    END.

    IF DECIMAL(deQuant:SCREEN-VALUE IN FRAME fPage0) = 0 THEN DO:
        RUN utp/ut-msgs.p (INPUT "SHOW":U,
                           INPUT 32272,
                           INPUT RETURN-VALUE).
        RETURN "NOK".
    END.


    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-data wWindow 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

assign d-tot-disponivel = 0
       d-tot-saldo-calc = 0
       dt-saldo = g-dt-saldo.
assign d-tot-disponivel  = d-tot-disponivel + (saldo-estoq.qtidade-atu - saldo-estoq.qt-alocada -
                                               saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped)
       d-saldo-calculado = saldo-estoq.qtidade-atu.

IF  item.tipo-con-est = 1
    OR  g-dt-saldo = TODAY /* se for a data que vem padr∆o (today) nao ira lcoalizar nenhum movimento
                              neste caso o melhor indice e' por data mesmo que o controle seja por lote */
    THEN
        open query qr-movto
        for each movto-estoq use-index item-data where
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    and
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    and
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  and
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    and
                             movto-estoq.lote        = saldo-estoq.lote         and
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  and
                             movto-estoq.esp-docto  <> 37                         and
                             movto-estoq.dt-trans    > g-dt-saldo no-lock.
    ELSE
        open query qr-movto
        for each movto-estoq use-index item-estab where
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    and
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    and
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  and
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    and
                             movto-estoq.lote        = saldo-estoq.lote         and
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  and
                             movto-estoq.esp-docto  <> 37                         and
                             movto-estoq.dt-trans    > g-dt-saldo no-lock.


get first qr-movto.
do while avail (movto-estoq):
    if movto-estoq.tipo-trans = 1 then
        assign d-saldo-calculado = d-saldo-calculado - movto-estoq.quantidade.
    else
        assign d-saldo-calculado = d-saldo-calculado + movto-estoq.quantidade.
    get next qr-movto.
end. /* while */ 

/*bloco comentado por efeito documentacional - sem funá∆o no programa*/
/* assign d-tot-saldo-calc = d-tot-saldo-calc + d-saldo-calculado.              */
/* find first tt-saldo where                                                    */
/*      tt-saldo.it-codigo   = b-saldo-estoq.it-codigo   and                    */
/*      tt-saldo.cod-depos   = b-saldo-estoq.cod-depos   and                    */
/*      tt-saldo.cod-estabel = b-saldo-estoq.cod-estabel and                    */
/*      tt-saldo.cod-localiz = b-saldo-estoq.cod-localiz and                    */
/*      tt-saldo.cod-refer   = b-saldo-estoq.cod-refer   and                    */
/*      tt-saldo.lote        = b-saldo-estoq.lote no-error.                     */
/* if not avail tt-saldo then do:                                               */
/*     create tt-saldo.                                                         */
/*     assign tt-saldo.it-codigo   = b-saldo-estoq.it-codigo                    */
/*            tt-saldo.cod-depos   = b-saldo-estoq.cod-depos                    */
/*            tt-saldo.cod-estabel = b-saldo-estoq.cod-estabel                  */
/*            tt-saldo.cod-localiz = b-saldo-estoq.cod-localiz                  */
/*            tt-saldo.cod-refer   = b-saldo-estoq.cod-refer                    */
/*            tt-saldo.lote        = b-saldo-estoq.lote                         */
/*            tt-saldo.qtidade-data = tt-saldo.qtidade-data + d-saldo-calculado */
/*            tt-saldo.qtidade-disp = tt-saldo.qtidade-disp + d-tot-disponivel. */
/*                                                                              */
/* end.                                                                         */


  RETURN d-saldo-calculado.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

