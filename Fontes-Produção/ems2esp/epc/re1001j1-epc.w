&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/* Connected Databases 
          movind           PROGRESS
*/

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-saldo-terc NO-UNDO LIKE saldo-terc
       field r-rowid as rowid
       .



/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i re1001j1-epc 2.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        re1001j1-epc
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Detail


&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp d-valor-total d-qtd-total
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR h-boin090   AS HANDLE NO-UNDO.
DEF VAR h-boin404re AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR  p-serie-ini-re1001j-upc       AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-serie-fim-re1001j-upc       AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-docto-ini-re1001j-upc       AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-docto-fim-re1001j-upc       AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-nat-oper-ini-re1001j-upc    AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-nat-oper-fim-re1001j-upc    AS CHAR  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-proporcao-re1001j-upc       AS DEC   NO-UNDO.
DEF NEW GLOBAL SHARED VAR  p-item-ini-re1001j-upc        AS CHAR  NO-UNDO. 
DEF NEW GLOBAL SHARED VAR  p-item-fim-re1001j-upc        AS CHAR  NO-UNDO. 

DEF NEW GLOBAL SHARED VAR P-emitente-re1001       LIKE DOCUM-EST.COD-EMITENTE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-serie-re1001          LIKE DOCUM-EST.serie         NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-nat-operacao-re1001   LIKE DOCUM-EST.nat-operacao  NO-UNDO.
DEF NEW GLOBAL SHARED VAR P-nro-docto-re1001      LIKE DOCUM-EST.nro-docto     NO-UNDO.

def var i-cod-emitente     as int    no-undo.
def var c-nat-operacao-aux as char   no-undo.
def var h-boin176          as handle no-undo.
def var l-enable-pr-total  as log    no-undo.
def var l-enable-qtde      as log    no-undo.

{inbo/boin176.i5 tt-item-terc }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE brSon1                                        */


/* Definitions for FRAME fpage0                                         */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


FUNCTION fnDescItem RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btMarca 
     LABEL "&Marca" 
     SIZE 10 BY 1.

DEFINE BUTTON btNenhum 
     LABEL "&Nenhum" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE BUTTON btTodos 
     LABEL "&Todos" 
     SIZE 10 BY 1.

DEFINE VARIABLE d-qtd-total AS DECIMAL FORMAT "->>>>>,>>9.9999" INITIAL 0 
     LABEL "Quantidade Total" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE VARIABLE d-valor-total AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999" INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 15.17.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */

DEFINE QUERY brSon1 FOR 
      tt-saldo-terc SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE brSon1

  QUERY brSon1 DISPLAY
      tt-saldo-terc.check-sum COLUMN-LABEL "." FORMAT "x(1)":U
      tt-saldo-terc.nro-docto COLUMN-LABEL "Nro Docto" FORMAT "x(16)":U
      tt-saldo-terc.serie-docto COLUMN-LABEL "SÇrie" FORMAT "x(5)":U
      tt-saldo-terc.nat-operacao FORMAT "x(06)":U
      tt-saldo-terc.dt-retorno COLUMN-LABEL "Dt Emiss∆o" FORMAT "99/99/9999":U
            WIDTH 9
      tt-saldo-terc.it-codigo FORMAT "X(19)":U
      tt-saldo-terc.quantidade FORMAT ">>,>>>,>>>,>>9.9999":U WIDTH 14.86
      tt-saldo-terc.dec-1 COLUMN-LABEL "Qt Alocada" FORMAT ">>,>>>,>>>,>>9.9999":U
      tt-saldo-terc.valor[1] COLUMN-LABEL "Preáo Total" FORMAT ">>,>>>,>>>,>>9.99":U
      tt-saldo-terc.cod-depos FORMAT "x(3)":U WIDTH 5.57
      tt-saldo-terc.lote FORMAT "x(10)":U WIDTH 12
      tt-saldo-terc.cod-refer FORMAT "x(8)":U WIDTH 8
      fnDescItem() @ tt-saldo-terc.char-2 COLUMN-LABEL "Descriá∆o Item" FORMAT "x(60)":U
  ENABLE
      tt-saldo-terc.quantidade
      tt-saldo-terc.valor[1]
      tt-saldo-terc.cod-depos
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 87.29 BY 11.88
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     brSon1 AT ROW 1.54 COL 2.29
     d-qtd-total AT ROW 14 COL 63 COLON-ALIGNED
     d-valor-total AT ROW 15 COL 63 COLON-ALIGNED
     btOK AT ROW 16.75 COL 2
     btMarca AT ROW 16.75 COL 13
     btTodos AT ROW 16.75 COL 24
     btNenhum AT ROW 16.75 COL 35
     btCancel AT ROW 16.75 COL 46
     btHelp AT ROW 16.75 COL 80
     RECT-3 AT ROW 1.25 COL 1
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-saldo-terc T "?" NO-UNDO movind saldo-terc
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.04
         WIDTH              = 90
         MAX-HEIGHT         = 17.54
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17.54
         VIRTUAL-WIDTH      = 90
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


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* BROWSE-TAB brSon1 rtToolBar fpage0 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE brSon1
     _TblList          = "Temp-Tables.tt-saldo-terc"
     _FldNameList[1]   > Temp-Tables.tt-saldo-terc.check-sum
"tt-saldo-terc.check-sum" "." "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tt-saldo-terc.nro-docto
"tt-saldo-terc.nro-docto" "Nro Docto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tt-saldo-terc.serie-docto
"tt-saldo-terc.serie-docto" "SÇrie" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.tt-saldo-terc.nat-operacao
     _FldNameList[5]   > Temp-Tables.tt-saldo-terc.dt-retorno
"tt-saldo-terc.dt-retorno" "Dt Emiss∆o" ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.tt-saldo-terc.it-codigo
"tt-saldo-terc.it-codigo" ? "X(19)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.tt-saldo-terc.quantidade
"tt-saldo-terc.quantidade" ? ">>,>>>,>>>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no "14.86" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.tt-saldo-terc.dec-1
"tt-saldo-terc.dec-1" "Qt Alocada" ">>,>>>,>>>,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.tt-saldo-terc.valor[1]
"tt-saldo-terc.valor[1]" "Preáo Total" ">>,>>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.tt-saldo-terc.cod-depos
"tt-saldo-terc.cod-depos" ? ? "character" ? ? ? ? ? ? yes ? no no "5.57" yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.tt-saldo-terc.lote
"tt-saldo-terc.lote" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.tt-saldo-terc.cod-refer
"tt-saldo-terc.cod-refer" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[13]   > "_<CALC>"
"fnDescItem() @ tt-saldo-terc.char-2" "Descriá∆o Item" "x(60)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE brSon1 */



/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
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


ON MOUSE-SELECT-DBLCLICK OF brSon1 IN FRAME fpage0
DO:
  apply "choose":U to btMarca in frame fPage0.
  apply "entry":U to btMarca in frame fPage0.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */


ON ROW-DISPLAY OF brSon1 IN FRAME fpage0
DO:
    RUN piRowDisplay IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF brSon1 IN FRAME fpage0
DO:
    RUN piValueChanged IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF tt-saldo-terc.quantidade IN BROWSE brSon1 /* Qtde */
DO:

    def var da-dt-aux like docum-est.dt-trans no-undo.

    IF AVAIL tt-saldo-terc THEN DO:
        run findNaturOper in h-boin404re ( tt-saldo-terc.nat-operacao ).
    
        run findSaldoTerc in h-boin404re ( tt-saldo-terc.cod-emitente,
                                           tt-saldo-terc.serie-docto,
                                           tt-saldo-terc.nro-docto,
                                           tt-saldo-terc.nat-operacao,
                                           tt-saldo-terc.sequencia,
                                           tt-saldo-terc.it-codigo,
                                           tt-saldo-terc.cod-refer ).
    
        run getDateField in h-boin090 ( input "dt-trans":U,
                                        output da-dt-aux ).
    
        run getValuesTerceiros in h-boin404re ( input da-dt-aux,
                                                input 100,
                                                input input browse brSon1 tt-saldo-terc.quantidade,
                                                output tt-saldo-terc.valor[1],
                                                output tt-saldo-terc.dec-2 ).
    
        disp tt-saldo-terc.valor[1] with browse brSon1.                              
    END.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btMarca IN FRAME fpage0 /* Marca */
DO:

    if  not avail tt-saldo-terc then
        return no-apply.
    ASSIGN d-qtd-total   = 0
           d-valor-total = 0.

    if  tt-saldo-terc.check-sum = "*":U THEN DO:
        ASSIGN d-qtd-total   = decimal(INPUT FRAME fPage0 d-qtd-total)   - dec(INPUT BROWSE brSon1 tt-saldo-terc.quantidade)
               d-valor-total = decimal(INPUT FRAME fPage0 d-valor-total) - dec(INPUT BROWSE brSon1 tt-saldo-terc.valor[1]).
    END.
    ELSE
        ASSIGN d-qtd-total   = decimal(INPUT FRAME fPage0 d-qtd-total )  + dec(INPUT BROWSE brSon1 tt-saldo-terc.quantidade)
               d-valor-total = decimal(INPUT FRAME fPage0 d-valor-total) + dec(INPUT BROWSE brSon1 tt-saldo-terc.valor[1]).

    assign tt-saldo-terc.check-sum = if  tt-saldo-terc.check-sum = "*":U  
                                  then " ":U
                                  else "*":U.
    brSon1:select-focused-row().


    disp tt-saldo-terc.check-sum with browse brSon1.

    DISP d-qtd-total   
         d-valor-total WITH FRAME fPage0.

    apply "row-display":U to browse brSon1.

    apply "value-changed":U to browse brSon1.   

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btNenhum IN FRAME fpage0 /* Nenhum */
DO:

    for each tt-saldo-terc:
        assign tt-saldo-terc.check-sum = "":U.
    end.  

    open query brSon1 for each tt-saldo-terc.

    apply "value-changed":U to browse brSon1. 

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
  
    
        if  not valid-handle ( h-boin176 ) then
            run inbo/boin176.p persistent set h-boin176.
        run openQueryStatic in h-boin176 ( input "Main":U ).

        RUN setTtItemTerc IN THIS-PROCEDURE.

        IF RETURN-VALUE = "OK":U THEN
            APPLY "CLOSE":U TO THIS-PROCEDURE.

        /*--- Elimina a BO de item-doc-est ---*/
        if  valid-handle ( h-boin176 ) then
            delete procedure h-boin176.
            assign h-boin176 = ?.
    
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btTodos IN FRAME fpage0 /* Todos */
DO:

    for each tt-saldo-terc:
        assign tt-saldo-terc.check-sum = "*":U.
    end.  

    open query brSon1 for each tt-saldo-terc.

    apply "value-changed":U to browse brSon1. 

END.

/* _UIB-CODE-BLOCK-END */

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    if  valid-handle ( h-boin176 ) then
        delete procedure h-boin176.
        assign h-boin176 = ?.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE AfterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    assign btMarca:row    in frame fpage0 = btCancel:row in frame fPage0
           btTodos:row    in frame fPage0 = btCancel:row in frame fPage0
           btNenhum:row   in frame fPage0 = btCancel:row in frame fPage0
           btCancel:col   in frame fPage0 = 46.

    /*--- habilita os widgets da tela ---*/
    enable brSon1
           btMarca
           btTodos
           btNenhum
           btCancel
           with frame fPage0.

    DISABLE d-valor-total
            d-qtd-total WITH FRAME fPage0.
         
    RUN InitializeDBOs.

    return "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE InitializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var i-emit-aux      like docum-est.cod-emitente no-undo.
    def var c-nro-aux       like docum-est.nro-docto    no-undo.
    def var c-serie-aux     like docum-est.serie-docto  no-undo.
    def var c-nat-aux       like docum-est.nat-operacao no-undo.
    def var da-dt-aux       like docum-est.dt-trans     no-undo.

  
     if  not valid-handle (h-boin090) then
        RUN inbo/boin090.p PERSISTENT SET h-boin090.

     RUN openQueryStatic IN h-boin090 (INPUT "Docto":U).

     RUN goToKey IN h-boin090 (INPUT p-serie-re1001 , 
                               INPUT p-nro-docto-re1001,
                               INPUT p-emitente-re1001, 
                               INPUT p-nat-operacao-re1001 ).
     IF RETURN-VALUE = "NOK":U THEN DO:
         RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT "DOCUMENTO":U).
         RETURN NO-APPLY.
     END.

   

    /*--- Recupera a chave do docum-est ---*/
    run getKey in h-boin090 ( output i-emit-aux,
                              output c-serie-aux,
                              output c-nro-aux,
                              output c-nat-aux ).

    run getDateField in h-boin090 ( input "dt-trans":U,
                                    output da-dt-aux ).

    /*--- inicializa a BO da tabela saldo-terc  ---*/
    if  not valid-handle ( h-boin404re ) THEN 
        run inbo/boin404re.p persistent set h-boin404re.

    if  valid-handle ( h-boin404re ) OR 
        h-boin404re:TYPE = "PROCEDURE":U OR 
        h-boin404re:FILE-NAME = "inbo/boin404re.p":U THEN DO:

       /*--- cria a temp-table baseada no saldo-terc da nota de remessa ---*/ 

         run getBatchRecordsOfVariosDocumentos in h-boin404re ( input  i-emit-aux,
                                                                input  c-nat-aux,
                                                                input  da-dt-aux,
                                                                input  p-proporcao-re1001j-upc,
                                                                input  p-serie-ini-re1001j-upc ,
                                                                input  p-docto-ini-re1001j-upc ,
                                                                input  p-nat-oper-ini-re1001j-upc,
                                                                input  p-serie-fim-re1001j-upc ,
                                                                input  p-docto-fim-re1001j-upc ,
                                                                input  p-nat-oper-fim-re1001j-upc,
                                                                output table tt-saldo-terc ).

                                                             
         FOR EACH tt-saldo-terc:
             IF tt-saldo-terc.it-codigo < p-item-ini-re1001j-upc
                 OR tt-saldo-terc.it-codigo > p-item-fim-re1001j-upc THEN
                 DELETE tt-saldo-terc.
         END.                                                     






        /*--- verifica se deve ser habilitado ou n o o campo 
          valor[1] ( preco-total ) no browse ---*/
         run enableQtdePrecoTotal in h-boin404re ( input  c-nat-aux, /* Natureza da nota retorno */
                                                   output l-enable-qtde,
                                                   output l-enable-pr-total ). 


    END.

    assign tt-saldo-terc.quantidade:read-only in browse brSon1 = not l-enable-qtde
           tt-saldo-terc.valor[1]:read-only   in browse brSon1 = not l-enable-pr-total.

    FOR EACH tt-saldo-terc:
        ASSIGN tt-saldo-terc.check-sum = "*".
        ASSIGN d-qtd-total   = d-qtd-total   + tt-saldo-terc.quantidade
               d-valor-total = d-valor-total + tt-saldo-terc.valor[1].
    END.
 
    DISP d-qtd-total d-valor-total WITH FRAME fPage0.

    /*--- abre a query no browse ---*/
    open query brSon1 for each tt-saldo-terc.  

        
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE PiRowDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 if  avail tt-saldo-terc then do:

      assign tt-saldo-terc.quantidade:font        in browse brSon1 = 6
             tt-saldo-terc.valor[1]:font             in browse brSon1 = if l-enable-pr-total then 6 else ?
             tt-saldo-terc.cod-depos:font         in browse brSon1 = 6.

      if  tt-saldo-terc.check-sum = "*":U then
          assign tt-saldo-terc.nro-docto:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.serie-docto:fgcolor    in browse brSon1 = 9
                 tt-saldo-terc.nat-oper:fgcolor       in browse brSon1 = 9
                 tt-saldo-terc.check-sum:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.it-codigo:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.quantidade:fgcolor     in browse brSon1 = 9
                 tt-saldo-terc.dec-1:fgcolor          in browse brSon1 = 9
                 tt-saldo-terc.valor[1]:fgcolor       in browse brSon1 = 9
                 tt-saldo-terc.cod-depos:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.char-2:fgcolor         in browse brSon1 = 9
                 tt-saldo-terc.cod-refer:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.lote:fgcolor           in browse brSon1 = 9.
      else 
          ASSIGN tt-saldo-terc.nro-docto:fgcolor      in browse brSon1 = 9
                 tt-saldo-terc.serie-docto:fgcolor    in browse brSon1 = 9
                 tt-saldo-terc.nat-oper:fgcolor       in browse brSon1 = 9
                 tt-saldo-terc.check-sum:fgcolor      in browse brSon1 = ?
                 tt-saldo-terc.it-codigo:fgcolor      in browse brSon1 = ?
                 tt-saldo-terc.quantidade:fgcolor     in browse brSon1 = ?
                 tt-saldo-terc.dec-1:fgcolor          in browse brSon1 = ?
                 tt-saldo-terc.valor[1]:fgcolor       in browse brSon1 = ?
                 tt-saldo-terc.cod-depos:fgcolor      in browse brSon1 = ?
                 tt-saldo-terc.char-2:fgcolor         in browse brSon1 = ?
                 tt-saldo-terc.cod-refer:fgcolor      in browse brSon1 = ?
                 tt-saldo-terc.lote:fgcolor           in browse brSon1 = ?.     
  end.

  return "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE piValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if avail tt-saldo-terc then
     assign btMarca:label in frame fPage0 = if tt-saldo-terc.check-sum = "":U then "&Marca" 
                                            else "&Desmarca".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE setTtItemTerc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Notes: Cria a temp-table tt-item-terc e envia para a BO de item-doc-est
------------------------------------------------------------------------------*/

def var conta-transfer as CHAR no-undo.

def var i-cod-emitente       as int                           no-undo.
def var c-serie-docto        as char                          no-undo.
def var c-nro-docto          as char                          no-undo.
def var c-nat-operacao       as char                          no-undo.

assign conta-transfer = "".

    for each  tt-saldo-terc
        where tt-saldo-terc.check-sum = "*":U:

        create tt-item-terc.
        assign tt-item-terc.rw-saldo-terc = tt-saldo-terc.r-rowid
               tt-item-terc.quantidade    = tt-saldo-terc.quantidade
               tt-item-terc.preco-total   = tt-saldo-terc.valor[1]
               tt-item-terc.desconto      = tt-saldo-terc.dec-2
               tt-item-terc.cod-depos     = tt-saldo-terc.cod-depos
               tt-item-terc.nr-ord-prod   = tt-saldo-terc.nr-ord-prod.
			   
			       FOR each movto-estoq 
					   where movto-estoq.serie-docto  = tt-saldo-terc.serie-docto
					     and movto-estoq.nro-docto    = tt-saldo-terc.nro-docto
					     and movto-estoq.nat-operacao = tt-saldo-terc.nat-operacao 
					     AND movto-estoq.sc-codigo    = ""
					     AND movto-estoq.tipo-valor   = 2:
						
						assign conta-transfer = movto-estoq.ct-codigo.
						
				   END.


    end.
	
	

    /*--- elimina a temp-table de erros ---*/
    run emptyRowErrors in h-boin176.

    /*--- cria os registros conforme itens da tt-item-terc ---*/
    run createItemOfComponente in h-boin176 ( input h-boin090,
                                              input table tt-item-terc ).
											  
											  /*mauricio-DSC*/
											  
    run getKey in h-boin090 ( output i-cod-emitente,
                              output c-serie-docto,
                              output c-nro-docto,
                              output c-nat-operacao ).

  
    if  not avail docum-est 
    or  docum-est.serie-docto  <> c-serie-docto
    or  docum-est.nro-docto    <> c-nro-docto
    or  docum-est.nat-operacao <> c-nat-operacao
    or  docum-est.cod-emitente <> i-cod-emitente then 
    

	for first docum-est
		fields ( serie-docto   nro-docto     nat-operacao
				 cod-emitente  esp-docto     cod-estabel
				 rec-fisico    usuario       mod-atual
				 dt-trans      dt-emissao    tot-desconto
				 cod-observa   mo-codigo     valor-embal
				 valor-frete   valor-mercad  valor-seguro
				 valor-outras  ce-atual      char-1)
				 

		where docum-est.serie-docto  = c-serie-docto
		  and docum-est.nro-docto    = c-nro-docto
		  and docum-est.nat-operacao = c-nat-operacao
		  and docum-est.cod-emitente = i-cod-emitente :
	end.
							
	if avail(docum-est) then do:
			
		assign docum-est.ct-transit  = if conta-transfer <> "" then conta-transfer else docum-est.ct-transit.
			
    end.

    /*--- mostra os erros em tela quando retornar NOK ---*/
    if  return-value = "NOK":U then do:
        run getRowErrors in h-boin176 ( output table RowErrors ).
        run showErrors.
        for each tt-item-terc:
            delete tt-item-terc.
        end.
        return "NOK":U.
    end.

    /*--- Posiciona no primeiro registro do browse ---*/
    find first tt-saldo-terc no-error.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE showErrors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    
    /* Procedure Description */
    {method/showmessage.i1}

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).

    /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
    
    /* Procedure Description */
    {method/showmessage.i2}

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


/* ************************  Function Implementations ***************** */


FUNCTION fnDescItem RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var c-desc-item as char no-undo.

  run findItem in h-boin404re ( input tt-saldo-terc.it-codigo ).

  run getDescriptionFields in h-boin404re ( input "item.desc-item",
                                            output c-desc-item ).

  RETURN c-desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */




