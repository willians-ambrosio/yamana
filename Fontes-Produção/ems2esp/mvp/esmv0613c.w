&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0613C 2.00.00.001}  /*** 010004 ***/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0613C
&GLOBAL-DEFINE Version        2.00.00.001

&GLOBAL-DEFINE WindowType    Detail

&GLOBAL-DEFINE Folder         no
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   

/**Definiá∆o das vari†veis da tela de parÉmetros **/
&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 ~
                              fiAtualiza fiCalend fiCorte fiPercFim fiPercIni ~
                              rsPlano tgAtivos tgAVencer tgInativos tgTpEventos ~
                              tgMotor tgNMotor tgProprios tgServico tgTerceiros ~
                              fiHora tgPlanoComp tgFuturo cb-durabilidade

{mvp/esmv0613.i} /** Definiá∆o da ttSelecao **/

/* Parameters Definitions --- */
define input-output parameter table for ttSelecao.

DEFINE VARIABLE wh-pesquisa AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-29 IMAGE-30 RECT-1 RECT-2 RECT-3 ~
RECT-4 RECT-5 RECT-6 rtToolBar fiCorte fiPercIni fiPercFim tgMotor tgAtivos ~
tgProprios tgNMotor tgInativos tgTerceiros tgAVencer tgPlanoComp tgServico ~
cb-durabilidade tgTpEventos rsPlano tgFuturo fiCalend fiAtualiza fiHora ~
btOK btCancel btHelp2 fiTexto1 fiTexto2 fiTexto3 fiMin fiTHora 
&Scoped-Define DISPLAYED-OBJECTS fiCorte fiPercIni fiPercFim tgMotor ~
tgAtivos tgProprios tgNMotor tgInativos tgTerceiros tgAVencer tgPlanoComp ~
tgServico cb-durabilidade tgTpEventos rsPlano tgFuturo fiCalend fiAtualiza ~
fiHora fiTexto1 fiTexto2 fiTexto3 fiMin fiTHora 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE cb-durabilidade AS CHARACTER FORMAT "X(256)":U 
     LABEL "Durabilidade Sub-Sistemas" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Vencimento","Todos","Garantia","N∆o verifica" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiAtualiza AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Atualiza tela" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fiCalend AS CHARACTER FORMAT "X(10)" 
     LABEL "Calend†rio":R21 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fiCorte AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Corte Vencimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiHora AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Antecipaá∆o na previs∆o de tÇrmino das Ordens de Manutená∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiMin AS CHARACTER FORMAT "X(256)":U INITIAL "minutos" 
      VIEW-AS TEXT 
     SIZE 8 BY .67 NO-UNDO.

DEFINE VARIABLE fiPercFim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiPercIni AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Percurso Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiTexto1 AS CHARACTER FORMAT "X(256)":U INITIAL "Equipamentos" 
      VIEW-AS TEXT 
     SIZE 11 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto2 AS CHARACTER FORMAT "X(256)":U INITIAL "Tarefas" 
      VIEW-AS TEXT 
     SIZE 7 BY .67 NO-UNDO.

DEFINE VARIABLE fiTexto3 AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Evento" 
      VIEW-AS TEXT 
     SIZE 9 BY .67 NO-UNDO.

DEFINE VARIABLE fiTHora AS CHARACTER FORMAT "X(256)":U INITIAL "horas" 
      VIEW-AS TEXT 
     SIZE 8 BY .67 NO-UNDO.

DEFINE IMAGE IMAGE-29
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE rsPlano AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Plano Manutená∆o", 1,
"Calend†rio Manutená∆o", 2
     SIZE 50 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 3.25.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.25.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 2.25.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tgAtivos AS LOGICAL INITIAL no 
     LABEL "Ativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgAVencer AS LOGICAL INITIAL no 
     LABEL "A Vencer" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgFuturo AS LOGICAL INITIAL no 
     LABEL "Exibe programaá∆o futura dos planos" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .83 NO-UNDO.

DEFINE VARIABLE tgInativos AS LOGICAL INITIAL no 
     LABEL "Inativos" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.14 BY .83 NO-UNDO.

DEFINE VARIABLE tgMotor AS LOGICAL INITIAL no 
     LABEL "Motorizados" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE tgNMotor AS LOGICAL INITIAL no 
     LABEL "N∆o Motorizados" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.14 BY .88 NO-UNDO.

DEFINE VARIABLE tgPlanoComp AS LOGICAL INITIAL no 
     LABEL "Planos de Componentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE tgProprios AS LOGICAL INITIAL no 
     LABEL "Pr¢prios" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgServico AS LOGICAL INITIAL no 
     LABEL "Serviáos (Pneus)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

DEFINE VARIABLE tgTerceiros AS LOGICAL INITIAL no 
     LABEL "Terceiros" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgTpEventos AS LOGICAL INITIAL yes 
     LABEL "Eventos Pendentes Back Log" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     fiCorte AT ROW 1.5 COL 20 COLON-ALIGNED
     fiPercIni AT ROW 2.5 COL 20 COLON-ALIGNED
     fiPercFim AT ROW 2.5 COL 43 COLON-ALIGNED NO-LABEL
     tgMotor AT ROW 4.29 COL 7
     tgAtivos AT ROW 4.29 COL 32
     tgProprios AT ROW 4.29 COL 58
     tgNMotor AT ROW 5.29 COL 7
     tgInativos AT ROW 5.29 COL 32
     tgTerceiros AT ROW 5.29 COL 58
     tgAVencer AT ROW 7.25 COL 7
     tgPlanoComp AT ROW 7.25 COL 32
     tgServico AT ROW 7.25 COL 58
     cb-durabilidade AT ROW 8.25 COL 56 COLON-ALIGNED
     tgTpEventos AT ROW 10.92 COL 7
     rsPlano AT ROW 13.08 COL 7 NO-LABEL
     tgFuturo AT ROW 14 COL 7
     fiCalend AT ROW 15.33 COL 13.57 COLON-ALIGNED HELP
          "Calend†rio Padr∆o"
     fiAtualiza AT ROW 15.33 COL 56 COLON-ALIGNED HELP
          "Atualiza a tela pela quantidade de minutos"
     fiHora AT ROW 16.33 COL 56 COLON-ALIGNED
     btOK AT ROW 17.67 COL 2
     btCancel AT ROW 17.67 COL 13
     btHelp2 AT ROW 17.67 COL 80
     fiTexto1 AT ROW 3.71 COL 2 COLON-ALIGNED NO-LABEL
     fiTexto2 AT ROW 6.25 COL 2 COLON-ALIGNED NO-LABEL
     fiTexto3 AT ROW 9.83 COL 2 COLON-ALIGNED NO-LABEL
     fiMin AT ROW 15.46 COL 62 COLON-ALIGNED NO-LABEL
     fiTHora AT ROW 16.46 COL 67 COLON-ALIGNED NO-LABEL
     IMAGE-29 AT ROW 2.5 COL 41.57
     IMAGE-30 AT ROW 2.5 COL 32.72
     RECT-1 AT ROW 4 COL 1.57
     RECT-2 AT ROW 6.54 COL 1.57
     RECT-3 AT ROW 12.79 COL 1.57
     RECT-4 AT ROW 10.08 COL 1.57
     RECT-5 AT ROW 1.21 COL 1.57
     RECT-6 AT ROW 15.17 COL 1.57
     rtToolBar AT ROW 17.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.88
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.88
         WIDTH              = 90
         MAX-HEIGHT         = 18
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 18
         VIRTUAL-WIDTH      = 90
         MAX-BUTTON         = no
         RESIZE             = no
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
    run piGrava in this-procedure.
    if return-value = "NOK":U then
        return no-apply.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCalend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCalend wWindow
ON MOUSE-SELECT-DBLCLICK OF fiCalend IN FRAME fpage0 /* Calend†rio */
OR "F5":U OF fiCalend IN FRAME fPage0 DO:
  {include/zoomvar.i  
           &prog-zoom="inzoom/z01in026.w"
           &campo=fiCalend
           &campozoom=cd-calen}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCorte wWindow
ON LEAVE OF fiCorte IN FRAME fpage0 /* Data Corte Vencimento */
DO:
    DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iMes AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iDia AS INTEGER    NO-UNDO.

    if input frame fPage0 fiCorte > today then do:
        if not(fiPercIni:sensitive in frame fPage0) then do:
            assign fiPercFim = today - day(today)
                   fiPercIni = date(month(fiPercFim),1,year(fiPercFim)).
            display fiPercIni fiPercFim 
                with frame fPage0.
        end.
        assign fiPercIni:sensitive in frame fPage0 = yes
               fiPercFim:sensitive in frame fPage0 = yes.
    end.
    else do:
        assign fiPercIni:sensitive in frame fPage0 = no
               fiPercFim:sensitive in frame fPage0 = no.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializaá∆o do programam ---*/
fiCalend:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame fPage0.

{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap¢s inicializaá∆o da tela.
------------------------------------------------------------------------------*/

/** Labels dos campos **/
{utp/ut-liter.i "Equipamento"}
assign fiTexto1:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Tarefas"}
assign fiTexto2:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Tipo Evento"}
assign fiTexto3:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Data Corte Vencimento"}
assign fiCorte:label in frame fPage0 = return-value.
{utp/ut-liter.i "Percurso Equipamento"}
assign fiPercIni:label in frame fPage0 = return-value.
{utp/ut-liter.i "Ativos"}
assign tgAtivos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Inativos"}
assign tgInativos:label in frame fPage0 = return-value.
{utp/ut-liter.i "Pr¢prios"}
assign tgProprios:label in frame fPage0 = return-value.
{utp/ut-liter.i "Terceiros"}
assign tgTerceiros:label in frame fPage0 = return-value.
{utp/ut-liter.i "Motorizados"}
assign tgMotor:label in frame fPage0 = return-value.
{utp/ut-liter.i "N∆o Motorizados"}
assign tgNMotor:label in frame fPage0 = return-value.
{utp/ut-liter.i "A Vencer"}
assign tgAVencer:label in frame fPage0 = return-value.
{utp/ut-liter.i "Serviáos (Pneus)"}
assign tgServico:label in frame fPage0 = return-value.
{utp/ut-liter.i "Planos de Componentes"}
assign tgPlanoComp:label in frame fPage0 = return-value.
{utp/ut-liter.i "Exibe programaá∆o futura dos planos"}
assign tgFuturo:label in frame fPage0 = return-value.
{utp/ut-liter.i "Calend†rio"}
assign fiCalend:label in frame fPage0 = return-value.
{utp/ut-liter.i "Atualiza tela em"}
assign fiAtualiza:label in frame fPage0 = return-value.
{utp/ut-liter.i "minutos"}
assign fiMin:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "horas"}
assign fiTHora:screen-value in frame fPage0 = return-value.
{utp/ut-liter.i "Antecipaá∆o na previs∆o de tÇrmino das Ordens de Manutená∆o"}
assign fiHora:label in frame fPage0 = return-value.
{utp/ut-liter.i "Durabilidade Sub-Sistemas"}
assign cb-durabilidade:label in frame fPage0 = return-value.
{utp/ut-liter.i "Eventos Pendentes Back Log"}
assign tgTpEventos:label in frame fPage0 = return-value.

/** Busca ParÉmtros **/
find first ttSelecao no-lock no-error.
if avail ttSelecao then do:
    assign tgAtivos:checked      in frame fPage0    = ttSelecao.lAtivos          
           tgProprios:checked    in frame fPage0    = ttSelecao.lProprios        
           tgInativos:checked    in frame fPage0    = ttSelecao.lInativos        
           tgTerceiros:checked   in frame fPage0    = ttSelecao.lTerceiros   
           tgMotor:checked       in frame fPage0    = ttSelecao.lMotor     
           tgNMotor:checked      in frame fPage0    = ttSelecao.lNMotor            
           tgAVencer:checked     in frame fPage0    = ttSelecao.lAVencer   
           tgServico:checked     in frame fPage0    = ttSelecao.lServico              
           tgPlanoComp:checked   in frame fPage0    = ttSelecao.lPlanoComp
           tgFuturo:checked      in frame fPage0    = ttSelecao.lPlanoFuturo
           tgTpEventos:checked   in frame fPage0    = ttSelecao.lTpEventos
           rsPlano                                  = ttSelecao.iPlano
           fiCorte                                  = ttSelecao.dt-corte   
           fiPercIni                                = ttSelecao.dt-perc-ini
           fiPercFim                                = ttSelecao.dt-perc-fim
           fiCalend:screen-value  in frame fPage0   = ttSelecao.cod-calend
           fiAtualiza                               = ttSelecao.iMinutos
           fiHora                                   = ttSelecao.deAntecipa
           cb-durabilidade:screen-value  in frame fPage0 = entry(ttselecao.idi-durabilidade,cb-durabilidade:LIST-ITEMS).

    display fiCorte fiPercIni fiPercFim fiAtualiza rsPlano fiHora
        with frame fPage0.
    if fiCorte > today then do:
        assign fiPercIni:sensitive in frame fPage0 = yes
               fiPercFim:sensitive in frame fPage0 = yes.
    end.
    else do:
        assign fiPercIni:sensitive in frame fPage0 = no
               fiPercFim:sensitive in frame fPage0 = no.
    end.
end.

assign tgPlanoComp:hidden in frame fPage0     = yes
       tgServico:hidden in frame fPage0       = yes
       cb-durabilidade:hidden in frame fPage0 = yes.

apply "VALUE-CHANGED":U to rsPlano in frame fPage0.
apply "LEAVE":U         to fiCorte in frame fPage0.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava wWindow 
PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     piGrava
  leters:  <none>
  Notes:       Grava os parÉmetros selecionados na temp-table
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTexto AS CHARACTER  NO-UNDO.

do with frame fPage0:
    assign rsPlano.
end.

/** Data de Corte n∆o deve ser menor que atual **/
if input frame fPage0 fiCorte < today then do:
    {utp/ut-liter.i "Corte Vencimento"}
    assign cTexto = trim(return-value).
    {utp/ut-liter.i "Atual"}
    run utp/ut-msgs.p (input "SHOW":U,
                      input 89,
                      input cTexto + '~~' + return-value).
    return "NOK":U.
end.
else do:
    if input frame fPage0 fiCorte > today then do:
        /** Data Inicial n∆o deve ser maior que atual **/
        if input frame fPage0 fiPercIni > today then do:
            {utp/ut-liter.i "Percurso Inicial"}
            assign cTexto = trim(return-value).
            {utp/ut-liter.i "Atual"}
            run utp/ut-msgs.p (input "SHOW":U,
                              input 26584,
                              input cTexto + '~~' + return-value).
            return "NOK":U.
        end.
        /** Data Final n∆o deve ser maior que atual **/
        if input frame fPage0 fiPercFim > today then do:
            {utp/ut-liter.i "Percurso Final"}
            assign cTexto = trim(return-value).
            {utp/ut-liter.i "Atual"}
            run utp/ut-msgs.p (input "SHOW":U,
                              input 26584,
                              input cTexto + '~~' + return-value).
            return "NOK":U.
        end.
    end.
end.
/** Calend†rio n∆o cadastrado **/
if input frame fPage0 fiCalend <> "" and
   not can-find (first calen-gener 
                 where calen-gener.cd-calen = input frame fPage0 fiCalend no-lock) then do:
    {utp/ut-liter.i "Calend†rio"} 
    run utp/ut-msgs.p (input "SHOW":U,
                      input 56,
                      input return-value).
    return "NOK":U.
end.
/** Minutos n∆o devem ser menores que 1 **/
if input frame fPage0 fiAtualiza < 1 then do:
    {utp/ut-liter.i "Minutos"}
    assign cTexto = trim(return-value).
    run utp/ut-msgs.p (input "SHOW":U,
                      input 7744,
                      input cTexto + '~~' + '1').
    return "NOK":U.
end.

find first ttSelecao exclusive-lock no-error.
if avail ttSelecao then do:
    assign ttSelecao.lAtivos      = tgAtivos:checked      in frame fPage0      
           ttSelecao.lProprios    = tgProprios:checked    in frame fPage0      
           ttSelecao.lInativos    = tgInativos:checked    in frame fPage0      
           ttSelecao.lTerceiros   = tgTerceiros:checked   in frame fPage0  
           ttSelecao.lMotor       = tgMotor:checked       in frame fPage0
           ttSelecao.lNMotor      = tgNMotor:checked      in frame fPage0       
           ttSelecao.lAVencer     = tgAVencer:checked     in frame fPage0
           ttSelecao.lServico     = tgServico:checked     in frame fPage0
           ttSelecao.lPlanoComp   = tgPlanoComp:checked   in frame fPage0
           ttSelecao.lPlanoFuturo = tgFuturo:checked      in frame fPage0
           ttSelecao.lTpEventos   = tgTpEventos:checked   in frame fPage0
           ttSelecao.iPlano       = rsPlano
           ttSelecao.dt-corte     = input frame fPage0 fiCorte
           ttSelecao.dt-perc-ini  = input frame fPage0 fiPercIni
           ttSelecao.dt-perc-fim  = input frame fPage0 fiPercFim
           ttSelecao.cod-calend   = input frame fPage0 fiCalend
           ttSelecao.iMinutos     = input frame fPage0 fiAtualiza
           ttSelecao.deAntecipa   = input frame fPage0 fiHora.
           &IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN 
               If lookup(cb-durabilidade:screen-value,cb-durabilidade:LIST-ITEM-PAIRS in frame fPage0,",") mod 2 <> 0 Then
                  ASSIGN ttSelecao.idi-durabilidade = (lookup(cb-durabilidade:screen-value,cb-durabilidade:LIST-ITEM-PAIRS in frame fPage0,",") + 1) / 2.
               else
                  ASSIGN ttSelecao.idi-durabilidade = lookup(cb-durabilidade:screen-value,cb-durabilidade:LIST-ITEM-PAIRS in frame fPage0,",") / 2.
           &else
               ASSIGN ttSelecao.idi-durabilidade = LOOKUP(cb-durabilidade:SCREEN-VALUE IN FRAME fPage0,cb-durabilidade:LIST-ITEMS).
           &endif
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

