&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cons-ind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cons-ind 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esp-esmv0616 0.12.00.000}

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

DEF NEW GLOBAL SHARED VAR i-ep-codigo-usuario LIKE mguni.empresa.ep-codigo NO-UNDO.
DEF TEMP-TABLE tt-disp-dia
  NO-UNDO
  FIELD cd-equipto LIKE equipto.cd-equipto
  FIELD data       LIKE data-turno-calen.data
  FIELD disponib   AS DEC
  INDEX idx01 AS PRIMARY UNIQUE cd-equipto data.
DEF TEMP-TABLE tt-equipamento 
  NO-UNDO 
  LIKE equipto
  FIELD tt-pmpl            AS DEC /* FORMAT ">,>>>,>>9.9999999999999999" */
  FIELD tt-mttr            AS DEC
  FIELD tt-mtbf            AS DEC
  FIELD tt-disponibilidade AS DEC.
DEF TEMP-TABLE tt-tag-manutencao 
  NO-UNDO 
  LIKE tag
  FIELD tt-disponibilidade AS DEC.
DEF TEMP-TABLE tt-ordem-manutencao 
  NO-UNDO 
  LIKE ord-manut
  FIELD tt-preventiva       AS LOG FORMAT "Sim/Nao"
  FIELD tt-tipo             AS CHAR FORMAT "X(30)"
  FIELD tt-tot-hora         AS DEC
  FIELD tt-hs-maq-real-prev AS DEC FORMAT ">>,>>9.99".
DEF VAR c-desc-equip LIKE equipto.descricao NO-UNDO.
DEF VAR c-arquivo    AS CHARACTER           NO-UNDO.
DEF VAR l-ok         AS LOGICAL             NO-UNDO.
def var h-acomp      as handle              no-undo.
{mvp\esp-esmv0616.i1}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-equipamentos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-equipamento tt-ordem-manutencao ~
tt-tag-manutencao

/* Definitions for BROWSE br-equipamentos                               */
&Scoped-define FIELDS-IN-QUERY-br-equipamentos /* tt-equipamento.cd-tag */ tt-equipamento.ep-codigo tt-equipamento.cod-estabel tt-equipamento.cd-calen tt-equipamento.cd-equipto tt-equipamento.descricao tt-equipamento.tt-disponibilidade tt-equipamento.tt-pmpl tt-equipamento.tt-mttr tt-equipamento.tt-mtbf ""   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-equipamentos   
&Scoped-define SELF-NAME br-equipamentos
&Scoped-define QUERY-STRING-br-equipamentos FOR EACH tt-equipamento
&Scoped-define OPEN-QUERY-br-equipamentos OPEN QUERY br-equipamentos FOR EACH tt-equipamento.
&Scoped-define TABLES-IN-QUERY-br-equipamentos tt-equipamento
&Scoped-define FIRST-TABLE-IN-QUERY-br-equipamentos tt-equipamento


/* Definitions for BROWSE br-ordens                                     */
&Scoped-define FIELDS-IN-QUERY-br-ordens tt-ordem-manutencao.tt-preventiva tt-ordem-manutencao.tt-tipo tt-ordem-manutencao.nr-ord-produ tt-ordem-manutencao.dt-manut tt-ordem-manutencao.dt-parada tt-ordem-manutencao.dt-retorno tt-ordem-manutencao.hr-parada tt-ordem-manutencao.hr-retorno tt-ordem-manutencao.tt-tot-hora ""   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ordens   
&Scoped-define SELF-NAME br-ordens
&Scoped-define QUERY-STRING-br-ordens FOR EACH tt-ordem-manutencao
&Scoped-define OPEN-QUERY-br-ordens OPEN QUERY br-ordens FOR EACH tt-ordem-manutencao.
&Scoped-define TABLES-IN-QUERY-br-ordens tt-ordem-manutencao
&Scoped-define FIRST-TABLE-IN-QUERY-br-ordens tt-ordem-manutencao


/* Definitions for BROWSE br-tag                                        */
&Scoped-define FIELDS-IN-QUERY-br-tag tt-tag-manutencao.cd-tag tt-tag-manutencao.descricao tt-tag-manutencao.tt-disponibilidade ""   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tag   
&Scoped-define SELF-NAME br-tag
&Scoped-define QUERY-STRING-br-tag FOR EACH tt-tag-manutencao
&Scoped-define OPEN-QUERY-br-tag OPEN QUERY br-tag FOR EACH tt-tag-manutencao.
&Scoped-define TABLES-IN-QUERY-br-tag tt-tag-manutencao
&Scoped-define FIRST-TABLE-IN-QUERY-br-tag tt-tag-manutencao


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-equipamentos}~
    ~{&OPEN-QUERY-br-ordens}~
    ~{&OPEN-QUERY-br-tag}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button btAbrir btSalvar btFiltro ~
btRefresh btExcel br-tag br-equipamentos br-ordens 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cons-ind AS WIDGET-HANDLE NO-UNDO.

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
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAbrir 
     IMAGE-UP FILE "image\im-open":U
     IMAGE-INSENSITIVE FILE "image\ii-open":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Abrir".

DEFINE BUTTON btExcel 
     IMAGE-UP FILE "image\excel":U
     LABEL "Excel" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar".

DEFINE BUTTON btFiltro 
     IMAGE-UP FILE "image/im-fil":U
     IMAGE-INSENSITIVE FILE "image/ii-fil":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Filtro".

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar".

DEFINE BUTTON btSalvar 
     IMAGE-UP FILE "image/im-grava":U
     IMAGE-INSENSITIVE FILE "image/ii-grava":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 127 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-equipamentos FOR 
      tt-equipamento SCROLLING.

DEFINE QUERY br-ordens FOR 
      tt-ordem-manutencao SCROLLING.

DEFINE QUERY br-tag FOR 
      tt-tag-manutencao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-equipamentos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-equipamentos w-cons-ind _FREEFORM
  QUERY br-equipamentos DISPLAY
      /* tt-equipamento.cd-tag */
tt-equipamento.ep-codigo      
tt-equipamento.cod-estabel 
tt-equipamento.cd-calen
tt-equipamento.cd-equipto
tt-equipamento.descricao
tt-equipamento.tt-disponibilidade COLUMN-LABEL "Disponib"
tt-equipamento.tt-pmpl COLUMN-LABEL "PMPL"
tt-equipamento.tt-mttr COLUMN-LABEL "MTTR" 
tt-equipamento.tt-mtbf COLUMN-LABEL "MTBF" 
""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 4.5
         FONT 1
         TITLE "EQUIPAMENTOS" ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE br-ordens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ordens w-cons-ind _FREEFORM
  QUERY br-ordens DISPLAY
      tt-ordem-manutencao.tt-preventiva COLUMN-LABEL "Prev"      
tt-ordem-manutencao.tt-tipo       COLUMN-LABEL "Tipo" WIDTH 30
tt-ordem-manutencao.nr-ord-produ 
tt-ordem-manutencao.dt-manut      COLUMN-LABEL "Dt Manut"
tt-ordem-manutencao.dt-parada   
tt-ordem-manutencao.dt-retorno    COLUMN-LABEL "Dt Retorno"
tt-ordem-manutencao.hr-parada
tt-ordem-manutencao.hr-retorno
tt-ordem-manutencao.tt-tot-hora   COLUMN-LABEL "Tempo Parada"
""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 4.5
         FONT 1
         TITLE "ORDENS DE MANUTEN€ÇO" ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE br-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tag w-cons-ind _FREEFORM
  QUERY br-tag DISPLAY
      tt-tag-manutencao.cd-tag
 tt-tag-manutencao.descricao
 tt-tag-manutencao.tt-disponibilidade COLUMN-LABEL "Disponib"
 ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 4.5
         FONT 1
         TITLE "TAGs" ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btAbrir AT ROW 1.13 COL 2.14 WIDGET-ID 6
     btSalvar AT ROW 1.13 COL 6.29 WIDGET-ID 8
     btFiltro AT ROW 1.13 COL 10.43 WIDGET-ID 2
     btRefresh AT ROW 1.13 COL 14.72 WIDGET-ID 4
     btExcel AT ROW 1.13 COL 18.86 WIDGET-ID 10
     br-tag AT ROW 2.75 COL 1 WIDGET-ID 200
     br-equipamentos AT ROW 7.5 COL 1 WIDGET-ID 300
     br-ordens AT ROW 12.21 COL 1 WIDGET-ID 400
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.29 BY 16.42
         FONT 1 WIDGET-ID 100.


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
  CREATE WINDOW w-cons-ind ASSIGN
         HIDDEN             = YES
         TITLE              = "Gerenciador de Indicadores"
         HEIGHT             = 16.08
         WIDTH              = 127.29
         MAX-HEIGHT         = 28.88
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 28.88
         VIRTUAL-WIDTH      = 182.86
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cons-ind 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cons-ind
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-tag btExcel f-cad */
/* BROWSE-TAB br-equipamentos br-tag f-cad */
/* BROWSE-TAB br-ordens br-equipamentos f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cons-ind)
THEN w-cons-ind:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-equipamentos
/* Query rebuild information for BROWSE br-equipamentos
     _START_FREEFORM
OPEN QUERY br-equipamentos FOR EACH tt-equipamento.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-equipamentos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ordens
/* Query rebuild information for BROWSE br-ordens
     _START_FREEFORM
OPEN QUERY br-ordens FOR EACH tt-ordem-manutencao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-ordens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tag
/* Query rebuild information for BROWSE br-tag
     _START_FREEFORM
OPEN QUERY br-tag FOR EACH tt-tag-manutencao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-tag */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cons-ind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cons-ind w-cons-ind
ON END-ERROR OF w-cons-ind /* Gerenciador de Indicadores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cons-ind w-cons-ind
ON WINDOW-CLOSE OF w-cons-ind /* Gerenciador de Indicadores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-equipamentos
&Scoped-define SELF-NAME br-equipamentos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-equipamentos w-cons-ind
ON VALUE-CHANGED OF br-equipamentos IN FRAME f-cad /* EQUIPAMENTOS */
DO:
  IF AVAIL tt-equipamento THEN
    DO:
      OPEN QUERY br-ordens 
        FOR EACH tt-ordem-manutencao
          WHERE tt-ordem-manutencao.cd-tag     = tt-equipamento.cd-tag
          AND   tt-ordem-manutencao.cd-equipto = tt-equipamento.cd-equipto
          NO-LOCK.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-tag
&Scoped-define SELF-NAME br-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tag w-cons-ind
ON VALUE-CHANGED OF br-tag IN FRAME f-cad /* TAGs */
DO:
  IF AVAIL tt-tag-manutencao THEN
    DO:
      OPEN QUERY br-equipamentos 
        FOR EACH tt-equipamento
        WHERE tt-equipamento.cd-tag = tt-tag-manutencao.cd-tag.
    END.  
  IF AVAIL tt-equipamento THEN
    DO:
      OPEN QUERY br-ordens 
        FOR EACH tt-ordem-manutencao
          WHERE tt-ordem-manutencao.cd-tag     = tt-equipamento.cd-tag
          AND   tt-ordem-manutencao.cd-equipto = tt-equipamento.cd-equipto
          NO-LOCK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAbrir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAbrir w-cons-ind
ON CHOOSE OF btAbrir IN FRAME f-cad /* Filtro */
DO:
   find first ttSelecao no-lock no-error.  
   IF NOT AVAIL ttSelecao THEN
     RUN criaTTSelecao.
   /** Troca as barras **/
   assign c-arquivo = replace(c-arquivo, "/", "~\").
   /** Abre caixa para abrir arquivo **/
   SYSTEM-DIALOG GET-FILE c-arquivo
       FILTERS "*.dat" "*.dat",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "dat"
       INITIAL-DIR entry (1, propath)
       USE-FILENAME
       UPDATE l-ok.
   /** Retorno de OK, abre o arquivo **/
   if l-ok then do:
      if search (c-arquivo) <> ? then
         run carregaFiltro (input c-arquivo).
   end.

/*    apply "ENTRY":U to btAbrir in frame fPage0. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel w-cons-ind
ON CHOOSE OF btExcel IN FRAME f-cad /* Excel */
DO:
  find first ttSelecao no-lock no-error.  
  IF NOT AVAIL ttSelecao THEN
    RUN criaTTSelecao.
  session:set-wait-state ("GENERAL").
  RUN mvp\esp-esmv0616b.p(
    input table ttSelecao,
    input table tt-equipamento,
    input table tt-tag-manutencao,
    input table tt-disp-dia,
    INPUT IF AVAIL tt-equipamento THEN tt-equipamento.cd-equipto ELSE ?). 
  session:set-wait-state ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFiltro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltro w-cons-ind
ON CHOOSE OF btFiltro IN FRAME f-cad /* Filtro */
DO:
  assign w-cons-ind:sensitive = no.
  find first ttSelecao no-lock no-error.  
  IF NOT AVAIL ttSelecao THEN
    RUN criaTTSelecao.
  RUN mvp\esp-esmv0616a.w(input-output  table ttSelecao).
  assign w-cons-ind:sensitive = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh w-cons-ind
ON CHOOSE OF btRefresh IN FRAME f-cad /* Filtro */
DO:
  find first ttSelecao no-lock no-error.  
  IF NOT AVAIL ttSelecao THEN
    RUN criaTTSelecao.
  session:set-wait-state ("GENERAL").
  run pi-executar.   
  session:set-wait-state ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSalvar w-cons-ind
ON CHOOSE OF btSalvar IN FRAME f-cad /* Filtro */
DO:
  find first ttSelecao no-lock no-error.  
  IF NOT AVAIL ttSelecao THEN
    RUN criaTTSelecao.
   /** Troca os tipos de barras **/ 
  assign c-arquivo = replace(c-arquivo, "/", "~\").

  /** Abre caixa de escolha de arquivos **/
  SYSTEM-DIALOG GET-FILE c-arquivo
      FILTERS "*.dat" "*.dat",
              "*.*" "*.*"
      ASK-OVERWRITE
      DEFAULT-EXTENSION "dat"
      INITIAL-DIR entry (1, propath)
      SAVE-AS
      USE-FILENAME
      UPDATE l-ok.
  /** Retorno de OK **/
  if l-ok then do:
     output to value (c-arquivo).
     /** Salva Sele‡Æo **/
     for first ttSelecao:
         export delimiter ";"
                ttSelecao.periodo-ini
                ttSelecao.periodo-fim
                ttSelecao.empresa-ini
                ttSelecao.empresa-fim
                ttSelecao.equipto-ini
                ttSelecao.equipto-fim
                ttSelecao.estab-ini
                ttSelecao.estab-fim
                ttSelecao.grupo-ini
                ttSelecao.grupo-fim
                ttSelecao.modelo-ini
                ttSelecao.modelo-fim
                ttSelecao.estrut-ini
                ttSelecao.estrut-fim
                ttSelecao.tag-ini
                ttSelecao.tag-fim
                ttSelecao.cc-ini
                ttSelecao.cc-fim
                ttSelecao.ano-fabric-ini
                ttSelecao.ano-fabric-fim
                ttSelecao.lMtbf
                ttSelecao.lMttr
                ttSelecao.lDispo
                ttSelecao.lPmpl
                ttSelecao.iIndicador
                ttSelecao.lMat
                ttSelecao.lGGF
                ttSelecao.lServ
                ttSelecao.lContratos
                ttSelecao.lCusto
                ttSelecao.lTotal
                ttSelecao.lMatMesAnt
                ttSelecao.iNivTag
                ttSelecao.dt-trans-ini 
                ttSelecao.dt-trans-fim
             .
     end.
     output close.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cons-ind
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cons-ind
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cons-ind
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-cons-ind
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cons-ind
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cons-ind
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-equipamentos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cons-ind 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cons-ind  _ADM-CREATE-OBJECTS
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
       RUN set-position IN h_p-exihel ( 1.17 , 110.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             btExcel:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cons-ind  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaFiltro w-cons-ind 
PROCEDURE carregaFiltro :
/*carregaFiltro*/
  define input parameter cNome  as character no-undo.
  
  define variable c-linha as char no-undo.
  
  input from value (cNome).
  /** Busca os parƒmetros e sele‡äes **/
  for first ttSelecao exclusive-lock:
      import unformatted c-linha.
      assign c-linha                  = replace (c-linha, chr(34), "")
             ttSelecao.periodo-ini    = date(entry(1,c-linha,";")) 
             ttSelecao.periodo-fim    = date(entry(2,c-linha,";")) 
             ttSelecao.empresa-ini    = entry(3,c-linha,";") 
             ttSelecao.empresa-fim    = entry(4,c-linha,";") 
             ttSelecao.equipto-ini    = entry(5,c-linha,";") 
             ttSelecao.equipto-fim    = entry(6,c-linha,";") 
             ttSelecao.estab-ini      = entry(7,c-linha,";") 
             ttSelecao.estab-fim      = entry(8,c-linha,";") 
             ttSelecao.grupo-ini      = entry(9,c-linha,";") 
             ttSelecao.grupo-fim      = entry(10,c-linha,";")
             ttSelecao.modelo-ini     = entry(11,c-linha,";")
             ttSelecao.modelo-fim     = entry(12,c-linha,";")
             ttSelecao.estrut-ini     = entry(13,c-linha,";")
             ttSelecao.estrut-fim     = entry(14,c-linha,";")
             ttSelecao.tag-ini        = entry(15,c-linha,";")
             ttSelecao.tag-fim        = entry(16,c-linha,";")
             ttSelecao.cc-ini         = entry(17,c-linha,";")
             ttSelecao.cc-fim         = entry(18,c-linha,";")
             ttSelecao.ano-fabric-ini = int(entry(19,c-linha,";"))
             ttSelecao.ano-fabric-fim = int(entry(20,c-linha,";"))
             ttSelecao.lMtbf          = (entry(21,c-linha,";") = "yes")
             ttSelecao.lMttr          = (entry(22,c-linha,";") = "yes")
             ttSelecao.lDispo         = (entry(23,c-linha,";") = "yes")
             ttSelecao.lPmpl          = (entry(24,c-linha,";") = "yes")
             ttSelecao.iIndicador     = int(entry(25,c-linha,";"))
             ttSelecao.lMat           = (entry(26,c-linha,";") = "yes") 
             ttSelecao.lGGF           = (entry(27,c-linha,";") = "yes")  
             ttSelecao.lServ          = (entry(28,c-linha,";") = "yes")  
             ttSelecao.lContratos     = (entry(29,c-linha,";") = "yes")
             ttSelecao.lCusto         = (entry(30,c-linha,";") = "yes")  
             ttSelecao.lTotal         = (entry(31,c-linha,";") = "yes")  
             ttSelecao.lMatMesAnt     = (entry(32,c-linha,";") = "yes")  
             ttSelecao.iNivTag        = int(entry(33,c-linha,";"))
             ttSelecao.dt-trans-ini   = date(entry(34,c-linha,";"))
             ttSelecao.dt-trans-fim   = date(entry(35,c-linha,";"))
          .
  end.
  
  input close.
  
  RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaTTSelecao w-cons-ind 
PROCEDURE criaTTSelecao :
/*criaTTSelecao*/
  create ttSelecao.
  assign ttSelecao.periodo-ini    = today - 30
         ttSelecao.periodo-fim    = today
         ttSelecao.empresa-ini    = ""
         ttSelecao.empresa-fim    = "ZZZ"
         ttSelecao.equipto-ini    = ""
         ttSelecao.equipto-fim    = "ZZZZZZZZZZZZZZZZ"
         ttSelecao.estab-ini      = ""
         ttSelecao.estab-fim      = "ZZZ"
         ttSelecao.grupo-ini      = ""
         ttSelecao.grupo-fim      = "ZZZZZZZZ"
         ttSelecao.modelo-ini     = ""
         ttSelecao.modelo-fim     = "ZZZZZZZZ"
         ttSelecao.estrut-ini     = ""
         ttSelecao.estrut-fim     = "ZZZZZZZZ"
         ttSelecao.tag-ini        = ""
         ttSelecao.tag-fim        = "ZZZZZZZZZZZZZZZZ"
         ttSelecao.cc-ini         = ""
         ttSelecao.cc-fim         = "ZZZZZZZZ"
         ttSelecao.ano-fabric-ini = 0
         ttSelecao.ano-fabric-fim = 9999
         ttSelecao.lMtbf          = yes
         ttSelecao.lMttr          = yes
         ttSelecao.lDispo         = yes
         ttSelecao.lPmpl          = yes
         ttSelecao.iIndicador     = 1
         ttSelecao.iTipoDispo     = 1
         ttSelecao.lMat           = yes
         ttSelecao.lGGF           = yes
         ttSelecao.lServ          = yes
         ttSelecao.lContratos     = yes
         ttSelecao.lCusto         = yes
         ttSelecao.lTotal         = yes
         ttSelecao.lMatMesAnt     = no
         ttSelecao.iNivTag        = 999
         ttSelecao.dt-trans-ini   = TODAY - 30
         ttSelecao.dt-trans-fim   = TODAY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cons-ind  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cons-ind)
  THEN DELETE WIDGET w-cons-ind.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cons-ind  _DEFAULT-ENABLE
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
  ENABLE rt-button btAbrir btSalvar btFiltro btRefresh btExcel br-tag 
         br-equipamentos br-ordens 
      WITH FRAME f-cad IN WINDOW w-cons-ind.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cons-ind.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cons-ind 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cons-ind 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cons-ind 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "esp-esmv0616" "0.12.00.000"}
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-mtbf w-cons-ind 
PROCEDURE pi-calc-mtbf :
/*pi-calc-mtbf*/
  DEF OUTPUT PARAM i-tot-tempo-disponivel AS DEC NO-UNDO.
  DEF VAR          i-aux                  AS DEC NO-UNDO.
  IF AVAIL tt-equipamento THEN
    DO:
      FIND calen-gener
        NO-LOCK
        WHERE calen-gener.cd-calen = tt-equipamento.cd-calen
        NO-ERROR.
      IF AVAIL calen-gener THEN
        DO:
          FIND FIRST turno-calen-gener
            NO-LOCK
            WHERE turno-calen-gener.cd-calen = calen-gener.cd-calen
            NO-ERROR.
          IF AVAIL turno-calen-gener THEN
            DO:
              ASSIGN i-tot-tempo-disponivel = 0.
              FOR EACH data-turno-calen
                NO-LOCK
                WHERE  data-turno-calen.cd-calen  = turno-calen-gener.cd-calen
                AND    data-turno-calen.cd-turno  = turno-calen-gener.cd-turno
                AND    data-turno-calen.data     >= ttSelecao.periodo-ini   
                AND    data-turno-calen.data     <= ttSelecao.periodo-fim
                AND    data-turno-calen.dia-manut = YES 
                AND    data-turno-calen.tipo-dia  = 1:
                  ASSIGN i-aux = 0.
                  RUN pi-calc-tempo-disponivel(OUTPUT i-aux).
                  ASSIGN i-tot-tempo-disponivel = i-tot-tempo-disponivel + i-aux.
                  FIND tt-disp-dia
                    NO-LOCK
                    WHERE tt-disp-dia.cd-equipto = tt-equipamento.cd-equipto
                    AND   tt-disp-dia.data       = data-turno-calen.data
                    NO-ERROR.
                  IF NOT AVAIL tt-disp-dia THEN
                    DO:
                      CREATE tt-disp-dia.
                      ASSIGN 
                        tt-disp-dia.cd-equipto = tt-equipamento.cd-equipto
                        tt-disp-dia.data       = data-turno-calen.data
                        tt-disp-dia.disponib   = i-aux.
                    END.
                END.
            END.            
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-tempo-disponivel w-cons-ind 
PROCEDURE pi-calc-tempo-disponivel :
/*pi-calc-tempo-disponivel*/
  DEF OUTPUT PARAM i-tot-hora   AS DEC INITIAL 0            NO-UNDO.
  DEF VAR hr-hora-ini           AS DEC FORMAT ">9.99"       NO-UNDO.
  DEF VAR hr-hora-fim           AS DEC FORMAT ">9.99"       NO-UNDO.
  DEF VAR c-hora-ini            AS CHAR FORMAT ">9.99"      NO-UNDO.
  DEF VAR c-hora-ret            AS CHAR FORMAT ">9.99"      NO-UNDO.
  DEF VAR da-data-parada        AS DATE FORMAT "99/99/9999" NO-UNDO.
  DEF VAR da-data-retorno       AS DATE FORMAT "99/99/9999" NO-UNDO.
  DEF VAR hr-min-temporario     AS CHAR                     NO-UNDO.
  ASSIGN 
    /*---------------------------------------------------------*/
    hr-hora-ini      = 
      (INT(ENTRY(1,data-turno-calen.hora-inicio,":")) + 
      INT(ENTRY(2,data-turno-calen.hora-inicio,":")) / 60)
    hr-hora-fim      =
      (INT(ENTRY(1,data-turno-calen.hora-termino,":")) + 
      INT(ENTRY(2,data-turno-calen.hora-termino,":")) / 60)
    /*---------------------------------------------------------*/
    i-tot-hora      = hr-hora-fim - hr-hora-ini.  
    /*---------------------------------------------------------*/
/*   MESSAGE data-turno-calen.hora-inicio " = " hr-hora-ini SKIP data-turno-calen.hora-termino " = " hr-hora-fim SKIP */
/*     i-tot-hora                                                                                                     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-tempo-parada w-cons-ind 
PROCEDURE pi-calc-tempo-parada :
/*pi-calc-tempo-parada*/
  DEF OUTPUT PARAM i-tot-hora   AS DEC INITIAL 0            NO-UNDO.
  DEF VAR hr-hora-ini           AS DEC FORMAT ">9.99"       NO-UNDO.
  DEF VAR hr-hora-fim           AS DEC FORMAT ">9.99"       NO-UNDO.
  DEF VAR c-hora-ini            AS CHAR FORMAT ">9.99"      NO-UNDO.
  DEF VAR c-hora-ret            AS CHAR FORMAT ">9.99"      NO-UNDO.
  DEF VAR da-data-parada        AS DATE FORMAT "99/99/9999" NO-UNDO.
  DEF VAR da-data-retorno       AS DATE FORMAT "99/99/9999" NO-UNDO.
  DEF VAR hr-min-temporario     AS CHAR                     NO-UNDO.
  ASSIGN 
    c-hora-ini      = STRING(ord-manut.hr-parada,"99,99")
    c-hora-ret      = STRING(ord-manut.hr-retorno,"99,99")
    hr-hora-ini     = 0
    hr-hora-fim     = 0
    da-data-parada  = ord-manut.dt-parada
    da-data-retorno = ord-manut.dt-retorno
    i-tot-hora      = 0.
  if 
    substring(c-hora-ini,1,2) <> "" and 
    substring(c-hora-ini,3,2) <> "" then
    assign hr-hora-ini  = 
      ((int(substring(c-hora-ini,1,2)) * 60) +
      ( int(substring(c-hora-ini,3,2)))) / 60.
  if 
    substring(c-hora-ret,1,2) <> "" AND 
    substring(c-hora-ret,3,2) <> "" then
    assign hr-hora-fim = 
      ((int(substring(c-hora-ret,1,2)) * 60) +
      (int(substring(c-hora-ret,3,2)))) / 60.
  if 
    da-data-parada    <> ? or 
    (da-data-retorno) <> ? then 
    do:
      assign i-tot-hora = 
        dec((da-data-retorno) - 
        (da-data-parada)).
      if i-tot-hora = 0 then 
        do:
          assign i-tot-hora = hr-hora-fim - hr-hora-ini.
        end.    
      else 
        do:
          assign i-tot-hora = i-tot-hora * 24.
          if hr-hora-fim < hr-hora-ini then
            assign i-tot-hora = i-tot-hora - (hr-hora-ini - hr-hora-fim).
          else 
            assign i-tot-hora = i-tot-hora + (hr-hora-fim - hr-hora-ini).
        end.
    end.
    else 
      do:
        assign i-tot-hora = hr-hora-fim - hr-hora-ini.
      end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calculos w-cons-ind 
PROCEDURE pi-calculos :
/*pi-calculos*/

  DEF VAR i-tot-tempo-disponivel AS DEC NO-UNDO.
  DEF VAR de-tot-hora-prev       LIKE tt-ordem-manutencao.tt-tot-hora NO-UNDO.
  DEF VAR de-tot-hora-corr       LIKE tt-ordem-manutencao.tt-tot-hora NO-UNDO.
  DEF VAR de-tot-hora-corr-aux   LIKE tt-ordem-manutencao.tt-tot-hora NO-UNDO.
  DEF VAR de-tot-om-corr         LIKE tt-ordem-manutencao.tt-tot-hora NO-UNDO.
  DEF VAR i-equipamento          AS INT                               NO-UNDO.
  run utp/ut-acomp.p persistent set h-acomp.
  run pi-inicializar in h-acomp (input "Calculando..").
/*   FOR EACH tt-disp-dia: DELETE tt-disp-dia. END. */
  FOR EACH tt-equipamento  
    NO-LOCK:
    ASSIGN 
      de-tot-hora-prev = 0
      de-tot-hora-corr = 0
      de-tot-om-corr   = 0.
    run pi-acompanhar in h-acomp (input "Equipamento: " + tt-equipamento.cd-equipto).
    FOR EACH tt-ordem-manutencao                                          
      NO-LOCK                                                             
      WHERE /* tt-ordem-manutencao.cd-tag        = tt-equipamento.cd-tag        
      AND   */ tt-ordem-manutencao.cd-equipto    = tt-equipamento.cd-equipto
      AND   tt-ordem-manutencao.tt-preventiva = YES:   
        ASSIGN de-tot-hora-prev = de-tot-hora-prev + tt-ordem-manutencao.tt-tot-hora.
      END.                                                                  
    FOR EACH tt-ordem-manutencao                                          
      NO-LOCK                                                             
      WHERE /* tt-ordem-manutencao.cd-tag        = tt-equipamento.cd-tag 
      AND   */ tt-ordem-manutencao.cd-equipto    = tt-equipamento.cd-equipto
      AND   tt-ordem-manutencao.tt-preventiva = NO:  
        /*
        MESSAGE tt-ordem-manutencao.tt-tot-hora
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        ASSIGN 
          de-tot-hora-corr = de-tot-hora-corr + tt-ordem-manutencao.tt-tot-hora
          de-tot-om-corr   = de-tot-om-corr   + 1.
      END. 
    /*
    MESSAGE 
       "de-tot-hora-prev: " de-tot-hora-prev SKIP
       "de-tot-hora-corr: " de-tot-hora-corr SKIP
       "de-tot-hora-prev + de-tot-hora-corr: " de-tot-hora-prev + de-tot-hora-corr SKIP
       "(de-tot-hora-prev / de-tot-hora-corr) * 100: " (de-tot-hora-prev / de-tot-hora-corr) * 100
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */    
    ASSIGN 
      de-tot-hora-corr-aux    = de-tot-hora-corr
      de-tot-hora-corr        = de-tot-hora-corr + de-tot-hora-prev
      tt-equipamento.tt-pmpl  = (de-tot-hora-prev / de-tot-hora-corr) * 100
      tt-equipamento.tt-mttr  = de-tot-hora-corr-aux /  de-tot-om-corr. 
    IF tt-equipamento.tt-pmpl = ? THEN
      ASSIGN tt-equipamento.tt-pmpl = 0.
    /*
    MESSAGE tt-equipamento.tt-pmpl 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */    
    RUN pi-calc-mtbf(OUTPUT i-tot-tempo-disponivel).
    ASSIGN 
      tt-equipamento.tt-mtbf            = i-tot-tempo-disponivel / de-tot-om-corr
      tt-equipamento.tt-disponibilidade = i-tot-tempo-disponivel.
    IF tt-equipamento.tt-mtbf = ? THEN
      ASSIGN tt-equipamento.tt-mtbf = 0.
    IF tt-equipamento.tt-mttr = ? THEN
      ASSIGN tt-equipamento.tt-mttr = 0.
    ASSIGN 
      i-tot-tempo-disponivel = 
        ((i-tot-tempo-disponivel - de-tot-hora-corr) * 100) 
        / i-tot-tempo-disponivel.
    ASSIGN
      tt-equipamento.tt-disponibilidade = i-tot-tempo-disponivel.
    /*
    MESSAGE 
      "i-tot-tempo-disponivel " i-tot-tempo-disponivel SKIP
      "de-tot-hora-corr       " de-tot-hora-corr       SKIP
      "de-tot-hora-prev       " de-tot-hora-prev       SKIP
      de-tot-hora-corr 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.  
    */  
  END. /* FOR EACH tt-equipamento */
  
  FOR EACH tt-tag-manutencao
    NO-LOCK:
    ASSIGN tt-tag-manutencao.tt-disponibilidade = 0.
    /*---------------------------------------------------*/
    ASSIGN i-equipamento = 0.
    FOR EACH tt-equipamento
      WHERE  tt-equipamento.cd-tag = tt-tag-manutencao.cd-tag:
      ASSIGN i-equipamento = i-equipamento + 1.
    END.
    /*---------------------------------------------------*/
    FOR EACH tt-equipamento
      WHERE  tt-equipamento.cd-tag = tt-tag-manutencao.cd-tag:
      ASSIGN 
        tt-tag-manutencao.tt-disponibilidade = 
        tt-tag-manutencao.tt-disponibilidade +
        tt-equipamento.tt-disponibilidade.
    END.
    /*
    MESSAGE tt-tag-manutencao.tt-disponibilidade SKIP i-equipamento
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */    
    ASSIGN tt-tag-manutencao.tt-disponibilidade = tt-tag-manutencao.tt-disponibilidade / i-equipamento.
  END.
  run pi-finalizar in h-acomp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-dados w-cons-ind 
PROCEDURE pi-cria-dados :
/*pi-cria-dados*/
  FIND tt-equipamento
    NO-LOCK
    WHERE tt-equipamento.cd-equipto = equipto.cd-equipto
    NO-ERROR.
  IF NOT AVAIL tt-equipamento THEN
    DO:
      CREATE tt-equipamento.
      BUFFER-COPY equipto TO tt-equipamento.
    END.
  FIND tag
    NO-LOCK
    WHERE tag.cd-tag = equipto.cd-tag
    NO-ERROR.
  IF AVAIL tag THEN
    DO:
      FIND tt-tag-manutencao
        NO-LOCK
        WHERE tt-tag-manutencao.cd-tag = tag.cd-tag
        NO-ERROR.
      IF NOT AVAIL tt-tag-manutencao THEN
        DO:
          CREATE tt-tag-manutencao.
          BUFFER-COPY tag TO tt-tag-manutencao.
        END.
    END.
  FIND tt-ordem-manutencao
    NO-LOCK
    WHERE tt-ordem-manutencao.nr-ord-produ = ord-manut.nr-ord-produ
    NO-ERROR.
  IF NOT AVAIL tt-ordem-manutencao THEN
    DO:
      CREATE tt-ordem-manutencao.
      BUFFER-COPY ord-manut TO tt-ordem-manutencao.
      RUN pi-calc-tempo-parada(OUTPUT tt-ordem-manutencao.tt-tot-hora).
      FIND tipo-manut
        NO-LOCK
        WHERE tipo-manut.cd-tipo = ord-manut.cd-tipo
        NO-ERROR.
      ASSIGN 
        tt-ordem-manutencao.tt-tipo = 
          IF AVAIL tipo-manut THEN STRING(ord-manut.cd-tipo) + " - " + tipo-manut.descricao
          ELSE "Indefinido".
      ASSIGN 
        tt-ordem-manutencao.tt-preventiva = 
          IF tipo-manut.tipo = 1 THEN YES
          ELSE NO.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-cons-ind 
PROCEDURE pi-executar :
/*pi-executar*/
  FOR EACH tt-disp-dia:       DELETE tt-disp-dia.       END. 
  FOR EACH tt-tag-manutencao: DELETE tt-tag-manutencao. END.
  FOR EACH tt-equipamento:    DELETE tt-equipamento.    END.
  FIND FIRST ttSelecao NO-LOCK NO-ERROR.
  IF NOT AVAIL ttSelecao THEN RETURN.
  /* depuracao
  MESSAGE 
      "ttSelecao.equipto-ini " ttSelecao.equipto-ini  SKIP
      "ttSelecao.equipto-fim " ttSelecao.equipto-fim  
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */    
  FOR EACH estabelec
    NO-LOCK
    WHERE estabelec.cod-estabel >= ttSelecao.estab-ini
    AND   estabelec.cod-estabel <= ttSelecao.estab-fim
    AND   estabelec.ep-codigo   >= ttSelecao.empresa-ini /* = i-ep-codigo-usuario */
    AND   estabelec.ep-codigo   <= ttSelecao.empresa-fim:
      /* depuracao
      IF estabelec.ep-codigo = "910" THEN
        MESSAGE 
          "estabelec.ep-codigo   " estabelec.ep-codigo    SKIP
          "estabelec.cod-estabel " estabelec.cod-estabel
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */    
      FOR EACH equipto
        NO-LOCK
        WHERE equipto.ep-codigo    = estabelec.ep-codigo 
        AND   equipto.cod-estabel  = estabelec.cod-estabel
        AND   equipto.cd-equipto  >= ttSelecao.equipto-ini
        AND   equipto.cd-equipto  <= ttSelecao.equipto-fim
        AND   equipto.fm-equipto  >= ttSelecao.modelo-ini
        AND   equipto.fm-equipto  <= ttSelecao.modelo-fim
        AND   equipto.cd-tag      >= ttSelecao.tag-ini
        AND   equipto.cd-tag      <= ttSelecao.tag-fim
        AND   equipto.cc-codigo   >= ttSelecao.cc-ini
        AND   equipto.cc-codigo   <= ttSelecao.cc-fim
/*         AND   equipto.cd-tag      <> "" */
          :
          blk-ord-manut:
          FOR EACH ord-manut
            NO-LOCK
            WHERE ord-manut.dt-manut   >= ttSelecao.periodo-ini
            AND   ord-manut.dt-manut   <= ttSelecao.periodo-fim
/*             AND   ord-manut.cd-tag      = equipto.cd-tag */
            AND   ord-manut.cd-equipto  = equipto.cd-equipto:
              RUN pi-cria-dados.
            END.
        END.
    END.
  RUN pi-calculos.
  OPEN QUERY br-tag FOR EACH tt-tag-manutencao NO-LOCK.
  IF AVAIL tt-tag-manutencao THEN
    DO:
      OPEN QUERY br-equipamentos 
        FOR EACH tt-equipamento
        WHERE tt-equipamento.cd-tag = tt-tag-manutencao.cd-tag
        NO-LOCK.
      IF AVAIL tt-equipamento THEN
        DO:
          OPEN QUERY br-ordens 
            FOR EACH tt-ordem-manutencao
              WHERE /* tt-ordem-manutencao.cd-tag     = tt-equipamento.cd-tag
              AND   */ tt-ordem-manutencao.cd-equipto = tt-equipamento.cd-equipto
              NO-LOCK.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cons-ind  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-tag-manutencao"}
  {src/adm/template/snd-list.i "tt-ordem-manutencao"}
  {src/adm/template/snd-list.i "tt-equipamento"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cons-ind 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
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

