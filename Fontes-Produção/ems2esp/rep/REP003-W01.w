&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-forma 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/****************************************************************************************** 
**             Programa: REP003-W01.w
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**            Data: 05/11/2018
**  Change/Chamado: XXXXXXX
**        Objetivo: Aprovar ou reprovar Atualizaá‰es de Notas No Recebimento
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: tt-param
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp/ut-glob.i}
{utp/utapi019.i}
{include/i-prgvrs.i CC001-W01 1.00.00.000}
{rep/re1005rp.i}


/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i CC001-W01 CCP}
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

def var v-row-1 as rowid no-undo.
def var v-row-2 as rowid no-undo.

DEF TEMP-TABLE tt-pendentes    NO-UNDO LIKE esp_pend_aprov
    FIELD nome-abrev  LIKE emitente.nome-abrev
    FIELD nom_usuario LIKE usuar_mestre.nom_usuario
    FIELD vl-a-pagar  LIKE dupli-apagar.vl-a-pagar
    FIELD dt-emissao  LIKE dupli-apagar.dt-emissao
    FIELD dt-vencim   LIKE dupli-apagar.dt-vencim
    FIELD selec       AS LOG
    FIELD Nom-modulo  AS CHAR FORMAT "X(30)". /*1 => Contas a pagar || 2 => Recebimento */

DEF TEMP-TABLE tt-selecionados    NO-UNDO LIKE esp_pend_aprov
    FIELD nome-abrev  LIKE emitente.nome-abrev
    FIELD aprov1      LIKE usuar_mestre.nom_usuario
    FIELD aprov2      LIKE usuar_mestre.nom_usuario
    FIELD reprov      LIKE usuar_mestre.nom_usuario
    FIELD nom_usuario LIKE usuar_mestre.nom_usuario
    FIELD vl-a-pagar  LIKE dupli-apagar.vl-a-pagar
    FIELD dt-emissao  LIKE dupli-apagar.dt-emissao
    FIELD dt-vencim   LIKE dupli-apagar.dt-vencim
    FIELD selec       AS LOG
    FIELD Nom-modulo  AS CHAR FORMAT "X(30)". /*1 => Contas a pagar || 2 => Recebimento */

DEFINE TEMP-TABLE tt-trata-arq
    FIELD linha-arq AS CHARACTER.

DEFINE VARIABLE c-mess AS CHARACTER   NO-UNDO.
DEFINE VARIABLE assun AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-aprovado AS LOGICAL     NO-UNDO.
DEFINE VARIABLE nome-arquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-mot-reprov AS CHARACTER   NO-UNDO.
DEFINE VARIABLE raw-param AS RAW NO-UNDO.
DEF VAR r-rowid-docum-est AS ROWID NO-UNDO.

define temp-table tt-raw-digita
    field raw-digita as raw.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-forma
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME b-pendentes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pendentes tt-selecionados

/* Definitions for BROWSE b-pendentes                                   */
&Scoped-define FIELDS-IN-QUERY-b-pendentes tt-pendentes.selec tt-pendentes.nro-docto tt-pendentes.cod-estabel tt-pendentes.nome-abrev tt-pendentes.vl-a-pagar tt-pendentes.nom_usuario tt-pendentes.data-geracao tt-pendentes.dt-vencim tt-pendentes.dt-emissao tt-pendentes.Dias tt-pendentes.Nom-modulo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-pendentes tt-pendentes.selec   
&Scoped-define ENABLED-TABLES-IN-QUERY-b-pendentes tt-pendentes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-b-pendentes tt-pendentes
&Scoped-define SELF-NAME b-pendentes
&Scoped-define QUERY-STRING-b-pendentes FOR EACH tt-pendentes
&Scoped-define OPEN-QUERY-b-pendentes OPEN QUERY {&SELF-NAME} FOR EACH tt-pendentes.
&Scoped-define TABLES-IN-QUERY-b-pendentes tt-pendentes
&Scoped-define FIRST-TABLE-IN-QUERY-b-pendentes tt-pendentes


/* Definitions for BROWSE b-selecionados                                */
&Scoped-define FIELDS-IN-QUERY-b-selecionados tt-selecionados.nro-docto tt-selecionados.cod-estabel tt-selecionados.aprov1 tt-selecionados.data-aprovacao1 /* tt-selecionados.aprov2 */ /* tt-selecionados.data-aprovacao2 */ tt-selecionados.reprov tt-selecionados.data-reprovacao tt-selecionados.Nom-modulo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-selecionados   
&Scoped-define SELF-NAME b-selecionados
&Scoped-define QUERY-STRING-b-selecionados FOR EACH tt-selecionados
&Scoped-define OPEN-QUERY-b-selecionados OPEN QUERY {&SELF-NAME} FOR EACH tt-selecionados.
&Scoped-define TABLES-IN-QUERY-b-selecionados tt-selecionados
&Scoped-define FIRST-TABLE-IN-QUERY-b-selecionados tt-selecionados


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-b-pendentes}~
    ~{&OPEN-QUERY-b-selecionados}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 rt-button rt-button-2 r-aprov-pend ~
b-pendentes b-selecionados but-reprovar but-confirmar 
&Scoped-Define DISPLAYED-OBJECTS f-Cod-Apro f-Nom-Aprov r-aprov-pend 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-forma AS WIDGET-HANDLE NO-UNDO.

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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON but-confirmar 
     LABEL "Aprovar" 
     SIZE 10 BY 1.

DEFINE BUTTON but-reprovar 
     LABEL "Reprovar" 
     SIZE 10 BY 1.

DEFINE VARIABLE f-Cod-Apro AS CHARACTER FORMAT "X(12)":U 
     LABEL "Aprovador" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE f-Nom-Aprov AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE r-aprov-pend AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pendentes", 1,
"Aprovados", 2,
"Reprovados", 3
     SIZE 35 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 1.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 105 BY 1.46
     BGCOLOR 7 .

DEFINE RECTANGLE rt-button-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 105 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b-pendentes FOR 
      tt-pendentes SCROLLING.

DEFINE QUERY b-selecionados FOR 
      tt-selecionados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b-pendentes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-pendentes w-forma _FREEFORM
  QUERY b-pendentes DISPLAY
      tt-pendentes.selec         COLUMN-LABEL "Aprov" VIEW-AS TOGGLE-BOX
      tt-pendentes.nro-docto     COLUMN-LABEL "Docto"             WIDTH 8
      tt-pendentes.cod-estabel   column-label "Est"               
      tt-pendentes.nome-abrev    column-label "Fornecedor"
      tt-pendentes.vl-a-pagar    column-label "Valor"             WIDTH 8
      tt-pendentes.nom_usuario   column-label "Usu†rio Digitaá∆o" WIDTH 15
      tt-pendentes.data-geracao  column-label "Data Digitaá∆o"    
      tt-pendentes.dt-vencim     column-label "Vencimento"
      tt-pendentes.dt-emissao    column-label "Emiss∆o"
      tt-pendentes.Dias          column-label "Nr Dias"
      tt-pendentes.Nom-modulo    column-label "M¢dulo"            WIDTH 10
ENABLE
      tt-pendentes.selec
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 8.25
         TITLE "Documentos Pendentes" FIT-LAST-COLUMN.

DEFINE BROWSE b-selecionados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-selecionados w-forma _FREEFORM
  QUERY b-selecionados DISPLAY
      tt-selecionados.nro-docto        COLUMN-LABEL "Docto"      WIDTH 8
      tt-selecionados.cod-estabel      column-label "Est"  
      tt-selecionados.aprov1           COLUMN-LABEL "Aprovador"  WIDTH 10
      tt-selecionados.data-aprovacao1  COLUMN-LABEL "Data Aprov"
/*       tt-selecionados.aprov2           COLUMN-LABEL "Aprovador"  WIDTH 10 */
/*       tt-selecionados.data-aprovacao2  COLUMN-LABEL "Data Aprov"          */
      tt-selecionados.reprov           COLUMN-LABEL "Reprovador"  WIDTH 10 
      tt-selecionados.data-reprovacao  COLUMN-LABEL "Data Reprov" 
      tt-selecionados.Nom-modulo       column-label "M¢dulo"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 8.25
         TITLE "Documentos Selecionados" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     f-Cod-Apro AT ROW 3.25 COL 12 COLON-ALIGNED WIDGET-ID 12
     f-Nom-Aprov AT ROW 3.25 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     r-aprov-pend AT ROW 3.25 COL 71 NO-LABEL WIDGET-ID 22
     b-pendentes AT ROW 4.75 COL 2 WIDGET-ID 200
     b-selecionados AT ROW 13.25 COL 2 WIDGET-ID 300
     but-reprovar AT ROW 21.92 COL 84 WIDGET-ID 30
     but-confirmar AT ROW 21.92 COL 95 WIDGET-ID 20
     RECT-22 AT ROW 2.75 COL 2 WIDGET-ID 2
     rt-button AT ROW 1 COL 2 WIDGET-ID 10
     rt-button-2 AT ROW 21.75 COL 2 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107 BY 22.46 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-forma
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-forma ASSIGN
         HIDDEN             = YES
         TITLE              = "Aprovaá∆o/Reprovaá∆o Documentos"
         HEIGHT             = 22.46
         WIDTH              = 107
         MAX-HEIGHT         = 28.33
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 28.33
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-forma 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
/*
{include/w-consim.i}
*/ 

{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-forma
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB b-pendentes r-aprov-pend f-cad */
/* BROWSE-TAB b-selecionados b-pendentes f-cad */
/* SETTINGS FOR FILL-IN f-Cod-Apro IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Nom-Aprov IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
THEN w-forma:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-pendentes
/* Query rebuild information for BROWSE b-pendentes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pendentes.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-pendentes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-selecionados
/* Query rebuild information for BROWSE b-selecionados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-selecionados.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-selecionados */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON END-ERROR OF w-forma /* Aprovaá∆o/Reprovaá∆o Documentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON WINDOW-CLOSE OF w-forma /* Aprovaá∆o/Reprovaá∆o Documentos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b-pendentes
&Scoped-define SELF-NAME b-pendentes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-pendentes w-forma
ON ROW-ENTRY OF b-pendentes IN FRAME f-cad /* Documentos Pendentes */
DO:

  IF b-pendentes:FETCH-SELECTED-ROW(1) THEN DO: /*Verifica se foi selecionado um campos do browse*/

      IF AVAIL(tt-pendentes) THEN DO: /*Verifica se existe um registro*/

        IF NOT tt-pendentes.selec THEN DO:
        
           IF NOT CAN-FIND(FIRST  tt-selecionados 
                           WHERE  tt-selecionados.serie-docto            = tt-pendentes.serie-docto              
                             AND  tt-selecionados.nro-docto              = tt-pendentes.nro-docto                
                             AND  tt-selecionados.cod-emitente           = tt-pendentes.cod-emitente             
                             AND  tt-selecionados.nat-operacao           = tt-pendentes.nat-operacao                                                  
                             AND  tt-selecionados.cod-estabel            = tt-pendentes.cod-estabel
                             AND  tt-selecionados.data-geracao           = tt-pendentes.data-geracao
                             
                           ) THEN DO:
           
                    CREATE tt-selecionados.
                    ASSIGN tt-selecionados.cod-estabel            = tt-pendentes.cod-estabel              
                           tt-selecionados.serie-docto            = tt-pendentes.serie-docto              
                           tt-selecionados.nro-docto              = tt-pendentes.nro-docto                
                           tt-selecionados.cod-emitente           = tt-pendentes.cod-emitente             
                           tt-selecionados.nat-operacao           = tt-pendentes.nat-operacao             
                           tt-selecionados.cod_usuario_geracao    = tt-pendentes.cod_usuario_geracao      
                           tt-selecionados.cod_usuario_aprovador1 = tt-pendentes.cod_usuario_aprovador1   
                           tt-selecionados.cod_usuario_aprovador2 = tt-pendentes.cod_usuario_aprovador2 
                           tt-selecionados.cod_usuario_reprovado  = tt-pendentes.cod_usuario_reprovado
                           tt-selecionados.data-reprovacao        = tt-pendentes.data-reprovacao
                           tt-selecionados.Dias                   = tt-pendentes.Dias                     
                           tt-selecionados.data-geracao           = tt-pendentes.data-geracao             
                           tt-selecionados.data-aprovacao1        = tt-pendentes.data-aprovacao1          
                           tt-selecionados.data-aprovacao2        = tt-pendentes.data-aprovacao2          
                           tt-selecionados.modulo                 = tt-pendentes.modulo
                           tt-selecionados.Nom-modulo             = tt-pendentes.Nom-modulo
                           tt-selecionados.selec                  = YES
                           tt-selecionados.nome-abrev             = tt-pendentes.nome-abrev
                           tt-selecionados.vl-a-pagar             = tt-pendentes.vl-a-pagar  
                           tt-selecionados.dt-emissao             = tt-pendentes.dt-emissao  
                           tt-selecionados.dt-vencim              = tt-pendentes.dt-vencim   
                           tt-selecionados.nom_usuario            = tt-pendentes.nom_usuario.
        
           END.        
             
           RUN CarregarSelecionados.      
           {&OPEN-QUERY-B-selecionados}

        END. /*IF NOT tt-pendentes.selec THEN DO:*/
        ELSE DO:

            IF CAN-FIND(FIRST  tt-selecionados 
                        WHERE  tt-selecionados.serie-docto            = tt-pendentes.serie-docto              
                          AND  tt-selecionados.nro-docto              = tt-pendentes.nro-docto                
                          AND  tt-selecionados.cod-emitente           = tt-pendentes.cod-emitente             
                          AND  tt-selecionados.nat-operacao           = tt-pendentes.nat-operacao
                          AND  tt-selecionados.cod-estabel            = tt-pendentes.cod-estabel
                          AND  tt-selecionados.data-geracao           = tt-pendentes.data-geracao
                        ) THEN DO:

                      DELETE tt-selecionados.

             END.        

             RUN CarregarSelecionados.      
             {&OPEN-QUERY-B-selecionados}


        END. /*ELSE DO:*/

      END. /*IF AVAIL(tt-pendentes) THEN DO:*/

  END. /*IF b-pendentes:FETCH-SELECTED-ROW(1) THEN DO:*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME but-confirmar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL but-confirmar w-forma
ON CHOOSE OF but-confirmar IN FRAME f-cad /* Aprovar */
DO:
    FOR EACH tt-selecionados:
        FIND FIRST esp_pend_aprov
             WHERE esp_pend_aprov.cod-estabel   = tt-selecionados.cod-estabel               
               AND esp_pend_aprov.serie-docto   = tt-selecionados.serie-docto               
               AND esp_pend_aprov.nro-docto     = tt-selecionados.nro-docto                 
               AND esp_pend_aprov.cod-emitente  = tt-selecionados.cod-emitente              
               AND esp_pend_aprov.nat-operacao  = tt-selecionados.nat-operacao
               AND esp_pend_aprov.data-geracao  = tt-selecionados.data-geracao EXCLUSIVE-LOCK NO-ERROR. 

        IF NOT AVAIL esp_pend_aprov THEN NEXT.

        IF esp_pend_aprov.cod_usuario_aprovador1 = "" THEN DO:
            ASSIGN esp_pend_aprov.cod_usuario_aprovador1 = c-seg-usuario
                   esp_pend_aprov.data-aprovacao1        = NOW.
        END.

        RUN aprovarNotas (INPUT TABLE tt-selecionados,
                          INPUT-OUTPUT l-aprovado).
            
        IF l-aprovado THEN DO:

            FIND FIRST esp_hist_aprov 
                 WHERE esp_hist_aprov.serie-docto  = esp_pend_aprov.serie-docto 
                   AND esp_hist_aprov.nro-docto    = esp_pend_aprov.nro-docto    
                   AND esp_hist_aprov.cod-emitente = esp_pend_aprov.cod-emitente 
                   AND esp_hist_aprov.nat-operacao = esp_pend_aprov.nat-operacao NO-ERROR.
            IF NOT AVAIL esp_hist_aprov THEN
            DO:

                CREATE esp_hist_aprov.
                ASSIGN esp_hist_aprov.cod-estabel               = esp_pend_aprov.cod-estabel 
                       esp_hist_aprov.serie-docto               = esp_pend_aprov.serie-docto 
                       esp_hist_aprov.nro-docto                 = esp_pend_aprov.nro-docto   
                       esp_hist_aprov.cod-emitente              = esp_pend_aprov.cod-emitente
                       esp_hist_aprov.nat-operacao              = esp_pend_aprov.nat-operacao
                       esp_hist_aprov.data-geracao              = esp_pend_aprov.data-geracao
                       esp_hist_aprov.cod_usuario_geracao       = c-seg-usuario  
                       esp_hist_aprov.dias                      = esp_pend_aprov.dias.
            END.
            RUN enviarEmail(INPUT TABLE tt-selecionados,
                            INPUT 2,
                            INPUT nome-arquivo).
            DELETE tt-selecionados.
        END.
        ELSE DO:
            /*DELETE esp_pend_aprov.*/
        END.
    END.

    EMPTY TEMP-TABLE tt-selecionados.
    EMPTY TEMP-TABLE tt-pendentes.
    RUN CarregarPendencias.      
    {&OPEN-QUERY-B-pendentes}
    RUN CarregarSelecionados.      
    {&OPEN-QUERY-B-selecionados}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME but-reprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL but-reprovar w-forma
ON CHOOSE OF but-reprovar IN FRAME f-cad /* Reprovar */
DO:
    FOR EACH tt-selecionados:

        RUN rep\rep003-d01.w (INPUT tt-selecionados.cod-estabel,
                              INPUT tt-selecionados.serie-docto,  
                              INPUT tt-selecionados.nro-docto,    
                              INPUT tt-selecionados.cod-emitente,
                              OUTPUT c-mot-reprov).
        IF c-mot-reprov <> "" THEN
        DO:
            FIND FIRST esp_pend_aprov
                 WHERE esp_pend_aprov.cod-estabel   = tt-selecionados.cod-estabel               
                   AND esp_pend_aprov.serie-docto   = tt-selecionados.serie-docto               
                   AND esp_pend_aprov.nro-docto     = tt-selecionados.nro-docto                 
                   AND esp_pend_aprov.cod-emitente  = tt-selecionados.cod-emitente              
                   AND esp_pend_aprov.nat-operacao  = tt-selecionados.nat-operacao
                   AND esp_pend_aprov.data-geracao  = tt-selecionados.data-geracao EXCLUSIVE-LOCK NO-ERROR. 
    
            IF NOT AVAIL esp_pend_aprov THEN NEXT.
    
            IF esp_pend_aprov.cod_usuario_reprovador = "" THEN DO:
    
               ASSIGN esp_pend_aprov.cod_usuario_reprovador = c-seg-usuario
                      esp_pend_aprov.data-reprovacao        = NOW
                      esp_pend_aprov.mot-reprov            = c-mot-reprov.
    
                RUN enviarEmail(INPUT TABLE tt-selecionados,
                                INPUT 1,
                                INPUT nome-arquivo).
    
            END.
            
            DELETE tt-selecionados.
        END.
    END. /*FOR EACH tt-selecionados:*/

    EMPTY TEMP-TABLE tt-selecionados.
    EMPTY TEMP-TABLE tt-pendentes.
    RUN CarregarPendencias.      
    {&OPEN-QUERY-B-pendentes}
    RUN CarregarSelecionados.      
    {&OPEN-QUERY-B-selecionados}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-forma
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-forma
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-forma
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-forma
ON MENU-DROP OF MENU mi-programa /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-forma
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-forma
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME r-aprov-pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r-aprov-pend w-forma
ON VALUE-CHANGED OF r-aprov-pend IN FRAME f-cad
DO:

  CASE INPUT FRAME {&FRAME-NAME} r-aprov-pend:
      WHEN 1 THEN DO:

        b-pendentes:TITLE IN FRAME {&FRAME-NAME} = "Documentos Pendentes".
        b-selecionados:TITLE IN FRAME {&FRAME-NAME} = "Documentos Selecionados".
        but-confirmar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        but-reprovar:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
        EMPTY TEMP-TABLE tt-selecionados.
        EMPTY TEMP-TABLE tt-pendentes.
        RUN CarregarPendencias.      
        {&OPEN-QUERY-B-pendentes}
        {&OPEN-QUERY-B-selecionados}

      END.
      WHEN 2 THEN DO:

        b-pendentes:TITLE IN FRAME {&FRAME-NAME} = "Documentos Aprovadas".
        b-selecionados:TITLE IN FRAME {&FRAME-NAME} = "Documentos Aprovadores".
        but-confirmar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        but-reprovar:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
        EMPTY TEMP-TABLE tt-selecionados.
        EMPTY TEMP-TABLE tt-pendentes.
        RUN CarregaAprovados.
        {&OPEN-QUERY-B-pendentes}
        {&OPEN-QUERY-B-selecionados}

      END.
      WHEN 3 THEN DO:

        b-pendentes:TITLE IN FRAME {&FRAME-NAME} = "Documentos Reprovadas".
        b-selecionados:TITLE IN FRAME {&FRAME-NAME} = "Documentos Reprovadores".
        but-confirmar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        but-reprovar:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
        EMPTY TEMP-TABLE tt-selecionados.
        EMPTY TEMP-TABLE tt-pendentes.
        RUN CarregaReprovados.
        {&OPEN-QUERY-B-pendentes}
        {&OPEN-QUERY-B-selecionados}

      END.
  END CASE.
     
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-forma 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-forma  _ADM-CREATE-OBJECTS
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
       RUN set-position IN h_p-exihel ( 1.08 , 89.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             f-Cod-Apro:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-forma  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AprovarNotas w-forma 
PROCEDURE AprovarNotas :
DEF INPUT PARAM TABLE FOR tt-selecionados.
DEF INPUT-OUTPUT PARAM l-aprovado AS LOGICAL.

l-aprovado = NO.

FIND FIRST docum-est 
     WHERE docum-est.cod-emitente = tt-selecionados.cod-emitente 
       AND docum-est.serie-docto  = tt-selecionados.serie-docto  
       AND docum-est.nro-docto    = tt-selecionados.nro-docto    
       AND docum-est.nat-operacao = tt-selecionados.nat-operacao  NO-LOCK NO-ERROR.

IF AVAIL docum-est THEN DO:
    assign nome-arquivo = SESSION:TEMP-DIRECTORY + "ERRO_NF" + tt-selecionados.nro-docto + ".txt".

    EMPTY TEMP-TABLE tt-param.
    EMPTY TEMP-TABLE tt-digita.
    EMPTY TEMP-TABLE tt-raw-digita.

    CREATE tt-param.
    ASSIGN tt-param.usuario         = docum-est.usuario
           tt-param.destino         = 3
           tt-param.data-exec       = TODAY
           tt-param.hora-exec       = TIME.
           tt-param.arquivo         = nome-arquivo.

    RAW-TRANSFER tt-param to raw-param.

    CREATE tt-digita.
    ASSIGN tt-digita.r-docum-est = ROWID(docum-est).

    ASSIGN r-rowid-docum-est = ROWID(docum-est).

    for each tt-raw-digita:
        delete tt-raw-digita.
    end.
    for each tt-digita:
        create tt-raw-digita.
        raw-transfer tt-digita to tt-raw-digita.raw-digita.
    end.

    RUN rep/re1005rp.p (INPUT raw-param, 
                        INPUT TABLE tt-raw-digita).

    FIND FIRST docum-est WHERE ROWID(docum-est) = r-rowid-docum-est NO-LOCK NO-ERROR.
      
    IF docum-est.ap-atual THEN l-aprovado = YES.
    
    IF NOT docum-est.ap-atual THEN
    DO:
        RUN TrataArquivo.
    END.

    RUN prgfin\apl\apya599.p (nome-arquivo).

    IF NOT l-aprovado THEN DO:
    
        RUN enviarEmail(INPUT TABLE tt-selecionados,
                        INPUT 3,
                        INPUT nome-arquivo).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CarregaAprovados w-forma 
PROCEDURE CarregaAprovados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST esp_aprovador WHERE esp_aprovador.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
 IF AVAIL esp_aprovador THEN DO:

    FOR EACH esp_estab_aprov 
        WHERE esp_estab_aprov.cod_usuario = esp_aprovador.cod_usuario NO-LOCK:

        FOR FIRST esp_param_bloq
            WHERE esp_param_bloq.cod-estabel = esp_estab_aprov.cod-estabel
              AND esp_param_bloq.bloq-atualizar:

            FOR EACH esp_pend_aprov 
                WHERE esp_pend_aprov.cod-estabel             =  esp_param_bloq.cod-estabel
                  AND esp_pend_aprov.cod_usuario_aprovador1  = esp_aprovador.cod_usuario 
/*                    OR esp_pend_aprov.cod_usuario_aprovador2  = esp_aprovador.cod_usuario */
                   : 

                FIND FIRST   emitente     WHERE emitente.cod-emitente    = esp_pend_aprov.cod-emitente NO-LOCK NO-ERROR.
                IF NOT AVAIL emitente     THEN RETURN ERROR.

                FIND FIRST   usuar_mestre WHERE usuar_mestre.cod_usuario = esp_pend_aprov.cod_usuario_geracao NO-LOCK NO-ERROR.
                IF NOT AVAIL usuar_mestre THEN RETURN ERROR.

                FIND FIRST   dupli-apagar
                     WHERE   dupli-apagar.serie-docto    = esp_pend_aprov.serie-docto   
                       AND   dupli-apagar.nro-docto      = esp_pend_aprov.nro-docto     
                       AND   dupli-apagar.cod-emitente   = esp_pend_aprov.cod-emitente  
                       AND   dupli-apagar.nat-operacao   = esp_pend_aprov.nat-operacao.
                IF NOT AVAIL dupli-apagar THEN RETURN ERROR.

                CREATE tt-pendentes.
                ASSIGN tt-pendentes.cod-estabel            = esp_pend_aprov.cod-estabel            
                       tt-pendentes.serie-docto            = esp_pend_aprov.serie-docto            
                       tt-pendentes.nro-docto              = esp_pend_aprov.nro-docto              
                       tt-pendentes.cod-emitente           = esp_pend_aprov.cod-emitente           
                       tt-pendentes.nat-operacao           = esp_pend_aprov.nat-operacao           
                       tt-pendentes.cod_usuario_geracao    = esp_pend_aprov.cod_usuario_geracao    
                       tt-pendentes.cod_usuario_aprovador1 = esp_pend_aprov.cod_usuario_aprovador1 
                       tt-pendentes.cod_usuario_aprovador2 = esp_pend_aprov.cod_usuario_aprovador2 
                       tt-pendentes.cod_usuario_reprovado  = esp_pend_aprov.cod_usuario_reprovado
                       tt-pendentes.data-reprovacao        = esp_pend_aprov.data-reprovacao
                       tt-pendentes.Dias                   = esp_pend_aprov.Dias                   
                       tt-pendentes.data-geracao           = esp_pend_aprov.data-geracao            
                       tt-pendentes.data-aprovacao1        = esp_pend_aprov.data-aprovacao1        
                       tt-pendentes.data-aprovacao2        = esp_pend_aprov.data-aprovacao2        
                       tt-pendentes.modulo                 = esp_pend_aprov.modulo   

                       tt-pendentes.nome-abrev             = emitente.nome-abrev 
                       tt-pendentes.nom_usuario            = usuar_mestre.nom_usuario 

                       tt-pendentes.vl-a-pagar             = dupli-apagar.vl-a-pagar
                       tt-pendentes.dt-emissao             = dupli-apagar.dt-emissao 
                       tt-pendentes.dt-vencim              = dupli-apagar.dt-vencim 

                       tt-pendentes.selec                  = NO.
                       
               
                CASE esp_pend_aprov.modulo:
                    WHEN 1 THEN
                        tt-pendentes.Nom-modulo = "Contas a pagar".
                    WHEN 2 THEN
                        tt-pendentes.Nom-modulo = "Recebimento". 
                    OTHERWISE
                        tt-pendentes.Nom-modulo = "".
                END CASE.

            END.

        END. /*FOR FIRST esp_param_bloq*/

    END. /*FOR EACH esp_estab_aprov */

 END. /*IF AVAIL esp_aprovador THEN DO:*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CarregaReprovados w-forma 
PROCEDURE CarregaReprovados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST esp_aprovador WHERE esp_aprovador.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
 IF AVAIL esp_aprovador THEN DO:

    FOR EACH esp_estab_aprov 
        WHERE esp_estab_aprov.cod_usuario = esp_aprovador.cod_usuario NO-LOCK:

        FOR FIRST esp_param_bloq
            WHERE esp_param_bloq.cod-estabel = esp_estab_aprov.cod-estabel
              AND esp_param_bloq.bloq-atualizar:

            FOR EACH esp_pend_aprov 
                WHERE esp_pend_aprov.cod-estabel             =  esp_param_bloq.cod-estabel
                  AND esp_pend_aprov.cod_usuario_reprovador  = esp_aprovador.cod_usuario: 

                FIND FIRST   emitente     WHERE emitente.cod-emitente    = esp_pend_aprov.cod-emitente NO-LOCK NO-ERROR.
                IF NOT AVAIL emitente     THEN RETURN ERROR.

                FIND FIRST   usuar_mestre WHERE usuar_mestre.cod_usuario = esp_pend_aprov.cod_usuario_geracao NO-LOCK NO-ERROR.
                IF NOT AVAIL usuar_mestre THEN RETURN ERROR.

                FIND FIRST   dupli-apagar
                     WHERE   dupli-apagar.serie-docto    = esp_pend_aprov.serie-docto   
                       AND   dupli-apagar.nro-docto      = esp_pend_aprov.nro-docto     
                       AND   dupli-apagar.cod-emitente   = esp_pend_aprov.cod-emitente  
                       AND   dupli-apagar.nat-operacao   = esp_pend_aprov.nat-operacao.
                IF NOT AVAIL dupli-apagar THEN RETURN ERROR.

                CREATE tt-pendentes.
                ASSIGN tt-pendentes.cod-estabel            = esp_pend_aprov.cod-estabel            
                       tt-pendentes.serie-docto            = esp_pend_aprov.serie-docto            
                       tt-pendentes.nro-docto              = esp_pend_aprov.nro-docto              
                       tt-pendentes.cod-emitente           = esp_pend_aprov.cod-emitente           
                       tt-pendentes.nat-operacao           = esp_pend_aprov.nat-operacao           
                       tt-pendentes.cod_usuario_geracao    = esp_pend_aprov.cod_usuario_geracao    
                       tt-pendentes.cod_usuario_aprovador1 = esp_pend_aprov.cod_usuario_aprovador1 
                       tt-pendentes.cod_usuario_aprovador2 = esp_pend_aprov.cod_usuario_aprovador2 
                       tt-pendentes.cod_usuario_reprovado  = esp_pend_aprov.cod_usuario_reprovado
                       tt-pendentes.data-reprovacao        = esp_pend_aprov.data-reprovacao
                       tt-pendentes.Dias                   = esp_pend_aprov.Dias                   
                       tt-pendentes.data-geracao           = esp_pend_aprov.data-geracao            
                       tt-pendentes.data-aprovacao1        = esp_pend_aprov.data-aprovacao1        
                       tt-pendentes.data-aprovacao2        = esp_pend_aprov.data-aprovacao2        
                       tt-pendentes.modulo                 = esp_pend_aprov.modulo     
                       tt-pendentes.nome-abrev             = emitente.nome-abrev 
                       tt-pendentes.nom_usuario            = usuar_mestre.nom_usuario 
                       tt-pendentes.vl-a-pagar             = dupli-apagar.vl-a-pagar
                       tt-pendentes.dt-emissao             = dupli-apagar.dt-emissao 
                       tt-pendentes.dt-vencim              = dupli-apagar.dt-vencim 
                       tt-pendentes.selec                  = NO.
                       
               
                CASE esp_pend_aprov.modulo:
                    WHEN 1 THEN
                        tt-pendentes.Nom-modulo = "Contas a pagar".
                    WHEN 2 THEN
                        tt-pendentes.Nom-modulo = "Recebimento". 
                    OTHERWISE
                        tt-pendentes.Nom-modulo = "".
                END CASE.

            END.

        END. /*FOR FIRST esp_param_bloq*/

    END. /*FOR EACH esp_estab_aprov */

 END. /*IF AVAIL esp_aprovador THEN DO:*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CarregarPendencias w-forma 
PROCEDURE CarregarPendencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND FIRST esp_aprovador WHERE esp_aprovador.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
 IF AVAIL esp_aprovador 
      AND data_vigen_ini <= TODAY AND data_vigen_fim >= TODAY THEN DO:

    FOR EACH esp_estab_aprov 
        WHERE esp_estab_aprov.cod_usuario = esp_aprovador.cod_usuario NO-LOCK:

        FOR FIRST esp_param_bloq
            WHERE esp_param_bloq.cod-estabel = esp_estab_aprov.cod-estabel
              AND esp_param_bloq.bloq-atualizar:

            FOR EACH esp_pend_aprov 
                WHERE esp_pend_aprov.cod-estabel             =  esp_param_bloq.cod-estabel
                  AND esp_pend_aprov.cod_usuario_aprovador1  = ""  
/*                    OR  esp_pend_aprov.cod_usuario_aprovador2  = "") */
/*                   AND esp_pend_aprov.cod_usuario_aprovador1  <> esp_aprovador.cod_usuario */
/*                   AND esp_pend_aprov.cod_usuario_aprovador2  <> esp_aprovador.cod_usuario */
                  AND esp_pend_aprov.cod_usuario_reprovador  = "": 

                FIND FIRST   emitente     WHERE emitente.cod-emitente    = esp_pend_aprov.cod-emitente NO-LOCK NO-ERROR.
                IF NOT AVAIL emitente     THEN RETURN ERROR.

                FIND FIRST   usuar_mestre WHERE usuar_mestre.cod_usuario = esp_pend_aprov.cod_usuario_geracao NO-LOCK NO-ERROR.
                IF NOT AVAIL usuar_mestre THEN RETURN ERROR.

                FIND FIRST   dupli-apagar
                     WHERE   dupli-apagar.serie-docto    = esp_pend_aprov.serie-docto   
                       AND   dupli-apagar.nro-docto      = esp_pend_aprov.nro-docto     
                       AND   dupli-apagar.cod-emitente   = esp_pend_aprov.cod-emitente  
                       AND   dupli-apagar.nat-operacao   = esp_pend_aprov.nat-operacao.
                IF NOT AVAIL dupli-apagar THEN RETURN ERROR.

                CREATE tt-pendentes.
                ASSIGN tt-pendentes.cod-estabel            = esp_pend_aprov.cod-estabel            
                       tt-pendentes.serie-docto            = esp_pend_aprov.serie-docto            
                       tt-pendentes.nro-docto              = esp_pend_aprov.nro-docto              
                       tt-pendentes.cod-emitente           = esp_pend_aprov.cod-emitente           
                       tt-pendentes.nat-operacao           = esp_pend_aprov.nat-operacao           
                       tt-pendentes.cod_usuario_geracao    = esp_pend_aprov.cod_usuario_geracao    
                       tt-pendentes.cod_usuario_aprovador1 = esp_pend_aprov.cod_usuario_aprovador1 
                       tt-pendentes.cod_usuario_aprovador2 = esp_pend_aprov.cod_usuario_aprovador2
                       tt-pendentes.cod_usuario_reprovado  = esp_pend_aprov.cod_usuario_reprovado
                       tt-pendentes.data-reprovacao        = esp_pend_aprov.data-reprovacao
                       tt-pendentes.Dias                   = esp_pend_aprov.Dias                   
                       tt-pendentes.data-geracao           = esp_pend_aprov.data-geracao            
                       tt-pendentes.data-aprovacao1        = esp_pend_aprov.data-aprovacao1        
                       tt-pendentes.data-aprovacao2        = esp_pend_aprov.data-aprovacao2
                       tt-pendentes.cod_usuario_reprovado  = esp_pend_aprov.cod_usuario_reprovado
                       tt-pendentes.data-reprovacao        = esp_pend_aprov.data-reprovacao
                       tt-pendentes.modulo                 = esp_pend_aprov.modulo     
                       tt-pendentes.nome-abrev             = emitente.nome-abrev 
                       tt-pendentes.nom_usuario            = usuar_mestre.nom_usuario 
                       tt-pendentes.vl-a-pagar             = dupli-apagar.vl-a-pagar
                       tt-pendentes.dt-emissao             = dupli-apagar.dt-emissao 
                       tt-pendentes.dt-vencim              = dupli-apagar.dt-vencim 
                       tt-pendentes.selec                  = NO.
                       
               
                CASE esp_pend_aprov.modulo:
                    WHEN 1 THEN
                        tt-pendentes.Nom-modulo = "Contas a pagar".
                    WHEN 2 THEN
                        tt-pendentes.Nom-modulo = "Recebimento". 
                    OTHERWISE
                        tt-pendentes.Nom-modulo = "".
                END CASE.

            END.

        END. /*FOR FIRST esp_param_bloq*/

    END. /*FOR EACH esp_estab_aprov */

 END. /*IF AVAIL esp_aprovador THEN DO:*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CarregarSelecionados w-forma 
PROCEDURE CarregarSelecionados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-selecionados:
    IF tt-selecionados.cod_usuario_aprovador1 <> "" AND tt-selecionados.aprov1 = "" THEN DO:
        FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = tt-selecionados.cod_usuario_aprovador1 NO-LOCK NO-ERROR.
        IF   AVAIL usuar_mestre THEN 
            tt-selecionados.aprov1 = usuar_mestre.nom_usuario.

    END.
/*     IF tt-selecionados.cod_usuario_aprovador2 <> "" AND tt-selecionados.aprov2 = "" THEN DO:                             */
/*        FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = tt-selecionados.cod_usuario_aprovador2 NO-LOCK NO-ERROR. */
/*        IF   AVAIL usuar_mestre THEN                                                                                      */
/*             tt-selecionados.aprov2 = usuar_mestre.nom_usuario.                                                           */
/*                                                                                                                          */
/*     END.                                                                                                                 */
    IF tt-selecionados.cod_usuario_reprovador <> "" AND tt-selecionados.reprov = "" THEN DO:
       FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = tt-selecionados.cod_usuario_reprovador NO-LOCK NO-ERROR.
       IF   AVAIL usuar_mestre THEN 
            tt-selecionados.reprov = usuar_mestre.nom_usuario.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CarregarUsuario w-forma 
PROCEDURE CarregarUsuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = c-seg-usuario.

 IF AVAIL usuar_mestre THEN DO:
     f-Cod-Apro:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = usuar_mestre.cod_usuario.
     f-Nom-Aprov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = usuar_mestre.nom_usuario.
 END.
           


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-forma  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
  THEN DELETE WIDGET w-forma.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-forma  _DEFAULT-ENABLE
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
  DISPLAY f-Cod-Apro f-Nom-Aprov r-aprov-pend 
      WITH FRAME f-cad IN WINDOW w-forma.
  ENABLE RECT-22 rt-button rt-button-2 r-aprov-pend b-pendentes b-selecionados 
         but-reprovar but-confirmar 
      WITH FRAME f-cad IN WINDOW w-forma.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-forma.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnviarEmail w-forma 
PROCEDURE EnviarEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM TABLE FOR tt-selecionados.
DEF INPUT PARAM num AS INT.
DEF INPUT PARAM nome-arquivo AS CHAR.

c-mess = "".

    FIND FIRST   usuar_mestre WHERE usuar_mestre.cod_usuario = esp_pend_aprov.cod_usuario_geracao NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar_mestre THEN RETURN ERROR.

    FIND FIRST usuar_mestre_ext WHERE usuar_mestre_ext.cod_usuario = usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar_mestre_ext THEN RETURN ERROR.

    FIND FIRST esp_param_bloq WHERE esp_param_bloq.cod-estabel = tt-selecionados.cod-estabel NO-LOCK NO-ERROR.
     

    c-mess =   "<html>"                                                                          
               + "<head>" 
               + "<style>"
               + "table, th, td"
               + "~{"
               + "border: 1px solid black;"
               + "border-collapse: collapse;"
               + "text-align: CENTER;"
               + "}"
               + "</style>"
               + "</head>"                                                                      
               + "<body>".
     IF num = 1 THEN DO:
     
           c-mess = c-mess + "<p>Prezado(a)(s),<p>"
                     + "<p>A nota fiscal abaixo foi reprovada pelo aprovador(a) "
                     + "<strong>" + INPUT FRAME {&FRAME-NAME} f-Nom-Aprov + "</strong>"
                     + " devido ao motivo:<p>" 
                     + esp_pend_aprov.mot-reprov + "</p>".

          /* assun = "Reprovada Nota no Recebimento - Prazo pagamento inferior a " + string(esp_param_bloq.dias-venc) + " dias".*/
           assun = "Documento Reprovado".

     END.
     ELSE IF num = 2  THEN DO:
     
           c-mess = c-mess + "<p>Prezado(a)(s),<p>"
                     + "<p>A nota fiscal abaixo foi aprovada pelo aprovador(a) "
                     + "<strong>" + INPUT FRAME {&FRAME-NAME} f-Nom-Aprov + "</strong>"
                     + ". </p>".

          /*assun = "Aprovado Nota no Recebimento - Prazo pagamento inferior a " + string(esp_param_bloq.dias-venc) + " dias".*/
          assun = "Documento Aprovado".

     END.
     ELSE IF num = 3  THEN DO:
     
           c-mess = c-mess + "<p>Prezado(a)(s),<p>"
                     + "<p> Ao tentar aprovar o documento abaixo foram apresentados os erros em anexo."
                     + "<p></p>"
                     + "<p></p>".

          assun = "NF " + tt-selecionados.nro-docto + " com erro" .

     END.

     c-mess = c-mess +  "<TABLE style=~"width:100%~">"                                                                      
               + "<tr>"                                                                         
               + "<th>" + "Estab" + "</th>"                                                   
               + "<th>" + "Fornecedor" + "</th>"                                              
               + "<th>" + "Nota" + "</th>"                                                    
               + "<th>" + "Valor" +  "</th>"                                                   
               + "<th>" + "Data Emiss∆o" + "</th>"                                            
               + "<th>" + "Data Vencimento" + "</th>"                                         
               + "<th>" + "Data da Atualizaá∆o" + "</th>"                                               
               + "<th>" + "Nr. Dias" + "</th>"                                                
               + "<th>" + "Usu†rio Digitaá∆o NF" + "</th>"                                    
               + "</tr>"
               + "<tr>"                                                                         
               + "<td>" + tt-selecionados.cod-estabel +                "</td>"                                                   
               + "<td>" +  string(tt-selecionados.cod-emitente) + " - " + tt-selecionados.nome-abrev +           "</td>"                                              
               + "<td>" + tt-selecionados.nro-docto +                 "</td>"                                                    
               + "<td>" + string(tt-selecionados.vl-a-pagar, ">>>>>,>>>,>>9.99") +                "</td>"                                                   
               + "<td>" + string(tt-selecionados.dt-emissao) +         "</td>"                                            
               + "<td>" + string(tt-selecionados.dt-vencim) +      "</td>"                                         
               + "<td>" + string(tt-selecionados.data-geracao,"99/99/9999 HH:MM") +            "</td>"                                               
               + "<td>" + string(tt-selecionados.Dias) +             "</td>"                                                
               + "<td>" + tt-selecionados.nom_usuario + "</td>"                                    
               + "</tr>"
               + "</table>"
               + "<p>&nbsp;</p>"
               + "<p>" + "At," + "</p>"
               + "<p>" + "<strong>" + "Sustená∆o Yamana." + "</strong>" + "</p>"
               + "</body>"                                                                      
               + "</html>" .                                                                     


       EMPTY TEMP-TABLE tt-envio2.
       EMPTY TEMP-TABLE tt-mensagem.

       FIND FIRST param_email NO-LOCK NO-ERROR.

       /* Cria os e-mail */

       CREATE tt-envio2.
       ASSIGN tt-envio2.versao-integracao = 1
              tt-envio2.exchange          = param_email.log_servid_exchange
              tt-envio2.servidor          = param_email.cod_servid_e_mail
              tt-envio2.porta             = param_email.num_porta
              tt-envio2.destino           =  usuar_mestre_ext.cod_usuar_so + "@yamana.com"
/*               tt-envio2.destino           = "v-fpereira@yamana.com" */
              tt-envio2.assunto           = assun
              tt-envio2.remetente         = tt-selecionados.nom-modulo + "@yamana.com"
              tt-envio2.copia             = ""
/*               tt-envio2.mensagem          = "Felipe" */
              tt-envio2.importancia       = 1
              tt-envio2.log-enviada       = NO
              tt-envio2.log-lida          = NO
              tt-envio2.acomp             = NO 
              tt-envio2.formato           = "html".

       IF num = 3 THEN
           tt-envio2.arq-anexo = nome-arquivo.

       CREATE tt-mensagem.
       ASSIGN tt-mensagem.seq-mensagem = 1
              tt-mensagem.mensagem     = c-mess.

      /*
      FOR EACH esp_aprovador NO-LOCK:

          FIND FIRST esp_estab_aprov 
               WHERE esp_estab_aprov.cod_usuario = esp_aprovador.cod_usuario
                 AND esp_estab_aprov.cod-estabel = tt-selecionados.cod-estabel NO-LOCK NO-ERROR.

          IF NOT AVAIL esp_estab_aprov THEN RETURN ERROR.

          FIND FIRST esp_param_bloq 
               WHERE esp_param_bloq.cod-estabel = esp_estab_aprov.cod-estabel NO-LOCK NO-ERROR.

          IF AVAIL esp_param_bloq AND esp_param_bloq.env-relat THEN DO:

            
             CREATE tt-envio2.
             ASSIGN tt-envio2.versao-integracao = 1
                    tt-envio2.exchange          = param_email.log_servid_exchange
                    tt-envio2.servidor          = param_email.cod_servid_e_mail
                    tt-envio2.porta             = param_email.num_porta
                    tt-envio2.destino           =  esp_aprovador.email
                    tt-envio2.assunto           = "Reprovada Nota no Recebimento - Prazo pagamento inferior a " + string(tt-selecionados.Dias) + " dias"
                    tt-envio2.remetente         = tt-selecionados.Nom-modulo + "@yamana.com"
                    tt-envio2.copia             = ""
/*                     tt-envio2.mensagem          = "Felipe" */
                    tt-envio2.importancia       = 1
                    tt-envio2.log-enviada       = NO
                    tt-envio2.log-lida          = NO
                    tt-envio2.acomp             = NO .
           
             CREATE tt-mensagem.
             ASSIGN tt-mensagem.seq-mensagem = i-cont
                    tt-mensagem.mensagem     = c-mess.

             i-cont = i-cont + 1.
             
          END.

      END.
      */



       RUN utp/utapi019.p PERSISTENT SET h-utapi019.

       RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                      INPUT  TABLE tt-mensagem,
                                      OUTPUT TABLE tt-erros).

/*        FOR EACH tt-erros:                                      */
/*                                                                */
/*            DISP tt-erros.cod-erro                              */
/*                 tt-erros.desc-erro                             */
/*                 tt-erros.desc-arq WITH WIDTH 500 DOWN FRAME f. */
/*        END.                                                    */

       DELETE PROCEDURE h-utapi019.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-forma 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "REP003-WO1" "1.00.00.000"} 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN CarregarUsuario.
  RUN CarregarPendencias.
  {&OPEN-QUERY-b-pendentes}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-forma  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-selecionados"}
  {src/adm/template/snd-list.i "tt-pendentes"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-forma 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TrataArquivo w-forma 
PROCEDURE TrataArquivo :
EMPTY TEMP-TABLE tt-trata-arq.
DEFINE VARIABLE c-linha AS CHARACTER   NO-UNDO.

INPUT FROM VALUE(nome-arquivo).
REPEAT:
    IMPORT UNFORMATTED c-linha.
    CREATE tt-trata-arq.
    ASSIGN
        tt-trata-arq.linha-arq = c-linha.
    IF SUBSTRING(c-linha, 106,8) = "RE1005RP" THEN LEAVE.
END.
INPUT CLOSE.

IF TEMP-TABLE tt-trata-arq:HAS-RECORDS THEN

FOR EACH tt-trata-arq:

    IF SUBSTRING(tt-trata-arq.linha-arq,53,26) = "Nota Fiscal atualizada com" THEN
    DO:
        ASSIGN SUBSTRING(tt-trata-arq.linha-arq,53,32) = "Nota Fiscal nao atualizada      ".
    END.
END.
OUTPUT TO VALUE(nome-arquivo).
FOR EACH tt-trata-arq:
    PUT tt-trata-arq.linha-arq FORMAT "x(132)" AT 01 SKIP.
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

