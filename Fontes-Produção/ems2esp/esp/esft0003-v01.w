&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer format ">>>>,>>9" label "Nœmero" column-label "Nßmero"
    field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia".

def var h_api_cta_ctbl       as handle no-undo.

def var h_api_ccusto         as handle no-undo.
def var c-formato-conta      as char no-undo.
def var c-formato-ccusto     as char no-undo. 

/* Variÿveis conta ctbl */
def var v_cod_cta_ctbl       as char   no-undo.
def var v_titulo_cta_ctbl    as char   no-undo.
def var v_num_tip_cta_ctbl   as int    no-undo.
def var v_num_sit_cta_ctbl   as int    no-undo.


def var i-empresa   like param-global.empresa-prin no-undo.


/*def new global shared var i-ep-codigo-usuario   like param-global.empresa-prin    no-undo.*/

{utp/ut-glob.i}

/*Inicio Unifica»’o de Conceitos CONTA CONTABIL 2011*/
DEF VAR i-empresaCtContabel LIKE param-global.empresa-prin NO-UNDO.
def var h_api_cta               as handle no-undo.
def var h_api_ccust             as handle no-undo.
def var v_cod_cta               as char   no-undo.
def var v_des_cta               as char   no-undo.
def var v_cod_ccust             as char   no-undo.
def var v_des_ccust             as char   no-undo.
def var v_cod_format_cta        as char   no-undo.
def var v_cod_format_ccust      as char   no-undo.
def var v_cod_format_inic       as char   no-undo.
def var v_cod_format_fim        as char   no-undo.
def var v_ind_finalid_cta       as char   no-undo.
def var v_num_tip_cta           as int    no-undo.
def var v_num_sit_cta           as int    no-undo.
def var v_log_utz_ccust         as log    no-undo.
def var v_cod_format_inic_ccust as char   no-undo.
def var v_cod_format_fim_ccust  as char   no-undo.
def var l-utiliza-ccusto        as LOG   no-undo.

/* def temp-table tt_log_erro no-undo                                                                           */
/*     field ttv_num_cod_erro  as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero"                */
/*     field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda" */
/*     field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia". */

/*Fim Unifica»’o de Conceitos CONTA CONTABIL 2011*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-conta-cfop
&Scoped-define FIRST-EXTERNAL-TABLE es-conta-cfop


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-conta-cfop.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-conta-cfop.ct-codigo 
&Scoped-define ENABLED-TABLES es-conta-cfop
&Scoped-define FIRST-ENABLED-TABLE es-conta-cfop
&Scoped-Define ENABLED-OBJECTS rt-key rt-key-2 
&Scoped-Define DISPLAYED-FIELDS es-conta-cfop.cod-estabel ~
es-conta-cfop.nat-operacao es-conta-cfop.ct-codigo es-conta-cfop.sc-codigo 
&Scoped-define DISPLAYED-TABLES es-conta-cfop
&Scoped-define FIRST-DISPLAYED-TABLE es-conta-cfop
&Scoped-Define DISPLAYED-OBJECTS fi-descricao-estabelec fi-descricao-cfop ~
fi-descricao-conta fi-descricao-subconta 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-conta-cfop.cod-estabel ~
es-conta-cfop.nat-operacao es-conta-cfop.ct-codigo es-conta-cfop.sc-codigo 
&Scoped-define ADM-MODIFY-FIELDS es-conta-cfop.ct-codigo ~
es-conta-cfop.sc-codigo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-descricao-cfop AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descricao-conta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descricao-estabelec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descricao-subconta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.25.

DEFINE RECTANGLE rt-key-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-conta-cfop.cod-estabel AT ROW 1.17 COL 20 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     fi-descricao-estabelec AT ROW 1.17 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     es-conta-cfop.nat-operacao AT ROW 2.17 COL 19 COLON-ALIGNED WIDGET-ID 4
          LABEL "Nat Opera‡Æo"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-descricao-cfop AT ROW 2.17 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     es-conta-cfop.ct-codigo AT ROW 3.67 COL 13.29 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-descricao-conta AT ROW 3.67 COL 25.57 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     es-conta-cfop.sc-codigo AT ROW 4.67 COL 13.29 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-descricao-subconta AT ROW 4.67 COL 25.57 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     rt-key AT ROW 1 COL 1
     rt-key-2 AT ROW 3.5 COL 1 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es-conta-cfop
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.33
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN es-conta-cfop.cod-estabel IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-conta-cfop.ct-codigo IN FRAME f-main
   1 3                                                                  */
/* SETTINGS FOR FILL-IN fi-descricao-cfop IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao-conta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao-estabelec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao-subconta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-conta-cfop.nat-operacao IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN es-conta-cfop.sc-codigo IN FRAME f-main
   NO-ENABLE 1 3                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-conta-cfop.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.cod-estabel V-table-Win
ON F5 OF es-conta-cfop.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   {include/zoomvar.i &prog-zoom=adzoom/z10ad107.w
                      &campo=es-conta-cfop.cod-estabel
                      &campozoom=cod-estabel
                      &campo2=fi-descricao-estabelec
                      &campozoom2=descricao}
                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.cod-estabel V-table-Win
ON LEAVE OF es-conta-cfop.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   {include/leave.i &tabela=estabelec
                    &atributo-ref=cod-estabel
                    &variavel-ref=es-conta-cfop.cod-estabel
                    &atributo-ref2=nome
                    &variavel-ref2=fi-descricao-estabelec
                    &where="estabelec.cod-estabel = input frame {&frame-name} es-conta-cfop.cod-estabel"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conta-cfop.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conta-cfop.ct-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.ct-codigo V-table-Win
ON F5 OF es-conta-cfop.ct-codigo IN FRAME f-main /* Conta */
DO:
    def var v_cod_finalid as char no-undo.                                               
  
   ASSIGN v_cod_finalid     = "(nenhum)"
          v_cod_cta_ctbl    = ""
          v_titulo_cta_ctbl = "".

   ASSIGN i-empresa = i-ep-codigo-usuario.


   run pi_zoom_cta_ctbl_integr in h_api_cta      (input  i-empresa,          /* EMPRESA EMS2 */
                                                  input  "CEP",              /* M–DULO */
                                                  input  "",                 /* PLANO DE CONTAS */
                                                  input  v_cod_finalid,      /* FINALIDADES */
                                                  input TODAY,               /* DATA TRANSACAO */
                                                  output v_cod_cta_ctbl,     /* CODIGO CONTA */
                                                  output v_titulo_cta_ctbl,  /* DESCRICAO CONTA */
                                                  output v_ind_finalid_cta,  /* FINALIDADE DA CONTA */
                                                  output table tt_log_erro). /* ERROS */ 

    if v_titulo_cta_ctbl <> "" then do:
        ASSIGN es-conta-cfop.ct-codigo:SCREEN-VALUE = v_cod_cta_ctbl.
        ASSIGN fi-descricao-conta:SCREEN-VALUE      = v_titulo_cta_ctbl.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.ct-codigo V-table-Win
ON LEAVE OF es-conta-cfop.ct-codigo IN FRAME f-main /* Conta */
DO:
    ASSIGN i-empresa = i-ep-codigo-usuario.

    IF SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:
        RUN limpaErros.
        ASSIGN v_cod_cta = SELF:SCREEN-VALUE NO-ERROR.

        ASSIGN v_cod_cta = REPLACE(v_cod_cta,".","").

        RUN pi_busca_dados_cta_ctbl IN h_api_cta (INPUT        i-empresa,           /* EMPRESA EMS2 */
                                                  INPUT        "",                  /* PLANO DE CONTAS */
                                                  INPUT-OUTPUT v_cod_cta,           /* CONTA */
                                                  INPUT        TODAY,               /* DATA TRANSACAO */   
                                                  OUTPUT       v_des_cta,           /* DESCRICAO CONTA */
                                                  OUTPUT       v_num_tip_cta,       /* TIPO DA CONTA */
                                                  OUTPUT       v_num_sit_cta,       /* SITUAÜ›O DA CONTA */
                                                  OUTPUT       v_ind_finalid_cta,   /* FINALIDADES DA CONTA */
                                                  OUTPUT TABLE tt_log_erro).        /* ERROS */ 

        IF NOT CAN-FIND(tt_log_erro) OR RETURN-VALUE = "OK" THEN DO:
            ASSIGN SELF:FORMAT                     IN FRAME {&FRAME-NAME} = v_cod_format_fim
                   SELF:SCREEN-VALUE               IN FRAME {&FRAME-NAME} = v_cod_cta
                   fi-descricao-conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_des_cta NO-ERROR.
                          
            RUN limpaErros.
            RUN pi_verifica_utilizacao_ccusto IN h_api_ccust (INPUT  i-empresa,           /* EMPRESA EMS 2 */
                                                              INPUT  "",                  /* ESTABELECIMENTO EMS2 */
                                                              INPUT  "",                  /* PLANO CONTAS */
                                                              INPUT  v_cod_cta,           /* CONTA */
                                                              INPUT  TODAY,               /* DT TRANSACAO */
                                                              OUTPUT v_log_utz_ccust,     /* UTILIZA CCUSTO ? */
                                                              OUTPUT TABLE tt_log_erro).  /* ERROS */                          
            IF v_log_utz_ccust THEN
                ASSIGN es-conta-cfop.sc-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.  
            ELSE
                ASSIGN es-conta-cfop.sc-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                       es-conta-cfop.sc-codigo:SENSITIVE    IN FRAME {&FRAME-NAME} = NO
                       fi-descricao-subconta:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = "".

           RETURN "OK".
        END.
        ELSE DO:
           FOR EACH tt_log_erro:
               RUN utp/ut-msgs.p (INPUT 'show',
                                  INPUT 17006, 
                                  INPUT STRING(tt_log_erro.ttv_des_msg_erro) + ' ('  + STRING(tt_log_erro.ttv_num_cod_erro) + ')' + "~~" + tt_log_erro.ttv_des_msg_ajuda).
               RETURN "adm-error".
           END.
           
           ASSIGN es-conta-cfop.sc-codigo:SCREEN-VALUE     IN FRAME {&FRAME-NAME} = ""
                  es-conta-cfop.sc-codigo:SENSITIVE        IN FRAME {&FRAME-NAME} = NO
                  fi-descricao-conta:SCREEN-VALUE          IN FRAME {&FRAME-NAME} = ""
                  fi-descricao-subconta:SCREEN-VALUE       IN FRAME {&FRAME-NAME} = "".
        END.
    END.
    ELSE
        ASSIGN es-conta-cfop.sc-codigo:SCREEN-VALUE         IN FRAME {&FRAME-NAME} = ""
               es-conta-cfop.sc-codigo:SENSITIVE            IN FRAME {&FRAME-NAME} = NO
               fi-descricao-conta:SCREEN-VALUE              IN FRAME {&FRAME-NAME} = ""
               fi-descricao-subconta:SCREEN-VALUE           IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.ct-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conta-cfop.ct-codigo IN FRAME f-main /* Conta */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.ct-codigo V-table-Win
ON TAB OF es-conta-cfop.ct-codigo IN FRAME f-main /* Conta */
DO:
  APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conta-cfop.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.nat-operacao V-table-Win
ON LEAVE OF es-conta-cfop.nat-operacao IN FRAME f-main /* Nat Opera‡Æo */
DO:
{include/leave.i &tabela=natur-oper 
                 &atributo-ref=denominacao
                 &variavel-ref=fi-descricao-cfop
                 &where="natur-oper.nat-operacao = input frame {&frame-name} es-conta-cfop.nat-operacao"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.nat-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conta-cfop.nat-operacao IN FRAME f-main /* Nat Opera‡Æo */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-conta-cfop.sc-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.sc-codigo V-table-Win
ON F5 OF es-conta-cfop.sc-codigo IN FRAME f-main /* Subconta */
DO:
    RUN limpaErros.
    run pi_zoom_ccusto in h_api_ccust (input  i-empresa,           /* EMPRESA EMS2 */
                                       input  "",                  /* CODIGO DO PLANO CCUSTO */
                                       input  "",                  /* UNIDADE DE NEGOCIO */
                                       input  today,               /* DATA DE TRANSACAO */
                                       output v_cod_ccust,         /* CODIGO CCUSTO */
                                       output v_des_ccust,         /* DESCRICAO CCUSTO */
                                       output table tt_log_erro).  /* ERROS */ 
    
    IF NOT CAN-FIND(tt_log_erro)OR RETURN-VALUE = "OK" THEN
        IF v_cod_ccust <> "" THEN
            ASSIGN SELF:SCREEN-VALUE                  IN FRAME {&FRAME-NAME} = v_cod_ccust
                   fi-descricao-subconta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_des_ccust.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.sc-codigo V-table-Win
ON LEAVE OF es-conta-cfop.sc-codigo IN FRAME f-main /* Subconta */
DO:
    RUN pi-valida-centro (INPUT SELF:SCREEN-VALUE).
    ASSIGN es-conta-cfop.sc-codigo:FORMAT IN FRAME {&FRAME-NAME} = IF RETURN-VALUE = "adm-error" THEN "x(20)" ELSE v_cod_format_fim_ccust.

    ASSIGN v_des_ccust = "".

    ASSIGN v_cod_ccust = INPUT FRAME {&FRAME-NAME} es-conta-cfop.sc-codigo NO-ERROR.                                                       
    run pi_busca_dados_ccusto in h_api_ccust (input  i-empresa,           /* EMPRESA EMS2 */
                                              input  "",                  /* CODIGO DO PLANO CCUSTO */
                                              input  v_cod_ccust,         /* CCUSTO */
                                              input  today,               /* DATA DE TRANSACAO */
                                              output v_des_ccust,         /* DESCRICAO DO CCUSTO */
                                              output table tt_log_erro).  /* ERROS */ 
    
   ASSIGN fi-descricao-subconta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_des_ccust.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-conta-cfop.sc-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-conta-cfop.sc-codigo IN FRAME f-main /* Subconta */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/
  es-conta-cfop.cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  es-conta-cfop.nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  es-conta-cfop.ct-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  es-conta-cfop.sc-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-conta-cfop"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-conta-cfop"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LimpaErros V-table-Win 
PROCEDURE LimpaErros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN /*v_cod_cta       = ""   /* CODIGO CONTA */*/
           v_des_cta         = ""   /* DESCRICAO CONTA */
           v_ind_finalid_cta = ""   /* FINALIDADE DA CONTA */
           v_cod_ccust       = ""   /* CODIGO CCUSTO */
           v_des_ccust       = "".  /* DESCRICAO CCUSTO */


    ASSIGN fi-descricao-conta    = ""
           fi-descricao-subconta = "".

    FOR EACH tt_log_erro:
      DELETE tt_log_erro.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    if  not frame {&frame-name}:validate() then
      return 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    RUN pi-validate.

    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN es-conta-cfop.sc-codigo = INPUT FRAME {&FRAME-NAME} es-conta-cfop.sc-codigo.

    ASSIGN es-conta-cfop.usuar-atualizacao = c-seg-usuario
           es-conta-cfop.data-atual        = TODAY
           es-conta-cfop.hora-atual        = STRING(TIME,"hh:mm:ss"). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    es-conta-cfop.sc-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pi-iniciar.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    

    FIND FIRST cta_ctbl
         WHERE cta_ctbl.cod_cta_ctbl = INPUT FRAME {&FRAME-NAME} es-conta-cfop.ct-codigo
         NO-LOCK NO-ERROR.
    IF AVAILABLE(cta_ctbl) THEN
       ASSIGN fi-descricao-conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cta_ctbl.des_tit_ctbl.
    ELSE
       ASSIGN fi-descricao-conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    es-conta-cfop.ct-codigo:FORMAT IN FRAME {&FRAME-NAME} = v_cod_format_fim.

    APPLY "leave" TO es-conta-cfop.nat-operacao IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es-conta-cfop.cod-estabel  IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es-conta-cfop.sc-codigo    IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-iniciar V-table-Win 
PROCEDURE pi-iniciar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN i-empresa = i-ep-codigo-usuario.

   FIND FIRST param-global
        NO-LOCK NO-ERROR.
   IF AVAILABLE(param-global) THEN
      ASSIGN i-empresa = param-global.empresa-prin.


 /*Inicio UnificaÂÆo de Conceitos CONTA CONTABIL 2011*/
    RUN prgint\utb\utb743za.py PERSISTENT SET h_api_cta.
    RUN prgint\utb\utb742za.py PERSISTENT SET h_api_ccust.
    
    /* ---------------- CONTA ---------------------------------------------------------------------------------------*/
    RUN limpaErros.
    RUN pi_retorna_formato_cta_ctbl in h_api_cta (input i-empresa,           /* EMPRESA EMS2 */
                                                  input  "",                 /* PLANO CONTAS */
                                                  input  today,              /* DATA DE TRANSACAO */
                                                  output v_cod_format_cta,   /* FORMATO cta */
                                                  output table tt_log_erro). /* ERROS */

    IF NOT CAN-FIND(tt_log_erro)
    OR RETURN-VALUE = "OK" THEN 
        ASSIGN v_cod_format_fim = v_cod_format_cta.

    /* ------------- CENTRO DE CUSTO --------------------------------------------------------------------------------------------*/
    RUN limpaErros.
    RUN pi_retorna_formato_ccusto in h_api_ccust (input  i-empresa,              /* EMPRESA EMS2 */
                                                  input  "",                     /* PLANO CCUSTO */
                                                  input  today,                  /* DATA DE TRANSACAO */
                                                  output v_cod_format_ccust,     /* FORMATO CCUSTO */
                                                  output table tt_log_erro).     /* ERROS */

    IF NOT CAN-FIND(tt_log_erro)
    OR RETURN-VALUE = "OK" THEN 
        ASSIGN v_cod_format_fim_ccust = v_cod_format_ccust.
    
    /*Fim UnificaÂÆo de Conceitos CONTA CONTABIL 2011*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-centro V-table-Win 
PROCEDURE pi-valida-centro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF INPUT PARAMETER p-centro AS CHAR NO-UNDO.

   RUN LimpaErros.
   
   RUN pi_busca_dados_ccusto IN h_api_ccust (INPUT        i-empresa,           /* EMPRESA EMS2 */
                                             INPUT        "",                  /* PLANO DE CONTAS */
                                             INPUT        p-centro,            /* CCUSTO */
                                             INPUT        TODAY,               /* DATA TRANSACAO */   
                                             OUTPUT       v_des_ccust,         /* DESCRICAO CCUSTO */
                                             OUTPUT TABLE tt_log_erro).        /* ERROS */
   
   /* validacao da conta */
   IF CAN-FIND(tt_log_erro) OR RETURN-VALUE = "NOK" THEN
      RETURN "adm-error".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-conta V-table-Win 
PROCEDURE pi-valida-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEF INPUT PARAMETER p-conta AS CHAR NO-UNDO.
   
   ASSIGN i-empresa = param-global.empresa-prin.
   
   IF p-conta <> "" THEN DO:

       ASSIGN v_cod_cta = p-conta.
       RUN limpaErros.
       run pi_valida_cta_ctbl_integr in h_api_cta (input  i-empresa,           /* EMPRESA EMS2 */
                                                   input  "CEP",               /* MODULO */
                                                   input  "",                  /* PLANO CONTAS */ 
                                                   input  v_cod_cta,           /* CONTA */
                                                   input  "(nenhum)",          /* FINALIDADES */
                                                   input  today,               /* DATA DE TRANSACAO */ 
                                                   output table tt_log_erro).  /* ERROS */
       /* validacao da conta */
       IF CAN-FIND(tt_log_erro) OR RETURN-VALUE = "NOK" THEN DO:
           FOR FIRST tt_log_erro:
               RUN utp/ut-msgs.p (INPUT 'show',
                                  INPUT 17006, 
                                  INPUT STRING(tt_log_erro.ttv_des_msg_erro) + ' ('  + STRING(tt_log_erro.ttv_num_cod_erro) + ')' + "~~" + tt_log_erro.ttv_des_msg_ajuda).
               RETURN "adm-error".
           END.
       END.
   END. /* FIM IF p-conta <> "" THEN DO: */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-ContaCentro V-table-Win 
PROCEDURE pi-valida-ContaCentro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER p-conta              AS CHARACTER  NO-UNDO.
    DEF INPUT PARAMETER p-centro             AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE     c-cod-estabel        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE     c-cod-unid-negoc-aux AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE     d-dt-trans           AS DATE       NO-UNDO.

    ASSIGN d-dt-trans           = TODAY
           c-cod-unid-negoc-aux = "00".

    FIND FIRST estabelec
         WHERE estabelec.ep-codigo = es-conta-cfop.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
        ASSIGN c-cod-estabel = estabelec.cod-estabel.

    FIND FIRST item-uni-estab
         WHERE item-uni-estab.it-codigo   = "" AND
               item-uni-estab.cod-estabel = es-conta-cfop.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         NO-LOCK NO-ERROR.
    IF AVAILABLE(item-uni-estab) THEN
       ASSIGN c-cod-unid-negoc-aux = item-uni-estab.cod-unid-negoc.         

    run pi_valida_conta_contabil in h_api_cta (input  i-empresa,            /* EMPRESA EMS2 */
                                               input  c-cod-estabel,        /* ESTABELECIMENTO EMS2 */
                                               input  c-cod-unid-negoc-aux, /* UNIDADE NEG…CIO */
                                               input  "",                   /* PLANO CONTAS */ 
                                               input  p-conta,              /* CONTA */
                                               input  "",                   /* PLANO CCUSTO */ 
                                               input  p-centro,             /* CCUSTO */
                                               input  d-dt-trans,           /* DATA TRANSACAO */
                                               output table tt_log_erro).   /* ERROS */

    IF CAN-FIND(tt_log_erro) OR RETURN-VALUE = "NOK" THEN DO:
        FOR FIRST tt_log_erro:
            RUN utp/ut-msgs.p (INPUT 'show',
            INPUT 17006, 
            INPUT STRING(tt_log_erro.ttv_des_msg_erro) + ' ('  + STRING(tt_log_erro.ttv_num_cod_erro) + ')' + "~~" + tt_log_erro.ttv_des_msg_ajuda).
        RETURN "adm-error".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */


    IF adm-new-record THEN DO:
       IF NOT CAN-FIND(FIRST estabelec
                       WHERE estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} es-conta-cfop.cod-estabel
                       NO-LOCK) THEN DO:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show":U, input 2, input "Estabelecimento~~Estabelecimento").
         return 'ADM-ERROR':U.
       END.

       IF NOT CAN-FIND(FIRST natur-oper
                       WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-conta-cfop.nat-operacao
                       NO-LOCK) THEN DO:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show":U, input 2, input "Natureza de Opera‡Æo~~Natureza de Opera‡Æo").
         return 'ADM-ERROR':U.
       END.

       IF NOT CAN-FIND(FIRST cta_ctbl
                       WHERE cta_ctbl.cod_cta_ctbl = INPUT FRAME {&FRAME-NAME} es-conta-cfop.ct-codigo
                       NO-LOCK) THEN DO:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show":U, input 2, input "Conta~~Conta").
         return 'ADM-ERROR':U.
       END.

       IF CAN-FIND(FIRST es-conta-cfop
                   WHERE es-conta-cfop.cod-estabel  = INPUT FRAME {&FRAME-NAME} es-conta-cfop.cod-estabel AND
                         es-conta-cfop.nat-operacao = INPUT FRAME {&FRAME-NAME} es-conta-cfop.nat-operacao
                       NO-LOCK) THEN DO:
         {include/i-vldprg.i}
         run utp/ut-msgs.p (input "show":U, input 1, input "CFOP x Conta Terceiros~~CFOP x Conta Terceiros").
         return 'ADM-ERROR':U.
       END.

    END.

    RUN pi-valida-conta (INPUT FRAME {&FRAME-NAME} es-conta-cfop.ct-codigo).

    IF RETURN-VALUE = "adm-error" THEN DO:
        run utp/ut-msgs.p ("show",47,"Conta Custo Cont bil").
        apply 'entry':U to es-conta-cfop.ct-codigo in frame {&FRAME-NAME}.
        return 'ADM-ERROR':U.
    END.
    IF es-conta-cfop.sc-codigo:SENSITIVE IN FRAME {&FRAME-NAME} THEN DO:
        RUN pi-valida-centro (INPUT FRAME {&FRAME-NAME} es-conta-cfop.sc-codigo).
        IF RETURN-VALUE = "adm-error" THEN DO:
            run utp/ut-msgs.p ("show",47,"CC Custo Cont bil").
            apply 'entry':U to es-conta-cfop.sc-codigo in frame {&FRAME-NAME}.
            return 'ADM-ERROR':U.
        END.
        RUN pi-valida-ContaCentro (INPUT FRAME {&FRAME-NAME} es-conta-cfop.ct-codigo, INPUT FRAME {&FRAME-NAME} es-conta-cfop.sc-codigo).
        IF RETURN-VALUE = "adm-error" THEN DO:
            APPLY 'entry':U TO es-conta-cfop.sc-codigo IN FRAME {&FRAME-NAME}.
            RETURN 'adm-error'.
        END.
    END.

/*     IF NOT CAN-FIND(FIRST cta_ctbl                                                                  */
/*                     WHERE cta_ctbl.cod_cta_ctbl = INPUT FRAME {&FRAME-NAME} es-conta-cfop.ct-codigo */
/*                     NO-LOCK) THEN DO:                                                               */
/*       {include/i-vldprg.i}                                                                          */
/*       run utp/ut-msgs.p (input "show":U, input 1, input "Conta~~Conta").                            */
/*       return 'ADM-ERROR':U.                                                                         */
/*     END.                                                                                            */

    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-conta-cfop"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

