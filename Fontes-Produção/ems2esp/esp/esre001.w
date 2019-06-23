&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: 
** Descricao.............: 
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 25/04/2013
*****************************************************************************/

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
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
DEFINE VARIABLE dt-movto AS DATE        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt_executar rt_rgf rt_cxcf RECT-8 RECT-9 ~
tg-natur-oper fi-arq-nat-oper tg-emitente fi-arq-emitente bt_fechar ~
bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS tg-natur-oper fi-arq-nat-oper tg-emitente ~
fi-arq-emitente 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_executar 
     IMAGE-UP FILE "image/im-run.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Executar" 
     SIZE 4 BY 1.13 TOOLTIP "Executar".

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-arq-emitente AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY .79 NO-UNDO.

DEFINE VARIABLE fi-arq-nat-oper AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .

DEFINE VARIABLE tg-emitente AS LOGICAL INITIAL yes 
     LABEL "Emitente" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg-natur-oper AS LOGICAL INITIAL yes 
     LABEL "Natureza de Opera‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt_executar AT ROW 1.17 COL 1.86 WIDGET-ID 76
     tg-natur-oper AT ROW 2.75 COL 4 WIDGET-ID 140
     fi-arq-nat-oper AT ROW 3.92 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     tg-emitente AT ROW 5.42 COL 4 WIDGET-ID 146
     fi-arq-emitente AT ROW 6.58 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     bt_fechar AT ROW 15.71 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 15.71 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 15.71 COL 70.43 WIDGET-ID 24
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 15.5 COL 1 WIDGET-ID 2
     RECT-8 AT ROW 3.17 COL 2 WIDGET-ID 6
     RECT-9 AT ROW 5.83 COL 2 WIDGET-ID 144
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 1
         CANCEL-BUTTON bt_cancelar WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ".: Exporta‡Æo de Dados :."
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 274.29
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
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f_main
   FRAME-NAME                                                           */
ASSIGN 
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Exporta‡Æo de Dados :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Exporta‡Æo de Dados :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME f_main /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancelar C-Win
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Cancelar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_executar C-Win
ON CHOOSE OF bt_executar IN FRAME f_main /* Executar */
DO:
    do on error undo, return no-apply:
        run pi_executar.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arq-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-emitente C-Win
ON LEAVE OF fi-arq-emitente IN FRAME f_main
DO:

    if index (fi-arq-emitente:screen-value in frame {&frame-name},'CSV') = 0 then do:
        assign fi-arq-emitente:screen-value in frame {&frame-name} = session:temp-directory + "emitente.csv".
    end.
    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arq-nat-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-nat-oper C-Win
ON LEAVE OF fi-arq-nat-oper IN FRAME f_main
DO:

    if index (fi-arq-nat-oper:screen-value in frame {&frame-name},'CSV') = 0 then do:
        assign fi-arq-nat-oper:screen-value in frame {&frame-name} = session:temp-directory + "nat-oper.csv".
    end.
    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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

    {include/i_dbinst.i}
    {include/i_dbtype.i}
    {include/i_fcldef.i}

    {include/i_fclwin.i c-win}
    {include/i_fclfrm.i f_main }

  RUN enable_UI.
  
  run pi_inicio.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY tg-natur-oper fi-arq-nat-oper tg-emitente fi-arq-emitente 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt_executar rt_rgf rt_cxcf RECT-8 RECT-9 tg-natur-oper fi-arq-nat-oper 
         tg-emitente fi-arq-emitente bt_fechar bt_cancelar Btn_Help 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cab-nat-oper C-Win 
PROCEDURE pi-cab-nat-oper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    PUT UNFORMAT "Natureza;".
    PUT UNFORMAT "CFOP;".
    put unformat "Denomina‡Æo;".
    put UNFORMAT "Tipo;".
    put unformat "Especie;".
    put unformat "Mercado;".
    put unformat "Canal de Venda;".
    put unformat "Cod Msg;".
    put unformat "Modelo Doc OF;".
    put unformat "Modelo Cupom Fiscal;".
    put unformat "Modelo NF-e;".
    put unformat "Especie Titulo;".
    put unformat "Tipo Rec/Desp;".
    /* Adicionais */
    put unformat "Opera‡Æo Transferencia;".
    put unformat "Natur Complementar;".
    put unformat "Gerar Nota Faturamento;".
    put unformat "Nota Rateio;".
    put unformat "Nota Propria;".
    put unformat "Nota Comercio;".
    put unformat "Compra / Venda Ativo;".
    put unformat "Gera Ficha Automatico;".
    put unformat "Inicio Credito Automatico;".
    put unformat "Venda Ambulante;".
    put unformat "Gerar Devol Somente Valor;".
    put unformat "Opera‡Æo Triangular;".
    put unformat "Natureza Drawback;".
    put unformat "Memorando Exporta‡Æo;".
    put unformat "Natureza Bonifica‡Æo;".
    put unformat "Opera‡Æo com Terceiros;".
    put unformat "Tipo Oper Terc;".
    put unformat "Alt Valor Itens Terc;".
    put unformat "Tipo Compra;".
    put unformat "Natur Bonifica‡Æo;".
    /* Atualiza‡äes */
    put unformat "Calculo Automatico;".
    put unformat "Impressao Automatica;".
    put unformat "Baixar Estoque;".
    put unformat "Controle Estoque Automatico;".
    put unformat "Gerar Duplicatas;".
    put unformat "CR Automatico;".
    put unformat "Gera Obrigacoes Fiscais;".
    put unformat "Atualziar Cotas;".
    put unformat "Cinsidera NFS nao trib p/ calculo coef. CIAP;".
    put unformat "Considera NFS p/ calculo coef CIAP;".
    put unformat "Enviar XML NF-e Manualemente;".
    put unformat "NF-e de Estorno;".
    put unformat "Gerar Contabiliza‡Æo;".
    put unformat "Contabilizacao Automatica;".
    put unformat "Atualizar Estatistica;".
    put unformat "Estatistica Automatica;".
    put unformat "Operacoes de Entrega Futura;".
    /* Impostos */
    PUT UNFORMAT "Tipo Base IPI;".
    put UNFORMAT "Incluir Frete Base IPI;".
    put UNFORMAT "C¢d Vincula‡Æo IPI;".
    put UNFORMAT "C¢d Tributa‡Æo IPI;".
    put UNFORMAT "% Redu‡Æo IPI;".
    PUT UNFORMAT "Imprime IPI Outras no DANFE;".
    PUT UNFORMAT "Incluir IPI Base ICMS;".
    PUT UNFORMAT "Incluir IPI ICMS Outras;".
    PUT UNFORMAT "Incluir IPI Outros Total NF;".
    PUT UNFORMAT "Incluir IPI Outros Base Subs;".
    PUT UNFORMAT "Escritura‡Æo IPI Frete;".
    PUT UNFORMAT "Estorna IPI;".
    PUT UNFORMAT "IPI Imune (Situa‡Æo Tribut ria);".
    PUT UNFORMAT "IPI NÆo Tributado (Situa‡Æo Tribut ria);".
    PUT UNFORMAT "SuspensÆo IPI Importa‡Æo;".
    PUT UNFORMAT "Tipo Base ISS;".
    PUT UNFORMAT "Cod Tributacao ISS;".
    put UNFORMAT "% Redu‡Æo ISS;".
    PUT UNFORMAT "Considera ICMS Outros na NF-e;".
    put UNFORMAT "Natur Vinculada;".
    put UNFORMAT "Retem IR na Fonte;".
    PUT UNFORMAT "%IRRF;".
    /* ICMS */
    put UNFORMAT "Cod Tributacao ICMS;".
    put UNFORMAT "Aliquota ICMS;".
    put unformat "Aliquota Complem ICMS;".
    PUT UNFORMAT "Base ICMS;".
    put UNFORMAT "Tipo Base ICMS;".
    put UNFORMAT "Estorna ICMS;".
    put UNFORMAT "% Desc ICMS;".
    put UNFORMAT "% Desc Zona Franca;".
    put UNFORMAT "% Reducao ICMS;".
    put UNFORMAT "Destino Reducao;".
    put UNFORMAT "Substituicao Tributaria;".
    put UNFORMAT "% ICMS Subs Trib;".
    put UNFORMAT "Item ICMS Cobrado Subs Tributaria;".
    PUT UNFORMAT "ICMS Outros Valor Subs Tributaria;".
    PUT UNFORMAT "Gerar Credito Subs Tributaria;".
    put UNFORMAT "Diminui Substituicao Total Frete;".
    put UNFORMAT "Comsumidor Final;".
    put UNFORMAT "Item ICMS Suspenso;".
    put UNFORMAT "ICMS Presumido;".
    PUT UNFORMAT "Item ICMS Diferido;".
    PUT UNFORMAT "NÆo Tributada (ICMS);".
    PUT UNFORMAT "Contrib Substituido Antecip;".
    PUT UNFORMAT "Cred Subst Trib Antecip;".
    PUT UNFORMAT "ICMS Subs Trib Antecip;".
    /* Outros */
    PUT UNFORMAT "Retem INSS na Fonte;".
    PUT UNFORMAT "% INSS;".
    PUT UNFORMAT "% SAT;".
    PUT UNFORMAT "% SENAR;".
    PUT UNFORMAT "Tributacao II;".
    PUT UNFORMAT "Suspensao Imposto Importacao;".
    PUT UNFORMAT "ICMS ST a Repassar / Deduzir;".
    PUT UNFORMAT "ICMS ST a Complementar;".
    PUT UNFORMAT "% Interno PIS;".
    PUT UNFORMAT "% Externo PIS;".
    PUT UNFORMAT "% At‚ 31/10/2002;".
    PUT UNFORMAT "Tributacao PIS;".
    PUT UNFORMAT "% Retencao PIS;".
    PUT UNFORMAT "% Desc ZFM PIS;".
    PUT UNFORMAT "% Interno COFINS;".
    PUT UNFORMAT "% Externo COFINS;".
    PUT UNFORMAT "% Anterior;".
    PUT UNFORMAT "Tributacao COFINS;".
    PUT UNFORMAT "% Retencao COFINS;".
    PUT UNFORMAT "% Desc ZFM COFINS;".
    PUT UNFORMAT "% Retencao CSLL;".
    PUT UNFORMAT "Inclui IPI na Base Contrib Sociais;".
    PUT UNFORMAT "Inclui IPI Outras na Base Contrib Sociais;".
    PUT UNFORMAT "Inclui IPI na Base Contrib Sociais Retido;".
    PUT UNFORMAT "Inclui IPI Outras Base Contrib Soc Retido;".
    PUT UNFORMAT "Inclui ICMS-ST na Base Contrib Soc Reitdo;".
    PUT UNFORMAT "Inclui ICMS-ST na Base do IR Retido;".
    PUT UNFORMAT "Deduz Desconto ZFM do Pre‡o Venda;".
    PUT UNFORMAT "Inclui Frete na Base Desconto ZFM;".
    /* Importa‡Æo */
    PUT UNFORMAT "Considera ICMS como despesa na Nota Fiscal de Entrada no Faturamento;".
    PUT UNFORMAT "Aliquota total no ICMS Reduzido (ICMS incide sobre propria base);".
    PUT UNFORMAT "ICMS incide base do ICMS;".
    PUT UNFORMAT "ICMS incide total da Nota Fiscal;".
    PUT UNFORMAT "Considera ICMS como despesa na Nota Fiscal de Entrada no Recebimento;".
    PUT UNFORMAT "Considera PIS como despesa na Nota Fiscal de Entrada no Faturamento;".
    PUT UNFORMAT "Considera COFINS como despesa na Nota Fiscal de Entrada no Faturamento;".
    PUT UNFORMAT "Desconsidera II Suspenso;".
    PUT UNFORMAT "Desconsidera IPI Suspenso;".
    PUT UNFORMAT "Desconsidera ICMS Suspenso;".

    PUT UNFORMAT "Dt Ult Movto;".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-emitente C-Win 
PROCEDURE pi-emitente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    OUTPUT TO VALUE (INPUT FRAME {&FRAME-NAME} fi-arq-emitente) NO-CONVERT.

    put unformat "Codigo;".
    put unformat "Nome Abrev;".
    put unformat "Nome;".
    put unformat "Grupo;".
    put unformat "Matriz;".
    put unformat "Micro Regiao;".
    put unformat "Ramo Atividade;".
    put unformat "Data Implantacao;".
    put unformat "Transportador Padrao;".
    put unformat "Linha Produto;".
    put unformat "Emite Etiqueta;".
    put unformat "Vencimento Igual Data Fluxo;".
    put unformat "Extra Fornecedor;".
    put unformat "Vencto Sabado;".
    put unformat "Vencto Domingo;".
    put unformat "Vencto Feriado;".
    put unformat "Emissao Pedido;".
    /* Financ */
    put unformat "Portador;".
    put unformat "Banco;".
    put unformat "Agencia;".
    put unformat "Conta Corrente;".
    put unformat "Taxa Financeira;".
    put unformat "Dias Taxa Financeira;".
    put unformat "Tipo Pagamento;".
    put unformat "Moeda Padrao;".
    put unformat "Tp Desp Padrao;".
    put unformat "Condicao Pagamento;".
    put unformat "Perc Desconto;".
    put unformat "Dias Compensacao;".
    put unformat "Rendimento;".
    put unformat "Rend Tributavel;".
    /* Fiscal */
    put unformat "Natureza;".
    put unformat "CNPJ;".
    put unformat "Inscri‡Æo Municipal;".
    put unformat "Inscri‡Æo Estadual;".
    put unformat "Inscri‡Æo INSS / CEI;".
    put unformat "Optante Credito Presumido Substituicao Tributaria;".
    put unformat "Retem Pagto;".
    put unformat "Contribuinte Substituto Intermediario;".
    put unformat "Tributacao PIS;".
    put unformat "Tributacao COFINS;".
    put unformat "PIS / COFINS por Unidade;".
    put unformat "Controla Vl Max INSS;".
    put unformat "Situacao;".
    put unformat "Fornecedor emite NF-e;".
    /* Endere‡o */    
    put unformat "Endere‡o Completo;".
    put unformat "Endere‡o;".
    put unformat "Endere‡o2;".
    put unformat "Bairro;".
    put unformat "CEP;".
    put unformat "Cidade;".
    put unformat "Estado;".
    put unformat "Pais;".
    put unformat "Caixa Postal;".
    /* Situa‡Æo */    
    put unformat "Sia‡Æo do Fornecedort;".
    put unformat "Vigencia Inicio;".
    put unformat "Vigencia Fim;".

    PUT UNFORMAT "Dt Ult Movto;".

    PUT SKIP(1).

    FOR EACH emitente NO-LOCK:
        RUN pi-acompanhar IN h-acomp (INPUT "Emitente: " + STRING (emitente.cod-emitente)).

        for first dist-emitente fields(extra-fornec mo-fatur idi-sit-fornec dat-vigenc-inicial dat-vigenc-final)
            where dist-emitente.cod-emitente = emitente.cod-emitente no-lock: 
        end.

        FIND LAST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-emitente = emitente.cod-emitente NO-ERROR.
        FIND LAST docum-est   NO-LOCK WHERE docum-est.cod-emitente   = emitente.cod-emitente NO-ERROR.

        /* Fornec */
        put unformat emitente.cod-emitente                                     ";".
        put unformat emitente.nome-abrev                                       ";".
        put unformat emitente.nome-emit                                        ";".
        put unformat emitente.cod-gr-forn                                      ";".
        put unformat emitente.nome-matriz                                      ";".
        put unformat emitente.nome-mic-reg                                     ";".
        put unformat emitente.atividade                                        ";".
        put unformat emitente.data-implant                                     ";".
        put unformat emitente.cod-transp                                       ";".
        put unformat emitente.linha-produt                                     ";".
        put unformat emitente.emite-etiq            format "SIM/NAO"           ";".
        put unformat emitente.vencto-dia-nao-util   format "SIM/NAO"           ";".
        IF AVAILABLE dist-emitente THEN put unformat dist-emitente.extra-fornec     format "SIM/NAO"           ";". ELSE PUT UNFORMAT "NAO;".
        put unformat ENTRY (emitente.ven-sabado,"Prorroga,Antecipa,Mantem")    ";".
        put unformat ENTRY (emitente.ven-domingo,"Prorroga,Antecipa,Mantem")   ";".
        put unformat ENTRY (emitente.ven-feriado,"Prorroga,Antecipa,Mantem")   ";".
        put unformat ENTRY (emitente.emissao-ped,"Formulario,Magnetico,Telex") ";".
        /* Financ */
        put unformat emitente.portador-ap                                      ";".
        put unformat emitente.cod-banco                                        ";".
        put unformat emitente.agencia                                          ";".
        put unformat emitente.conta-corren                                     ";".
        put unformat emitente.taxa-financ                                      ";".
        put unformat emitente.nr-dias-taxa                                     ";".
        IF emitente.tp-pagto = 0 THEN
            PUT ";".
        ELSE
            put unformat ENTRY (emitente.tp-pagto,"DOC,Cr‚dito Conta Corrente,Cheque Administrativo,Cobran‡a em Carteira,Cheque Nominal DOCCr‚dito Conta CorrenteCheque AdministrativoCobran‡a em CarteiraCheque Nominal DOC,Cr‚dito Conta Corrente,Cheque Administrativo,Cobran‡a em Carteira,Cheque Nominal DOC,Cr‚dito Conta Corrente,Cheque Administrativo,Cobran‡a em Carteira,Cheque Nominal,D‚bito em Conta Corrente, CartÆo de Cr‚dito,Agendamento Eletronico") ";".
        IF AVAILABLE dist-emitente THEN put unformat dist-emitente.mo-fatur    ";". ELSE PUT UNFORMAT "0;".
        put unformat emitente.tp-desp-padrao                                   ";".
        put unformat emitente.cod-cond-pag                                     ";".
        put unformat emitente.bonificacao                                      ";".
        put unformat emitente.dias-comp                                        ";".
        put unformat emitente.ind-rendiment         format "SIM/NAO"           ";".
        put unformat emitente.rend-tribut                                      ";".
        /* Fiscal */
        put unformat {adinc/i03ad098.i 04 emitente.natureza}                   ";".
        put unformat emitente.cgc                                              ";".
        put unformat emitente.ins-municipal                                    ";".
        put unformat emitente.ins-estadual                                     ";".
        put unformat emitente.cod-inscr-inss                                   ";".
        if emitente.int-1 = 0 THEN PUT "NAO;". ELSE PUT "SIM;".
        put unformat ";". /*'Retem Pagto'*/
        put unformat emitente.log-contribt-subst-interm  FORMAT "SIM/NAO"      ";".
        if emitente.idi-tributac-pis = 1 THEN PUT UNFORMAT 'Tributado;'. ELSE PUT 'Isento;'.
        if emitente.idi-tributac-cofins = 1 THEN PUT UNFORMAT 'Tributado;'. ELSE PUT 'Isento;'.
        put unformat emitente.log-calcula-pis-cofins-unid FORMAT "SIM/NAO"     ";".
        put unformat emitente.log-controla-val-max-inss FORMAT "SIM/NAO"       ";".
        put unformat ENTRY (emitente.flag-pag,"OK,Suspenso para pagamentos")   ";".
        IF &IF  "{&bf_mat_versao_ems}"  >=  "2.07"  &THEN
              emitente.log-possui-nf-eletro 
           &ELSE SUBSTR(emitente.char-1,99,1) = "1" &ENDIF THEN PUT "SIM;". ELSE PUT "NAO;".
        /* Endere‡o */    
        put unformat replace (emitente.endereco_text,";"," ")                  ";".
        put unformat replace (emitente.endereco     ,";"," ")                  ";".
        put unformat replace (emitente.endereco2    ,";"," ")                  ";".
        put unformat replace (emitente.bairro       ,";"," ")                  ";".
        put unformat emitente.cep                                              ";".
        put unformat emitente.cidade                                           ";".
        put unformat emitente.estado                                           ";".
        put unformat emitente.pais                                             ";".
        put unformat emitente.caixa-postal                                     ";".
        /* Situa‡Æo */    
        IF AVAILABLE dist-emitente THEN DO:
            put unformat ENTRY (dist-emitente.idi-sit-fornec,"Ativo,Restri‡Æo Compras,Restri‡Æo Compras e Recebimento,Inativo") ";".
            put unformat dist-emitente.dat-vigenc-inicial                      ";".
            put unformat dist-emitente.dat-vigenc-final                        ";".
        END.
        ELSE DO:
            PUT ";".
            PUT ";".
            PUT ";".
        END.

        ASSIGN dt-movto = ?.

        IF AVAILABLE nota-fiscal THEN
            ASSIGN dt-movto = nota-fiscal.dt-emis-nota.

        IF AVAILABLE docum-est AND dt-movto < docum-est.dt-emissao THEN
            ASSIGN dt-movto = docum-est.dt-emissao.

        PUT UNFORMAT dt-movto ";".

        PUT SKIP.
    END.
    
    OUTPUT CLOSE.

    OS-COMMAND NO-WAIT VALUE (INPUT FRAME {&FRAME-NAME} fi-arq-emitente) NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-nat-operacao C-Win 
PROCEDURE pi-nat-operacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i-ind-entreg-futur      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-oper-entrega-fatur    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE h-boin245               AS HANDLE      NO-UNDO.
    DEFINE VARIABLE v-ind-entfut            like natur-oper.ind-entfut  no-undo.
    DEFINE VARIABLE v-ind-est-qtd           like natur-oper.ind-est-qtd no-undo.

    RUN inbo/boin245.p PERSISTENT SET h-boin245.

    OUTPUT TO VALUE (INPUT FRAME {&FRAME-NAME} fi-arq-nat-oper) NO-CONVERT.

    RUN pi-cab-nat-oper.

    PUT SKIP(1).

    FOR EACH natur-oper NO-LOCK:

        RUN pi-acompanhar IN h-acomp (INPUT "Natureza: " + natur-oper.nat-operacao).

        FIND FIRST ri-nat-operacao NO-LOCK
             WHERE ri-nat-operacao.nat-operacao = natur-oper.nat-operacao NO-ERROR.

        FIND LAST nota-fiscal NO-LOCK WHERE nota-fiscal.nat-operacao = natur-oper.nat-operacao NO-ERROR.
        FIND LAST docum-est   NO-LOCK WHERE docum-est.nat-operacao   = natur-oper.nat-operacao NO-ERROR.

        assign i-ind-entreg-futur = if  substr(natur-oper.char-1,1,5) = "1    ":U
                                        then 1
                                        else 2.
        ASSIGN v-ind-entfut  = natur-oper.ind-entfut
               v-ind-est-qtd = natur-oper.ind-est-qtd.
        run convertOperationType in h-boin245 (input 2,
                                               input-output i-oper-entrega-fatur,
                                               input-output i-ind-entreg-futur,
                                               input-output v-ind-entfut,
                                               input-output v-ind-est-qtd).

        PUT UNFORMAT natur-oper.nat-operacao                                ";".
        PUT UNFORMAT natur-oper.cod-cfop                                    ";".
        put unformat natur-oper.denominacao                                 ";".
        put UNFORMAT ENTRY (natur-oper.tipo,"Entrada,Sa¡da,Servi‡o")        ";".
        put unformat natur-oper.especie-doc                                 ";".
        put unformat ENTRY (natur-oper.mercado,"Interno,Externo,Diversos")  ";".
        put unformat natur-oper.cod-canal-venda                             ";".
        put unformat natur-oper.cod-mensagem                                ";".
        put unformat natur-oper.cd-situacao                                 ";".
        put unformat natur-oper.modelo-cupom                                ";".
        put unformat natur-oper.modelo-docto                                ";".
        put unformat natur-oper.cod-esp                                     ";".
        put unformat natur-oper.tp-rec-desp                                 ";".
        /* Adicionais */
        put unformat natur-oper.transf FORMAT "SIM/NAO"                     ";".
        put unformat natur-oper.nat-comp                                    ";".
        put unformat natur-oper.imp-nota FORMAT "SIM/NAO"                   ";".
        put unformat natur-oper.nota-rateio FORMAT "SIM/NAO"                ";".
        put unformat (IF natur-oper.ind-nota-propria = 0 THEN "NAO" ELSE "SIM") ";".
        put unformat natur-oper.log-2 FORMAT "SIM/NAO"                      ";".
        put unformat natur-oper.venda-ativo FORMAT "SIM/NAO"                ";".
        IF AVAILABLE ri-nat-operacao THEN DO:
            put unformat ri-nat-operacao.ind-gera-ficha-auto FORMAT "SIM/NAO" ";".
            put unformat ri-nat-operacao.ind-ini-cred-auto FORMAT "SIM/NAO"   ";".
        END.
        ELSE DO:
            put unformat "NAO;".
            put unformat "NAO;".
        END.
        put unformat (IF natur-oper.ind-venda-ambulante = 0 THEN "NAO" ELSE "SIM") ";".
        put unformat natur-oper.ind-devolucao-valor FORMAT "SIM/NAO"        ";".
        put unformat natur-oper.log-oper-triang FORMAT "SIM/NAO"            ";".
        put unformat natur-oper.log-natur-operac-draw FORMAT "SIM/NAO"      ";".
        put unformat natur-oper.log-memorando FORMAT "SIM/NAO"              ";".
        put unformat natur-oper.log-natureza-bonif FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.terceiros          FORMAT "SIM/NAO"         ";".
        put unformat ENTRY (natur-oper.tp-oper-terc,"Remessa Beneficiamento,Retorno Beneficiamento,Remessa Consigna‡Æo,Faturamento Consigna‡Æo,Devolu‡Æo Consigna‡Æo,Reajuste Pre‡o,Drawback") ";".
        put unformat natur-oper.alt-vl-it-terc     FORMAT "SIM/NAO"         ";".
        put unformat ENTRY (natur-oper.tipo-compra,"Normal,Frete,Devolu‡Æo Cliente,Material Agregado,Servi‡os Comunica‡äes,Energia El‚trica") ";".
        put unformat natur-oper.cod-natur-oper-bonif                        ";" .
        /* Atualiza‡äes */
        put unformat natur-oper.calc-auto          FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.log-impres-autom   FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.baixa-estoq        FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.auto-ce            FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.emite-duplic       FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.auto-cr            FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.ind-gera-of        FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.log-atual-cotas    FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.credito-ciap       FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.log-acum-ciap      FORMAT "SIM/NAO"         ";".
        put unformat /* natur-oper.tg-envio-xml-nfe-manual FORMAT "SIM/NAO" */ ";".
        put unformat /* natur-oper.tg-nfe-estorno          FORMAT "SIM/NAO" */ ";".
        put unformat natur-oper.INd-contabilizacao FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.auto-ct            FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.atual-estat        FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.auto-est           FORMAT "SIM/NAO"         ";".
        put unformat ENTRY (i-oper-entrega-fatur,"Nenhum,Faturamento Antecipado,Remessa de Faturamento Antecipado,Faturamento com Entrega Futura,Remessa de Entrega Futura") ";".
        /* Impostos */
        if substr(natur-oper.char-2,11,1) <> "" then 
            PUT UNFORMAT {varinc/var00074.i 04 int(substr(natur-oper.char-2,11,5))} ";".
        ELSE 
            PUT UNFORMAT {varinc/var00074.i 04 1} ";".
        IF INTEGER (SUBSTRING (natur-oper.char-2,6,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.cd-vinc-ipi                                 ";".
        put UNFORMAT ENTRY (natur-oper.cd-trib-ipi,"Tributado,Isento,Outros,Reduzido") ";".
        put UNFORMAT natur-oper.perc-red-ipi                                ";".

        &if '{&bf_dis_versao_ems}' >= '2.08' &then
            IF natur-oper.log-impr-ipi-danfe THEN
        &else
            IF SUBSTRING(natur-oper.char-1,151,1) = "S":U THEN
        &endif
            PUT UNFORMAT "SIM;".
        ELSE 
            PUT UNFORMAT "NAO;".
        IF INTEGER (SUBSTRING (natur-oper.char-2,1,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE 
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.ipi-icm-out        FORMAT "SIM/NAO"         ";".
        IF INTEGER (SUBSTRING (natur-oper.char-2,16,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        IF INTEGER (SUBSTRING (natur-oper.char-2,46,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.usa-pick           FORMAT "SIM/NAO"         ";".
        put unformat natur-oper.manut-ipi          FORMAT "SIM/NAO"         ";".

        &if '{&bf_dis_versao_ems}' >= '2.09' &then
            if natur-oper.log-ipi-imune THEN
        &else
            if SUBSTRING(natur-oper.char-1,152,1) = "S":U THEN
        &endif
            PUT UNFORMAT "SIM;".
        ELSE 
            PUT UNFORMAT "NAO;".

        &if '{&bf_dis_versao_ems}' >= '2.08' &then
            IF natur-oper.log-ipi-nao-tributad THEN
        &else
            IF SUBSTRING(natur-oper.char-1,153,1) = "S":U THEN
        &endif
            PUT UNFORMAT "SIM;".
        ELSE 
            PUT UNFORMAT "NAO;".
        PUT UNFORMAT natur-oper.log-suspens-ipi-import FORMAT "SIM/NAO"     ";".

        if  substr(natur-oper.char-2,21,5) <> "" then 
            PUT UNFORMAT {varinc/var00074.i 04 int(substr(natur-oper.char-2,21,5))} ";".
        else 
            PUT UNFORMAT {varinc/var00074.i 04 1}                           ";".
        PUT UNFORMAT {ininc/i11in172.i 04 natur-oper.cd-trib-iss}           ";".
        put UNFORMAT natur-oper.perc-red-iss                                ";".

        &if '{&bf_dis_versao_ems}' >= '2.08' &then
             IF natur-oper.log-consid-icms-outras THEN
        &else
             IF SUBSTRING(natur-oper.char-1,148,1) = "S":U THEN
        &endif
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.nat-vinculada                               ";".
        put UNFORMAT natur-oper.ind-imprenda       FORMAT "SIM/NAO"         ";".
        PUT UNFORMAT natur-oper.val-perc-impto-renda                        ";".
        /* ICMS */
        put UNFORMAT {ininc/i01in245.i 04 natur-oper.cd-trib-icm}           ";".
        put UNFORMAT natur-oper.aliquota-icm                                ";".
        put unformat natur-oper.aliq-icm-com                                ";".
        PUT UNFORMAT {ininc/i05in245.i 04 natur-oper.merc-base-icms}        ";".
        put UNFORMAT natur-oper.tp-base-icm                                 ";".
        put UNFORMAT natur-oper.manut-icm          FORMAT "SIM/NAO"         ";".        
        put UNFORMAT natur-oper.per-des-icms                                ";".
        put UNFORMAT SUBSTRING (natur-oper.char-2,66,5)                     ";".
        put UNFORMAT natur-oper.perc-red-icm                                ";".
        if natur-oper.dec-2 = 1 then 
            put UNFORMAT {varinc/var00071.i 04 1}                           ";".
        else 
            put UNFORMAT {varinc/var00071.i 04 2}                           ";".
        put UNFORMAT natur-oper.subs-trib           FORMAT "SIM/NAO"        ";".
        put UNFORMAT natur-oper.icms-subs-trib                              ";".
        put UNFORMAT natur-oper.ind-it-icms         FORMAT "SIM/NAO"        ";".
        IF INTEGER (SUBSTRING (natur-oper.char-2,51,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        IF INTEGER (SUBSTRING (natur-oper.char-2,56,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.usa-nota-ent        FORMAT "SIM/NAO"        ";".
        put UNFORMAT natur-oper.consum-final        FORMAT "SIM/NAO"        ";".
        IF natur-oper.ind-it-sub-dif = YES THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        put UNFORMAT natur-oper.log-icms-presmdo    FORMAT "SIM/NAO"        ";".
        IF natur-oper.ind-it-sub-dif = ? THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        PUT UNFORMAT natur-oper.ind-tipo-vat        FORMAT "SIM/NAO"        ";".

        &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN
            PUT UNFORMAT (IF natur-oper.log-contrib-st-antec THEN "SIM;" ELSE "NAO;").
            PUT UNFORMAT (IF natur-oper.log-cr-st-antec      THEN "SIM;" ELSE "NAO;").
        &else
            &IF '{&bf_dis_versao_ems}' >= '2.04' &THEN
                PUT UNFORMAT (IF SUBSTRING(natur-oper.char-1,149,1) = "1" THEN "SIM;" ELSE "NAO;").
                PUT UNFORMAT (IF SUBSTRING(natur-oper.char-1,150,1) = "1" THEN "SIM;" ELSE "NAO;").
            &ENDIF
        &ENDIF

        &IF "{&bf_dis_versao_ems}":U >= "2.04":U AND "{&bf_dis_versao_ems}":U < "2.08":U &THEN
            IF SUBSTRING(natur-oper.char-1,147,1) = "1" THEN
        &ENDIF
        &IF "{&bf_dis_versao_ems}":U >= "2.08":U &THEN
            IF natur-oper.log-icms-substto-antecip THEN
        &ENDIF
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        /* Outros */
        IF INTEGER (SUBSTRING (natur-oper.char-2,71,5)) = 1 THEN
            PUT UNFORMAT "SIM;".
        ELSE
            PUT UNFORMAT "NAO;".
        PUT UNFORMAT string(dec(substr(natur-oper.char-1,40,5)))            ";".
        PUT UNFORMAT STRING(natur-oper.val-perc-sat,">9.99":U)              ";".
        PUT UNFORMAT STRING(natur-oper.val-perc-senar,">9.99":U)            ";".
        IF integer(substring(natur-oper.char-1,10,2)) <> 0 THEN
            PUT UNFORMAT {ininc/i10in172.i 04 integer(substring(natur-oper.char-1,10,2))} ";".
        ELSE
            PUT UNFORMAT ";".
        PUT UNFORMAT natur-oper.log-suspens-impto-import    FORMAT "SIM/NAO" ";".
        PUT UNFORMAT natur-oper.log-icms-substto-repas      FORMAT "SIM/NAO" ";".
        PUT UNFORMAT natur-oper.log-icms-substto-compltar   FORMAT "SIM/NAO" ";".
        PUT UNFORMAT natur-oper.perc-pis[1]                                  ";".
        PUT UNFORMAT natur-oper.perc-pis[2]                                  ";".
        PUT UNFORMAT STRING(DEC(SUBSTR(natur-oper.char-1,76,5)),">9.99":U)   ";".
        PUT UNFORMAT {ininc/i11in172.i 04 int(substr(natur-oper.char-1,86,1))} ";".
        PUT UNFORMAT string(natur-oper.cdd-perc-retenc-pis,">9.99":U )       ";".
        PUT UNFORMAT STRING(natur-oper.val-perc-desc-pis-zfm,">9.99999":U )  ";".
        PUT UNFORMAT natur-oper.per-fin-soc[1]                               ";".
        PUT UNFORMAT natur-oper.per-fin-soc[2]                               ";".
        PUT UNFORMAT STRING(DEC(SUBSTR(natur-oper.char-1,81,5)),">9.99":U)   ";".
        PUT UNFORMAT {ininc/i11in172.i 04 int(substr(natur-oper.char-1,87,1))} ";".
        PUT UNFORMAT string(natur-oper.cdd-perc-retenc-cofins,">9.99":U)     ";".
        PUT UNFORMAT STRING(natur-oper.val-perc-desc-cofins-zfm,">9.99999":U) ";".
        PUT UNFORMAT string(natur-oper.cdd-perc-retenc-csll,">9.99":U)       ";".
        PUT UNFORMAT natur-oper.log-ipi-contrib-social FORMAT "SIM/NAO"      ";".
        PUT UNFORMAT natur-oper.log-ipi-outras-contrib-social FORMAT "SIM/NAO" ";".
        PUT UNFORMAT natur-oper.log-ipi-contrib-retid         FORMAT "SIM/NAO" ";".
        PUT UNFORMAT natur-oper.log-ipi-outras-contrib-retid  FORMAT "SIM/NAO" ";".

        &if '{&bf_dis_versao_ems}' >= '2.04' AND '{&bf_dis_versao_ems}' < '2.07' &THEN
            IF SUBSTR(natur-oper.char-1,138,1) = "1" THEN PUT UNFORMAT "SIM;". ELSE PUT UNFORMAT "NAO;".
            IF SUBSTR(natur-oper.char-1,139,1) = "1" THEN PUT UNFORMAT "SIM;". ELSE PUT UNFORMAT "NAO;".
        &endif
        &if '{&bf_dis_versao_ems}' >= '2.07' &THEN
            IF natur-oper.log-icms-substto-base-contrib THEN PUT UNFORMAT "SIM;". ELSE PUT UNFORMAT "NAO;".
            IF natur-oper.log-icms-substto-base-irf-retid THEN PUT UNFORMAT "SIM;". ELSE PUT UNFORMAT "NAO;".
        &endif

        PUT UNFORMAT natur-oper.log-deduz-desc-zfm-tot-nf     FORMAT "SIM/NAO" ";".
        PUT UNFORMAT ";".
        /* Importa‡Æo */

        &IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
        if  natur-oper.log-icms-despes-import        then put unformatted "SIM;". else put unformatted "NAO;".
        PUT UNFORMAT /* tgConsideraAliqTotICMS */ ";".
        if  natur-oper.log-integr-base-calc-icms-nfr then put unformatted "SIM;". else put unformatted "NAO;".
        if  natur-oper.log-incid-icms-tot-nfr        then put unformatted "SIM;". else put unformatted "NAO;".
        if  natur-oper.log-icms-despes-import-nfr    then put unformatted "SIM;". else put unformatted "NAO;".
        if  natur-oper.log-pis-despes-import         then put unformatted "SIM;". else put unformatted "NAO;".
        if  natur-oper.log-cofins-despes-import      then put unformatted "SIM;". else put unformatted "NAO;".
        &ELSE
        if  SUBSTR(natur-oper.char-2,141,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        PUT UNFORMAT /* tgConsideraAliqTotICMS */ ";".
        if  SUBSTR(natur-oper.char-2,144,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        if  SUBSTR(natur-oper.char-2,145,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        if  SUBSTR(natur-oper.char-2,146,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        if  SUBSTR(natur-oper.char-2,142,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        if  SUBSTR(natur-oper.char-2,143,1) = "1" then put unformatted "SIM;". else put unformatted "NAO;".
        &ENDIF

        PUT UNFORMAT /* tgdesconsideraIISuspenso   */ ";".
        PUT UNFORMAT /* tgDesconsideraIPISuspenso  */ ";".
        PUT UNFORMAT /* tgDesconsideraICMSSuspenso */ ";".

        IF AVAILABLE nota-fiscal THEN
            ASSIGN dt-movto = nota-fiscal.dt-emis-nota.

        IF AVAILABLE docum-est THEN
            ASSIGN dt-movto = docum-est.dt-emissao.

        PUT UNFORMAT STRING (dt-movto,"99/99/9999") ";".

        PUT SKIP.

    END.

    OUTPUT CLOSE.

    OS-COMMAND NO-WAIT VALUE (INPUT FRAME {&FRAME-NAME} fi-arq-nat-oper) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_executar C-Win 
PROCEDURE pi_executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Inicializando").

do on error undo, return error on stop  undo, return error:

    IF INPUT FRAME {&FRAME-NAME} tg-natur-oper = YES THEN DO:
        RUN pi-acompanhar IN h-acomp (INPUT "Processando Natureza de Opera‡Æo").

        RUN pi-nat-operacao.
    END.

    IF INPUT FRAME {&FRAME-NAME} tg-emitente = YES THEN DO:
        RUN pi-acompanhar IN h-acomp (INPUT "Processando Emitente").

        RUN pi-emitente.
    END.

end.

run pi-finalizar in h-acomp. 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    assign fi-arq-nat-oper:screen-value in frame {&frame-name} = session:temp-directory + "nat-oper.csv".
    assign fi-arq-emitente:screen-value in frame {&frame-name} = session:temp-directory + "emitente.csv".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

