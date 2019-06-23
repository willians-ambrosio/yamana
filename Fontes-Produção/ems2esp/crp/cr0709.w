&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-concom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-concom 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i CR0709 2.00.00.044}  /*** 010044 ***/

/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters: 
      <none>

  Output Parameters: 
      <none>

  Version: 1.00.00

  History: 

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
&glob version 1.00.00
/* miniflexibilizaá∆o */
{include/i_dbvers.i}

{cdp/cdcfgcex.i}

def new global shared var gr-titulo     as rowid   no-undo.
def new global shared var i-emp-selecao as char    no-undo.
def var c-versao                        as char    no-undo.
def var dilo as char.
def var v-row-titulo as rowid no-undo.
DEF VAR h-bocx185 AS HANDLE NO-UNDO. 
DEF VAR c-return  AS CHAR NO-UNDO.
def new global shared var gr-processo-exp as rowid no-undo.
def var v-pagar   as char no-undo.
def var v-receber as char no-undo.
DEF VAR h-boad355 AS HANDLE NO-UNDO.
def var de-vl-pis       as dec format ">>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-cofins    as dec format ">>>,>>>,>>>,>>9.99" no-undo.
def var de-vl-csll      as dec format ">>>,>>>,>>>,>>9.99" no-undo.
def var l-ret-impto     as log                             no-undo.  
def var l-ret-impto-lq  as log                             no-undo.  
def var de-vl-base-calc as dec format ">>>,>>>,>>>,>>9.99" no-undo.
def new global shared var l-chamado-por-espc as log init no no-undo.

&if  "{&mgadm_version}" >= "2.04" &then
    DEFINE TEMP-TABLE tt-processo-exp NO-UNDO LIKE processo-exp
           field r-rowid as rowid.
&endif

def buffer b-titulo  for titulo.
def buffer b-mov-tit for mov-tit.

def new global shared var da-cr0709-emis-ini as date        no-undo.
def new global shared var da-cr0709-emis-fim as date        no-undo.

{crp/cr0709.i}

if l-chamado-por-espc and l-mostra-faixa-titulo then
   assign l-mostra-faixa-titulo = no.  

if l-mostra-faixa-titulo then
   assign da-cr0709-emis-fim = today
          da-cr0709-emis-ini = da-cr0709-emis-fim - 30.
else
   assign da-cr0709-emis-ini = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/1001 &ENDIF  
          da-cr0709-emis-fim = 12/31/9999.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 bt-empresa bt-mov-dif bt-processo ~
bt-vl-retencao bt-faixa rt-button 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-concom AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM m_Chave_Alternativa LABEL "Chave &Alternativa" ACCELERATOR "CTRL-A"
       MENU-ITEM m_Empresa      LABEL "&Empresa"       ACCELERATOR "CTRL-E"
       RULE
       MENU-ITEM mi-Consultas   LABEL "C&onsulta"      ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "Impress∆o"      ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b01ad136 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b01ad228 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b01ad289 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b02ad183 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b05ad183 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b20ad183 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q09ad264 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v02ad264 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v10ad264 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v15ad264 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v18ad264 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-empresa 
     IMAGE-UP FILE "image~\emsinm":U
     LABEL "Empresa" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-faixa 
     IMAGE-UP FILE "image~\im-ran":U
     LABEL "&Selecionar" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-mov-dif 
     IMAGE-UP FILE "image~\ii-cla":U
     LABEL "Empresa" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-processo 
     IMAGE-UP FILE "image/im-notah.bmp":U
     LABEL "Processo de Exportaá∆o" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-vl-retencao 
     IMAGE-UP FILE "image/dolar.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-cotac.bmp":U
     LABEL "Valor Retená∆o" 
     SIZE 4 BY 1.25.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image~\im-segu2":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 86.43 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BUTTON-1 AT ROW 1.33 COL 26.29 HELP
          "Chave Alternativa"
     bt-empresa AT ROW 1.33 COL 30.43 HELP
          "Seleciona a empresa dos t°tulos"
     bt-mov-dif AT ROW 1.33 COL 34.57 HELP
          "Seleciona os Movimentos de Diferenáa do T°tulo."
     bt-processo AT ROW 1.33 COL 38.86 HELP
          "Informaá‰es do Processo de Exportaá∆o."
     bt-vl-retencao AT ROW 1.33 COL 43.57
     bt-faixa AT ROW 1.33 COL 48 HELP
          "Selecionar"
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-concom ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta T°tulo"
         HEIGHT             = 17.21
         WIDTH              = 87.43
         MAX-HEIGHT         = 26.25
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.25
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-concom 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-concom.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-concom
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-concom)
THEN w-concom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-concom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-concom w-concom
ON END-ERROR OF w-concom /* Consulta T°tulo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  /* IF THIS-PROCEDURE:PERSISTENT THEN  */ 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-concom w-concom
ON WINDOW-CLOSE OF w-concom /* Consulta T°tulo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-empresa w-concom
ON CHOOSE OF bt-empresa IN FRAME f-cad /* Empresa */
DO:

       run cdp/cd0726.w (input-output i-emp-selecao).

       find first b-titulo no-lock
            where b-titulo.ep-codigo = i-emp-selecao no-error.

       if  not avail b-titulo then do:
           run utp/ut-msgs.p (input "show",
                              input 8499,
                              input string(i-emp-selecao)).
       end.
       else do:
           run pi-reposiciona in h_q09ad264 (input rowid(b-titulo)).
       end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-faixa w-concom
ON CHOOSE OF bt-faixa IN FRAME f-cad /* Selecionar */
DO:
    run crp/cr0709l.w.

    RUN dispatch IN h_q09ad264 ( INPUT 'open-query':U ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mov-dif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mov-dif w-concom
ON CHOOSE OF bt-mov-dif IN FRAME f-cad /* Empresa */
DO: 
    run pi-retorna-titulo in h_v15ad264 (output gr-titulo).
    if  gr-titulo <> ? then
        run crp/cr0709g.w.
    else
        run utp/ut-msgs.p (input "show",
                            input 7146,
                            input "").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processo w-concom
ON CHOOSE OF bt-processo IN FRAME f-cad /* Processo de Exportaá∆o */
DO:
  &if  "{&mgadm_version}" >= "2.04" &then

    assign c-return = "".
    run pi-retorna-titulo in h_v15ad264 (output gr-titulo).
    if  gr-titulo <> ? THEN DO:

       FIND titulo no-lock 
            WHERE rowid(titulo) = gr-titulo NO-ERROR. 
       IF AVAIL titulo THEN DO:

          if titulo.nr-proc-exp = "" then

             RETURN NO-APPLY.
          else DO:
              run cxbo/bocx185.p persistent set h-bocx185.
              run findPk_proc_exp in h-bocx185 (input  titulo.cod-estabel,
                                                input  titulo.nr-proc-exp,
                                                output c-return).
              if c-return = "" then do:
                 run getcurrent in h-bocx185 (output table tt-processo-exp).

                 find first tt-processo-exp no-lock no-error.
                 if avail tt-processo-exp then do:
                    assign gr-processo-exp = tt-processo-exp.r-rowid.
                    &IF '{&bf_cex_versao_ems}' >= '2.062' &THEN
                        RUN exp/ex3101.w.
                    &ELSE
                        RUN exp/ex1185.w.
                    &ENDIF
                 end.
              end.
              else do:
                 RUN utp/ut-msgs.p (INPUT "show":U, INPUT 26109, INPUT "").

                 RETURN "NOK":U.
              end.
          end.
       END.
    END.
  &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vl-retencao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vl-retencao w-concom
ON CHOOSE OF bt-vl-retencao IN FRAME f-cad /* Valor Retená∆o */
DO:
    ASSIGN l-ret-impto-lq = NO.

    if avail b-titulo then do:

        FIND FIRST mov-tit NO-LOCK
             WHERE mov-tit.ep-codigo   = b-titulo.ep-codigo
               AND mov-tit.cod-estabel = b-titulo.cod-estabel
               AND mov-tit.cod-esp     = b-titulo.cod-esp
               AND mov-tit.serie       = b-titulo.serie
               AND mov-tit.nr-docto    = b-titulo.nr-docto
               AND mov-tit.parcela     = b-titulo.parcela
               AND mov-tit.transacao   = 2 NO-ERROR.

        &if "{&mgadm_version}":U < "2.06b":U &then
            assign de-vl-pis    = dec(substr(b-titulo.char-2,1,15))  
                   de-vl-cofins = dec(substr(b-titulo.char-2,16,15)) 
                   de-vl-csll   = dec(substr(b-titulo.char-2,31,15))
                   l-ret-impto  = IF substr(b-titulo.char-2,46,01) = "S" THEN YES ELSE NO
                   de-vl-base-calc = dec(substr(b-titulo.char-2,47,15)).
            IF  AVAIL mov-tit THEN
                ASSIGN l-ret-impto-lq = IF NUM-ENTRIES(mov-tit.char-1,';') > 11 AND ENTRY(12,mov-tit.char-1,';') = 'S' THEN YES ELSE NO NO-ERROR.
            ELSE DO:
                FOR EACH mov-tit NO-LOCK
                   WHERE mov-tit.ep-codigo   = b-titulo.ep-codigo
                     AND mov-tit.cod-estabel = b-titulo.cod-estabel
                     AND mov-tit.cod-esp     = b-titulo.cod-esp
                     AND mov-tit.serie       = b-titulo.serie
                     AND mov-tit.nr-docto    = b-titulo.nr-docto
                     AND mov-tit.parcela     = b-titulo.parcela
                     AND mov-tit.transacao   = 13:
                    IF  NUM-ENTRIES(mov-tit.char-1,';') > 11 AND ENTRY(12,mov-tit.char-1,';') = 'S' THEN DO:
                        ASSIGN l-ret-impto-lq = YES.
                        LEAVE.
                    END.
                    ELSE DO:
                        IF  CAN-FIND(FIRST b-mov-tit NO-LOCK
                                     WHERE b-mov-tit.num-id-mov-tit = mov-tit.num-id-mov-tit-origem
                                       AND b-mov-tit.transacao      = 2) THEN DO:
                            ASSIGN l-ret-impto-lq = YES.
                            LEAVE.
                        END.
                    END.
                END.
            END.
        &else
            assign de-vl-pis    = b-titulo.vl-pis    
                   de-vl-cofins = b-titulo.vl-cofins 
                   de-vl-csll   = b-titulo.vl-csll
                   l-ret-impto  = b-titulo.l-ret-impto
                   de-vl-base-calc = b-titulo.vl-base-calc-ret.
            IF  AVAIL mov-tit THEN
                ASSIGN l-ret-impto-lq = mov-tit.l-ret-impto.
            ELSE DO:
                FOR EACH mov-tit NO-LOCK
                   WHERE mov-tit.ep-codigo   = b-titulo.ep-codigo
                     AND mov-tit.cod-estabel = b-titulo.cod-estabel
                     AND mov-tit.cod-esp     = b-titulo.cod-esp
                     AND mov-tit.serie       = b-titulo.serie
                     AND mov-tit.nr-docto    = b-titulo.nr-docto
                     AND mov-tit.parcela     = b-titulo.parcela
                     AND mov-tit.transacao   = 13:
                    IF  mov-tit.l-ret-impto THEN DO:
                        ASSIGN l-ret-impto-lq = YES.
                        LEAVE.
                    END.
                    ELSE DO:
                        IF  CAN-FIND(FIRST b-mov-tit
                                     WHERE b-mov-tit.num-id-mov-tit = mov-tit.num-id-mov-tit-origem
                                       AND b-mov-tit.transacao      = 2) THEN DO:
                            ASSIGN l-ret-impto-lq = YES.
                            LEAVE.
                        END.
                    END.
                END.
            END.
        &endif
    end.

    run crp/cr0709j.w (input de-vl-pis,   
                       input de-vl-cofins,
                       input de-vl-csll,
                       input l-ret-impto,
                       input l-ret-impto-lq,
                       input de-vl-base-calc).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-concom
ON CHOOSE OF BUTTON-1 IN FRAME f-cad /* Button 1 */
DO:
    {crp/cr5555.i "h_q09ad264"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-concom
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-concom
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:

   run pi-disable-menu.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-Consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-Consultas w-concom
ON CHOOSE OF MENU-ITEM mi-Consultas /* Consulta */
DO:
   RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-concom
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-concom
ON CHOOSE OF MENU-ITEM mi-imprimir /* Impress∆o */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-concom
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-concom
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-concom
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-concom
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-concom
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-concom
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-concom
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Chave_Alternativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Chave_Alternativa w-concom
ON CHOOSE OF MENU-ITEM m_Chave_Alternativa /* Chave Alternativa */
DO:
     {crp/cr5555.i "h_q09ad264"}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Empresa w-concom
ON CHOOSE OF MENU-ITEM m_Empresa /* Empresa */
DO:

       run cdp/cd0726.w (input-output i-emp-selecao).

       find first b-titulo no-lock
            where b-titulo.ep-codigo = i-emp-selecao no-error.

       if  not avail b-titulo then do:
           run utp/ut-msgs.p (input "show",
                              input 8499,
                              input string(i-emp-selecao)).
       end.
       else do:
           run pi-reposiciona in h_q09ad264 (input rowid(b-titulo)).
       end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-concom 


{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-concom  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 71.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v15ad264.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v15ad264 ).
       RUN set-position IN h_v15ad264 ( 2.88 , 1.57 ) NO-ERROR.
       /* Size in UIB:  ( 2.50 , 86.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'T°tulo|Movto.|Hist.|Repres|FASB|CMCAC|Banco|Impto.|Vendor' + ',
                     FOLDER-TAB-TYPE = 3':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 5.50 , 1.86 ) NO-ERROR.
       RUN set-size IN h_folder ( 12.17 , 86.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adqry/q09ad264.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Key-Name = ,
                     ProgPesquisa = adzoom/z06ad264.w,
                     ProgVaPara = adgo/g01ad264.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q09ad264 ).
       RUN set-position IN h_q09ad264 ( 1.25 , 63.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 6.43 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartViewer h_v15ad264. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_v15ad264 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q09ad264. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q09ad264 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q09ad264 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q09ad264 ).
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'State':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v02ad264.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v02ad264 ).
       RUN set-position IN h_v02ad264 ( 7.00 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.04 , 78.00 ) */

       /* Links to SmartViewer h_v02ad264. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_v02ad264 ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'adbrw/b20ad183.w':U ,
           &ELSE
             INPUT c-versao ,
           &ENDIF
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b20ad183 ).
       RUN set-position IN h_b20ad183 ( 7.00 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.33 , 84.57 ) */

       /* Links to SmartBrowser h_b20ad183. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b20ad183 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b01ad136.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b01ad136 ).
       RUN set-position IN h_b01ad136 ( 6.88 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 81.00 ) */

       /* Links to SmartBrowser h_b01ad136. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b01ad136 ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b01ad228.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b01ad228 ).
       RUN set-position IN h_b01ad228 ( 6.92 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 9.92 , 83.00 ) */

       /* Links to SmartBrowser h_b01ad228. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b01ad228 ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b02ad183.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b02ad183 ).
       RUN set-position IN h_b02ad183 ( 7.21 , 3.57 ) NO-ERROR.
       /* Size in UIB:  ( 9.75 , 82.00 ) */

       /* Links to SmartBrowser h_b02ad183. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b02ad183 ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b05ad183.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b05ad183 ).
       RUN set-position IN h_b05ad183 ( 6.92 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.83 , 83.00 ) */

       /* Links to SmartBrowser h_b05ad183. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b05ad183 ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v10ad264.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v10ad264 ).
       RUN set-position IN h_v10ad264 ( 7.29 , 9.86 ) NO-ERROR.
       /* Size in UIB:  ( 9.42 , 67.29 ) */

       /* Links to SmartViewer h_v10ad264. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_v10ad264 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adbrw/b01ad289.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b01ad289 ).
       RUN set-position IN h_b01ad289 ( 7.29 , 4.43 ) NO-ERROR.
       /* Size in UIB:  ( 9.63 , 81.00 ) */

       /* Links to SmartBrowser h_b01ad289. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_b01ad289 ).

    END. /* Page 8 */

    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'advwr/v18ad264.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v18ad264 ).
       RUN set-position IN h_v18ad264 ( 7.17 , 3.86 ) NO-ERROR.
       /* Size in UIB:  ( 9.50 , 68.57 ) */

       /* Links to SmartViewer h_v18ad264. */
       RUN add-link IN adm-broker-hdl ( h_q09ad264 , 'Record':U , h_v18ad264 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 9 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-concom  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-concom  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-concom)
  THEN DELETE WIDGET w-concom.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-concom  _DEFAULT-ENABLE
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
  ENABLE BUTTON-1 bt-empresa bt-mov-dif bt-processo bt-vl-retencao bt-faixa 
         rt-button 
      WITH FRAME f-cad IN WINDOW w-concom.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-concom.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-concom 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  if valid-handle(h-boad355) then
     delete procedure h-boad355.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-concom 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  assign l-chamado-por-espc = no.
  APPLY "CLOSE":U TO THIS-PROCEDURE.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-concom 
PROCEDURE local-initialize :
{utp/ut9000.i "CR0709" "2.00.00.044"}

/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  assign i-emp-selecao = i-ep-codigo-usuario.

  {include/win-size.i}
  run pi-before-initialize.

  find first param-global no-lock no-error.

  &if  "{&mgadm_version}" <= "2.02" &then
    vers_block:
    do:
       &if "{&mgadm_version}" = "2.02" &then
           find first histor_exec_especial no-lock
                where histor_exec_especial.cod_modul_dtsul = 'CRP'
                and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_DESPESAS_CARTORIO' no-error.
           if  avail histor_exec_especial then
               leave vers_block.  
       &endif    
       assign c-versao = "adbrw/b20ad183.w".
    end.  
    vers_block2:
    do:
       &if "{&mgadm_version}" = "2.02" &then
           find first histor_exec_especial no-lock
                where histor_exec_especial.cod_modul_dtsul = 'CRP'
                and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_GERA_AD_BANCO' no-error.
           if  avail histor_exec_especial then
               leave vers_block2.  
       &endif    
       assign bt-mov-dif:sensitive in frame {&FRAME-NAME} = no
              bt-mov-dif:visible   in frame {&FRAME-NAME} = no.
    end.  
    assign bt-processo:visible   in frame {&FRAME-NAME} = no
           bt-processo:sensitive in frame {&FRAME-NAME} = no.
  &endif  

  &if  "{&mgadm_version}" >= "2.02" &then
    vers_block:
    do:
        &if "{&mgadm_version}" = "2.02" &then
          find first histor_exec_especial no-lock
                 where histor_exec_especial.cod_modul_dtsul = 'CRP'
                 and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_DESPESAS_CARTORIO' no-error.
         if  not avail histor_exec_especial then
             leave vers_block.  
        &endif
        assign c-versao = "adbrw/b23ad183.w".
    end.
    vers_block2:
    do:
       &if "{&mgadm_version}" = "2.02" &then
           find first histor_exec_especial no-lock
                where histor_exec_especial.cod_modul_dtsul = 'CRP'
                and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_GERA_AD_BANCO' no-error.
           if  avail histor_exec_especial then
               leave vers_block2.  
       &endif    
       assign bt-mov-dif:visible   in frame {&FRAME-NAME} = yes
              bt-mov-dif:sensitive in frame {&FRAME-NAME} = yes.
    end.

    &if "{&mgadm_version}" >= "2.04" &then

       run adbo/boad355.p persistent set h-boad355.
       run pi-verificar-produto in h-boad355 (output v-pagar,
                                              output v-receber).

       if v-receber = "EMS 2" then do:
          if param-global.modulo-ex = yes then
             assign bt-processo:visible   in frame {&FRAME-NAME} = yes
                    bt-processo:sensitive in frame {&FRAME-NAME} = yes.
          else 
             assign bt-processo:visible   in frame {&FRAME-NAME} = no
                    bt-processo:sensitive in frame {&FRAME-NAME} = no.
       end.
    &endif

    &if "{&mgadm_version}" < "2.04" &then
         assign bt-processo:visible   in frame {&FRAME-NAME} = no
                bt-processo:sensitive in frame {&FRAME-NAME} = no.
    &endif

  &endif

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  if  l-mostra-faixa-titulo = no then
      assign bt-faixa:visible   in frame {&frame-name} = no
             bt-faixa:sensitive in frame {&frame-name} = no.
  else
      assign bt-faixa:sensitive in frame {&frame-name} = yes
             bt-faixa:visible in frame {&frame-name}   = yes.

  if  l-chamado-cons-cli 
  and l-mostra-titulo-cliente 
  and gr-titulo <> ? then do:
      run enable-va-para in h_p-navega(input no).
      run enable-zoom    in h_p-navega(input no).
      assign bt-empresa:sensitive in frame {&frame-name} = no
             BUTTON-1:sensitive in frame {&frame-name} = no.
  end.  

  if  gr-titulo <> ? then
      run pi-reposiciona-query in h_q09ad264 (input gr-titulo).

  run pi-after-initialize.

  RUN init-pages ("9").
    if not ((search("vep/ve2028.p") <> ? or search("vep/ve2028.r") <> ?) and param-global.modulo-05 = yes) then do: 
      RUN dispatch IN h_v18ad264 ('destroy':U).
      RUN delete-folder-page IN h_folder
      (INPUT 9).
    end. 

  if l-mostra-faixa-titulo then
     run utp/ut-msgs.p (input "show",
                        input 30613,
                        input string(da-cr0709-emis-ini) + "~~" +
                              string(da-cr0709-emis-fim)).

  RETURN "OK":U. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available w-concom 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     assign gr-titulo = if avail titulo then rowid(titulo) else ?. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_ems_xref w-concom 
PROCEDURE pi_ems_xref :
/*------------------------------------------------------------------------------
  Purpose:     Referàncias XRef
  Parameters:  <none> 
  Notes:       N∆o altere/elimine o c¢digo abaixo.
------------------------------------------------------------------------------*/
RUN adzoom/z06ad264.w.
RUN adgo/g01ad264.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RTB_xref_generator w-concom 
PROCEDURE RTB_xref_generator :
/* -----------------------------------------------------------
Purpose:    Generate RTB xrefs for SMARTOBJECTS.
Parameters: <none>
Notes:      This code is generated by the UIB.  DO NOT modify it.
            It is included for Roundtable Xref generation. Without
            it, Xrefs for SMARTOBJECTS could not be maintained by
            RTB.  It will in no way affect the operation of this
            program as it never gets executed.
-------------------------------------------------------------*/
  RUN "panel/p-navega.w *RTB-SmObj* ".
  RUN "panel/p-exihel.w *RTB-SmObj* ".
  RUN "advwr/v15ad264.w *RTB-SmObj* ".
  RUN "adm/objects/folder.w *RTB-SmObj* ".
  RUN "adqry/q09ad264.w *RTB-SmObj* ".
  RUN "advwr/v02ad264.w *RTB-SmObj* ".
  RUN "adbrw/b20ad183.w *RTB-SmObj* ".
  RUN "adbrw/b01ad136.w *RTB-SmObj* ".
  RUN "adbrw/b01ad228.w *RTB-SmObj* ".
  RUN "adbrw/b02ad183.w *RTB-SmObj* ".
  RUN "adbrw/b05ad183.w *RTB-SmObj* ".
  RUN "advwr/v10ad264.w *RTB-SmObj* ".
  RUN "adbrw/b01ad289.w *RTB-SmObj* ".
  RUN "advwr/v18ad264.w *RTB-SmObj* ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-concom  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-concom 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  def var v-row-titulo as rowid no-undo.

  run pi-retorna-titulo in h_q09ad264 (output v-row-titulo).
  find b-titulo no-lock 
      where rowid(b-titulo) = v-row-titulo no-error.
  if avail b-titulo then do:
      &if "{&mgadm_version}":U < "2.06":U &then
          if dec(substr(b-titulo.char-2,1,15))  <> 0
          or dec(substr(b-titulo.char-2,16,15)) <> 0
          or dec(substr(b-titulo.char-2,31,15)) <> 0
          or dec(substr(b-titulo.char-2,47,15)) <> 0
          or substr(b-titulo.char-2,46,01)       = "S" then
              assign bt-vl-retencao:sensitive in frame {&frame-name} = yes.
          else
              assign bt-vl-retencao:sensitive in frame {&frame-name} = no.
      &else
          if b-titulo.vl-pis           <> 0
          or b-titulo.vl-cofins        <> 0
          or b-titulo.vl-csll          <> 0
          or b-titulo.vl-base-calc-ret <> 0
          or b-titulo.l-ret-impto       = YES then
              assign bt-vl-retencao:sensitive in frame {&frame-name} = yes.
          else
              assign bt-vl-retencao:sensitive in frame {&frame-name} = no.  
      &endif
  end.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

