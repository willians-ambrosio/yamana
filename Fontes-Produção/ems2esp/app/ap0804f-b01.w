&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          movadm           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804F-B01 2.00.00.021}  /*** 010021 ***/
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/*Tratamento para independˆncia de objetos*/
&glob ORIGINALNAME 'adbrw~\b26ad180.w'

{cdp/cdcfgfin.i}

def temp-table tt-documento like mov-ap
    field c-transacao  like mov-ap.transacao
    field c-lancamento like mov-ap.lancamento
    field c-lanca      as char format "x(01)"
    field c-modalidade as char format "x(15)"
    field i-nr-bordero like mov-ap.nr-bordero
    field c-origem     as char format "x(20)"
    field gr-tt-doc    as rowid
    field de-vl-juros-neg as decimal format "->,>>>,>>9.99"
    field de-vl-juros-me-neg as decimal format "->,>>>,>>9.99"
    FIELD i-sequencia  AS INTEGER FORMAT ">>>>>>>>>9" LABEL "Sequˆncia"
    field d-data-1     as date format "99/99/9999" label "Vencto Cheq".

def new global shared var gr-mov-ap as rowid no-undo.
def new global shared var gr-tit-ap as rowid no-undo.

def var c-liter1    as char format "x(15)" no-undo.
def var c-transacao as char no-undo.
def var c-lancamento  as char no-undo.
def var c-lista-trans as char no-undo.
def var c-lista-lanc  as char no-undo.
def var c-lanc        as char format "x(01)" no-undo.

def buffer b-mov-ap     for mov-ap.
def buffer b-mov-ap-aux for mov-ap.
def buffer b-tit-ap     for tit-ap.

{include/i-vrtab.i mov-ap}

DEFINE VARIABLE c-format-emit-fita AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-format-contabiliza AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-format-contabilizou AS CHARACTER NO-UNDO.


def temp-table tt-espec-ap no-undo like espec-ap.

def new global shared var rw-tit      as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME smart
&Scoped-define BROWSE-NAME br-documento

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES tit-ap
&Scoped-define FIRST-EXTERNAL-TABLE tit-ap


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tit-ap.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-documento

/* Definitions for BROWSE br-documento                                  */
&Scoped-define FIELDS-IN-QUERY-br-documento entry(tt-documento.transacao , c-lista-trans) @ c-transacao tt-documento.portador tt-documento.c-modalidade tt-documento.dt-transacao tt-documento.c-lanca tt-documento.valor-mov tt-documento.valor-mov-me tt-documento.moeda tt-documento.cotacao-dia tt-documento.i-nr-bordero tt-documento.nr-cheque tt-documento.d-data-1 tt-documento.emit-fita tt-documento.contabiliza tt-documento.contabilizou tt-documento.valor-presente tt-documento.valor-juros tt-documento.valor-juros-me tt-documento.vl-desconto tt-documento.vl-desconto-me tt-documento.de-vl-juros-neg tt-documento.de-vl-juros-me-neg &if defined(BF_FIN_MULTA_ABATIMENTO) &then tt-documento.vl-multa tt-documento.vl-multa-me tt-documento.vl-abatimento tt-documento.vl-abatimento-me &endif tt-documento.frete tt-documento.frete-me tt-documento.diversos tt-documento.diversos-me tt-documento.dt-vencimen tt-documento.referencia tt-documento.esp-ant tt-documento.serie-ant tt-documento.docto-ant tt-documento.parc-ant tt-documento.fornec-ant tt-documento.vl-antecip tt-documento.vl-antecip-me tt-documento.port-transf tt-documento.usuario tt-documento.c-origem tt-documento.i-sequencia tt-documento.dt-today tt-documento.c-time   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-documento   
&Scoped-define SELF-NAME br-documento
&Scoped-define OPEN-QUERY-br-documento run pi-criar-tt-documento.  OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.num-id-mov-ap.  apply 'value-changed' to br-documento in frame {&FRAME-NAME}.
&Scoped-define TABLES-IN-QUERY-br-documento tt-documento
&Scoped-define FIRST-TABLE-IN-QUERY-br-documento tt-documento


/* Definitions for FRAME smart                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-smart ~
    ~{&OPEN-QUERY-br-documento}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-documento bt-contas bt-caixa-banco ~
bt-relacto 
&Scoped-Define DISPLAYED-OBJECTS c-historico 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-caixa-banco 
     LABEL "C&aixa Bancos" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-contas 
     LABEL "&Contas Cont beis" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-relacto 
     LABEL "&Titulo Relacionado" 
     SIZE 20 BY 1.

DEFINE VARIABLE c-historico AS CHARACTER FORMAT "x(100)" 
     LABEL "Hist¢rico":R11 
     VIEW-AS FILL-IN 
     SIZE 74.29 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-documento FOR 
      tt-documento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-documento B-table-Win _FREEFORM
  QUERY br-documento DISPLAY
      entry(tt-documento.transacao , c-lista-trans) @ c-transacao
tt-documento.portador
tt-documento.c-modalidade
tt-documento.dt-transacao  
tt-documento.c-lanca
tt-documento.valor-mov
tt-documento.valor-mov-me
tt-documento.moeda
tt-documento.cotacao-dia
tt-documento.i-nr-bordero
tt-documento.nr-cheque
tt-documento.d-data-1
tt-documento.emit-fita    
tt-documento.contabiliza  
tt-documento.contabilizou 
tt-documento.valor-presente
tt-documento.valor-juros
tt-documento.valor-juros-me
tt-documento.vl-desconto
tt-documento.vl-desconto-me
tt-documento.de-vl-juros-neg
tt-documento.de-vl-juros-me-neg
&if defined(BF_FIN_MULTA_ABATIMENTO) &then
tt-documento.vl-multa
tt-documento.vl-multa-me
tt-documento.vl-abatimento
tt-documento.vl-abatimento-me
&endif
tt-documento.frete
tt-documento.frete-me COLUMN-LABEL "Vl Frete ME"
tt-documento.diversos
tt-documento.diversos-me COLUMN-LABEL "Vl Diversos ME"
tt-documento.dt-vencimen
tt-documento.referencia format "x(11)"
tt-documento.esp-ant
tt-documento.serie-ant
tt-documento.docto-ant
tt-documento.parc-ant
tt-documento.fornec-ant format ">>>>>>>>>>9"
tt-documento.vl-antecip
tt-documento.vl-antecip-me
tt-documento.port-transf  
tt-documento.usuario
tt-documento.c-origem
tt-documento.i-sequencia
tt-documento.dt-today
tt-documento.c-time
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84.57 BY 7.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME smart
     br-documento AT ROW 1 COL 1
     c-historico AT ROW 8.38 COL 9.57 COLON-ALIGNED HELP
          "Descri‡Æo do hist¢rico"
     bt-contas AT ROW 9.42 COL 1
     bt-caixa-banco AT ROW 9.42 COL 21.29
     bt-relacto AT ROW 9.42 COL 41.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: movadm.tit-ap
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.46
         WIDTH              = 85.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME smart
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-documento 1 smart */
ASSIGN 
       FRAME smart:SCROLLABLE       = FALSE
       FRAME smart:HIDDEN           = TRUE.

ASSIGN 
       br-documento:NUM-LOCKED-COLUMNS IN FRAME smart     = 1.

/* SETTINGS FOR FILL-IN c-historico IN FRAME smart
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-documento
/* Query rebuild information for BROWSE br-documento
     _START_FREEFORM
run pi-criar-tt-documento.

OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.num-id-mov-ap.

apply 'value-changed' to br-documento in frame {&FRAME-NAME}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-documento */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME smart
/* Query rebuild information for FRAME smart
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME smart */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-documento
&Scoped-define SELF-NAME br-documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-documento B-table-Win
ON ROW-DISPLAY OF br-documento IN FRAME smart
DO:
    if  avail tt-documento then
    ASSIGN tt-documento.emit-fita:FORMAT    IN BROWSE br-documento = c-format-emit-fita
           tt-documento.contabiliza:FORMAT  IN BROWSE br-documento = c-format-contabiliza
           tt-documento.contabilizou:FORMAT IN BROWSE br-documento = c-format-contabilizou.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-documento B-table-Win
ON VALUE-CHANGED OF br-documento IN FRAME smart
DO:

    if  avail tt-documento then do:

        IF  tt-documento.esp-ant <> "" THEN
            ASSIGN bt-relacto:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        ELSE
            ASSIGN bt-relacto:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        
        find first mov-ap where rowid(mov-ap) = tt-documento.gr-tt-doc no-error.
        if  avail mov-ap then do:

            assign gr-mov-ap = rowid(mov-ap).

            find first tt-espec-ap where tt-espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.
            if  not avail tt-espec-ap then do:
                find espec-ap where espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.               
                if avail espec-ap then do:
                   create tt-espec-ap.
                   buffer-copy espec-ap to tt-espec-ap.
                end.   
            end.    

            if  mov-ap.contabiliza = no or
                mov-ap.transacao   = 07 or
                mov-ap.transacao   = 10 or
                mov-ap.transacao   = 11 or
                mov-ap.transacao   = 12 or
                mov-ap.transacao   = 13 or
                mov-ap.transacao   = 14 or
                mov-ap.transacao   = 15 or
                mov-ap.transacao   = 16 or
               (mov-ap.transacao   = 05 and
                mov-ap.tipo        = 10 and
                mov-ap.contabiliza = no) or
               (avail tt-espec-ap
                  and tt-espec-ap.contabiliza = no
                  or  (tt-espec-ap.tipo  = 3 and mov-ap.transacao <> 3 and mov-ap.transacao <> 4) 
                  or  tt-espec-ap.tipo  = 4
                  or  tt-espec-ap.tipo  = 5) then do:
                assign bt-contas:sensitive in frame {&Frame-name} = no.
            end.
            else
                assign bt-contas:sensitive in frame {&frame-name} = yes.

            if  tt-documento.contabiliza = yes then do:
                if  mov-ap.transacao = 2 or
                    mov-ap.transacao = 3 or
                    mov-ap.transacao = 4 then
                    assign bt-contas:sensitive in frame {&frame-name} = yes.
            end.

            assign c-historico:screen-value in frame {&FRAME-NAME} = mov-ap.historico.
        end.
   end.
   ELSE
        ASSIGN bt-relacto:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-caixa-banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-caixa-banco B-table-Win
ON CHOOSE OF bt-caixa-banco IN FRAME smart /* Caixa Bancos */
DO:

  run app/ap0804k.w.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contas B-table-Win
ON CHOOSE OF bt-contas IN FRAME smart /* Contas Cont beis */
DO:
    if avail tt-documento then do:
       find first mov-ap where rowid(mov-ap) = tt-documento.gr-tt-doc no-error.
       if avail mov-ap then do:       
           assign gr-mov-ap = rowid(mov-ap).
           find tit-ap 
              where tit-ap.ep-codigo   = mov-ap.ep-codigo
                and tit-ap.cod-estabel = mov-ap.cod-estabel
                and tit-ap.cod-fornec  = mov-ap.cod-fornec
                and tit-ap.cod-esp     = mov-ap.cod-esp
                and tit-ap.serie       = mov-ap.serie
                and tit-ap.nr-docto    = mov-ap.nr-docto
                and tit-ap.parcela     = mov-ap.parcela
                no-lock no-error.
           if avail tit-ap then
              assign gr-tit-ap = rowid(tit-ap).       
           run app/ap0804l.w.
       end.
       else do:
          {utp/ut-liter.i Movimento}
          assign c-liter1 = trim(return-value).
          {utp/ut-liter.i consultar_conta_contabil}
          run utp/ut-msgs.p (input "show",
                             input 5823,
                             input c-liter1 + "~~" + trim(return-value)).
          return "adm-error":U.                         

       end.
   end.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-relacto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-relacto B-table-Win
ON CHOOSE OF bt-relacto IN FRAME smart /* Titulo Relacionado */
DO:

    IF  AVAIL tt-documento THEN DO:
        
        FIND FIRST b-tit-ap NO-LOCK
             WHERE b-tit-ap.ep-codigo   = tt-documento.ep-codigo
             AND   b-tit-ap.cod-estabel = tt-documento.cod-estabel
             AND   b-tit-ap.cod-fornec  = tt-documento.fornec-ant       
             AND   b-tit-ap.serie       = tt-documento.serie-ant        
             AND   b-tit-ap.cod-esp     = tt-documento.esp-ant          
             AND   b-tit-ap.nr-docto    = tt-documento.docto-ant        
             AND   b-tit-ap.parcela     = tt-documento.parc-ant  
             NO-ERROR.

        IF  AVAIL b-tit-ap THEN DO:
            ASSIGN rw-tit = ROWID(b-tit-ap).
            RUN app/ap0804f.w.
        END.
    END.
    ELSE DO:
        run utp/ut-msgs.p (input "show",
                           input 17197,
                           input "um registro").
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{utp/ut-field.i mgadm tit-ap transacao 1}
assign c-transacao:label in browse br-documento = return-value.

{utp/ut-liter.i LC}
  assign tt-documento.c-lanca:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Modalidade * R}.
assign tt-documento.c-modalidade:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Bordero}.
assign tt-documento.i-nr-bordero:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Origem * R}.
assign tt-documento.c-origem:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Contabilizou?}
assign tt-documento.contabilizou:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Valor}
assign tt-documento.valor-mov:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Valor_ME}
assign tt-documento.valor-mov-me:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Vl_Juros_Neg}
assign tt-documento.de-vl-juros-neg:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Vl_Juros_ME_Neg}
assign tt-documento.de-vl-juros-me-neg:label in browse {&BROWSE-NAME} = trim(return-value).

&if defined(BF_FIN_MULTA_ABATIMENTO) &then
{utp/ut-liter.i Vl_Multa}
assign tt-documento.vl-multa:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Vl_Multa_ME}
assign tt-documento.vl-multa-me:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Vl_Abatimento}
assign tt-documento.vl-abatimento:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Vl_Abatimento_ME}
assign tt-documento.vl-abatimento-me:label in browse {&browse-name} = trim(return-value).
&endif

{utp/ut-liter.i Port}
assign tt-documento.portador:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Vl_Desconto_ME}
assign tt-documento.vl-desconto-me:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Vl_Antecip_ME}
assign tt-documento.vl-antecip-me:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Hist¢rico}
assign c-historico:label in frame {&FRAME-NAME} = trim(return-value).

assign c-lista-trans      = {adinc/i01ad165.i 03}
       c-lista-lanc       = {adinc/i01ad059.i 03}.

{utp/ut-field.i mgadm mov-ap emit-fita 4}
assign c-format-emit-fita = return-value.

{utp/ut-field.i mgadm mov-ap contabiliza 4}
assign c-format-contabiliza = return-value.

{utp/ut-field.i mgadm mov-ap contabilizou 4}
assign c-format-contabilizou = return-value.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "tit-ap"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tit-ap"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME smart.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  apply 'value-changed' to br-documento in frame {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-documento B-table-Win 
PROCEDURE pi-criar-tt-documento :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    for each tt-documento exclusive-lock:
        delete tt-documento.
    end.   

    for each  mov-ap
        where mov-ap.ep-codigo    = tit-ap.ep-codigo 
        and   mov-ap.cod-estabel  = tit-ap.cod-estabel
        and   mov-ap.cod-esp      = tit-ap.cod-esp
        and   mov-ap.serie        = tit-ap.serie
        and   mov-ap.nr-docto     = tit-ap.nr-docto
        and   mov-ap.parcela      = tit-ap.parcela
        and   mov-ap.cod-fornec   = tit-ap.cod-fornec 
        and   mov-ap.transacao   <> 7 
        and   mov-ap.transacao   <> 8
        and   mov-ap.transacao   <> 9 no-lock:

        find first tt-espec-ap where tt-espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.
        if  not avail tt-espec-ap then do:
            find espec-ap where espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.               
            if avail espec-ap then do:
               create tt-espec-ap.
               buffer-copy espec-ap to tt-espec-ap.
            end.   
        end.    

        if  avail tt-espec-ap and (tt-espec-ap.tipo = 2 or tt-espec-ap.tipo = 10) then do:

            for each  b-mov-ap-aux use-index ch-antecip
                where b-mov-ap-aux.ep-codigo   = mov-ap.ep-codigo 
                and   b-mov-ap-aux.cod-estabel = mov-ap.cod-estabel
                and   b-mov-ap-aux.esp-ant     = mov-ap.cod-esp
                and   b-mov-ap-aux.serie-ant   = mov-ap.serie
                and   b-mov-ap-aux.docto-ant   = mov-ap.nr-docto
                and   b-mov-ap-aux.parc-ant    = mov-ap.parcela
                and   b-mov-ap-aux.fornec-ant  = mov-ap.cod-fornec
                and   b-mov-ap-aux.transacao   = 2 no-lock:

                find first tt-documento 
                     where tt-documento.num-id-mov-ap  = b-mov-ap-aux.num-id-mov-ap no-error.

                run pi-lancamento(input mov-ap.cod-esp,
                                  input b-mov-ap-aux.transacao,  
                                  input b-mov-ap-aux.lancamento,
                                  output c-lanc).
                if  not avail tt-documento then do:
                    create tt-documento.
                    assign tt-documento.transacao        = b-mov-ap-aux.transacao
                           /*tt-documento.lancamento       = b-mov-ap-aux.lancamento*/
                           tt-documento.c-lanca          = c-lanc
                           tt-documento.portador         = b-mov-ap-aux.portador
                           tt-documento.modalidade       = b-mov-ap-aux.modalidade
                           tt-documento.i-nr-bordero     = b-mov-ap-aux.nr-bordero
                           tt-documento.dt-transacao     = b-mov-ap-aux.dt-transacao
                           tt-documento.dt-vencimen      = b-mov-ap-aux.dt-vencimen
                           tt-documento.nr-cheque        = b-mov-ap-aux.nr-cheque
                           tt-documento.d-data-1         = b-mov-ap-aux.data-1
                           tt-documento.referencia       = b-mov-ap-aux.referencia
                           tt-documento.emit-fita        = b-mov-ap-aux.emit-fita
                           tt-documento.contabilizou     = b-mov-ap-aux.contabilizou
                           tt-documento.contabiliza      = b-mov-ap-aux.contabiliza
                           tt-documento.moeda            = b-mov-ap-aux.moeda
                           tt-documento.cotacao-dia      = b-mov-ap-aux.cotacao-dia
                           tt-documento.valor-mov        = b-mov-ap-aux.valor-mov
                           tt-documento.valor-mov-me     = b-mov-ap-aux.valor-mov-me
                           tt-documento.valor-presente   = b-mov-ap-aux.valor-presente
                           tt-documento.vl-desconto      = b-mov-ap-aux.vl-desconto
                           tt-documento.vl-desconto-me   = b-mov-ap-aux.vl-desconto-me
                           tt-documento.valor-juros      = b-mov-ap-aux.valor-juros
                           tt-documento.valor-juros-me   = b-mov-ap-aux.valor-juros-me
                           &if defined(BF_FIN_MULTA_ABATIMENTO) &then
                           tt-documento.vl-abatimento    = b-mov-ap-aux.vl-abatimento
                           tt-documento.vl-abatimento-me = b-mov-ap-aux.vl-abatimento-me
                           tt-documento.vl-multa         = b-mov-ap-aux.vl-multa
                           tt-documento.vl-multa-me      = b-mov-ap-aux.vl-multa-me
                           &endif
                           tt-documento.vl-antecip       = b-mov-ap-aux.vl-antecip
                           tt-documento.vl-antecip-me    = b-mov-ap-aux.vl-antecip-me
                           tt-documento.frete            = b-mov-ap-aux.frete
                           tt-documento.frete-me         = b-mov-ap-aux.frete-me
                           tt-documento.diversos         = b-mov-ap-aux.diversos
                           tt-documento.diversos-me      = b-mov-ap-aux.diversos-me
                           tt-documento.ep-codigo        = b-mov-ap-aux.ep-codigo
                           tt-documento.cod-estabel      = b-mov-ap-aux.cod-estabel
                           tt-documento.serie            = b-mov-ap-aux.serie
                           tt-documento.cod-esp          = b-mov-ap-aux.cod-esp
                           tt-documento.nr-docto         = b-mov-ap-aux.nr-docto
                           tt-documento.parcela          = b-mov-ap-aux.parcela
                           tt-documento.cod-fornec       = b-mov-ap-aux.cod-fornec
                           tt-documento.serie-ant        = b-mov-ap-aux.serie
                           tt-documento.esp-ant          = b-mov-ap-aux.cod-esp
                           tt-documento.docto-ant        = b-mov-ap-aux.nr-docto
                           tt-documento.parc-ant         = b-mov-ap-aux.parcela
                           tt-documento.fornec-ant       = b-mov-ap-aux.cod-fornec
                           tt-documento.usuario          = b-mov-ap-aux.usuario
                           tt-documento.i-sequencia      = b-mov-ap-aux.sequencia
                           tt-documento.dt-today         = b-mov-ap-aux.dt-today
                           tt-documento.c-time           = b-mov-ap-aux.c-time
                           tt-documento.num-id-mov-ap    = b-mov-ap-aux.num-id-mov-ap
                           tt-documento.baixa-transf     = b-mov-ap-aux.baixa-transf
                           tt-documento.origem           = b-mov-ap-aux.origem
                           tt-documento.port-transf      = b-mov-ap-aux.port-transf
                           tt-documento.gr-tt-doc        = rowid(b-mov-ap-aux).  

                    if  tt-documento.modalidade = 0 then
                        assign tt-documento.modalidade = 1.

                    assign tt-documento.c-modalidade = {adinc/i03ad209.i 4 tt-documento.modalidade}.

                    if  tt-documento.origem <> 0 then
                        assign tt-documento.c-origem = {adinc/i01ad170.i 4 tt-documento.origem}.

                end.      
            end.
        end.

        run pi-lancamento(input mov-ap.cod-esp,
                          input mov-ap.transacao,  
                          input mov-ap.lancamento,
                          output c-lanc).
        find first tt-documento where tt-documento.num-id-mov-ap  = mov-ap.num-id-mov-ap no-error.
        if  not avail tt-documento then do:           
            create tt-documento.
            assign tt-documento.transacao          = mov-ap.transacao
/*                   tt-documento.lancamento         = mov-ap.lancamento*/
                   tt-documento.c-lanca              = c-lanc
                   tt-documento.portador           = mov-ap.portador
                   tt-documento.modalidade         = mov-ap.modalidade
                   tt-documento.i-nr-bordero       = mov-ap.nr-bordero
                   tt-documento.dt-transacao       = mov-ap.dt-transacao
                   tt-documento.dt-vencimen        = mov-ap.dt-vencimen
                   tt-documento.nr-cheque          = mov-ap.nr-cheque
                   tt-documento.d-data-1           = mov-ap.data-1
                   tt-documento.referencia         = mov-ap.referencia
                   tt-documento.emit-fita          = mov-ap.emit-fita
                   tt-documento.contabilizou       = mov-ap.contabilizou
                   tt-documento.contabiliza        = mov-ap.contabiliza
                   tt-documento.moeda              = mov-ap.moeda
                   tt-documento.cotacao-dia        = mov-ap.cotacao-dia
                   tt-documento.valor-mov          = mov-ap.valor-mov
                   tt-documento.valor-mov-me       = mov-ap.valor-mov-me
                   tt-documento.valor-presente     = mov-ap.valor-presente
                   tt-documento.vl-desconto        = mov-ap.vl-desconto
                   tt-documento.vl-desconto-me     = mov-ap.vl-desconto-me
                   tt-documento.valor-juros        = mov-ap.valor-juros
                   tt-documento.de-vl-juros-neg    = mov-ap.valor-juros
                   tt-documento.valor-juros-me     = mov-ap.valor-juros-me
                   tt-documento.de-vl-juros-me-neg = mov-ap.valor-juros-me
                   &if defined(BF_FIN_MULTA_ABATIMENTO) &then
                   tt-documento.vl-abatimento      = mov-ap.vl-abatimento
                   tt-documento.vl-abatimento-me   = mov-ap.vl-abatimento-me
                   tt-documento.vl-multa           = mov-ap.vl-multa
                   tt-documento.vl-multa-me        = mov-ap.vl-multa-me
                   &endif
                   tt-documento.vl-antecip         = mov-ap.vl-antecip
                   tt-documento.vl-antecip-me      = mov-ap.vl-antecip-me
                   tt-documento.frete              = mov-ap.frete
                   tt-documento.frete-me           = mov-ap.frete-me
                   tt-documento.diversos           = mov-ap.diversos
                   tt-documento.diversos-me        = mov-ap.diversos-me
                   tt-documento.ep-codigo          = mov-ap.ep-codigo
                   tt-documento.cod-estabel        = mov-ap.cod-estabel
                   tt-documento.serie              = mov-ap.serie
                   tt-documento.cod-esp            = mov-ap.cod-esp
                   tt-documento.nr-docto           = mov-ap.nr-docto
                   tt-documento.parcela            = mov-ap.parcela
                   tt-documento.cod-fornec         = mov-ap.cod-fornec
                   tt-documento.serie-ant          = mov-ap.serie-ant
                   tt-documento.esp-ant            = mov-ap.esp-ant
                   tt-documento.docto-ant          = mov-ap.docto-ant
                   tt-documento.parc-ant           = mov-ap.parc-ant
                   tt-documento.fornec-ant         = mov-ap.fornec-ant
                   tt-documento.usuario            = mov-ap.usuario
                   tt-documento.i-sequencia        = mov-ap.sequencia
                   tt-documento.dt-today           = mov-ap.dt-today
                   tt-documento.c-time             = mov-ap.c-time
                   tt-documento.num-id-mov-ap      = mov-ap.num-id-mov-ap
                   tt-documento.baixa-transf       = mov-ap.baixa-transf
                   tt-documento.port-transf        = mov-ap.port-transf
                   tt-documento.origem             = mov-ap.origem
                   tt-documento.gr-tt-doc          = rowid(mov-ap).

            if  tt-documento.modalidade = 0 then
                assign tt-documento.modalidade = 1.

            assign tt-documento.c-modalidade = {adinc/i03ad209.i 4 tt-documento.modalidade}.

            if  tt-documento.origem <> 0 then
                assign tt-documento.c-origem = {adinc/i01ad170.i 4 tt-documento.origem}.

            if tt-documento.transacao = 2 then
               assign tt-documento.lancamento = 2.    

        end.       
    end. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-lancamento B-table-Win 
PROCEDURE pi-lancamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input  parameter c-codesp      like mov-ap.cod-esp  no-undo.
    def input  parameter i-transacao   like mov-ap.transacao  no-undo.
    def input  parameter i-lancamento  like mov-ap.lancamento no-undo.
    def output parameter c-sinal   as char format "x(01)" no-undo.

    assign c-sinal = " ".


    find first tt-espec-ap where tt-espec-ap.cod-esp = c-codesp no-lock no-error.
    if  not avail tt-espec-ap then do:
        find espec-ap where espec-ap.cod-esp = c-codesp no-lock no-error.               
        if avail espec-ap then do:
           create tt-espec-ap.
           buffer-copy espec-ap to tt-espec-ap.
        end.   
    end.    

    if  avail tt-espec-ap then do:

        if  tt-espec-ap.tipo = 2
        or  tt-espec-ap.tipo = 3
        or  tt-espec-ap.tipo = 5 then do:

            if  i-transacao  = 1  or
               (i-transacao  = 5  and i-lancamento = 2) then
                assign c-sinal = "+".

            if (i-transacao  = 2) or
               (i-transacao  = 3) or
               (i-transacao  = 5 and i-lancamento = 1) then
                assign c-sinal = "-".
        end.

        if  tt-espec-ap.tipo = 1
        or  tt-espec-ap.tipo = 4 
        or  tt-espec-ap.tipo = 7 
        or  tt-espec-ap.tipo = 8
        or  tt-espec-ap.tipo = 9
        or  tt-espec-ap.tipo = 10 then do:
            if  i-transacao = 1
            or (i-transacao = 5 and i-lancamento = 1)
            or (i-transacao = 6 and i-lancamento = 2) then    
                assign c-sinal = "+".
            if  i-transacao = 2
            or (i-transacao = 5 and i-lancamento = 2)
            or (i-transacao = 6 and i-lancamento = 1) then    
                assign c-sinal = "-".
        end.     

        if  i-transacao = 16 then do:
            case tt-espec-ap.tipo:
                when 4 then assign c-sinal = "+".
                when 5 then assign c-sinal = "-".
            end case.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tit-ap"}
  {src/adm/template/snd-list.i "tt-documento"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

