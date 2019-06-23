&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2movme            PROGRESS
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
{include/i-prgvrs.i B23AD183 2.00.00.009}  /*** 010009 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B23AD183 MUT}
&ENDIF



/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/*cop adm-attribute-dlg support/browserd.w*/

/* MINIFLEXIBILIZA€ÇO */

{include/i_dbvers.i}


def new global shared var v-row-parent as rowid no-undo.
def new global shared var gr-mov-tit   as rowid no-undo.
def new global shared var gr-titulo    as rowid no-undo.
def new global shared var r-registro-atual      as Rowid                            no-undo.

def var c-transacao   as char                no-undo.
def var c-lista-trans as char                no-undo.
def var c-modalidade  as char format "x(15)" no-undo.
def var c-lista-mod   as char format "x(15)" no-undo.
def var c-lancamento  as char format "x(06)" no-undo.
def var c-lista-lanc  as char                no-undo.
def var c-liter1      as char format "x(15)" no-undo.
def var c-nota-db     as char                no-undo.
def var c-doc-org     as char                no-undo.
def var l-controla    as log no-undo.


def var v-row-table   as rowid.
def var l-nota        as log   no-undo.
def var row-nota      as rowid no-undo.
def var de-calc-liq   as dec format "->>,>>>,>>>,>>9.99" no-undo.
def var c-lanc        as char format "x(01)" no-undo.
def var l-habil-cta-contab as logical no-undo. 

def temp-table tt-documento like mov-tit
    field c-transacao    like mov-tit.transacao
    field c-lancamento   like mov-tit.lancamento
    field c-lanca         as char format "x(01)"
    field c-modalidade   like mov-tit.modalidade
    field cob-banc-eletr like titulo.cob-banc-eletr
    field vl-cart-receb  as dec format "->>>>>>>,>>9.99"
    field gr-tt-doc      as  rowid.

def buffer b-titulo      for titulo.
def buffer b-mov-tit     for mov-tit.
def buffer b-mov-tit-aux for mov-tit.

{include/i-vrtab.i mov-tit}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-documento

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES titulo
&Scoped-define FIRST-EXTERNAL-TABLE titulo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR titulo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-documento

/* Definitions for BROWSE br-documento                                  */
&Scoped-define FIELDS-IN-QUERY-br-documento entry(tt-documento.transacao , c-lista-trans) @ c-transacao /*entry(tt-documento.lancamento , c-lista-lanc) @ c-lancamento*/ tt-documento.dt-trans tt-documento.cod-portador entry(tt-documento.modalidade , c-lista-mod) @ c-modalidade tt-documento.c-lanca tt-documento.vl-baixa tt-documento.vl-baixa-me tt-documento.dt-baixa tt-documento.dt-credito tt-documento.mo-codigo tt-documento.cotacao-dia tt-documento.contabilizou tt-documento.flag-contab tt-documento.vl-liquido tt-documento.vl-desconto tt-documento.vl-desconto-me tt-documento.vl-juros-rec tt-documento.vl-juros-rec-me tt-documento.vl-cart-receb tt-documento.vl-abatimen tt-documento.vl-abatimen-me tt-documento.vl-desp-banc tt-documento.vl-desp-banc-me tt-documento.vl-desp-financ tt-documento.vl-desp-financ-me tt-documento.vl-multa tt-documento.vl-multa-me tt-documento.vl-iof tt-documento.vl-iof-me tt-documento.frete tt-documento.frete-me tt-documento.diversos tt-documento.diversos-me tt-documento.baixa-subs tt-documento.estorna tt-documento.emit-fita tt-documento.arq-escritural tt-documento.banco tt-documento.agencia tt-documento.cta-corrente tt-documento.cob-banc-eletr tt-documento.nr-docto-emp tt-documento.referencia tt-documento.esp-antecip tt-documento.serie-antecip tt-documento.doc-antecip tt-documento.parc-antecip tt-documento.vl-antecip tt-documento.vl-antecip-me tt-documento.observacao tt-documento.usuario tt-documento.dt-today tt-documento.c-time   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-documento   
&Scoped-define SELF-NAME br-documento
&Scoped-define OPEN-QUERY-br-documento run pi-criar-tt-documento.  OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.dt-today                                               by tt-documento.num-id-mov-tit                                               by tt-documento.c-time.  apply 'value-changed' to br-documento in frame {&FRAME-NAME}.
&Scoped-define TABLES-IN-QUERY-br-documento tt-documento
&Scoped-define FIRST-TABLE-IN-QUERY-br-documento tt-documento


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-documento}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-documento bt-contas bt-cheques ~
bt-nota-db-cr bt-obs 

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
</FOREIGN-KEYS
><EXECUTING-CODE>
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
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cheques 
     LABEL "C&heques" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-contas 
     LABEL "&Contas Cont beis" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-nota-db-cr 
     LABEL "&Nota D‚bito/Cr‚dito" 
     SIZE 20 BY 1.

DEFINE BUTTON bt-obs 
     LABEL "&Observa‡Æo" 
     SIZE 20 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-documento FOR 
      tt-documento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-documento B-table-Win _FREEFORM
  QUERY br-documento NO-LOCK DISPLAY
      entry(tt-documento.transacao , c-lista-trans) @ c-transacao
/*entry(tt-documento.lancamento , c-lista-lanc) @ c-lancamento*/
tt-documento.dt-trans
tt-documento.cod-portador    
entry(tt-documento.modalidade , c-lista-mod) @ c-modalidade   
tt-documento.c-lanca
tt-documento.vl-baixa format "->>>>>>>,>>9.99"
tt-documento.vl-baixa-me format "->>>>>>>,>>9.99"
tt-documento.dt-baixa
tt-documento.dt-credito
tt-documento.mo-codigo
tt-documento.cotacao-dia
tt-documento.contabilizou
tt-documento.flag-contab
tt-documento.vl-liquido format "->>>>>>>,>>9.99"
tt-documento.vl-desconto format "->>>>>>>,>>9.99"
tt-documento.vl-desconto-me format "->>>>>>>,>>9.99"
tt-documento.vl-juros-rec format "->>>>>>>,>>9.99"
tt-documento.vl-juros-rec-me format "->>>>>>>,>>9.99"
tt-documento.vl-cart-receb format "->>>>>>>,>>9.99"
tt-documento.vl-abatimen format "->>>>>>>,>>9.99" 
tt-documento.vl-abatimen-me format "->>>>>>>,>>9.99"
tt-documento.vl-desp-banc format "->>>>>>>,>>9.99" 
tt-documento.vl-desp-banc-me format "->>>>>>>,>>9.99"
tt-documento.vl-desp-financ format "->>>>>>>,>>9.99"
tt-documento.vl-desp-financ-me format "->>>>>>>,>>9.99"
tt-documento.vl-multa format "->>>>>>>,>>9.99"
tt-documento.vl-multa-me format "->>>>>>>,>>9.99"
tt-documento.vl-iof format "->>>>>>>,>>9.99"
tt-documento.vl-iof-me format "->>>>>>>,>>9.99"
tt-documento.frete format "->>>>>>>,>>9.99"
tt-documento.frete-me format "->>>>>>>,>>9.99"
tt-documento.diversos format "->>>>>>>,>>9.99"
tt-documento.diversos-me format "->>>>>>>,>>9.99"
tt-documento.baixa-subs
tt-documento.estorna format "NÆo/Sim"
tt-documento.emit-fita
tt-documento.arq-escritural
tt-documento.banco
tt-documento.agencia
tt-documento.cta-corrente
tt-documento.cob-banc-eletr
tt-documento.nr-docto-emp
tt-documento.referencia format "x(20)"
tt-documento.esp-antecip
tt-documento.serie-antecip
tt-documento.doc-antecip
tt-documento.parc-antecip
tt-documento.vl-antecip format "->>>>>>>,>>9.99"
tt-documento.vl-antecip-me format "->>>>>>>,>>9.99"
tt-documento.observacao
tt-documento.usuario
tt-documento.dt-today
tt-documento.c-time
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84.57 BY 8.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-documento AT ROW 1 COL 1
     bt-contas AT ROW 9.33 COL 1
     bt-cheques AT ROW 9.33 COL 21.43
     bt-nota-db-cr AT ROW 9.33 COL 41.86
     bt-obs AT ROW 9.33 COL 62.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ems2movme.titulo
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
         HEIGHT             = 9.5
         WIDTH              = 84.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
/* {utp/ut-glob.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-documento 1 F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 9.5
       FRAME F-Main:WIDTH            = 84.72.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-documento
/* Query rebuild information for BROWSE br-documento
     _START_FREEFORM
run pi-criar-tt-documento.

OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.dt-today
                                              by tt-documento.c-time
                                              by tt-documento.num-id-mov-tit.

apply 'value-changed' to br-documento in frame {&FRAME-NAME}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-documento */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-documento
&Scoped-define SELF-NAME br-documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-documento B-table-Win
ON VALUE-CHANGED OF br-documento IN FRAME F-Main
DO:

    {src/adm/template/brschnge.i}

    assign l-nota                                         = yes
           row-nota                                       = ?
           bt-contas:sensitive     in frame {&FRAME-NAME} = yes
           bt-cheques:sensitive    in frame {&FRAME-NAME} = no
           bt-nota-db-cr:sensitive in frame {&FRAME-NAME} = no.

    if  avail tt-documento then do:
        find first mov-tit no-lock
             where rowid(mov-tit) = tt-documento.gr-tt-doc no-error.
        if  avail mov-tit then do:  

            find esp-doc where 
                 esp-doc.cod-esp = mov-tit.cod-esp no-lock no-error.

            /* bt-contas */
            if mov-tit.transacao =  7 or  /* Abatimento */
               mov-tit.transacao =  8 or  /* Cancelamento */
               mov-tit.transacao =  9 or  /* Pedido de Devolu‡Æo */
               mov-tit.transacao = 10 or  /* Pedido de Baixa */
               mov-tit.transacao = 11 or  /* Protesto */
               mov-tit.transacao = 12 or  /* Susta‡Æo de Protesto */
               mov-tit.tipo      =  3 or  /* previsÆo */
               mov-tit.tipo      =  4 or  /* Nota de D‚bito */
               mov-tit.tipo      =  5 or  /* Nota de Cr‚dito */
              (mov-tit.tipo      =  6 and
               mov-tit.transacao <> 2) or  /* Docto Juros com transacao <> de baixa */
              (mov-tit.flag-contab = no and l-habil-cta-contab = no)   /* esp‚cie nÆo contabiliza */
               then do:
               assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
            end.

            if titulo.tipo       = 9  and  /* Pagare - Argentina */
               mov-tit.transacao = 14 then /* Implantacao */
               assign bt-contas:sensitive in frame {&FRAME-NAME} = no.

            if avail esp-doc then 
               if esp-doc.tipo       = 6 and
                 (mov-tit.transacao  = 14 or
                  mov-tit.transacao  = 13 or
                 (mov-tit.transacao  = 2 and
                  mov-tit.baixa-subs = yes)) then
                  assign bt-contas:sensitive in frame {&FRAME-NAME} = no.  

            /* bt-obs*/
            if mov-tit.observacao <> "" then
                 assign bt-obs:sensitive in frame {&FRAME-NAME} = yes.
            else     
                 assign bt-obs:sensitive in frame {&FRAME-NAME} = no.

            /* bt-cheques */
            if ((mov-tit.transacao = 2   and  /* baixa documento normal */
                (mov-tit.tipo      = 1   or
                 mov-tit.tipo      = 7   or   /* Recibo */
                 mov-tit.tipo      = 6)) or   /* Aviso de D‚bito */ 
                 mov-tit.transacao = 4)  and  /* implanta‡Æo de antecipa‡äes */
                 can-find(first relacto-titulo-cheque where 
                                relacto-titulo-cheque.num-id-mov-tit = mov-tit.num-id-mov-tit
                                no-lock) then do:
                 assign bt-cheques:sensitive in frame {&FRAME-NAME} = yes.
            end.

            if  mov-tit.baixa-subs = yes then
                assign bt-cheques:sensitive in frame {&FRAME-NAME} = no.


            /* bt-nota-db-cr */       
            if avail esp-doc then do:
               assign l-controla = no.
               if (esp-doc.tipo = 4 or esp-doc.tipo = 5) then do:
                  assign l-nota                                     = no
                         bt-nota-db-cr:label in frame {&FRAME-NAME} = c-doc-org.
                  find first b-mov-tit no-lock
                       where b-mov-tit.ep-codigo     = mov-tit.ep-codigo   
                       and   b-mov-tit.cod-estabel   = mov-tit.cod-estabel 
                       and   b-mov-tit.esp-antecip   = mov-tit.cod-esp     
                       and   b-mov-tit.serie-antecip = mov-tit.serie       
                       and   b-mov-tit.doc-antecip   = mov-tit.nr-docto    
                       and   b-mov-tit.parc-antecip  = mov-tit.parcela     
                       and   (b-mov-tit.transacao     = 13 
                        or   b-mov-tit.transacao     = 3) no-error.
                  if avail b-mov-tit then do:
                     find first b-titulo no-lock  
                          where b-titulo.ep-codigo   = b-mov-tit.ep-codigo
                          and   b-titulo.cod-estabel = b-mov-tit.cod-estabel
                          and   b-titulo.cod-esp     = b-mov-tit.cod-esp
                          and   b-titulo.serie       = b-mov-tit.serie
                          and   b-titulo.nr-docto    = b-mov-tit.nr-docto                          
                          and   b-titulo.parcela     = b-mov-tit.parcela no-error.                         
                     if avail b-titulo then assign l-controla = yes.       
                 end.         
               end.   
               else do:
                  assign l-nota                                     = yes
                         bt-nota-db-cr:label in frame {&FRAME-NAME} = c-nota-db.
                  find first b-titulo no-lock  
                       where b-titulo.ep-codigo   = mov-tit.ep-codigo
                       and   b-titulo.cod-estabel = mov-tit.cod-estabel
                       and   b-titulo.cod-esp     = mov-tit.esp-antecip
                       and   b-titulo.serie       = mov-tit.serie-antecip
                       and   b-titulo.nr-docto    = mov-tit.doc-antecip
                       and   b-titulo.parcela     = mov-tit.parc-antecip
                       and  (b-titulo.tipo        = 4
                       or    b-titulo.tipo        = 5) no-error.
                  if avail b-titulo then assign l-controla = yes.       
               end.  
               if l-controla = yes then do:
                  if avail b-titulo then 
                     assign row-nota = rowid(b-titulo)
                            bt-nota-db-cr:sensitive in frame {&FRAME-NAME} = yes.
               end.
            end.      
        end.    
    end.
    else
        assign bt-contas:sensitive in frame {&FRAME-NAME} = no
               bt-obs:sensitive in frame {&FRAME-NAME} = no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cheques
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cheques B-table-Win
ON CHOOSE OF bt-cheques IN FRAME F-Main /* Cheques */
DO:
  if  avail tt-documento then do:
      find first mov-tit no-lock
           where rowid(mov-tit) = tt-documento.gr-tt-doc no-error.
      if  avail mov-tit then do: 
          assign gr-mov-tit = rowid(mov-tit).
          run crp/cr0709c.w.
      end.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contas B-table-Win
ON CHOOSE OF bt-contas IN FRAME F-Main /* Contas Cont beis */
DO:
   if avail tt-documento then do:
      find first mov-tit no-lock
           where rowid(mov-tit) = tt-documento.gr-tt-doc no-error.
      if avail mov-tit then do:       
           assign gr-mov-tit = rowid(mov-tit).  
           run crp/cr0709a.w.
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


&Scoped-define SELF-NAME bt-nota-db-cr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nota-db-cr B-table-Win
ON CHOOSE OF bt-nota-db-cr IN FRAME F-Main /* Nota D‚bito/Cr‚dito */
DO:
  if row-nota <> ? then 
     run crp/cr0709d.w (input l-nota,
                        input row-nota).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-obs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-obs B-table-Win
ON CHOOSE OF bt-obs IN FRAME F-Main /* Observa‡Æo */
DO:
    if avail tt-documento then do:
        run crp/cr0709f.w (input tt-documento.gr-tt-doc).
    end.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "titulo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "titulo"}

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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
ASSIGN cAuxTraducao001 = {adinc/i02ad166.i 03}.
RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
ASSIGN c-lista-trans      = cAuxTraducao001.
&else
ASSIGN c-lista-trans      = {adinc/i02ad166.i 03}.
&endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao002 = {adinc/i01ad059.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao002).
      ASSIGN c-lista-lanc       = cAuxTraducao002.
  &else
      ASSIGN c-lista-lanc       = {adinc/i01ad059.i 03}.
  &endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao003 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao003 = {adinc/i03ad209.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao003).
      ASSIGN c-lista-mod        = cAuxTraducao003.
  &else
      ASSIGN c-lista-mod        = {adinc/i03ad209.i 03}.
  &endif

  {utp/ut-liter.i Trans * R}
  assign c-transacao:label                  in browse br-documento = return-value.
  {utp/ut-liter.i Contabilizou * R}
  assign tt-documento.contabilizou:label    in browse br-documento = return-value.
  {utp/ut-liter.i Contabiliza? * R}
  assign tt-documento.flag-contab:label     in browse br-documento = return-value.
 /*{utp/ut-liter.i LC * R}
  assign c-lancamento:label                   in browse br-documento = return-value.*/
  {utp/ut-liter.i LC}
  assign tt-documento.c-lanca:label in browse {&browse-name} = trim(return-value).  
  {utp/ut-liter.i Modalidade * R}
  assign c-modalidade:label                   in browse br-documento = return-value.
  {utp/ut-liter.i Portador * R}
  assign tt-documento.cod-portador:label      in browse br-documento = return-value.   
  {utp/ut-liter.i Data_Transa‡Æo * R}
  assign tt-documento.dt-trans:label          in browse br-documento = return-value.   
  {utp/ut-liter.i Data__Baixa_ * R}
  assign tt-documento.dt-baixa:label          in browse br-documento = return-value.   
  {utp/ut-liter.i Nota_D‚bito/Cr‚dito * R}
  assign c-nota-db = return-value.
  {utp/ut-liter.i Vl_Movto_ME_ * R}
  assign tt-documento.vl-baixa-me:label       in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Desp_Cart_Recebidas * R}
  assign tt-documento.vl-cart-receb:label    in browse br-documento = return-value.
   {utp/ut-liter.i Vl_Desconto_ME_ * R}
  assign tt-documento.vl-desconto-me:label    in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Juros_ME_ * R}
  assign tt-documento.vl-juros-rec-me:label   in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Abatimen_ME_ * R}
  assign tt-documento.vl-abatimen-me:label    in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Desp_Banc_ME_ * R}
  assign tt-documento.vl-desp-banc-me:label   in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Desp_Fin_ME_ * R}
  assign tt-documento.vl-desp-financ-me:label in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Multa_ME_ * R}
  assign tt-documento.vl-multa-me:label       in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_IOF_ME_ * R}
  assign tt-documento.vl-iof-me:label         in browse br-documento = return-value.   
  {utp/ut-liter.i Vl_Antecip_ME_ * R}
  assign tt-documento.vl-antecip-me:label     in browse br-documento = return-value.
  {utp/ut-liter.i Vl_Diversos_ME_ * R}
  assign tt-documento.diversos-me:label       in browse br-documento = return-value.
  {utp/ut-liter.i Vl_Frete_ME_ * R}
  assign tt-documento.frete-me:label          in browse br-documento = return-value.
  {utp/ut-liter.i Arq_Escritural * R}
  assign tt-documento.arq-escritural:label    in browse br-documento = return-value.


  {utp/ut-liter.i Docto_Original * R}
  assign c-doc-org = return-value.

  find first funcao
       where funcao.cd-funcao = "spp-habilita-cta-contab":U no-lock no-error.
  if avail funcao then
      assign l-habil-cta-contab = yes.
  else
      assign l-habil-cta-contab = no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

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

    for each tt-documento:
        delete tt-documento.
    end.   

    for each  mov-tit
        where mov-tit.ep-codigo    = titulo.ep-codigo 
        and   mov-tit.cod-estabel  = titulo.cod-estabel
        and   mov-tit.cod-esp      = titulo.cod-esp
        and   mov-tit.serie        = titulo.serie
        and   mov-tit.nr-docto     = titulo.nr-docto
        and   mov-tit.parcela      = titulo.parcela
        and   mov-tit.transacao   <> 15 
        and   mov-tit.transacao   <> 16
        no-lock:

        find esp-doc where esp-doc.cod-esp = mov-tit.cod-esp no-lock no-error.       

        if  avail esp-doc and esp-doc.tipo = 2 then do:

            for each  b-mov-tit-aux use-index ch-antecip
                where b-mov-tit-aux.ep-codigo     = mov-tit.ep-codigo 
                and   b-mov-tit-aux.cod-estabel   = mov-tit.cod-estabel
                and   b-mov-tit-aux.esp-antecip   = mov-tit.cod-esp
                and   b-mov-tit-aux.serie-antecip = mov-tit.serie
                and   b-mov-tit-aux.doc-antecip   = mov-tit.nr-docto
                and   b-mov-tit-aux.parc-antecip  = mov-tit.parcela
                and   b-mov-tit-aux.transacao    <> 15 
                and   b-mov-tit-aux.transacao    <> 16
                no-lock:

                find first tt-documento 
                     where tt-documento.num-id-mov-tit  = b-mov-tit-aux.num-id-mov-tit no-error.

               run pi-lancamento(input mov-tit.cod-esp,
                                 input b-mov-tit-aux.transacao,  
                                 input b-mov-tit-aux.lancamento,
                                 output c-lanc).


                if  not avail tt-documento then do:

                    create tt-documento.
                    assign tt-documento.transacao         = b-mov-tit-aux.transacao                           
                           /*tt-documento.lancamento      = b-mov-tit-aux.lancamento*/                           
                           tt-documento.c-lanca           = c-lanc                                                      
                           tt-documento.cod-portador      = b-mov-tit-aux.cod-portador
                           tt-documento.modalidade        = b-mov-tit-aux.modalidade
                           tt-documento.dt-trans          = b-mov-tit-aux.dt-trans
                           tt-documento.emit-fita         = b-mov-tit-aux.emit-fita
                           tt-documento.arq-escritural    = b-mov-tit-aux.arq-escritural
                           tt-documento.contabilizou      = b-mov-tit-aux.contabilizou
                           tt-documento.flag-contab       = b-mov-tit-aux.flag-contab
                           tt-documento.dt-baixa          = b-mov-tit-aux.dt-baixa
                           tt-documento.dt-credito        = b-mov-tit-aux.dt-credito
                           tt-documento.cotacao-dia       = b-mov-tit-aux.cotacao-dia
                           tt-documento.baixa-subs        = b-mov-tit-aux.baixa-subs
                           tt-documento.estorna           = b-mov-tit-aux.estorna
                           tt-documento.mo-codigo         = b-mov-tit-aux.mo-codigo
                           tt-documento.vl-desconto       = b-mov-tit-aux.vl-desconto
                           tt-documento.vl-desconto-me    = b-mov-tit-aux.vl-desconto-me
                           tt-documento.vl-desconto       = b-mov-tit-aux.vl-desconto
                           tt-documento.banco             = b-mov-tit-aux.banco
                           tt-documento.vl-juros-rec      = b-mov-tit-aux.vl-juros-rec
                           tt-documento.vl-juros-rec-me   = b-mov-tit-aux.vl-juros-rec-me
                          &if  "{&ems2movme_version}" >= "2.02" &then
                           tt-documento.vl-cart-receb     = b-mov-tit-aux.vl-desp-cart-recebidas
                          &endif 
                           tt-documento.agencia           = b-mov-tit-aux.agencia
                           tt-documento.vl-abatimen       = b-mov-tit-aux.vl-abatimen
                           tt-documento.vl-abatimen-me    = b-mov-tit-aux.vl-abatimen-me
                           tt-documento.cta-corrente      = b-mov-tit-aux.cta-corrente
                           tt-documento.vl-desp-banc      = b-mov-tit-aux.vl-desp-banc
                           tt-documento.vl-desp-banc-me   = b-mov-tit-aux.vl-desp-banc-me
                           tt-documento.cob-banc-eletr    = titulo.cob-banc-eletr
                           tt-documento.vl-desp-financ    = b-mov-tit-aux.vl-desp-financ
                           tt-documento.vl-desp-financ-me = b-mov-tit-aux.vl-desp-financ-me
                           tt-documento.vl-baixa          = if  b-mov-tit-aux.transacao = 4 then
                                                                b-mov-tit-aux.vl-original
                                                            else
                                                                b-mov-tit-aux.vl-baixa
                           tt-documento.vl-baixa-me       = if  b-mov-tit-aux.transacao = 4 then
                                                                b-mov-tit-aux.vl-original-me
                                                            else
                                                                b-mov-tit-aux.vl-baixa-me
                           tt-documento.vl-antecip        = b-mov-tit-aux.vl-antecip
                           tt-documento.vl-antecip-me     = b-mov-tit-aux.vl-antecip-me                           
                           tt-documento.nr-docto-emp      = b-mov-tit-aux.nr-docto-emp
                           tt-documento.vl-iof            = b-mov-tit-aux.vl-iof
                           tt-documento.vl-iof-me         = b-mov-tit-aux.vl-iof-me
                           tt-documento.referencia        = b-mov-tit-aux.referencia
                           tt-documento.vl-multa          = b-mov-tit-aux.vl-multa
                           tt-documento.vl-multa-me       = b-mov-tit-aux.vl-multa-me
                           tt-documento.usuario           = b-mov-tit-aux.usuario
                           tt-documento.dt-today          = b-mov-tit-aux.dt-today
                           tt-documento.c-time            = b-mov-tit-aux.c-time
                           tt-documento.serie             = b-mov-tit-aux.serie
                           tt-documento.cod-esp           = b-mov-tit-aux.cod-esp
                           tt-documento.nr-docto          = b-mov-tit-aux.nr-docto
                           tt-documento.parcela           = b-mov-tit-aux.parcela
                           tt-documento.serie-antecip     = b-mov-tit-aux.serie
                           tt-documento.esp-antecip       = b-mov-tit-aux.cod-esp
                           tt-documento.doc-antecip       = b-mov-tit-aux.nr-docto
                           tt-documento.parc-antecip      = b-mov-tit-aux.parcela
                           tt-documento.observacao        = b-mov-tit-aux.observacao
                           tt-documento.num-id-mov-tit    = b-mov-tit-aux.num-id-mov-tit
                           tt-documento.gr-tt-doc         = rowid(b-mov-tit-aux)
                           tt-documento.vl-liquido        = b-mov-tit-aux.vl-liquido
                           tt-documento.frete             = b-mov-tit-aux.frete
                           tt-documento.frete-me          = b-mov-tit-aux.frete-me
                           tt-documento.diversos          = b-mov-tit-aux.diversos
                           tt-documento.diversos-me       = b-mov-tit-aux.diversos-me.

                    if  tt-documento.modalidade = 0 then
                        assign tt-documento.modalidade = 1.                     
                end.      
            end.
        end.


        run pi-lancamento(input mov-tit.cod-esp,
                          input mov-tit.transacao,  
                          input mov-tit.lancamento,
                          output c-lanc).


        find first tt-documento where tt-documento.num-id-mov-tit  = mov-tit.num-id-mov-tit no-error.
        if  not avail tt-documento then do:           
            create tt-documento.
            assign tt-documento.transacao         = mov-tit.transacao
                   /*tt-documento.lancamento        = mov-tit.lancamento*/
                   tt-documento.c-lanca            = c-lanc                  
                   tt-documento.cod-portador      = mov-tit.cod-portador
                   tt-documento.modalidade        = mov-tit.modalidade
                   tt-documento.dt-trans          = mov-tit.dt-trans
                   tt-documento.emit-fita         = mov-tit.emit-fita
                   tt-documento.arq-escritural    = mov-tit.arq-escritural
                   tt-documento.contabilizou      = mov-tit.contabilizou
                   tt-documento.flag-contab       = mov-tit.flag-contab
                   tt-documento.dt-baixa          = mov-tit.dt-baixa
                   tt-documento.dt-credito        = mov-tit.dt-credito
                   tt-documento.cotacao-dia       = mov-tit.cotacao-dia
                   tt-documento.vl-var-monet      = mov-tit.vl-var-monet
                   tt-documento.baixa-subs        = mov-tit.baixa-subs
                   tt-documento.estorna           = mov-tit.estorna
                   tt-documento.mo-codigo         = mov-tit.mo-codigo
                   tt-documento.vl-desconto       = mov-tit.vl-desconto
                   tt-documento.vl-desconto-me    = mov-tit.vl-desconto-me
                   tt-documento.vl-desconto       = mov-tit.vl-desconto
                   tt-documento.banco             = mov-tit.banco
                   tt-documento.vl-juros-rec      = mov-tit.vl-juros-rec
                   tt-documento.vl-juros-rec-me   = mov-tit.vl-juros-rec-me
                   &if  "{&ems2movme_version}" >= "2.02" &then
                    tt-documento.vl-cart-receb     = mov-tit.vl-desp-cart-recebidas
                   &endif
                   tt-documento.agencia           = mov-tit.agencia
                   tt-documento.vl-baixa          = if  mov-tit.transacao = 14 
                                                    or  mov-tit.transacao = 4 then 
                                                        mov-tit.vl-original
                                                    else
                                                        mov-tit.vl-baixa
                   tt-documento.vl-baixa-me       = if  mov-tit.transacao = 14
                                                    or  mov-tit.transacao = 4 then 
                                                        mov-tit.vl-original-me
                                                    else
                                                        mov-tit.vl-baixa-me
                   tt-documento.vl-antecip        = mov-tit.vl-antecip
                   tt-documento.vl-antecip-me     = mov-tit.vl-antecip-me
                   tt-documento.vl-abatimen       = mov-tit.vl-abatimen
                   tt-documento.vl-abatimen-me    = mov-tit.vl-abatimen-me
                   tt-documento.cta-corrente      = mov-tit.cta-corrente
                   tt-documento.vl-desp-banc      = mov-tit.vl-desp-banc
                   tt-documento.vl-desp-banc-me   = mov-tit.vl-desp-banc-me
                   tt-documento.cob-banc-eletr    = titulo.cob-banc-eletr
                   tt-documento.vl-desp-financ    = mov-tit.vl-desp-financ
                   tt-documento.vl-desp-financ-me = mov-tit.vl-desp-financ-me
                   tt-documento.nr-docto-emp      = mov-tit.nr-docto-emp
                   tt-documento.vl-iof            = mov-tit.vl-iof
                   tt-documento.vl-iof-me         = mov-tit.vl-iof-me
                   tt-documento.referencia        = mov-tit.referencia
                   tt-documento.vl-multa          = mov-tit.vl-multa
                   tt-documento.vl-multa-me       = mov-tit.vl-multa-me
                   tt-documento.usuario           = mov-tit.usuario
                   tt-documento.dt-today          = mov-tit.dt-today
                   tt-documento.c-time            = mov-tit.c-time
                   tt-documento.serie             = mov-tit.serie
                   tt-documento.cod-esp           = mov-tit.cod-esp
                   tt-documento.nr-docto          = mov-tit.nr-docto
                   tt-documento.parcela           = mov-tit.parcela
                   tt-documento.serie-antecip     = mov-tit.serie-antecip
                   tt-documento.esp-antecip       = mov-tit.esp-antecip
                   tt-documento.doc-antecip       = mov-tit.doc-antecip
                   tt-documento.parc-antecip      = mov-tit.parc-antecip
                   tt-documento.observacao        = mov-tit.observacao
                   tt-documento.num-id-mov-tit    = mov-tit.num-id-mov-tit
                   tt-documento.gr-tt-doc         = rowid(mov-tit)
                   tt-documento.vl-liquido        = mov-tit.vl-liquido
                   tt-documento.frete             = mov-tit.frete
                   tt-documento.frete-me          = mov-tit.frete-me
                   tt-documento.diversos          = mov-tit.diversos
                   tt-documento.diversos-me       = mov-tit.diversos-me.

            if  tt-documento.modalidade = 0 then
                assign tt-documento.modalidade = 1.

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

def input  parameter c-codesp      like mov-tit.cod-esp  no-undo.
def input  parameter i-transacao   like mov-tit.transacao  no-undo.
def input  parameter i-lancamento  like mov-tit.lancamento no-undo.
def output parameter c-sinal   as char format "x(01)" no-undo.

    assign c-sinal = " ".

    find esp-doc where esp-doc.cod-esp = c-codesp no-lock no-error.       

    if  avail esp-doc then do:

        if i-transacao = 13 then do:
              if (esp-doc.tipo = 1  or
                  esp-doc.tipo = 4  or
                  esp-doc.tipo = 6  or
                  esp-doc.tipo = 7  or
                  esp-doc.tipo = 9  ) then do:
                  if (i-lancamento = 2) then 
                     assign c-sinal = "+".               
                  else 
                     if (i-lancamento = 1) then 
                         assign c-sinal = "-".
              end.           
              else do:              
                   if (esp-doc.tipo = 2  or esp-doc.tipo = 5) then do:                   
                      if (i-lancamento = 2)  then 
                          assign c-sinal = "-".
                      else  
                         if (i-lancamento = 1) then 
                            assign c-sinal = "+".                                                        
                   end.    
              end.
        end.

        if i-transacao = 18 then do:
              if (esp-doc.tipo = 4)  then 
                  assign c-sinal = "-".               
              else do:
                   if (esp-doc.tipo = 5) then 
                      assign c-sinal = "+".
              end.
        end.

        if (i-transacao = 14 or i-transacao = 4) then
             assign c-sinal = "+".


        if (i-transacao = 2  or
            i-transacao = 3  or
            i-transacao = 21 or
            i-transacao = 22 or
            i-transacao = 23) then
               assign c-sinal = "-".
        if (i-transacao = 1 and esp-doc.tipo = 2) then
            assign c-sinal = "-".      
    end.
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

