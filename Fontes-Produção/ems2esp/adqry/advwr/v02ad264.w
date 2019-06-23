&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
          movadm           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer cotacao              for ems2cadme.cotacao.
def buffer moeda                for ems2cadme.moeda.
def buffer histor_exec_especial for ems2cadme.histor_exec_especial.

{include/i-prgvrs.i V02AD264 2.00.00.026}  /*** 010026 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i V02AD264 MUT}
&ENDIF



/*------------------------------------------------------------------------
  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

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
{include/i_dbvers.i}

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var c-lista-tipo-origem     as char    no-undo.
def var c-lista-tipo-modalidade as char    no-undo.
def var c-lista-tipo-situacao   as char    no-undo.
def var de-dolar                as decimal no-undo.
def var l-ok-ecl                as logical no-undo.

def new global shared var gr-titulo  as rowid no-undo.

{cdp/cdcfgfin.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES titulo
&Scoped-define FIRST-EXTERNAL-TABLE titulo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR titulo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS titulo.dt-emissao titulo.dt-fluxo ~
titulo.mo-codigo titulo.dt-vencimen titulo.vl-original-me ~
titulo.dt-ult-pagto titulo.vl-saldo-me titulo.dt-pg-prev ~
titulo.vl-liquido-me titulo.dt-liq titulo.dt-desconto titulo.dt-multa ~
titulo.vl-desconto-me titulo.perc-multa titulo.perc-juros 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}dt-emissao ~{&FP2}dt-emissao ~{&FP3}~
 ~{&FP1}dt-fluxo ~{&FP2}dt-fluxo ~{&FP3}~
 ~{&FP1}mo-codigo ~{&FP2}mo-codigo ~{&FP3}~
 ~{&FP1}dt-vencimen ~{&FP2}dt-vencimen ~{&FP3}~
 ~{&FP1}vl-original-me ~{&FP2}vl-original-me ~{&FP3}~
 ~{&FP1}dt-ult-pagto ~{&FP2}dt-ult-pagto ~{&FP3}~
 ~{&FP1}vl-saldo-me ~{&FP2}vl-saldo-me ~{&FP3}~
 ~{&FP1}dt-pg-prev ~{&FP2}dt-pg-prev ~{&FP3}~
 ~{&FP1}vl-liquido-me ~{&FP2}vl-liquido-me ~{&FP3}~
 ~{&FP1}dt-liq ~{&FP2}dt-liq ~{&FP3}~
 ~{&FP1}dt-desconto ~{&FP2}dt-desconto ~{&FP3}~
 ~{&FP1}dt-multa ~{&FP2}dt-multa ~{&FP3}~
 ~{&FP1}vl-desconto-me ~{&FP2}vl-desconto-me ~{&FP3}~
 ~{&FP1}perc-multa ~{&FP2}perc-multa ~{&FP3}~
 ~{&FP1}perc-juros ~{&FP2}perc-juros ~{&FP3}
&Scoped-define ENABLED-TABLES titulo
&Scoped-define FIRST-ENABLED-TABLE titulo
&Scoped-Define DISPLAYED-FIELDS titulo.dt-emissao titulo.dt-fluxo ~
titulo.mo-codigo titulo.dt-vencimen titulo.vl-original-me ~
titulo.dt-ult-pagto titulo.vl-saldo-me titulo.dt-pg-prev ~
titulo.vl-liquido-me titulo.dt-liq titulo.dt-desconto titulo.dt-multa ~
titulo.vl-desconto-me titulo.perc-multa titulo.perc-juros 
&Scoped-Define DISPLAYED-OBJECTS fi-origem c-descricao-moeda ~
de-vl-orig-fasb de-vl-sal-fasb fi-situacao de-perc-desc-an l-e-collect 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-descricao-moeda AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .88.

DEFINE VARIABLE de-perc-desc-an AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-orig-fasb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vl original FASB" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-sal-fasb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Vl saldo FASB" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE fi-origem AS CHARACTER FORMAT "X(15)":U 
     LABEL "Origem" 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-situacao AS CHARACTER FORMAT "X(15)":U 
     LABEL "Situaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE l-e-collect AS LOGICAL INITIAL no 
     LABEL "Enviado via e-Collect" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.72 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     titulo.dt-emissao AT ROW 1 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     fi-origem AT ROW 1.04 COL 19.43 COLON-ALIGNED
     titulo.dt-fluxo AT ROW 2 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     titulo.mo-codigo AT ROW 2.04 COL 19.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .88
     c-descricao-moeda AT ROW 2.04 COL 23.57 COLON-ALIGNED NO-LABEL
     titulo.dt-vencimen AT ROW 3 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     titulo.vl-original-me AT ROW 3.04 COL 19.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     titulo.dt-ult-pagto AT ROW 4 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     titulo.vl-saldo-me AT ROW 4.04 COL 19.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     titulo.dt-pg-prev AT ROW 5 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     titulo.vl-liquido-me AT ROW 5.04 COL 19.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     titulo.dt-liq AT ROW 6 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     de-vl-orig-fasb AT ROW 6.04 COL 19.43 COLON-ALIGNED
     titulo.dt-desconto AT ROW 7 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     de-vl-sal-fasb AT ROW 7.04 COL 19.43 COLON-ALIGNED
     titulo.dt-multa AT ROW 8 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     titulo.vl-desconto-me AT ROW 8.04 COL 19.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     titulo.perc-multa AT ROW 9 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     fi-situacao AT ROW 9.04 COL 19.43 COLON-ALIGNED
     titulo.perc-juros AT ROW 10 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     de-perc-desc-an AT ROW 10.04 COL 19.43 COLON-ALIGNED
     l-e-collect AT ROW 10.04 COL 34.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.titulo
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 11
         WIDTH              = 84.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN        FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-descricao-moeda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-perc-desc-an IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN        de-perc-desc-an:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR FILL-IN de-vl-orig-fasb IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-vl-sal-fasb IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-origem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX l-e-collect IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN        l-e-collect:HIDDEN IN FRAME f-main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao001 = {adinc/i01ad264.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
      ASSIGN  c-lista-tipo-origem     = cAuxTraducao001.
  &else
      ASSIGN c-lista-tipo-origem     = {adinc/i01ad264.i 03}.
  &endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao002 = {adinc/i04ad264.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao002).
      ASSIGN  c-lista-tipo-modalidade = cAuxTraducao002.
  &else
      ASSIGN c-lista-tipo-modalidade = {adinc/i04ad264.i 03}.
  &endif
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      DEFINE VARIABLE cAuxTraducao003 AS CHARACTER NO-UNDO.
      ASSIGN cAuxTraducao003 = {adinc/i03ad264.i 03}.
      RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao003).
      ASSIGN  c-lista-tipo-situacao   = cAuxTraducao003.
  &else
      ASSIGN c-lista-tipo-situacao   = {adinc/i03ad264.i 03}.
  &endif

  {utp/ut-field.i mgadm titulo origem 1}
  assign fi-origem:label in frame {&frame-name} = return-value.

  {utp/ut-field.i mgadm titulo situacao 1}
  assign fi-situacao:label in frame {&frame-name} = return-value.

  {utp/ut-liter.i Valor_Original_FASB}
  assign de-vl-orig-fasb:label in frame {&frame-name} = return-value.

  {utp/ut-liter.i Valor_Saldo_FASB}
  assign de-vl-sal-fasb:label in frame {&frame-name} = return-value.



  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscar-cotacao-moeda V-table-Win 
PROCEDURE buscar-cotacao-moeda :
/******************************************************************************
**
**  Objetivo: Buscar a Cotacao de Uma moeda em determinada data.
**
******************************************************************************/
    def var i-moeda-fasb like param-fasb.moeda-fasb.

    if  param-fasb.moeda-fasb <> 0 then do:
        find cotacao
            where cotacao.mo-codigo   = param-fasb.moeda-fasb
            and   cotacao.ano-periodo = string(year(titulo.dt-emissao)) + string(month(titulo.dt-emissao), "99")
            and   cotacao.cotacao[int(day(titulo.dt-emissao))] <> 0 no-lock no-error.

        if  avail cotacao then do:

            assign de-dolar = cotacao.cotacao[int(day(titulo.dt-emissao))].

        end.

        if avail param-fasb then do:
            assign i-moeda-fasb = param-fasb.moeda-fasb.
            if  de-dolar = 0 then do:

                 run utp/ut-msgs.p (input "show",
                                   input 58,
                                   input string(i-moeda-fasb)).
            end.
        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-valor-saldo-moeda-fasb V-table-Win 
PROCEDURE calc-valor-saldo-moeda-fasb :
/******************************************************************************
**
**  Objetivo: Calcular o valor do saldo em moeda fasb.
**  - Substitui a include - CR0709.i
**
****************************************************************************/

    find first param-fasb 
         where param-fasb.ep-codigo = titulo.ep-codigo
         no-lock no-error.

    if  avail param-fasb then do:

        if  titulo.tipo = 2 then do: /* Antecipaá∆o */
            assign de-dolar = titulo.vl-sal-fasb / 100.
        end.
        else do:
            if  param-fasb.tipo-calculo = 0 then do:
                run buscar-cotacao-moeda.
                assign de-dolar = de-dolar * (titulo.vl-sal-fasb / titulo.vl-orig-fasb).
            end.
            else do:
                assign de-dolar = titulo.vl-sal-fasb / 100.
            end.
        end.
        if  de-dolar <> 0 and de-dolar <> ? then do:
            assign de-vl-sal-fasb  = round((titulo.vl-saldo / de-dolar), 2)
                   de-vl-orig-fasb = titulo.vl-original / de-dolar.
        end.
        else do:
               assign de-vl-sal-fasb  = 0
                      de-vl-orig-fasb = 0.
        end.

        find first mov-tit of titulo
             where mov-tit.transacao = 14
             or    mov-tit.transacao = 4 
             no-lock no-error.
        if avail mov-tit then 
           if titulo.tipo <> 3 then /* Previs∆o */
              assign de-vl-orig-fasb = mov-tit.vl-orig-fasb.
    end.

    if de-vl-orig-fasb = ?
       then assign de-vl-orig-fasb = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  &if  defined(ADM-MODIFY-FIELDS) &then
      disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  if  avail titulo then do:
      assign fi-origem     = entry(titulo.origem,     c-lista-tipo-origem)
             fi-situacao   = entry(titulo.situacao,   c-lista-tipo-situacao).

      find first moeda
           where moeda.mo-codigo = titulo.mo-codigo no-lock no-error.

       if  avail moeda then do:
           assign c-descricao-moeda = moeda.descricao.
       end.

       run calc-valor-saldo-moeda-fasb.
  end.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  if  avail titulo then do with frame {&FRAME-NAME}:

      if  titulo.dt-liq = 12/31/9999 then
          assign titulo.dt-liq:screen-value = ?.
      else  
          assign titulo.dt-liq:screen-value = string(titulo.dt-liq).

      if  titulo.mo-codigo = 0 then 
          assign titulo.vl-saldo-me:screen-value = string(titulo.vl-saldo).

  end.
  run pi-versao(2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  &if  defined(ADM-MODIFY-FIELDS) &then
      enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
  &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  run pi-versao(1). 

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-versao V-table-Win 
PROCEDURE pi-versao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param p-ind as int no-undo.

    &if "{&mgadm_version}" < "2.02" &then    
        case p-ind:
            when 1 then  /* initialize */
                 assign de-perc-desc-an:visible in frame {&FRAME-NAME} = no.                
        end case.    
    &endif
    &if "{&mgadm_version}" >= "2.02" &then 
        vers_block:
        do:
            find first histor_exec_especial no-lock
                 where histor_exec_especial.cod_modul_dtsul = 'CRP'
                and   histor_exec_especial.cod_prog_dtsul  = 'EMS_202_DESCTO_ANTECIPACAO' no-error.
            if   not  avail histor_exec_especial then do:
                case p-ind:
                    when 1 then  /* initialize */
                     assign de-perc-desc-an:visible in frame {&FRAME-NAME} = no.
                end case.
                leave vers_block.  
            end.    
            case p-ind:
                when 1 then  /* initialize */
                do:
                     {utp/ut-liter.i %_Descto_dia_Antecipado * l}.
                     assign de-perc-desc-an:visible in frame {&FRAME-NAME} = yes
                            de-perc-desc-an:label in frame {&FRAME-NAME} = RETURN-VALUE.
                end.       
                when 2 then /* display fields */
                    if avail titulo then
                        assign de-perc-desc-an:screen-value in frame {&FRAME-NAME} = string(titulo.perc-desc-an).
            end case.    
        end.
    &endif

    /* Testa liberaá∆o do M¢dulo e-Collect e Atualiza flag da tela */
    &IF DEFINED(BF_FIN_MOD_E_COLLECT) &THEN
        case p-ind:
           when 1 then do:
              run cdp/cdapi556.p (output l-ok-ecl).

              if l-ok-ecl then
                 assign l-e-collect:visible in frame {&FRAME-NAME} = YES.
           end.
           when 2 then
              if l-ok-ecl and avail titulo then
                 assign l-e-collect:checked in frame {&FRAME-NAME} = if avail titulo and titulo.arq-remessa = "e-Collect":U
                                                                     then yes
                                                                     else no.
        end.
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "titulo"}

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


