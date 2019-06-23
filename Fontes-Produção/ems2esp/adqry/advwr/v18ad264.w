&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
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
{include/i-prgvrs.i V18AD264 2.00.00.003}  /*** 010003 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i V18AD264 MUT}
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
def var i-equal-fec  as int format "9" no-undo.
def var l-vendor     as logical        no-undo.

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
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-3 RECT-1 RECT-2 c-data-base ~
c-equalizacao c-tx-negociada c-vl-vencimento 
&Scoped-Define DISPLAYED-OBJECTS c-nr-planilha dt-vencto da-fechamento ~
de-iof i-cod-cond-cli c-desc-cond-pagto i-dias-base c-equal-fec da-base ~
de-equalizacao de-tx-cli de-vl-cli de-tx-ban de-vl-ban c-data-base ~
c-equalizacao c-tx-negociada c-vl-vencimento 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS de-iof 

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
DEFINE VARIABLE c-data-base AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE c-desc-cond-pagto AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE c-equal-fec AS CHARACTER FORMAT "X(10)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-equalizacao AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17.72 BY .67 NO-UNDO.

DEFINE VARIABLE c-nr-planilha AS CHARACTER FORMAT "X(16)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 21.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-tx-negociada AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 18.86 BY .67 NO-UNDO.

DEFINE VARIABLE c-vl-vencimento AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20.57 BY .67 NO-UNDO.

DEFINE VARIABLE da-base AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE da-fechamento AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE de-equalizacao AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE de-iof AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE de-tx-ban AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE de-tx-cli AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-ban AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-cli AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE dt-vencto AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-cond-cli AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-dias-base AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25.14 BY 2.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.67.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25.14 BY 2.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     c-nr-planilha AT ROW 1.17 COL 22.43 COLON-ALIGNED
     dt-vencto AT ROW 1.17 COL 57.43 COLON-ALIGNED
     da-fechamento AT ROW 2.17 COL 22.43 COLON-ALIGNED
     de-iof AT ROW 2.17 COL 50.43 COLON-ALIGNED
     i-cod-cond-cli AT ROW 3.17 COL 22.43 COLON-ALIGNED
     c-desc-cond-pagto AT ROW 3.17 COL 27.57 COLON-ALIGNED NO-LABEL
     i-dias-base AT ROW 5.17 COL 22.43 COLON-ALIGNED
     c-equal-fec AT ROW 5.17 COL 47.57 COLON-ALIGNED
     da-base AT ROW 6.17 COL 22.43 COLON-ALIGNED
     de-equalizacao AT ROW 6.17 COL 47.57 COLON-ALIGNED
     de-tx-cli AT ROW 8.33 COL 22.43 COLON-ALIGNED
     de-vl-cli AT ROW 8.33 COL 47.57 COLON-ALIGNED
     de-tx-ban AT ROW 9.33 COL 22.43 COLON-ALIGNED
     de-vl-ban AT ROW 9.33 COL 47.57 COLON-ALIGNED
     c-data-base AT ROW 4.33 COL 12.72 COLON-ALIGNED NO-LABEL
     c-equalizacao AT ROW 4.33 COL 39.57 COLON-ALIGNED NO-LABEL
     c-tx-negociada AT ROW 7.5 COL 12.72 COLON-ALIGNED NO-LABEL
     c-vl-vencimento AT ROW 7.5 COL 39.57 COLON-ALIGNED NO-LABEL
     RECT-4 AT ROW 4.67 COL 13.57
     RECT-3 AT ROW 4.67 COL 40.43
     RECT-1 AT ROW 7.83 COL 13.57
     RECT-2 AT ROW 7.83 COL 40.43
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
         HEIGHT             = 9.75
         WIDTH              = 82.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-desc-cond-pagto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-equal-fec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-planilha IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN da-base IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN da-fechamento IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-equalizacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-iof IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN de-tx-ban IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-tx-cli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-vl-ban IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-vl-cli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-vencto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-cod-cond-cli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-dias-base IN FRAME f-main
   NO-ENABLE                                                            */
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

{include/i-auxtab.i}
{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as valida‡äes */
    /* NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
    /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ).*/
    
    /* Code placed here will execute AFTER standard behavior.    */
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).*/
  if l-vendor then do:
     run vep/ve2028.p(input  rowid(titulo),
                      output c-nr-planilha,
                      output da-fechamento, 
                      output i-equal-fec,   
                      output c-equal-fec,   
                      output de-tx-cli,     
                      output de-tx-ban,    
                      output i-cod-cond-cli,
                      output i-dias-base,   
                      output da-base,       
                      output de-equalizacao,
                      output de-iof,        
                      output de-vl-cli,     
                      output de-vl-ban,
                      output dt-vencto).     
                      
     assign c-desc-cond-pagto = "".
     
    if c-nr-planilha <> " " then 
        assign  dt-vencto  = titulo.dt-vencimen.      
    else                    
        assign  dt-vencto  = ?.
                       
     if i-cod-cond-cli > 0 then do:
        find cond-pagto where
             cond-pagto.cod-cond-pag = i-cod-cond-cli no-lock no-error.
        if avail cond-pagto then             
           assign c-desc-cond-pagto = cond-pagto.descricao.
     end.                  
  end.

  disp c-nr-planilha
       da-fechamento 
       c-equal-fec
       de-tx-cli  
       de-tx-ban  
       i-cod-cond-cli
       c-desc-cond-pagto 
       i-dias-base    
       da-base        
       de-equalizacao 
       de-iof       
       de-vl-cli    
       de-vl-ban    
       c-data-base
       c-equalizacao
       c-tx-negociada
       c-vl-vencimento
       dt-vencto
       with frame {&frame-name}.
  
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
    /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ).*/
    
    /* Code placed here will execute AFTER standard behavior.    */
    if adm-new-record = yes then
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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*------------Vendor------------------------------------------*/
  find first param-global no-lock no-error.
  assign l-vendor = no.
  if (search("vep/ve2028.p") <> ? or search("vep/ve2028.r") <> ?) and param-global.modulo-05 = yes then  
     assign l-vendor = yes.
  /*----------Fim Vendor-----------------------------------------*/
  {utp/ut-liter.i Nr._Planilha * R}
  assign c-nr-planilha:label  in frame {&frame-name} = return-value.
  {utp/ut-liter.i Dt_Vencimento * R}
  assign dt-vencto:label  in frame {&frame-name} = return-value.
  {utp/ut-liter.i Dt._Fechamento * R}
  assign da-fechamento:label  in frame {&frame-name} = return-value.
  {utp/ut-liter.i IOF * R}
  assign de-iof:label         in frame {&frame-name} = return-value.
  {utp/ut-liter.i Cond._Pagto * R}
  assign i-cod-cond-cli:label in frame {&frame-name} = return-value.
  {utp/ut-liter.i Carˆncia * R}
  assign i-dias-base:label    in frame {&frame-name} = return-value.
  {utp/ut-liter.i Data * R}
  assign da-base:label        in frame {&frame-name} = return-value.
  {utp/ut-liter.i Tipo * R}
  assign c-equal-fec:label    in frame {&frame-name} = return-value.
  {utp/ut-liter.i Valor * R}
  assign de-equalizacao:label in frame {&frame-name} = return-value.
  {utp/ut-liter.i Cliente * R}
  assign de-tx-cli:label      in frame {&frame-name} = return-value
         de-vl-cli:label      in frame {&frame-name} = return-value.
  {utp/ut-liter.i Banco * R}
  assign de-tx-ban:label      in frame {&frame-name} = return-value
         de-vl-ban:label      in frame {&frame-name} = return-value.
  {utp/ut-liter.i Data_Base * R}
  assign c-data-base     = return-value.
  {utp/ut-liter.i Equaliza‡Æo * R}
  assign c-equalizacao   = return-value.
  {utp/ut-liter.i Taxas_Negociadas * R}
  assign c-tx-negociada  = return-value.
  {utp/ut-liter.i Valor_Vencimento * R}
  assign c-vl-vencimento = return-value.
  
  assign dt-vencto         =  ?.
  if avail titulo then
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */
    
/*/*    Segue um exemplo de valida‡Æo de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

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


