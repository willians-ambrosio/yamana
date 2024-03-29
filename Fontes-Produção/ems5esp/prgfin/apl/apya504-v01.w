&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

/* Chamada a include do gerenciador de licen�as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m�dulo>:  Informar qual o m�dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m�dulo dever� ser MUT                    */

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

DEF NEW GLOBAL SHARED VAR  v_rec_imposto AS RECID format ">>>>>>9":U initial ? no-undo.

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
&Scoped-define EXTERNAL-TABLES es_produt_financ
&Scoped-define FIRST-EXTERNAL-TABLE es_produt_financ


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_produt_financ.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_produt_financ.cod_imposto ~
es_produt_financ.log_contrato es_produt_financ.log_drawdown ~
es_produt_financ.log_rof es_produt_financ.log_cambio 
&Scoped-define ENABLED-TABLES es_produt_financ
&Scoped-define FIRST-ENABLED-TABLE es_produt_financ
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es_produt_financ.cod_produt_financ ~
es_produt_financ.cod_imposto es_produt_financ.log_contrato ~
es_produt_financ.log_drawdown es_produt_financ.log_rof ~
es_produt_financ.log_cambio 
&Scoped-define DISPLAYED-TABLES es_produt_financ
&Scoped-define FIRST-DISPLAYED-TABLE es_produt_financ
&Scoped-Define DISPLAYED-OBJECTS fi-descricao fi-descricao-imp 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_produt_financ.cod_produt_financ 
&Scoped-define ADM-ASSIGN-FIELDS es_produt_financ.cod_imposto ~
es_produt_financ.log_contrato es_produt_financ.log_drawdown ~
es_produt_financ.log_rof 
&Scoped-define ADM-MODIFY-FIELDS es_produt_financ.cod_imposto ~
es_produt_financ.log_contrato es_produt_financ.log_drawdown ~
es_produt_financ.log_rof 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod_produt_financ|y|y|mgesp.es_produt_financ.cod_produt_financ
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod_produt_financ",
     Keys-Supplied = "cod_produt_financ"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descricao-imp AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.42.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_produt_financ.cod_produt_financ AT ROW 1.25 COL 21.57 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-descricao AT ROW 1.25 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     es_produt_financ.cod_imposto AT ROW 2.21 COL 21.57 COLON-ALIGNED WIDGET-ID 8
          LABEL "Cod. Impt. de Renda"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     fi-descricao-imp AT ROW 2.21 COL 27.86 COLON-ALIGNED HELP
          "Descri��o Imposto" NO-LABEL WIDGET-ID 10
     es_produt_financ.log_contrato AT ROW 3.67 COL 23.72 WIDGET-ID 4
          LABEL "Vincula produto no contrato m�e?"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .83
     es_produt_financ.log_drawdown AT ROW 4.54 COL 23.72 WIDGET-ID 12
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .83 TOOLTIP "Contrato Drawdown obrigat�rio?"
     es_produt_financ.log_rof AT ROW 5.42 COL 23.72 WIDGET-ID 14
          LABEL "ROF"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .83 TOOLTIP "Contrato ROF obrigat�rio?"
     es_produt_financ.log_cambio AT ROW 6.38 COL 23.72 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .83 TOOLTIP "Contrato possui c�mbio?"
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es_produt_financ
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
         HEIGHT             = 6.58
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

/* SETTINGS FOR FILL-IN es_produt_financ.cod_imposto IN FRAME f-main
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN es_produt_financ.cod_produt_financ IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-descricao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao-imp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX es_produt_financ.log_contrato IN FRAME f-main
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR TOGGLE-BOX es_produt_financ.log_drawdown IN FRAME f-main
   2 3                                                                  */
/* SETTINGS FOR TOGGLE-BOX es_produt_financ.log_rof IN FRAME f-main
   2 3 EXP-LABEL                                                        */
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

&Scoped-define SELF-NAME es_produt_financ.cod_imposto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_imposto V-table-Win
ON F5 OF es_produt_financ.cod_imposto IN FRAME f-main /* Cod. Impt. de Renda */
DO:
  
    RUN prgint/utb/utb085ka.p.

    IF v_rec_imposto <> ? 
    THEN DO:

        FIND imposto WHERE RECID(imposto) = v_rec_imposto NO-LOCK NO-ERROR.
        IF AVAIL imposto THEN DO:
            ASSIGN es_produt_financ.cod_imposto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = imposto.cod_imposto
                   fi-descricao-imp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = imposto.des_imposto.
        END.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_imposto V-table-Win
ON LEAVE OF es_produt_financ.cod_imposto IN FRAME f-main /* Cod. Impt. de Renda */
DO:
  
     {include/leave.i &tabela=imposto
                    &atributo-ref=des_imposto
                    &variavel-ref=fi-descricao-imp
                    &where="imposto.cod_imposto = input frame {&frame-name} es_produt_financ.cod_imposto"}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_imposto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_produt_financ.cod_imposto IN FRAME f-main /* Cod. Impt. de Renda */
DO:
   APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_produt_financ.cod_produt_financ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_produt_financ V-table-Win
ON ENTRY OF es_produt_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
  
    FIND FIRST produt_financ NO-LOCK
         WHERE produt_financ.cod_produt_financ = input frame {&frame-name} es_produt_financ.cod_produt_financ NO-ERROR.
            IF AVAIL produt_financ 
                THEN es_produt_financ.cod_imposto:SENSITIVE IN FRAME {&FRAME-NAME} = produt_financ.log_incid_impto_apl.
                ELSE es_produt_financ.cod_imposto:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_produt_financ V-table-Win
ON F5 OF es_produt_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
   {include/zoomvar.i &prog-zoom="prgfin/apl/apya504-z02.w"
                      &campo=es_produt_financ.cod_produt_financ
                      &campozoom=cod_produt_financ
                      &campo2=fi-descricao
                      &campozoom2=des_produt_financ}   
                       
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_produt_financ V-table-Win
ON LEAVE OF es_produt_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
  
   {include/leave.i &tabela=produt_financ
                    &atributo-ref=des_produt_financ
                    &variavel-ref=fi-descricao
                    &where="produt_financ.cod_produt_financ = input frame {&frame-name} es_produt_financ.cod_produt_financ"}
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_produt_financ.cod_produt_financ V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_produt_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
  APPLY "F5" TO SELF.
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
    es_produt_financ.cod_produt_financ:LOAD-MOUSE-POINTER("image/lupa.cur").
    es_produt_financ.cod_imposto:LOAD-MOUSE-POINTER("image/lupa.cur").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cod_produt_financ':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = es_produt_financ
           &WHERE = "WHERE es_produt_financ.cod_produt_financ eq key-value"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "es_produt_financ"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_produt_financ"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as valida��es */
    /*:T N�o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

        RUN pi-validate.

 if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assign�s n�o feitos pelo assign-record devem ser feitos aqui */  
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */ 
     APPLY "leave":U         TO es_produt_financ.cod_produt_financ IN FRAME {&FRAME-NAME}. 
     APPLY "leave":U         TO es_produt_financ.cod_imposto       IN FRAME {&FRAME-NAME}. 
    


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
    
       FIND FIRST produt_financ NO-LOCK
       WHERE produt_financ.cod_produt_financ = input frame {&frame-name} es_produt_financ.cod_produt_financ NO-ERROR.
    IF AVAIL produt_financ 
        THEN es_produt_financ.cod_imposto:SENSITIVE IN FRAME {&FRAME-NAME} = produt_financ.log_incid_impto_apl.
        ELSE es_produt_financ.cod_imposto:SENSITIVE IN FRAME {&FRAME-NAME} = NO. 
     

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
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N�o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida��es, pois neste ponto do programa o registro 
  ainda n�o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida��o de dicion�rio */


   IF adm-new-record THEN DO:
      IF NOT CAN-FIND(FIRST produt_financ
                      WHERE produt_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_produt_financ
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i ems5 produt_financ 1}
         run utp/ut-msgs.p (input "show":U, input 2, input return-value).
         return 'ADM-ERROR':U.                                           

      END.

      IF CAN-FIND(FIRST es_produt_financ
                  WHERE es_produt_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_produt_financ
                  NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i mgesp es_produt_financ 1}
         run utp/ut-msgs.p (input "show":U, input 1, input return-value).
         return 'ADM-ERROR':U.                                           

      END.

      /* ---- */
      
      IF NOT CAN-FIND(FIRST imposto NO-LOCK
                      WHERE imposto.cod_imposto = INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_imposto
                      ) AND INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_imposto  <> "" THEN DO:

          {include/i-vldprg.i}
          {utp/ut-table.i ms5 imposto 1}
          run utp/ut-msgs.p (input "show":U, input 3459, INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_imposto + " ").
          return 'ADM-ERROR':U.  

      END.
      
      IF CAN-FIND(FIRST produt_financ
                  WHERE produt_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_produt_financ
                  AND   produt_financ.log_incid_impto_apl = NO
                  NO-LOCK)
         AND INPUT FRAME {&FRAME-NAME} es_produt_financ.cod_imposto  <> "" THEN DO:

          {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show":U, input 17006, "Produto n�o incide imposto~~Imposto s� poder ser cadastrado no produto").
          return 'ADM-ERROR':U.

      END.
           


      /* ---- */

   END.
    
/*:T    Segue um exemplo de valida��o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod_produt_financ" "es_produt_financ" "cod_produt_financ"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "es_produt_financ"}

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

