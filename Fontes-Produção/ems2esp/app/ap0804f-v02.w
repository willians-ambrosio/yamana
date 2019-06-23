&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
{include/i-prgvrs.i AP0804F-V02 2.00.00.005}  /*** 010005 ***/


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

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/*Tratamento para independància de objetos*/
&glob ORIGINALNAME 'advwr~\v08ad260.w'

/* miniflexibilizaªío */
{include/i_dbvers.i}  
{cdp/cdcfgfin.i}

/* global variable definitions */
def new global shared var v-row-parent as rowid no-undo.                                
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def new global shared var gr-titulo  as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES tit-ap
&Scoped-define FIRST-EXTERNAL-TABLE tit-ap


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tit-ap.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tit-ap.referencia tit-ap.dt-emissao ~
tit-ap.dt-transacao tit-ap.portador tit-ap.dias-atraso tit-ap.dt-vencimen ~
tit-ap.perc-mora tit-ap.dt-prev-pag tit-ap.perc-juros tit-ap.dt-desconto ~
tit-ap.moeda tit-ap.vl-orig-me tit-ap.dt-ult-pagto tit-ap.cod-retencao ~
tit-ap.vl-saldo-me tit-ap.dt-fluxo tit-ap.dt-liq tit-ap.tp-codigo 
&Scoped-define ENABLED-TABLES tit-ap
&Scoped-define FIRST-ENABLED-TABLE tit-ap
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-15 RECT-14 RECT-6 RECT-7 RECT-8 ~
RECT-56 RECT-10 
&Scoped-Define DISPLAYED-FIELDS tit-ap.referencia tit-ap.dt-emissao ~
tit-ap.dt-transacao tit-ap.portador tit-ap.dias-atraso tit-ap.dt-vencimen ~
tit-ap.perc-mora tit-ap.dt-prev-pag tit-ap.perc-juros tit-ap.dt-desconto ~
tit-ap.moeda tit-ap.vl-orig-me tit-ap.dt-ult-pagto tit-ap.cod-retencao ~
tit-ap.vl-saldo-me tit-ap.dt-fluxo tit-ap.dt-liq tit-ap.tp-codigo ~
tit-ap.nr-bordero 
&Scoped-define DISPLAYED-TABLES tit-ap
&Scoped-define FIRST-DISPLAYED-TABLE tit-ap
&Scoped-Define DISPLAYED-OBJECTS c-origem c-estado tg-emite c-despesa-rec ~
c-tp-pgto c-favorecido 

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
DEFINE VARIABLE c-despesa-rec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE c-estado AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-favorecido AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 42.72 BY .88.

DEFINE VARIABLE c-origem AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-tp-pgto AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87.43 BY 1.38.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27.57 BY 1.29.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28.86 BY 1.29.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30.29 BY 8.58.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.72 BY 1.33.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28.43 BY 3.42.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27.57 BY 3.42.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.72 BY 2.25.

DEFINE VARIABLE tg-emite AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.43 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     c-origem AT ROW 1.21 COL 43.57 COLON-ALIGNED
     tit-ap.referencia AT ROW 1.21 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     tit-ap.dt-emissao AT ROW 1.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.dt-transacao AT ROW 2.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.portador AT ROW 2.63 COL 43.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     tit-ap.dias-atraso AT ROW 2.63 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .88
     tit-ap.dt-vencimen AT ROW 3.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.perc-mora AT ROW 3.63 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     c-estado AT ROW 3.67 COL 43.72 COLON-ALIGNED
     tit-ap.dt-prev-pag AT ROW 4.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.perc-juros AT ROW 4.63 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.72 BY .88
     tg-emite AT ROW 4.71 COL 45.72
     tit-ap.dt-desconto AT ROW 5.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.moeda AT ROW 6.04 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .88
     tit-ap.vl-orig-me AT ROW 6.04 COL 67.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .88
     tit-ap.dt-ult-pagto AT ROW 6.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.cod-retencao AT ROW 7.04 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     tit-ap.vl-saldo-me AT ROW 7.04 COL 67.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     tit-ap.dt-fluxo AT ROW 7.29 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.dt-liq AT ROW 8.25 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .88
     tit-ap.tp-codigo AT ROW 8.5 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     c-despesa-rec AT ROW 8.5 COL 51.86 COLON-ALIGNED NO-LABEL
     c-tp-pgto AT ROW 9.96 COL 3.72 NO-LABEL
     c-favorecido AT ROW 9.96 COL 42.14 COLON-ALIGNED
     tit-ap.nr-bordero AT ROW 10 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .88
     RECT-2 AT ROW 1 COL 1
     RECT-15 AT ROW 1 COL 31.86
     RECT-14 AT ROW 1 COL 61
     RECT-6 AT ROW 2.38 COL 31.86
     RECT-7 AT ROW 2.38 COL 61
     RECT-8 AT ROW 5.88 COL 31.86
     RECT-56 AT ROW 8.25 COL 31.86
     RECT-10 AT ROW 9.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.tit-ap
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 10.21
         WIDTH              = 87.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-despesa-rec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-estado IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-favorecido IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       c-favorecido:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR FILL-IN c-origem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-tp-pgto IN FRAME f-main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tit-ap.nr-bordero IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       tit-ap.nr-bordero:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tg-emite IN FRAME f-main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME c-tp-pgto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-tp-pgto V-table-Win
ON LEAVE OF c-tp-pgto IN FRAME f-main
DO:
  /*run pi-versao (input 3).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  
  {utp/ut-liter.i Emitiu_AP}
  assign tg-emite:label in frame {&frame-name} = trim(return-value).
  
  {utp/ut-liter.i Favorecido}
  assign c-favorecido:label in frame {&frame-name} = return-value.
  
  {utp/ut-liter.i Situaá∆o}
  assign c-estado:label in frame {&frame-name} = return-value.
  
  {utp/ut-liter.i Origem}
  assign c-origem:label in frame {&frame-name} = return-value.
  
  assign tit-ap.dias-atraso:format in frame {&FRAME-NAME} = "->>9".
    
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
      RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

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
  /* Ponha na pi-validate todas as validaá‰es */
  /* N∆o gravar nada no registro antes do dispatch do assign-record e 
  nem na PI-validate. */  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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
   
  run pi-versao (input 4).
   
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

  
  if  avail tit-ap then do:
  
      IF  tit-ap.estado <> 0 THEN
          assign c-estado = {adinc/i03ad170.i 04 tit-ap.estado}.
      ELSE
          ASSIGN c-estado = "".

      IF  tit-ap.origem <> 0 THEN
          ASSIGN c-origem = {adinc/i01ad170.i 04 tit-ap.origem}.
      ELSE
          ASSIGN c-origem = "".
       
      find FIRST emite-ap 
           where emite-ap.ep-codigo    = tit-ap.ep-codigo
             and emite-ap.cod-estabel  = tit-ap.cod-estabel
             and emite-ap.cod-fornec   = tit-ap.cod-fornec
             and emite-ap.cod-esp      = tit-ap.cod-esp 
             and emite-ap.serie        = tit-ap.serie
             and emite-ap.nr-docto     = tit-ap.nr-docto
             and emite-ap.parcela      = tit-ap.parcela no-lock no-error.

      if avail emite-ap and emite-ap.emitiu = yes then 
         assign tg-emite = yes.
      else 
         assign tg-emite = no.
     
      disp tg-emite with frame {&frame-name} .  

      find tipo-rec-desp 
           where tipo-rec-desp.tp-codigo = tit-ap.tp-codigo no-lock no-error.
           
      if avail tipo-rec-desp then
         assign c-despesa-rec = tipo-rec.descricao.
      else 
         assign c-despesa-rec = "".
  end.        
  
      
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  if  avail tit-ap then do:
      
      run pi-versao (input 2).
              
      /*--- Tratamento multimoeda, n∆o retirar este c¢digo ---*/
      if  tit-ap.moeda = 0 then
          assign tit-ap.vl-saldo-me:screen-value in frame {&FRAME-NAME} = string(tit-ap.valor-saldo).
         
      assign c-tp-pgto:screen-value in frame {&frame-name} = {adinc/i22ad098.i 04 tit-ap.tp-pagto}.
     
      if  tit-ap.dt-liq = 12/31/9999 then
          assign tit-ap.dt-liq:screen-value in frame {&FRAME-NAME} = '?'.
     
  end.
  else do:
      assign c-tp-pgto:screen-value in frame {&FRAME-NAME} = " ".
  end.

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
  run get-attribute ('adm-new-record').
  /* Dispatch standard ADM method.            */
  
    
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  run pi-versao (input 3).

  
  /* Code placed here will execute AFTER standard behavior.    */
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
    
  run pi-versao (input 1).
 
 /* Dispatch standard ADM method.                             */
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
  
  if  avail tit-ap then
      assign gr-titulo = rowid(tit-ap).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
  {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
   
   /*Segue um exemplo de validaá∆o de programa 
 *    find tabela where tabela.campo1 = c-variavel and
 *                      tabela.campo2 > i-variavel no-lock no-error.
 *    {include/i-vldprg.i} /* Este include deve ser colocado sempre antes do ut-msgs.p */                  
 *    run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *    return 'ADM-ERROR':U.*/
 
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
            when 1 then do: /* initialize */
                assign tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = yes
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no
                       c-favorecido:visible        in frame {&FRAME-NAME} = no
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no.
            end.
            when 2 then do: /* display */
                assign tit-ap.nr-bordero:screen-value in frame {&FRAME-NAME} = string(tit-ap.nr-bordero).
            end.
            when 3 then do: /* enable */
                assign tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = yes
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no
                       c-favorecido:visible        in frame {&FRAME-NAME} = no
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no.
            end.
            when 4 then do: /* disable */
                assign tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = yes
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no
                       c-favorecido:visible        in frame {&FRAME-NAME} = no
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no.
            end.
        end case.    
    &endif.


    &if "{&mgadm_version}" >= "2.02" &then    
       case p-ind:
            when 1 then do: 
                assign c-favorecido:visible        in frame {&FRAME-NAME} = yes
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no.
            end.
            when 2 then do:
                assign c-favorecido:screen-value in frame {&FRAME-NAME} = tit-ap.favorecido.
            end. 
            when 3 then do: 
                assign c-favorecido:visible        in frame {&FRAME-NAME} = yes
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no.
            end.
            when 4 then do: /* disable */
                assign c-favorecido:visible        in frame {&FRAME-NAME} = yes
                       c-favorecido:sensitive      in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:visible   in frame {&FRAME-NAME} = no
                       tit-ap.nr-bordero:sensitive in frame {&FRAME-NAME} = no.
            end.
        end case.    
    &endif.

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
  {src/adm/template/snd-list.i "tit-ap"}

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

