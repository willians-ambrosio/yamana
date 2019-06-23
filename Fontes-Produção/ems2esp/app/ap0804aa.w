&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME ap0804d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ap0804d 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda for ems2cadme.moeda.

{include/i-prgvrs.i AP0804AA 2.00.00.005}  /*** 010005 ***/


/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

def input-output param p-matriz          as logical   no-undo.
def input-output param p-aberto          as logical   no-undo.
def input-output param p-faixa           as logical   no-undo.
def input-output param p-cod-moeda       as integer   no-undo.
def input-output param p-cod-estabel     as character no-undo.
def input-output param p-cod-estabel-ini as character no-undo.
def input-output param p-cod-estabel-fim as character no-undo.
def input-output param p-cod-esp-ini     as character no-undo.
def input-output param p-cod-esp-fim     as character no-undo.
def input-output param p-portador-ini    as integer   no-undo.
def input-output param p-portador-fim    as integer   no-undo.
def input-output param p-dt-emissao-ini  as date      no-undo.
def input-output param p-dt-emissao-fim  as date      no-undo.
def input-output param p-dt-conversao    as date      no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME ap0804d

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 l-matriz l-aberto l-faixa RECT-2 ~
i-moeda c-cod-estabel RECT-4 cod-est-ini IMAGE-46 IMAGE-47 cod-est-fim ~
cod-esp-ini IMAGE-4 IMAGE-5 cod-esp-fim cod-port-ini IMAGE-44 IMAGE-45 ~
cod-port-fim dt-emissao-ini IMAGE-6 IMAGE-7 dt-emissao-fim rt-buttom bt-ok ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS l-matriz l-aberto l-faixa i-moeda ~
c-descricao da-conversao c-cod-estabel c-desc-estabel cod-est-ini ~
cod-est-fim cod-esp-ini cod-esp-fim cod-port-ini cod-port-fim ~
dt-emissao-ini dt-emissao-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estabel AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 33.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.29 BY .88 NO-UNDO.

DEFINE VARIABLE cod-esp-fim AS CHARACTER FORMAT "!!" INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE cod-esp-ini AS CHARACTER FORMAT "!!" INITIAL "AA" 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE cod-est-fim AS CHARACTER FORMAT "x(03)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE cod-est-ini AS CHARACTER FORMAT "x(03)" 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE cod-port-fim AS INTEGER FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE cod-port-ini AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "":R9 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE da-conversao AS DATE FORMAT "99/99/9999":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "":R18 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-moeda AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "":R7 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-44
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-45
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE IMAGE IMAGE-46
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-47
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE IMAGE IMAGE-5
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE IMAGE IMAGE-6
     FILENAME "image\ii-fir":U
     SIZE 3.43 BY 1.08.

DEFINE IMAGE IMAGE-7
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 3.42.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 3.21.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 4.33.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 60 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE l-aberto AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.72 BY .83 NO-UNDO.

DEFINE VARIABLE l-faixa AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-matriz AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.72 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ap0804d
     l-matriz AT ROW 1.21 COL 20
     l-aberto AT ROW 2.21 COL 20
     l-faixa AT ROW 3.21 COL 20
     i-moeda AT ROW 4.54 COL 18 COLON-ALIGNED HELP
          "C¢digo da moeda"
     c-descricao AT ROW 4.54 COL 22.14 COLON-ALIGNED NO-LABEL
     da-conversao AT ROW 5.54 COL 18 COLON-ALIGNED
     c-cod-estabel AT ROW 6.54 COL 18 COLON-ALIGNED HELP
          "F5 para zoom"
     c-desc-estabel AT ROW 6.54 COL 24.14 COLON-ALIGNED NO-LABEL
     cod-est-ini AT ROW 8.04 COL 18 COLON-ALIGNED
     cod-est-fim AT ROW 8.04 COL 43 COLON-ALIGNED NO-LABEL
     cod-esp-ini AT ROW 9.04 COL 18 COLON-ALIGNED
     cod-esp-fim AT ROW 9.04 COL 43 COLON-ALIGNED NO-LABEL
     cod-port-ini AT ROW 10.04 COL 18 COLON-ALIGNED
     cod-port-fim AT ROW 10.04 COL 43 COLON-ALIGNED NO-LABEL
     dt-emissao-ini AT ROW 11.04 COL 18 COLON-ALIGNED
     dt-emissao-fim AT ROW 11.04 COL 43 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 12.5 COL 1.72
     bt-cancela AT ROW 12.5 COL 12
     bt-ajuda AT ROW 12.5 COL 50.43
     RECT-3 AT ROW 1 COL 1
     RECT-2 AT ROW 4.33 COL 1
     RECT-4 AT ROW 7.83 COL 1
     IMAGE-46 AT ROW 8.04 COL 35
     IMAGE-47 AT ROW 8.04 COL 41
     IMAGE-4 AT ROW 9.04 COL 35
     IMAGE-5 AT ROW 9.04 COL 41
     IMAGE-44 AT ROW 10.04 COL 35
     IMAGE-45 AT ROW 10.04 COL 41
     IMAGE-6 AT ROW 11.04 COL 35
     IMAGE-7 AT ROW 11.04 COL 41
     rt-buttom AT ROW 12.25 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Filtro Documentos"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX ap0804d
   L-To-R                                                               */
ASSIGN 
       FRAME ap0804d:SCROLLABLE       = FALSE
       FRAME ap0804d:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-desc-estabel IN FRAME ap0804d
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-descricao IN FRAME ap0804d
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN da-conversao IN FRAME ap0804d
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX ap0804d
/* Query rebuild information for DIALOG-BOX ap0804d
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX ap0804d */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB ap0804d 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ap0804d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804d ap0804d
ON WINDOW-CLOSE OF FRAME ap0804d /* Filtro Documentos */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda ap0804d
ON CHOOSE OF bt-ajuda IN FRAME ap0804d /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok ap0804d
ON CHOOSE OF bt-ok IN FRAME ap0804d /* OK */
DO:
 
  find moeda
       where moeda.mo-codigo = input frame {&frame-name} i-moeda no-lock no-error.

    if  not avail moeda then do:
        run utp/ut-msgs.p (input "show",
                           input 151,
                           input "").
        apply 'entry' to i-moeda in frame {&frame-name}.
        return no-apply.
    end.

   
  if input frame {&FRAME-NAME} l-faixa = no then do:
     find first estabelec
         where estabelec.cod-estabel = input frame {&frame-name} c-cod-estabel no-lock no-error.

     if not avail estabelec then do:
        run utp/ut-msgs.p (input "show",
                           input 2061,
                           input "").
        apply 'entry' to c-cod-estabel in frame {&frame-name}.
        return no-apply.
     end.
   end.
   
   if  input frame {&FRAME-NAME} cod-est-ini > input frame {&FRAME-NAME} cod-est-fim then do:
       run utp/ut-msgs.p (input "show",
                          input 1168,
                          input "").
       apply 'entry' to cod-est-ini in frame {&FRAME-NAME}.
       return no-apply.
   end.    

   if  input frame {&FRAME-NAME} cod-esp-ini > input frame {&FRAME-NAME} cod-esp-fim then do:
       run utp/ut-msgs.p (input "show",
                          input 1168,
                          input "").
       apply 'entry' to cod-esp-ini in frame {&FRAME-NAME}.
       return no-apply.
   end.    

   if  input frame {&FRAME-NAME} cod-port-ini > input frame {&FRAME-NAME} cod-port-fim then do:
       run utp/ut-msgs.p (input "show",
                          input 1168,
                          input "").
       apply 'entry' to cod-port-ini in frame {&FRAME-NAME}.
       return no-apply.
   end.    

   if  input frame {&FRAME-NAME} dt-emissao-ini > input frame {&FRAME-NAME} dt-emissao-fim then do:
       run utp/ut-msgs.p (input "show",
                          input 1168,
                          input "").
       apply 'entry' to dt-emissao-ini in frame {&FRAME-NAME}.
       return no-apply.
   end.    

    assign p-matriz          = input frame {&frame-name} l-matriz
           p-aberto          = input frame {&frame-name} l-aberto
           p-faixa           = input frame {&frame-name} l-faixa
           p-cod-moeda       = input frame {&frame-name} i-moeda
           p-cod-estabel     = input frame {&frame-name} c-cod-estabel
           p-cod-estabel-ini = input frame {&frame-name} cod-est-ini
           p-cod-estabel-fim = input frame {&frame-name} cod-est-fim
           p-cod-esp-ini     = input frame {&frame-name} cod-esp-ini
           p-cod-esp-fim     = input frame {&frame-name} cod-esp-fim
           p-portador-ini    = input frame {&frame-name} cod-port-ini
           p-portador-fim    = input frame {&frame-name} cod-port-fim
           p-dt-emissao-ini  = input frame {&frame-name} dt-emissao-ini
           p-dt-emissao-fim  = input frame {&frame-name} dt-emissao-fim
           p-dt-conversao    = input frame {&frame-name} da-conversao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel ap0804d
ON LEAVE OF c-cod-estabel IN FRAME ap0804d
DO:
    
    find first estabelec no-lock    
         where estabelec.cod-estabel = input frame {&FRAME-NAME} c-cod-estabel no-error.
    
    if  avail estabelec then 
        assign c-desc-estabel:screen-value in frame {&FRAME-NAME} = estabelec.nome.
    else
        assign c-desc-estabel:screen-value in frame {&FRAME-NAME} = "".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-moeda ap0804d
ON LEAVE OF i-moeda IN FRAME ap0804d
DO:
    {include/leave.i &tabela=moeda
                     &atributo-ref=descricao
                     &variavel-ref=c-descricao
                     &where="moeda.mo-codigo = input frame {&frame-name} i-moeda}.
     
    assign da-conversao:sensitive = if input frame {&frame-name} i-moeda <> 0 then yes
                                    else no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-faixa ap0804d
ON VALUE-CHANGED OF l-faixa IN FRAME ap0804d
DO:
        
    if  input frame {&FRAME-NAME} l-faixa = yes then
        assign c-cod-estabel:sensitive    in frame {&FRAME-NAME} = no
               cod-est-ini:sensitive      in frame {&FRAME-NAME} = yes
               cod-est-fim:sensitive      in frame {&FRAME-NAME} = yes
               c-cod-estabel:screen-value in frame {&FRAME-NAME} = "".
    else
        assign c-cod-estabel:sensitive  in frame {&FRAME-NAME} = yes
               cod-est-ini:sensitive    in frame {&FRAME-NAME} = no
               cod-est-fim:sensitive    in frame {&FRAME-NAME} = no
               cod-est-ini:screen-value in frame {&FRAME-NAME} = ""
               cod-est-fim:screen-value in frame {&FRAME-NAME} = "ZZZ".
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ap0804d 


/* ***************************  Main Block  *************************** */
    
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects ap0804d _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available ap0804d _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ap0804d _DEFAULT-DISABLE
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
  HIDE FRAME ap0804d.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ap0804d _DEFAULT-ENABLE
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
  DISPLAY l-matriz l-aberto l-faixa i-moeda c-descricao da-conversao 
          c-cod-estabel c-desc-estabel cod-est-ini cod-est-fim cod-esp-ini 
          cod-esp-fim cod-port-ini cod-port-fim dt-emissao-ini dt-emissao-fim 
      WITH FRAME ap0804d.
  ENABLE RECT-3 l-matriz l-aberto l-faixa RECT-2 i-moeda c-cod-estabel RECT-4 
         cod-est-ini IMAGE-46 IMAGE-47 cod-est-fim cod-esp-ini IMAGE-4 IMAGE-5 
         cod-esp-fim cod-port-ini IMAGE-44 IMAGE-45 cod-port-fim dt-emissao-ini 
         IMAGE-6 IMAGE-7 dt-emissao-fim rt-buttom bt-ok bt-cancela bt-ajuda 
      WITH FRAME ap0804d.
  VIEW FRAME ap0804d.
  {&OPEN-BROWSERS-IN-QUERY-ap0804d}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy ap0804d 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize ap0804d 
PROCEDURE local-initialize :
{utp/ut9000.i "AP0804AA" "2.00.00.005"}

/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    do  with frame {&frame-name}:
    
        {utp/ut-liter.i T°tulos_Matriz}
        assign l-matriz:label = trim(return-value).
    
        {utp/ut-liter.i T°tulos_Baixados}
        assign l-aberto:label = trim(return-value).
    
        {utp/ut-liter.i Faixa_Estabelecimento}
        assign l-faixa:label = trim(return-value).

        {utp/ut-liter.i Data_Transacao}
        assign dt-emissao-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-Port 1}
        assign cod-port-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo mo-codigo 1}
        assign i-moeda:label  = trim(return-value).
        
        {utp/ut-liter.i Data_Convers∆o}
        assign da-conversao:label = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-estabel 1}
        assign cod-est-ini:label  = trim(return-value).

        {utp/ut-field.i mgadm titulo cod-esp 1}
        assign cod-esp-ini:label  = trim(return-value).
        
        {utp/ut-field.i mgadm titulo cod-estabel 1}
        assign c-cod-estabel:label  = trim(return-value).
    
    end.            
        
    assign l-matriz        = p-matriz
           l-aberto        = p-aberto
           l-faixa         = p-faixa
           i-moeda         = p-cod-moeda
           c-cod-estabel   = p-cod-estabel
           cod-est-ini     = p-cod-estabel-ini
           cod-est-fim     = p-cod-estabel-fim
           cod-esp-ini     = p-cod-esp-ini
           cod-esp-fim     = p-cod-esp-fim
           cod-port-ini    = p-portador-ini
           cod-port-fim    = p-portador-fim
           dt-emissao-ini  = p-dt-emissao-ini
           dt-emissao-fim  = p-dt-emissao-fim
           da-conversao    = p-dt-conversao.
        
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view ap0804d 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .
        
    apply 'leave' to i-moeda       in frame {&FRAME-NAME}.
    apply 'leave' to c-cod-estabel in frame {&FRAME-NAME}.
        
    apply 'value-changed' to l-faixa in frame {&FRAME-NAME}.
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records ap0804d _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed ap0804d 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


