&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/* {include/i-prgvrs.i XX9999 9.99.99.999}*/ 

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE h-acomp AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-23 RECT-24 IMAGE-1 IMAGE-2 ~
RECT-25 IMAGE-3 IMAGE-4 i-nr-ord-produ-ini i-nr-ord-produ-fim dt-inicial ~
dt-fim rs-ordem ct-origem sc-origem ct-destino sc-destino bt-processa bt-ok 
&Scoped-Define DISPLAYED-OBJECTS i-nr-ord-produ-ini i-nr-ord-produ-fim ~
dt-inicial dt-fim rs-ordem ct-origem sc-origem ct-destino sc-destino 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-processa 
     LABEL "&Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE ct-destino AS CHARACTER FORMAT "X(8)":U 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE ct-origem AS CHARACTER FORMAT "X(8)":U 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-fim AS DATE FORMAT "99/99/9999":U INITIAL 04/30/08 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt-inicial AS DATE FORMAT "99/99/9999":U INITIAL 01/01/07 
     LABEL "Data Abertura" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-ord-produ-fim AS INTEGER FORMAT "999,999,999":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-ord-produ-ini AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Ord Manut" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE sc-destino AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE sc-origem AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-ordem AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "MI", 1,
"Frotas", 2
     SIZE 40 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 58 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 2.25.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 4.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY 2.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-nr-ord-produ-ini AT ROW 2.5 COL 18.86 COLON-ALIGNED
     i-nr-ord-produ-fim AT ROW 2.5 COL 39.86 COLON-ALIGNED NO-LABEL
     dt-inicial AT ROW 3.5 COL 18.86 COLON-ALIGNED 
     dt-fim AT ROW 3.5 COL 39.86 COLON-ALIGNED NO-LABEL 
     rs-ordem AT ROW 4.75 COL 13 NO-LABEL 
     ct-origem AT ROW 6.83 COL 19 COLON-ALIGNED 
     sc-origem AT ROW 6.83 COL 29 COLON-ALIGNED NO-LABEL 
     ct-destino AT ROW 9.25 COL 19 COLON-ALIGNED
     sc-destino AT ROW 9.25 COL 29 COLON-ALIGNED NO-LABEL
     bt-processa AT ROW 11 COL 24
     bt-ok AT ROW 12.5 COL 3
     bt-cancelar AT ROW 12.5 COL 14
     "Origem" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 6.08 COL 5 
     "Seleá∆o" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.75 COL 5
     "Destino:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 8.5 COL 5
     RECT-1 AT ROW 12.25 COL 2
     RECT-23 AT ROW 8.75 COL 4
     RECT-24 AT ROW 2 COL 4
     IMAGE-1 AT ROW 2.5 COL 34.86
     IMAGE-2 AT ROW 2.5 COL 38.86
     RECT-25 AT ROW 6.33 COL 4 
     IMAGE-3 AT ROW 3.5 COL 34.86 
     IMAGE-4 AT ROW 3.5 COL 38.86 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.43 BY 12.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Acerto de Base"
         HEIGHT             = 12.63
         WIDTH              = 60.14
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-cancelar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-cancelar:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Acerto de Base */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Acerto de Base */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-window
ON CHOOSE OF bt-processa IN FRAME F-Main /* Executar */
DO:

  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp(INPUT "Processando Acerto").
  RUN pi-desabilita-cancela IN h-acomp.

  OUTPUT TO VALUE(session:temp-directory + "log-acerto.txt") convert target "iso8859-1".

  PUT UNFORMATTED "FAIXA ORDEM:      " + i-nr-ord-produ-ini:SCREEN-VALUE + " |>  <| " + i-nr-ord-produ-fim:SCREEN-VALUE  SKIP
                  "CONTA DESTINO:    " + ct-destino:SCREEN-VALUE + "." + sc-destino:SCREEN-VALUE SKIP(2)
                  "ORDENS ALTERADAS: " SKIP.

  DO WITH FRAME {&FRAME-NAME}:

      ASSIGN i-nr-ord-produ-ini i-nr-ord-produ-fim
             dt-inicial dt-fim
             ct-origem sc-origem
             ct-destino sc-destino
             rs-ordem.

      IF rs-ordem = 1 /* MI */ THEN DO:
            RUN pi-ordem-mi.
      END.
      ELSE DO: /* Frotas */
            RUN pi-ordem-frota.
      END.

  END.

  OUTPUT CLOSE.

  RUN pi-executar(INPUT "notepad.exe",
                  INPUT session:temp-directory + "log-acerto.txt").
  RUN pi-finalizar IN h-acomp.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ct-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ct-destino w-window
ON MOUSE-SELECT-DBLCLICK OF ct-destino IN FRAME F-Main /* Conta Cont†bil */
OR "F5":U OF ct-destino IN FRAME {&FRAME-NAME} DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad049.w
                     &campo=ct-destino
                     &campozoom=ct-codigo
                     &campo2=sc-destino
                     &campozoom2=sc-codigo}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ct-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ct-origem w-window
ON MOUSE-SELECT-DBLCLICK OF ct-origem IN FRAME F-Main /* Conta Cont†bil */
OR "F5":U OF ct-origem IN FRAME {&FRAME-NAME} DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad049.w
                     &campo=ct-origem
                     &campozoom=ct-codigo
                     &campo2=sc-origem
                     &campozoom2=sc-codigo}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sc-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sc-destino w-window
ON MOUSE-SELECT-DBLCLICK OF sc-destino IN FRAME F-Main
OR "F5":U OF sc-destino IN FRAME {&FRAME-NAME} DO:
    apply "F5":U to ct-destino in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sc-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sc-origem w-window
ON MOUSE-SELECT-DBLCLICK OF sc-origem IN FRAME F-Main
OR "F5":U OF sc-origem IN FRAME {&FRAME-NAME} DO:
    apply "F5":U to ct-origem in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
find first plano_cta_ctbl no-lock
     where plano_cta_ctbl.cod_plano_cta_ctbl = "CONTSOC" no-error.
if  not avail plano_cta_ctbl then next.

find first tip_grp_cta_ctbl of plano_cta_ctbl no-lock no-error.
if  not avail tip_grp_cta_ctbl then next.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

procedure pi-executar:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param c-exec as char.
  def input param c-param as char.
  def var h-Inst as int.
  
  run ShellExecute{&A} in hpApi (input 0,
                                 input "open",
                                 input c-exec,
                                 input c-param,
                                 input "",
                                 input 1,
                                 output h-inst).

  if h-inst < 0 or h-inst > 32 then return "OK".
  else return "NOK".                               
                                 
                                 
end procedure.

ct-origem:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame  {&FRAME-NAME}.
sc-origem:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame  {&FRAME-NAME}.
ct-destino:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame {&FRAME-NAME}.
sc-destino:LOAD-MOUSE-POINTER("image/lupa.cur":U) in frame {&FRAME-NAME}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY i-nr-ord-produ-ini i-nr-ord-produ-fim dt-inicial dt-fim rs-ordem 
          ct-origem sc-origem ct-destino sc-destino 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-23 RECT-24 IMAGE-1 IMAGE-2 RECT-25 IMAGE-3 IMAGE-4 
         i-nr-ord-produ-ini i-nr-ord-produ-fim dt-inicial dt-fim rs-ordem 
         ct-origem sc-origem ct-destino sc-destino bt-processa bt-ok 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable w-window 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

    IF TODAY > 09/10/2008 THEN do:
      MESSAGE 'Data (10/09/2008) expirou, entre em contato com suporte Datasul' + CHR(10) +
              '(47) 2101-7400 - Opá∆o Manufatura - Manutená∆o.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "CHOOSE":U TO bt-ok IN FRAME {&FRAME-NAME}.
      /*RUN local-exit.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ordem-frota w-window 
PROCEDURE pi-ordem-frota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Primeiro comeáa pela OP */
blk-om-frota:
FOR EACH ord-prod EXCLUSIVE-LOCK
   WHERE ord-prod.nr-ord-produ >= i-nr-ord-produ-ini
     AND ord-prod.nr-ord-produ <= i-nr-ord-produ-fim
     AND ord-prod.ct-desp       = ct-origem
     AND ord-prod.sc-desp       = sc-origem
     AND ord-prod.dt-inicio    >= dt-inicial
     AND ord-prod.dt-inicio    <= dt-fim
     AND ord-prod.origem        = "MV": /* Deixa mais perform†tico */

    IF ord-prod.estado = 8 AND ord-prod.valorizada = YES THEN NEXT blk-om-frota.

    RUN pi-acompanhar IN h-acomp (INPUT STRING(ord-prod.nr-ord-produ)).

    FOR FIRST mmv-ord-manut EXCLUSIVE-LOCK
        WHERE mmv-ord-manut.nr-ord-produ = ord-prod.nr-ord-produ:
        FOR FIRST mab-eqpto
            WHERE mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto
            AND   mab-eqpto.ep-codigo = mmv-ord-manut.ep-codigo:
            IF mab-eqpto.cc-codigo <> sc-destino THEN DO:
                /* Somente avisa, n∆o deixa de trocar */
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Ccusto Destino <> Centro de Custo Equipamento~~"
                                       + "Ccusto Destino:    " + sc-destino + CHR(10)
                                       + "Centro Custo mab-eqpto: " + mab-eqpto.cc-codigo
                                       + "Ordem:                " + string(mmv-ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                /*RETURN NO-APPLY.*/
            END.
        END.

        for FIRST cta_ctbl_integr no-lock
            where /* cta_ctbl_integr.cod_modul_dtsul = "CEP"
              and */ cta_ctbl_integr.cod_cta_ctbl    = ct-destino
              and cta_ctbl_integr.dat_inic_valid <= today
              and cta_ctbl_integr.dat_fim_valid  >= today:
            find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                 where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                 IF grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 /* Despesa */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Despesa~~"
                                          + "Ordem: " + string(mmv-ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-frota.
                END.

                IF cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" /* Consumo */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Consumo~~"
                                          + "Ordem: " + string(mmv-ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-frota.
                END.
            
        END.
            IF NOT AVAIL cta_ctbl_integr THEN DO:
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Conta Destino N∆o Existe~~"
                                       + "Empresa: " + STRING(mmv-ord-manut.ep-codigo) + CHR(10)
                                       + "Conta:   " + ct-destino + CHR(10)
                                       + "Ccusto:  " + sc-destino
                                       + "Ordem:   " + string(mmv-ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                NEXT blk-om-frota.
            END.

        ASSIGN ord-prod.conta-despesa  = ct-destino + sc-destino
               ord-prod.ct-desp        = ct-destino
               ord-prod.sc-desp        = sc-destino
               mmv-ord-manut.ct-codigo = ct-destino
               mmv-ord-manut.cc-codigo = sc-destino.

        PUT UNFORMATTED "     " + string(mmv-ord-manut.nr-ord-produ) SKIP.
    END.
END.
/* Executa a busca pelo contr†rio, caso exista algum acerto e as contas estejam diferentes na OP e na OM */
blk-om-frota2:
FOR EACH mmv-ord-manut EXCLUSIVE-LOCK
   WHERE mmv-ord-manut.nr-ord-produ >= i-nr-ord-produ-ini
     AND mmv-ord-manut.nr-ord-produ <= i-nr-ord-produ-fim
     AND mmv-ord-manut.ct-codigo     = ct-origem
     AND mmv-ord-manut.cc-codigo     = sc-origem
     AND mmv-ord-manut.dat-abert    >= dt-inicial
     AND mmv-ord-manut.dat-abert    <= dt-fim:

    FOR FIRST ord-prod EXCLUSIVE-LOCK
        WHERE ord-prod.nr-ord-produ = mmv-ord-manut.nr-ord-produ:

        IF ord-prod.estado = 8 AND ord-prod.valorizada = YES THEN NEXT blk-om-frota2.

        RUN pi-acompanhar IN h-acomp (INPUT STRING(ord-prod.nr-ord-produ)).

        FOR FIRST mab-eqpto
            WHERE mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto
            AND   mab-eqpto.ep-codigo = mmv-ord-manut.ep-codigo:
            IF mab-eqpto.cc-codigo <> sc-destino THEN DO:
                /* Somente avisa, n∆o deixa de trocar */
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Ccusto Destino <> Centro de Custo Equipamento~~"
                                       + "Ccusto Destino:    " + sc-destino + CHR(10)
                                       + "Centro Custo mab-eqpto: " + mab-eqpto.cc-codigo
                                       + "Ordem:                " + string(mmv-ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                /*RETURN NO-APPLY.*/
            END.
        END.

        for FIRST cta_ctbl_integr no-lock
            where /* cta_ctbl_integr.cod_modul_dtsul = "CEP"
              and */ cta_ctbl_integr.cod_cta_ctbl    = ct-destino
              and cta_ctbl_integr.dat_inic_valid <= today
              and cta_ctbl_integr.dat_fim_valid  >= today:
            find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                 where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                 IF grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 /* Despesa */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Despesa~~"
                                          + "Ordem: " + string(mmv-ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-frota2.
                END.
            
                IF cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" /* Consumo */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Consumo~~"
                                          + "Ordem: " + string(mmv-ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-frota2.
                END.
            
        END.
            IF NOT AVAIL cta_ctbl_integr THEN DO:
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Conta Destino N∆o Existe~~"
                                       + "Empresa: " + STRING(mmv-ord-manut.ep-codigo) + CHR(10)
                                       + "Conta:   " + ct-destino + CHR(10)
                                       + "Ccusto:  " + sc-destino
                                       + "Ordem:   " + string(mmv-ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                NEXT blk-om-frota2.
            END.

        ASSIGN ord-prod.conta-despesa  = ct-destino + sc-destino
               ord-prod.ct-desp        = ct-destino
               ord-prod.sc-desp        = sc-destino
               mmv-ord-manut.ct-codigo = ct-destino
               mmv-ord-manut.cc-codigo = sc-destino.

        PUT UNFORMATTED "     " + string(mmv-ord-manut.nr-ord-produ) SKIP.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ordem-mi w-window 
PROCEDURE pi-ordem-mi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Primeiro comeáa pela OP */
blk-om-mi:
FOR EACH ord-prod EXCLUSIVE-LOCK
   WHERE ord-prod.nr-ord-produ >= i-nr-ord-produ-ini
     AND ord-prod.nr-ord-produ <= i-nr-ord-produ-fim
     AND ord-prod.ct-desp       = ct-origem
     AND ord-prod.sc-desp       = sc-origem
     AND ord-prod.dt-inicio    >= dt-inicial
     AND ord-prod.dt-inicio    <= dt-fim
     AND ord-prod.origem        = "MI": /* Deixa mais perform†tico */

    IF ord-prod.estado = 8 AND ord-prod.valorizada = YES THEN NEXT blk-om-mi.

    RUN pi-acompanhar IN h-acomp (INPUT STRING(ord-prod.nr-ord-produ)).

    FOR FIRST ord-manut EXCLUSIVE-LOCK
        WHERE ord-manut.nr-ord-produ = ord-prod.nr-ord-produ:
        FOR FIRST equipto
            WHERE equipto.cd-equipto = ord-manut.cd-equipto:
            IF equipto.cc-codigo <> sc-destino THEN DO:
                /* Somente avisa, n∆o deixa de trocar */
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Ccusto Destino <> Centro de Custo Equipamento~~"
                                       + "Ccusto Destino:    " + sc-destino + CHR(10)
                                       + "Centro Custo Equipto: " + equipto.cc-codigo
                                       + "Ordem:                " + string(ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                /*RETURN NO-APPLY.*/
            END.
        END.

          for FIRST cta_ctbl_integr no-lock
              where /* cta_ctbl_integr.cod_modul_dtsul = "CEP"
                and */ cta_ctbl_integr.cod_cta_ctbl    = ct-destino
                and cta_ctbl_integr.dat_inic_valid <= today
                and cta_ctbl_integr.dat_fim_valid  >= today:
              find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                   where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                 IF grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 /* Despesa */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Despesa~~"
                                          + "Ordem: " + string(ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-mi.
                END.
            
                IF cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" /* Consumo */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Consumo~~"
                                          + "Ordem: " + string(ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-mi.
                END.
            
        END.
            IF NOT AVAIL cta_ctbl_integr THEN DO:
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Conta Destino N∆o Existe~~"
                                       + "Empresa: " + STRING(ord-manut.ep-codigo) + CHR(10)
                                       + "Conta:   " + ct-destino + CHR(10)
                                       + "Ccusto:  " + sc-destino
                                       + "Ordem:   " + string(ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                NEXT blk-om-mi.
            END.

        ASSIGN ord-prod.conta-despesa  = ct-destino + sc-destino
               ord-prod.ct-desp        = ct-destino
               ord-prod.sc-desp        = sc-destino
               ord-manut.conta-despesa = ct-destino + sc-destino
               ord-manut.ct-desp       = ct-destino
               ord-manut.sc-desp       = sc-destino.

        PUT UNFORMATTED "     " + string(ord-manut.nr-ord-produ) SKIP.
    END.
END.
/* Executa a busca pelo contr†rio, caso exista algum acerto e as contas estejam diferentes na OP e na OM */
blk-om-mi2:
FOR EACH ord-manut EXCLUSIVE-LOCK
   WHERE ord-manut.nr-ord-produ >= i-nr-ord-produ-ini
     AND ord-manut.nr-ord-produ <= i-nr-ord-produ-fim
     AND ord-manut.ct-desp       = ct-origem
     AND ord-manut.sc-desp       = sc-origem
     AND ord-manut.dt-manut    >= dt-inicial
     AND ord-manut.dt-manut    <= dt-fim:

    FOR FIRST ord-prod EXCLUSIVE-LOCK
        WHERE ord-prod.nr-ord-produ = ord-manut.nr-ord-produ:

        IF ord-prod.estado = 8 AND ord-prod.valorizada = YES THEN NEXT blk-om-mi2.

        RUN pi-acompanhar IN h-acomp (INPUT STRING(ord-prod.nr-ord-produ)).

        FOR FIRST equipto
            WHERE equipto.cd-equipto = ord-manut.cd-equipto:
            IF equipto.cc-codigo <> sc-destino THEN DO:
                /* Somente avisa, n∆o deixa de trocar */
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Ccusto Destino <> Centro de Custo Equipamento~~"
                                       + "Ccusto Destino:    " + sc-destino + CHR(10)
                                       + "Centro Custo Equipto: " + equipto.cc-codigo
                                       + "Ordem:                " + string(ord-manut.nr-ord-produ)).
MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                /*RETURN NO-APPLY.*/
            END.
        END.

          for FIRST cta_ctbl_integr no-lock
              where /* cta_ctbl_integr.cod_modul_dtsul = "CEP"
                and */ cta_ctbl_integr.cod_cta_ctbl    = ct-destino
                and cta_ctbl_integr.dat_inic_valid <= today
                and cta_ctbl_integr.dat_fim_valid  >= today:
              find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                   where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                 IF grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 /* Despesa */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Despesa~~"
                                          + "Ordem: " + string(ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-mi2.
                END.

                IF cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" /* Consumo */ THEN DO:
                    RUN utp/ut-msgs.p(INPUT "msg",
                                      INPUT 17006,
                                      INPUT "Conta Destino N∆o Ç de Consumo~~"
                                          + "Ordem: " + string(ord-manut.nr-ord-produ)).
            MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
            /*                 RUN pi-finalizar IN h-acomp. */
            /*                 RETURN NO-APPLY.             */
                    NEXT blk-om-mi2.
                END.
            
        END.
            IF NOT AVAIL cta_ctbl_integr THEN DO:
                RUN utp/ut-msgs.p (INPUT "msg",
                                   INPUT 17006,
                                   INPUT "Conta Destino N∆o Existe~~"
                                       + "Empresa: " + STRING(ord-manut.ep-codigo) + CHR(10)
                                       + "Conta:   " + ct-destino + CHR(10)
                                       + "Ccusto:  " + sc-destino
                                       + "Ordem:   " + string(ord-manut.nr-ord-produ)).
                MESSAGE RETURN-VALUE + "  OM: " + string(ord-prod.nr-ord-produ).
                /*RUN pi-finalizar IN h-acomp.*/
                NEXT blk-om-mi2.
            END.

        ASSIGN ord-prod.conta-despesa  = ct-destino + sc-destino
               ord-prod.ct-desp        = ct-destino
               ord-prod.sc-desp        = sc-destino
               ord-manut.conta-despesa = ct-destino + sc-destino
               ord-manut.ct-desp       = ct-destino
               ord-manut.sc-desp       = sc-destino.

        PUT UNFORMATTED "     " + string(ord-manut.nr-ord-produ) SKIP.
    END.
END.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

