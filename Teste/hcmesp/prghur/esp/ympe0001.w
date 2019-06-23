&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*****************************************************************************
**  Programa :                                                              **         
**  Descricao:                                                              **       
**  Data     :                                                              **         
**  Autor    : Datasul HCM                                                  **
******************************************************************************/                             
{include/buffers_RH.i}

{include/i-prgvrs.i ympe0001 1.00.00.001}

/* Variÿveis */
/*def var arquivo                 as char format 'x(100)'     label 'Destino' no-undo.*/
def var c-programa              as char                                     no-undo.

def var v_dat_ini    as date no-undo.
def var v_dat_fim    as date no-undo.
def var v_dat_inicio as date no-undo.
def var v_dat_final  as date no-undo.

/*Destino Arquivo*/
def var c-arq-conv  as char no-undo.
          
/*Janela de Acompanhamento*/
def new shared var h-acomp  as handle   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v_cdn_empresa_ini v_cdn_empresa_fim ~
v_cdn_estab_ini v_cdn_estab_fim v_cdn_funcionario_ini v_cdn_funcionario_fim ~
v_mes_ini Btn_OK Btn_Cancel RECT-16 RECT-2 IMAGE-19 IMAGE-20 IMAGE-21 ~
IMAGE-22 IMAGE-23 IMAGE-24 v_ano_ini IMAGE-25 IMAGE-26 v_mes_fim v_ano_fim 
&Scoped-Define DISPLAYED-OBJECTS v_cdn_empresa_ini v_cdn_empresa_fim ~
v_cdn_estab_ini v_cdn_estab_fim v_cdn_funcionario_ini v_cdn_funcionario_fim ~
v_mes_ini v_ano_ini v_mes_fim v_ano_fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE v_ano_fim AS INTEGER FORMAT "9999":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE v_ano_ini AS INTEGER FORMAT "9999":U INITIAL 2000
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_empresa_fim AS CHARACTER FORMAT "x(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_empresa_ini AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_fim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_ini AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Matricula" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE v_mes_fim AS INTEGER FORMAT "99":U INITIAL 12 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88 NO-UNDO.

DEFINE VARIABLE v_mes_ini AS INTEGER FORMAT "99":U INITIAL 01 
     LABEL "Mˆs~\Ano Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 4.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 62 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     v_cdn_empresa_ini AT ROW 2.25 COL 22.14 COLON-ALIGNED
     v_cdn_empresa_fim AT ROW 2.25 COL 40.86 COLON-ALIGNED NO-LABEL
     v_cdn_estab_ini AT ROW 3.25 COL 22.14 COLON-ALIGNED
     v_cdn_estab_fim AT ROW 3.25 COL 40.86 COLON-ALIGNED NO-LABEL
     v_cdn_funcionario_ini AT ROW 4.25 COL 22.14 COLON-ALIGNED
     v_cdn_funcionario_fim AT ROW 4.25 COL 40.86 COLON-ALIGNED NO-LABEL
     v_mes_ini AT ROW 5.25 COL 22.14 COLON-ALIGNED
     Btn_OK AT ROW 7.21 COL 2.29
     Btn_Cancel AT ROW 7.21 COL 13
     v_ano_ini AT ROW 5.25 COL 26.29 COLON-ALIGNED NO-LABEL
     v_mes_fim AT ROW 5.25 COL 40.86 COLON-ALIGNED NO-LABEL
     v_ano_fim AT ROW 5.25 COL 45 COLON-ALIGNED NO-LABEL
     "Sele‡Æo" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 1.5 COL 5.14
     RECT-16 AT ROW 1.83 COL 3
     RECT-2 AT ROW 7 COL 1.57
     IMAGE-19 AT ROW 2.25 COL 34.86
     IMAGE-20 AT ROW 2.25 COL 39
     IMAGE-21 AT ROW 3.25 COL 34.86
     IMAGE-22 AT ROW 3.25 COL 39
     IMAGE-23 AT ROW 4.29 COL 34.86
     IMAGE-24 AT ROW 4.29 COL 39
     IMAGE-25 AT ROW 5.29 COL 34.86
     IMAGE-26 AT ROW 5.29 COL 39
     SPACE(21.99) SKIP(2.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ympe0001"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Smart,Query
   Container Links: 
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* ympe0001 */
DO:
/*     APPLY "END-ERROR":U TO SELF.  */
    APPLY "CLOSE":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO: 
   run pi-executar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.

   WAIT-FOR close of this-procedure.
END.
RUN disable_UI.

PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY v_cdn_empresa_ini v_cdn_empresa_fim v_cdn_estab_ini v_cdn_estab_fim 
          v_cdn_funcionario_ini v_cdn_funcionario_fim v_mes_ini v_ano_ini 
          v_mes_fim v_ano_fim 
      WITH FRAME Dialog-Frame.
  ENABLE v_cdn_empresa_ini v_cdn_empresa_fim v_cdn_estab_ini v_cdn_estab_fim 
         v_cdn_funcionario_ini v_cdn_funcionario_fim v_mes_ini Btn_OK 
         Btn_Cancel RECT-16 RECT-2 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-23 
         IMAGE-24 v_ano_ini IMAGE-25 IMAGE-26 v_mes_fim v_ano_fim 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Dialog-Frame 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run utp/ut-acomp.p persistent set h-acomp.
    
    /* Variÿveis Utilizadas no Programa */
    assign input frame {&frame-name} v_cdn_empresa_ini   
           input frame {&frame-name} v_cdn_empresa_fim   
           input frame {&frame-name} v_cdn_estab_ini
           input frame {&frame-name} v_cdn_estab_fim
           input frame {&frame-name} v_cdn_funcionario_ini
           input frame {&frame-name} v_cdn_funcionario_fim
           input frame {&frame-name} v_mes_ini
           input frame {&frame-name} v_ano_ini
           input frame {&frame-name} v_mes_fim
           input frame {&frame-name} v_ano_fim.

    run pi-inicializar in h-acomp (input 'Zerando saldo do banco').


    assign  v_dat_ini = date(v_mes_ini,01,v_ano_ini)
            v_dat_fim = if v_mes_fim = 12
                        then date(12,31,v_ano_fim)
                        else date(v_mes_fim + 1,01,v_ano_fim) - 1.

    FOR EACH sit_calc_ptoelet_func no-lock
       WHERE sit_calc_ptoelet_func.cdn_empresa     >= v_cdn_empresa_ini
         AND sit_calc_ptoelet_func.cdn_empresa     <= v_cdn_empresa_fim
         AND sit_calc_ptoelet_func.cdn_estab       >= v_cdn_estab_ini
         AND sit_calc_ptoelet_func.cdn_estab       <= v_cdn_estab_fim
         AND sit_calc_ptoelet_func.cdn_funcionario >= v_cdn_funcionario_ini
         AND sit_calc_ptoelet_func.cdn_funcionario <= v_cdn_funcionario_fim:

        assign v_dat_inicio = date(sit_calc_ptoelet_func.num_mes_primei_calc_realzdo,01,sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet).
               v_dat_final  = if sit_calc_ptoelet_func.num_mes_primei_calc_realzdo = 12
                              then date(12,31,sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet)
                              else date(sit_calc_ptoelet_func.num_mes_primei_calc_realzdo + 1,01,sit_calc_ptoelet_func.num_ano_primei_calc_ptoelet) - 1.

        if v_dat_inicio >= v_dat_ini and
           v_dat_final  <= v_dat_fim then do:

           for each bco_hrs_compens_func exclusive-lock
              where bco_hrs_compens_func.cdn_empresa           = sit_calc_ptoelet_func.cdn_empresa
                and bco_hrs_compens_func.cdn_estab             = sit_calc_ptoelet_func.cdn_estab
                and bco_hrs_compens_func.cdn_funcionario       = sit_calc_ptoelet_func.cdn_funcionario
                and bco_hrs_compens_func.dat_atualiz_bco_hora >= sit_calc_ptoelet_func.dat_inic_period_apurac_pto_mes
                and bco_hrs_compens_func.dat_atualiz_bco_hora <= sit_calc_ptoelet_func.dat_term_period_apurac_pto_mes
                and (bco_hrs_compens_func.idi_hrs_posit        = 1 
                  or bco_hrs_compens_func.idi_hrs_posit        = 2):

               run pi-acompanhar in h-acomp (input string(sit_calc_ptoelet_func.cdn_empresa) + 
                                      ' - Func: ' + string(sit_calc_ptoelet_func.cdn_estab) + '\' + string(sit_calc_ptoelet_func.cdn_funcionario)).

               if bco_hrs_compens_func.idi_hrs_posit = 1 then
                  assign bco_hrs_compens_func.idi_hrs_posit = 3.
               if bco_hrs_compens_func.idi_hrs_posit = 2 then
                  assign bco_hrs_compens_func.idi_hrs_posit = 4.
           end.
        end.
    end.
    
    /*Elimina o Handle do acompanhamento*/
    run pi-finalizar in h-acomp.

    MESSAGE "Programa executado com sucesso!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


