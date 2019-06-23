&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP006-W01 1.02.00.001 } /*** 010001 ***/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MFP}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER ipi-tipo AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipr-row  AS RECID       NO-UNDO.

/* Local Variable Definitions ---                                       */


DEF BUFFER bf_item_lote_impl_ap FOR item_lote_impl_ap.
DEF BUFFER bf_item_bord_ap FOR ITEM_bord_ap.   
DEF BUFFER bf_ext_item_lote_impl_ap FOR ext_item_lote_impl_ap.

&IF "{1}" = "1" &THEN
    &SCOPED-DEFINE tabela bf_item_lote_impl_ap
    
      &SCOPED-DEFINE procura  FIND ext_ITEM_lote_impl_ap ~
                              WHERE ext_item_lote_impl_ap.cod_estab = bf_item_lote_impl_ap.cod_estab ~
                                AND ext_ITEM_lote_impl_ap.cod_refer = bf_item_lote_impl_ap.cod_refer ~
                                AND ext_ITEM_lote_impl_ap.num_seq_refer = bf_item_lote_impl_ap.num_seq_refer NO-ERROR.~
     
    &SCOPED-DEFINE campo1 cod_estab    
    &SCOPED-DEFINE campo2 cod_refer    
    &SCOPED-DEFINE campo3 num_seq_refer
    
&ELSE
     &SCOPED-DEFINE tabela bf_ITEM_bord_ap
     &SCOPED-DEFINE procura  FIND ext_ITEM_lote_impl_ap ~
                                WHERE ext_ITEM_lote_impl_ap.cod_estab     = bf_item_bord_ap.cod_estab_bord ~
                                  AND ext_ITEM_lote_impl_ap.cod_refer     = STRING(bf_item_bord_ap.cod_portador) ~
                                  AND ext_ITEM_lote_impl_ap.num_seq_refer = bf_item_bord_ap.num_id_item_bord_ap NO-ERROR. ~
                              IF NOT AVAIL ext_item_lote_impl_ap THEN ~
                                  FIND FIRST ext_ITEM_lote_impl_ap ~
                                    WHERE ext_ITEM_lote_impl_ap.cod_estab     = bf_item_bord_ap.cod_estab_bord ~
                                      AND ext_ITEM_lote_impl_ap.cod_refer     = STRING(bf_item_bord_ap.cod_portador) NO-ERROR.

    &scoped-define campo1 cod_estab_bord 
    &scoped-define campo2 cod_portador
    &scoped-define campo3 num_id_item_bord_ap 

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-20 RECT-3 i_cod_receita ~
rd_tipo bt_cnpj c_id_contribuinte dt_competencia d_vlr_outras_gps ~
c_refer_darf c_id_fgts c_lacre_fgts d_vl_bruto_darf i_dig_lacre_fgts ~
d_perc_darf bt-ok bt-ok-2 bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS i_fornec c-esp c_titulo c-serie c-parc ~
d-valor dt_vencto i_cod_receita rd_tipo c_id_contribuinte dt_competencia ~
d_vlr_outras_gps c_refer_darf c_id_fgts c_lacre_fgts d_vl_bruto_darf ~
i_dig_lacre_fgts d_perc_darf text-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 c_id_fgts c_lacre_fgts i_dig_lacre_fgts 
&Scoped-define List-2 c_refer_darf 
&Scoped-define List-3 dt_competencia d_vlr_outras_gps 

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

DEFINE BUTTON bt-ok-2 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cnpj 
     IMAGE-UP FILE "adeicon/cnfginfo.ico":U
     LABEL "Button 1" 
     SIZE 5 BY 1 TOOLTIP "Contribuinte Ç a pr¢pria empresa".

DEFINE VARIABLE c-esp AS CHARACTER FORMAT "X(04)":U 
     LABEL "EspÇcie" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c-parc AS CHARACTER FORMAT "X(02)":U 
     LABEL "Parcela" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(4)":U 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c_id_contribuinte AS CHARACTER FORMAT "X(20)":U 
     LABEL "Id Contribuinte" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE c_id_fgts AS CHARACTER FORMAT "X(16)":U 
     LABEL "Id FGTS" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c_lacre_fgts AS CHARACTER FORMAT "X(9)":U 
     LABEL "Lacre Conectividade Social" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c_refer_darf AS CHARACTER FORMAT "X(17)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .88 NO-UNDO.

DEFINE VARIABLE c_titulo AS CHARACTER FORMAT "X(256)":U 
     LABEL "T°tulo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE d-valor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_competencia AS DATE FORMAT "99/99/9999":U 
     LABEL "Competància/Per°odo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE dt_vencto AS DATE FORMAT "99/99/9999":U 
     LABEL "Data de vencimento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d_perc_darf AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Percentual" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE d_vlr_outras_gps AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor outra Entidade" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE d_vl_bruto_darf AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Receita Bruta" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE i_cod_receita AS INTEGER FORMAT "999999":U INITIAL 0 
     LABEL "C¢d. Receita" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i_dig_lacre_fgts AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Digito Lacre" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE i_fornec AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE text-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE rd_tipo AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pessoa F°sica", 1,
"Pessoa Jur°dica", 2
     SIZE 28.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 7.75.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 3.75.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 70 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     i_fornec AT ROW 1.75 COL 14 COLON-ALIGNED WIDGET-ID 160
     c-esp AT ROW 1.75 COL 36 COLON-ALIGNED WIDGET-ID 170
     c_titulo AT ROW 1.75 COL 49 COLON-ALIGNED WIDGET-ID 162
     c-serie AT ROW 2.75 COL 14 COLON-ALIGNED WIDGET-ID 168
     c-parc AT ROW 2.75 COL 49 COLON-ALIGNED WIDGET-ID 172
     d-valor AT ROW 3.75 COL 14 COLON-ALIGNED WIDGET-ID 164
     dt_vencto AT ROW 3.75 COL 49 COLON-ALIGNED WIDGET-ID 166
     i_cod_receita AT ROW 5.75 COL 29.86 COLON-ALIGNED WIDGET-ID 126
     rd_tipo AT ROW 6.75 COL 31.86 NO-LABEL WIDGET-ID 142
     bt_cnpj AT ROW 7.58 COL 54.14 WIDGET-ID 152
     c_id_contribuinte AT ROW 7.75 COL 29.86 COLON-ALIGNED WIDGET-ID 130
     dt_competencia AT ROW 8.79 COL 29.86 COLON-ALIGNED WIDGET-ID 140
     d_vlr_outras_gps AT ROW 9.79 COL 30 COLON-ALIGNED WIDGET-ID 150
     c_refer_darf AT ROW 9.83 COL 29.86 COLON-ALIGNED WIDGET-ID 138
     c_id_fgts AT ROW 9.83 COL 30 COLON-ALIGNED WIDGET-ID 132
     c_lacre_fgts AT ROW 10.75 COL 29.86 COLON-ALIGNED WIDGET-ID 134
     d_vl_bruto_darf AT ROW 10.75 COL 30 COLON-ALIGNED WIDGET-ID 180
     i_dig_lacre_fgts AT ROW 11.75 COL 30 COLON-ALIGNED WIDGET-ID 136
     d_perc_darf AT ROW 11.75 COL 30 COLON-ALIGNED WIDGET-ID 182
     bt-ok AT ROW 13.63 COL 2.86
     bt-ok-2 AT ROW 13.63 COL 3 WIDGET-ID 178
     bt-cancela AT ROW 13.63 COL 14
     bt-ajuda AT ROW 13.63 COL 61
     text-2 AT ROW 5.21 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     " T°tulo" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 1.08 COL 4 WIDGET-ID 176
     "Tipo de Contribuinte:" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 6.92 COL 17.72 WIDGET-ID 146
     rt-buttom AT ROW 13.38 COL 2
     RECT-20 AT ROW 5.5 COL 2 WIDGET-ID 154
     RECT-3 AT ROW 1.33 COL 2 WIDGET-ID 174
     SPACE(0.42) SKIP(9.79)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       bt-ok:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN c-esp IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-parc IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c_id_fgts IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN c_lacre_fgts IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN c_refer_darf IN FRAME D-Dialog
   2                                                                    */
/* SETTINGS FOR FILL-IN c_titulo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-valor IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt_competencia IN FRAME D-Dialog
   3                                                                    */
/* SETTINGS FOR FILL-IN dt_vencto IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d_vlr_outras_gps IN FRAME D-Dialog
   3                                                                    */
/* SETTINGS FOR FILL-IN i_dig_lacre_fgts IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN i_fornec IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON GO OF FRAME D-Dialog /* <insert SmartDialog title> */
DO:
/*   APPLY "END-ERROR":U TO SELF. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok-2 D-Dialog
ON CHOOSE OF bt-ok-2 IN FRAME D-Dialog /* OK */
DO:
    IF i_cod_receita:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" OR 
       INT(i_cod_receita:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN DO:

        MESSAGE "C¢digo da Receita deve ser informado!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "ENTRY" TO  i_cod_receita.
       RETURN NO-APPLY.
    END.

    IF c_id_contribuinte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        MESSAGE "Id DO Contribuinte deve ser informado!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        APPLY "ENTRY" TO c_id_contribuinte.
        RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} dt_competencia = ? THEN DO:

        MESSAGE "Competància Ç obrigat¢ria e n∆o foi informada!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO dt_competencia.
        RETURN NO-APPLY.                
    END.
    
    CASE ipi-tipo:

      /* FGTS */
      WHEN 1 THEN DO:

          IF c_id_fgts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
              MESSAGE "ID FGTS est† em branco, favor informar o ID!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
                   
              APPLY "ENTRY" TO c_id_fgts.
              RETURN NO-APPLY.
          END.

          IF c_lacre_fgts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

              MESSAGE "Lacre da Conectividade Social deve ser informado!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              
              APPLY "ENTRY" TO c_lacre_fgts.
              RETURN NO-APPLY.

          END.

          IF i_dig_lacre_fgts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

               MESSAGE "D°gito DO Lacre da Conectividade Social deve ser informado!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              
              APPLY "ENTRY" TO i_dig_lacre_fgts.
              RETURN NO-APPLY.
          END.
      END.

      /* DARF */
      WHEN 2 THEN DO:
           
      END.

      /* GPS */
      WHEN 3 THEN DO:

          
      END.
    END CASE.

   
    FIND ext_espec_docto NO-LOCK WHERE         
         ext_espec_docto.cod_espec_docto = {&tabela}.cod_espec_docto NO-ERROR. 
    IF NOT AVAIL ext_espec_docto THEN NEXT.                                            
                                                                                                               
    IF NOT ext_espec_docto.log_fgts AND NOT ext_espec_docto.log_gps AND NOT ext_espec_docto.log_imposto THEN NEXT. 
                          
      
    DO TRANS:
       {&procura}
        IF NOT AVAIL ext_ITEM_lote_impl_ap THEN DO:
    
            CREATE ext_ITEM_lote_impl_ap.
            ASSIGN ext_ITEM_lote_impl_ap.cod_estab     = {&tabela}.{&campo1} 
                   ext_ITEM_lote_impl_ap.cod_refer     = STRING({&tabela}.{&campo2})
                   ext_ITEM_lote_impl_ap.num_seq_refer = {&tabela}.{&campo3}.
        END.
        
        ASSIGN ext_item_lote_impl_ap.cod_receita    = INPUT FRAME {&frame-name} i_cod_receita
               ext_item_lote_impl_ap.id_contrib     = INPUT FRAME {&frame-name} c_id_contribuinte
               ext_item_lote_impl_ap.dig_lacre      = INPUT FRAME {&frame-name} i_dig_lacre_fgts
               ext_item_lote_impl_ap.id_fgts        = INPUT FRAME {&frame-name} c_id_fgts
               ext_item_lote_impl_ap.lacre_conect   = INPUT FRAME {&frame-name} c_lacre_fgts
               ext_item_lote_impl_ap.perc_impto     = 0
               ext_item_lote_impl_ap.competencia    = INPUT FRAME {&frame-name} dt_competencia
               ext_item_lote_impl_ap.referencia     = INPUT FRAME {&frame-name} c_refer_darf
               ext_item_lote_impl_ap.vl_outras      = INPUT FRAME {&frame-name} d_vlr_outras_gps
               ext_item_lote_impl_ap.vl_receita_bruta = 0
               ext_item_lote_impl_ap.id_contrib       = REPLACE(ext_item_lote_impl_ap.id_contrib,".","")
               ext_item_lote_impl_ap.id_contrib       = REPLACE(ext_item_lote_impl_ap.id_contrib,"/","")
               ext_item_lote_impl_ap.id_fgts          = REPLACE(ext_item_lote_impl_ap.id_fgts,".","")
               ext_item_lote_impl_ap.id_fgts          = REPLACE(ext_item_lote_impl_ap.id_fgts,"/","").
    END.
  
    APPLY "CHOOSE" TO BT-OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cnpj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cnpj D-Dialog
ON CHOOSE OF bt_cnpj IN FRAME D-Dialog /* Button 1 */
DO:

  IF rd_tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN RETURN.

  ASSIGN c_id_contribuinte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  FIND estabelecimento NO-LOCK WHERE 
       estabelecimento.cod_estab = {&tabela}.cod_estab NO-ERROR.
  IF AVAIL estabelecimento
       THEN ASSIGN c_id_contribuinte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(estabelecimento.cod_id_feder,"99.999.999/9999-99").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c_id_contribuinte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_id_contribuinte D-Dialog
ON LEAVE OF c_id_contribuinte IN FRAME D-Dialog /* Id Contribuinte */
DO:
  IF rd_tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN 
      ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME},"999.999.999-99").
  ELSE ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME},"99.999.999/9999-99").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_tipo D-Dialog
ON VALUE-CHANGED OF rd_tipo IN FRAME D-Dialog
DO:
  ASSIGN c_id_contribuinte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY i_fornec c-esp c_titulo c-serie c-parc d-valor dt_vencto i_cod_receita 
          rd_tipo c_id_contribuinte dt_competencia d_vlr_outras_gps c_refer_darf 
          c_id_fgts c_lacre_fgts d_vl_bruto_darf i_dig_lacre_fgts d_perc_darf 
          text-2 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-20 RECT-3 i_cod_receita rd_tipo bt_cnpj 
         c_id_contribuinte dt_competencia d_vlr_outras_gps c_refer_darf 
         c_id_fgts c_lacre_fgts d_vl_bruto_darf i_dig_lacre_fgts d_perc_darf 
         bt-ok bt-ok-2 bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit D-Dialog 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "AP006-W01" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND {&tabela}  NO-LOCK 
      WHERE RECID({&tabela}) = ipr-row NO-ERROR.

  ASSIGN c_titulo:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = {&tabela}.cod_tit_ap
         c-esp:SCREEN-VALUE      IN FRAME {&FRAME-NAME} = {&tabela}.cod_espec_docto
         c-parc:SCREEN-VALUE     IN FRAME {&FRAME-NAME} = {&tabela}.cod_parcela
         c-serie:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = {&tabela}.cod_ser_docto
         dt_vencto:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = STRING({&tabela}.dat_vencto_tit_ap,"99/99/9999")
         i_fornec:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = STRING({&tabela}.cdn_fornec).

  &IF "{1}" = "1" &THEN
        d-valor:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = STRING({&tabela}.val_tit_ap,">>>,>>>,>>>,>>9.99"). 
  &ELSE
       d-valor:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = STRING({&tabela}.val_pagto,">>>,>>>,>>>,>>9.99"). 

       /* Localiza o item da implantaá∆o - qdo existir */
       FIND tit_ap NO-LOCK 
           WHERE tit_ap.cod_estab       = {&tabela}.cod_estab 
           AND   tit_ap.cod_espec_docto = {&tabela}.cod_espec_docto
           AND   tit_ap.cod_ser_docto   = {&tabela}.cod_ser_docto
           AND   tit_ap.cod_tit_ap      = {&tabela}.cod_tit_ap 
           AND   tit_ap.cdn_fornec      = {&tabela}.cdn_fornec 
           AND   tit_ap.cod_parcela     = {&tabela}.cod_parcela NO-ERROR.
       IF AVAIL tit_ap THEN DO:

           FIND item_lote_impl_ap NO-LOCK 
               WHERE item_lote_impl_ap.cod_estab     = tit_ap.cod_estab 
                 AND item_lote_impl_ap.cod_refer     = tit_ap.cod_refer 
                 AND item_lote_impl_ap.num_seq_refer = tit_ap.num_seq_refer NO-ERROR.
           IF AVAIL ITEM_lote_impl_ap THEN DO:
               FIND ext_ITEM_lote_impl_ap  NO-LOCK                                                                     
                       WHERE ext_ITEM_lote_impl_ap.cod_estab     = item_lote_impl_ap.cod_estab                     
                       AND   ext_ITEM_lote_impl_ap.cod_refer     = item_lote_impl_ap.cod_refer                     
                       AND   ext_ITEM_lote_impl_ap.num_seq_refer = item_lote_impl_ap.num_seq_refer  NO-ERROR.        
               IF AVAIL ext_item_lote_impl_ap THEN DO:

                   CREATE bf_ext_item_lote_impl_ap.
                   BUFFER-COPY ext_item_lote_impl_ap EXCEPT ext_ITEM_lote_impl_ap.cod_refer ext_ITEM_lote_impl_ap.num_seq_refer TO bf_ext_item_lote_impl_ap 
                       ASSIGN bf_ext_item_lote_impl_ap.cod_estab      = {&tabela}.cod_estab_bord 
                              bf_ext_item_lote_impl_ap.cod_refer      = STRING({&tabela}.cod_portador)
                              bf_ext_item_lote_impl_ap.num_seq_refer  = {&tabela}.num_id_item_bord_ap.
               END.
           END.
        END.

    &ENDIF

    {&procura} 
    IF AVAIL ext_ITEM_lote_impl_ap THEN 
        ASSIGN i_cod_receita    :screen-value in frame {&frame-name} = STRING(ext_item_lote_impl_ap.cod_receita)
               c_id_contribuinte:screen-value in frame {&frame-name} = ext_item_lote_impl_ap.id_contrib   
               i_dig_lacre_fgts :screen-value in frame {&frame-name} = ext_item_lote_impl_ap.dig_lacre    
               c_id_fgts        :screen-value in frame {&frame-name} = ext_item_lote_impl_ap.id_fgts      
               c_lacre_fgts     :screen-value in frame {&frame-name} = ext_item_lote_impl_ap.lacre_conect 
               dt_competencia   :screen-value in frame {&frame-name} = STRING(ext_item_lote_impl_ap.competencia,"99/99/9999")
               c_refer_darf     :screen-value in frame {&frame-name} = ext_item_lote_impl_ap.referencia   
               d_vlr_outras_gps :screen-value in frame {&frame-name} = STRING(ext_item_lote_impl_ap.vl_outras,">>>,>>>,>>9.99")
               d_perc_darf      :screen-value in frame {&frame-name} = STRING(ext_item_lote_impl_ap.vl_receita_bruta,">>>,>>>,>>9.99")
               d_vl_bruto_darf  :screen-value in frame {&frame-name} = STRING(ext_item_lote_impl_ap.perc_impto,">>9.99").
    
    CASE ipi-tipo:
      /* FGTS */
      WHEN 1 THEN 
          ASSIGN c_refer_darf:VISIBLE        IN FRAME {&FRAME-NAME} = NO 
                 d_perc_darf :VISIBLE        IN FRAME {&FRAME-NAME} = NO  
                 d_vl_bruto_Darf:SENSITIVE   IN FRAME {&FRAME-NAME} = NO  
                 d_vlr_outras_gps:VISIBLE    IN FRAME {&FRAME-NAME} = NO 
                 c_refer_darf:SENSITIVE      IN FRAME {&FRAME-NAME} = NO           
                 d_vlr_outras_gps:SENSITIVE  IN FRAME {&FRAME-NAME} = NO   
                 d_perc_darf :SENSITIVE      IN FRAME {&FRAME-NAME} = NO      
                 d_vl_bruto_darf:SENSITIVE   IN FRAME {&FRAME-NAME} = NO  
                 d_perc_darf :VISIBLE        IN FRAME {&FRAME-NAME} = NO     
                 d_vl_bruto_darf:VISIBLE     IN FRAME {&FRAME-NAME} = NO  
                 text-2:SCREEN-VALUE         IN FRAME {&FRAME-NAME} = "FGTS".

      /* DARF */
      WHEN 2 THEN 
           ASSIGN c_refer_darf:VISIBLE        IN FRAME {&FRAME-NAME} = YES   
                  c_refer_darf:SENSITIVE      IN FRAME {&FRAME-NAME} = YES
                  d_perc_darf :SENSITIVE      IN FRAME {&FRAME-NAME} = YES                     
                  d_vl_bruto_darf:SENSITIVE   IN FRAME {&FRAME-NAME} = YES
                  d_perc_darf :VISIBLE        IN FRAME {&FRAME-NAME} = YES
                  d_vl_bruto_darf:VISIBLE     IN FRAME {&FRAME-NAME} = YES
                  d_vlr_outras_gps:VISIBLE    IN FRAME {&FRAME-NAME} = NO    
                  d_vlr_outras_gps:SENSITIVE  IN FRAME {&FRAME-NAME} = NO    
                  c_id_fgts:SENSITIVE         IN FRAME {&FRAME-NAME} = NO  
                  c_lacre_fgts:SENSITIVE      IN FRAME {&FRAME-NAME} = NO  
                  i_dig_lacre_fgts:SENSITIVE  IN FRAME {&FRAME-NAME} = NO 
                  c_id_fgts:VISIBLE           IN FRAME {&FRAME-NAME} = NO           
                  c_lacre_fgts:VISIBLE        IN FRAME {&FRAME-NAME} = NO        
                  i_dig_lacre_fgts:VISIBLE    IN FRAME {&FRAME-NAME} = NO
                  text-2:SCREEN-VALUE         IN FRAME {&FRAME-NAME} = "DARF".

      /* GPS */
      WHEN 3 THEN
          ASSIGN c_refer_darf:VISIBLE        IN FRAME {&FRAME-NAME} = NO          
                 c_refer_darf:SENSITIVE      IN FRAME {&FRAME-NAME} = NO    
                 d_perc_darf :SENSITIVE      IN FRAME {&FRAME-NAME} = NO   
                 d_vl_bruto_darf:SENSITIVE   IN FRAME {&FRAME-NAME} = NO      
                 d_perc_darf :VISIBLE        IN FRAME {&FRAME-NAME} = NO   
                 d_vl_bruto_darf:VISIBLE     IN FRAME {&FRAME-NAME} = NO 
                 d_vlr_outras_gps:VISIBLE    IN FRAME {&FRAME-NAME} = YES         
                 d_vlr_outras_gps:SENSITIVE  IN FRAME {&FRAME-NAME} = YES         
                 c_id_fgts:SENSITIVE         IN FRAME {&FRAME-NAME} = NO          
                 c_lacre_fgts:SENSITIVE      IN FRAME {&FRAME-NAME} = NO          
                 i_dig_lacre_fgts:SENSITIVE  IN FRAME {&FRAME-NAME} = NO          
                 c_id_fgts:VISIBLE           IN FRAME {&FRAME-NAME} = NO          
                 c_lacre_fgts:VISIBLE        IN FRAME {&FRAME-NAME} = NO          
                 i_dig_lacre_fgts:VISIBLE    IN FRAME {&FRAME-NAME} = NO
                 text-2:SCREEN-VALUE         IN FRAME {&FRAME-NAME} = "GPS".
          
    END CASE.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

