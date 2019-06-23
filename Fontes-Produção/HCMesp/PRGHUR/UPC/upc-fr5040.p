/*---------------------------------------------------------*
** Programa: upc-fp1500.p
** objetivo: Avisar duplicidade de agencia e conta corrente
**            
** Autor   : Joao B. C. Bisneto - DSC-Praxis 
** Data    : 16/03/2016
**---------------------------------------------------------*/

{utp\ut-glob.i}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR NO-UNDO.
DEF INPUT PARAM p-wgh-object AS handle NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS widget-handle NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR NO-UNDO.
DEF INPUT PARAM p-row-table  AS rowid NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-banco            AS WIDGET-HANDLE              NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-agencia          AS WIDGET-HANDLE              NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-conta            AS WIDGET-HANDLE              NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-func             AS WIDGET-HANDLE              NO-UNDO.
def new global shared var v_cod_usuar_corren  like usuar_mestre.cod_usuario no-undo.
DEF NEW GLOBAL SHARED VAR dt-demissao-fp1500  AS DATE                       NO-UNDO.

DEF VAR wh-frame    AS WIDGET-HANDLE                  NO-UNDO.
DEF VAR c-objeto    AS CHAR                           NO-UNDO.
DEF VAR c-cpf       LIKE rh_pessoa_fisic.cod_id_feder NO-UNDO.
DEF VAR l-achou-cta AS LOG                            NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER  NO-UNDO.

DEFINE BUFFER b-funcionario   FOR funcionario.
DEFINE BUFFER b1-funcionario  FOR funcionario.

/**/
def var c-container                                 as char                 no-undo.
def var wgh-grupo                                   as widget-handle        no-undo.
def var wgh-child                                   as widget-handle        no-undo.
def var h-num-niv-sal                               as handle               no-undo.
def var v_log_usuar_espec                           as logical initial no   no-undo.
def var v_dat_aux_histor                            as date                 no-undo.
def var wh-objeto                                   as widget-handle        no-undo.
def new global shared var wh-cdn-estab              as widget-handle    no-undo.
def new global shared var wh-cdn-funcionario        as widget-handle    no-undo.
def new global shared var wh-cod-rh-ccusto          as widget-handle    no-undo.
def new global shared var wh-label-desligto_func    as widget-handle    no-undo.
def new global shared var wh-dat_desligto_func      as widget-handle    no-undo.
/**/

/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)          skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/*
/*pega o nome do programa sem o diretorio e as barras*/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA,"~\"), p-wgh-object:PRIVATE-DATA, "~\").
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(c-objeto,"~/"), c-objeto, "~/").

IF 
  p-ind-event = "VALIDATE"                    AND 
  p-ind-object = "VIEWER"                     AND
  c-objeto     = "v01py085.w"                 AND
  VALID-HANDLE(wh-label-desligto_func)        AND 
  VALID-HANDLE(wh-dat_desligto_func)          THEN 
  DO:
    ASSIGN dt-demissao-fp1500 = date(wh-dat_desligto_func:SCREEN-VALUE).
  END.

IF 
  p-ind-event = "before-initialize"         AND 
  p-ind-object = "VIEWER"                   AND
  c-objeto     = "v02py085.w"               AND 
  NOT VALID-HANDLE(wh-label-desligto_func)  AND 
  NOT VALID-HANDLE(wh-dat_desligto_func)    THEN 
  DO:
    ASSIGN dt-demissao-fp1500 = ?.
    create text wh-label-desligto_func
    assign frame             = p-wgh-frame
           format            = "x(18)"
           width             = 13.00
           height            = 0.88
           screen-value      = "Desligto Func:"
           row               = 10.5
           col               = 40.20.
    create fill-in wh-dat_desligto_func 
    assign frame              = p-wgh-frame
           name               = "wh-dat_desligto_func"
           data-type          = "date"
           format             = "99/99/9999"
           width              = 10.00
           height             = 0.88
           row                = 10.5
           col                = 51.00
           visible            = yes
           sensitive          = no
           HELP               = "Desligto Func".
  END.  
/*----------------------------------------------------------------------------*/
if 
  p-ind-object = "VIEWER"     AND
  c-objeto     = "v02py085.w" THEN 
  do:        
    FIND FIRST b-funcionario 
      NO-LOCK
      WHERE ROWID(b-funcionario) = p-row-table
      NO-ERROR.
    IF AVAIL b-funcionario THEN
      DO: 
        case p-ind-event:  
          when "display" then do:
              find first func_desligto 
                NO-LOCK
                where func_desligto.cdn_empresa     = b-funcionario.cdn_empresa
                and   func_desligto.cdn_estab       = b-funcionario.cdn_estab
                and   func_desligto.cdn_funcionario = b-funcionario.cdn_funcionario 
                no-error.
              if avail func_desligto then do:
                  if func_desligto.dat_desligto <> ? then do:
                      assign wh-dat_desligto_func:screen-value = string(func_desligto.dat_desligto).
                  end.
                  else do:
                      assign wh-dat_desligto_func:screen-value = ?.
                  end.
              end.
              else do:
                  assign wh-dat_desligto_func:screen-value = ?.
              end.
              
          end.
          when "enable" then do:
              assign wh-dat_desligto_func:sensitive = yes.
          end.
          when "disable" then do:
              assign wh-dat_desligto_func:sensitive = no.
          end.
        end case.
      END.
  END.
/*----------------------------------------------------------------------------*/
IF 
  p-ind-event = "after-end-update"                          AND 
  p-ind-object = "VIEWER"                                   AND
  p-wgh-object:PRIVATE-DATA = "object/sopy/vwr/v01py085.w"  THEN 
  DO:
    FIND FIRST b-funcionario 
      NO-LOCK
      WHERE ROWID(b-funcionario) = p-row-table
      NO-ERROR.
    IF AVAIL b-funcionario THEN
      DO: 
        ASSIGN wh-dat_desligto_func:SCREEN-VALUE = STRING(dt-demissao-fp1500).
        RUN pi-mensag-conta.
        /*-------------------------------------------------------------------------------*/
        find first func_desligto 
          where func_desligto.cdn_empresa     = b-funcionario.cdn_empresa
          and   func_desligto.cdn_estab       = b-funcionario.cdn_estab
          and   func_desligto.cdn_funcionario = b-funcionario.cdn_funcionario 
          no-error.
        if not avail func_desligto then 
          DO:
            create func_desligto.
            assign 
              func_desligto.cdn_empresa     = b-funcionario.cdn_empresa                      
              func_desligto.cdn_estab       = b-funcionario.cdn_estab            
              func_desligto.cdn_funcionario = b-funcionario.cdn_funcionario.
          END.
        ASSIGN          
          func_desligto.existe-cta      = l-achou-cta
          func_desligto.usuar_alter     = v_cod_usuar_corren.
        IF 
          func_desligto.dat_desligto <> date(wh-dat_desligto_func:screen-value) THEN
              DO:
                ASSIGN 
                  func_desligto.dat_desligto    = DATE(wh-dat_desligto_func:SCREEN-VALUE).
                RUN prghur\upc\fp1500ymupa.p(
                  INPUT ROWID(b-funcionario),
                  INPUT ROWID(func_desligto)).
              END.
        /*-------------------------------------------------------------------------------*/
      END.
  END.
PROCEDURE pi-mensag-conta:
  ASSIGN c-cpf = "" l-achou-cta = NO.
  RUN pi-busca-cpf(OUTPUT c-cpf).  
  FIND FIRST b1-funcionario 
    NO-LOCK
    WHERE b1-funcionario.cdn_cta_corren     =  b-funcionario.cdn_cta_corren    
    AND   b1-funcionario.cdn_bco_liq        =  b-funcionario.cdn_bco_liq       
    AND   b1-funcionario.cdn_agenc_bcia_liq =  b-funcionario.cdn_agenc_bcia_liq
    AND   b1-funcionario.cod_id_feder       <> b-funcionario.cod_id_feder
    NO-ERROR.
  IF AVAIL b1-funcionario THEN 
    DO:
      /*-----------------------------------------------------------------------*/ 
      ASSIGN l-achou-cta = YES.
      /*-----------------------------------------------------------------------*/ 
      DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
        LABEL "&Fechar" 
        SIZE 10 BY 1
        BGCOLOR 8.
      DEFINE RECTANGLE db-rt-botoes
        EDGE-PIXELS 2 GRAPHIC-EDGE  
        SIZE 58 BY 1.42
        BGCOLOR 7.  
      DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT  "X(45)" NO-UNDO.
      ASSIGN c-mensagem = 
        "Matr: " + STRING(b-funcionario.cdn_funcionario) + " - " +
        b-funcionario.nom_pessoa_fisic    + CHR(10) + 
        "Ctps: "+ b-funcionario.cod_cart_trab + "  Serie: " + 
        b-funcionario.cod_ser_cart_trab + CHR(10) + 
        "Pis: " + b-funcionario.cod_pis + CHR(10) + 
        "Banco: " + string(b-funcionario.cdn_bco_liq) + 
        "  Agencia: " + STRING(b-funcionario.cdn_agenc_bcia_liq) + 
        "  Conta: "+ string(b-funcionario.cdn_cta_corren) + "-" +
        STRING(b-funcionario.cod_digito_cta_corren)  .
      DEFINE RECTANGLE db-rect-1
       EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
       SIZE 59 BY 4.30.
      DEFINE FRAME db-frame-1
        c-mensagem NO-LABEL VIEW-AS EDITOR  SIZE 55 BY 3
           at ROW 2.7 col 2 NO-TAB-STOP FONT 0
        db-rect-1 AT ROW 1.9 COL 01
        db-bt-cancel      AT ROW 7.3 COL 23             
        db-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
        THREE-D SCROLLABLE TITLE "* * * Conta corrente Duplicada! * * *" FONT 1
        DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.
      DISPLAY c-mensagem WITH FRAME db-frame-1.
      ASSIGN 
        c-mensagem:SENSITIVE = YES
        c-mensagem:read-only = yes
        c-mensagem:AUTO-RESIZE = yes.
      ENABLE db-bt-cancel 
        WITH FRAME db-frame-1. 
      WAIT-FOR "GO":U OF FRAME db-frame-1.
      /*-----------------------------------------------------------------------*/
    END.
END PROCEDURE.
PROCEDURE pi-busca-cpf:
  DEF OUTPUT PARAM p_cpf             LIKE rh_pessoa_fisic.cod_id_feder NO-UNDO.
  /*------------------------------------------------------------------------*/
  IF AVAIL b-funcionario THEN
    DO:
      FIND FIRST rh_pessoa_fisic 
        NO-LOCK 
        WHERE rh_pessoa_fisic.num_pessoa_fisic = b-funcionario.num_pessoa_fisic
        NO-ERROR.
      IF AVAIL rh_pessoa_fisic THEN
        ASSIGN p_cpf = rh_pessoa_fisic.cod_id_feder.
    END.  
  /*------------------------------------------------------------------------*/
END PROCEDURE.
*/
