/***************************************************************************
 **Programa.: ESFR0001RP.P
 **Autor....: FLµVIO CAPITANIO (11) 9756-8761 
 **Objetivo.: Gera Programaá∆o de FÇrias
 **Data.....: 19 de Maráo de 2.004 - 03062005
 **Cliente..: 
 ***************************************************************************/

/*Include para controle de Versao*/
{include/buffers_RH.i}

{include/i-prgvrs.i esfr0001RP 2.06.00.000}.

/*Definicao de Temp-Table's para recebimento de parametros*/

DEFINE temp-table tt-param
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
    field v_dat_valid             as date format "99/99/9999"
    field v_log_expande_estrut    as log
    field v_num_salta_pg          as integer
    field v_num_quebra            as integer
    field v_num_faixa             as integer
    field destino                 as integer
    field arquivo                 as char format "x(35)"
    field usuario                 as char format "x(12)"
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field cc-codigo-ini           like func_ptoelet.cod_rh_ccusto
    field cc-codigo-fim           like func_ptoelet.cod_rh_ccusto
    field v_log_mensal            as logical
    field v_log_horista           as logical
    field v_log_semanal           as logical
    field v_log_quinzenal         as logical
    field v_log_tarefa            as logical
    field v_log_diarista          as logical
    field v_log_usa_ponto         as logical
    field v_log_ass_resp          as logical
    field v_num_tip_aces_usuar    as integer format "9"
    FIELD v_dt_admissao           AS DATE FORMAT "99/99/9999"
    FIELD c-mes                   AS INTE
    FIELD c-ano                   AS INTE
    FIELD tb-parametro            AS LOGICAL
    FIELD v_dt_pt_fim             AS DATE 
    FIELD v_dt_pt_ini             AS DATE 
    FIELD l-digita                AS LOGICAL.

define temp-table tt-digita no-undo
       field v_cdn_empres_usuar like empresa.ep-codigo INIT 1
       field v_cdn_estab        LIKE funcionario.cdn_estab     
       field v_cdn_funcionario  LIKE funcionario.cdn_funcionario     
       FIELD v_num_dv_func      LIKE funcionario.num_digito_verfdor_func    
       FIELD v_nom_pessoa       LIKE funcionario.nom_pessoa_fisic
       field cdn_plano_lotac    LIKE plano_lotac.cdn_plano_lotac
       field v_cod_unid_lotac   LIKE unid_lotac.cod_unid_lotac
       FIELD v_cs_codigo        LIKE funcionario.cdn_categ_sal
    index id is primary 
          v_cdn_empres_usuar
          v_cdn_estab
          v_cdn_funcionario
          v_num_dv_func.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def new shared var v_han_acomp as handle no-undo. /* deve ser criada */
Def Buffer b_habilit_ferias For habilit_ferias.

RUN utp/ut-acomp.p PERSISTENT SET v_han_acomp.

/*{prghur/fpp/FP4140tt.i NEW shared}  /* cria tt-param padrao para selecao da folha */     */
                           
{prghur/fpp/fp9200.i10 NEW shared}  /* cria tt_lotac_funcionario e esta bem fechado */   
{prghur/fpp/fp9200.i8}              /* cria algumas variaveis importantes para lotacao */

/*Recebimento de Parametros*/

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-digita.
   RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

{prghur/fpp/fp9200.i6}                    /* esta include alimenta a temp-table */

RUN pi-finalizar IN v_han_acomp.

/*Include padrao para variaveis de relatorio*/

{include/i-rpvar.i}

/*Definicao de variaveis*/
DEF VAR l_cons_13   AS c FORM "!"                          NO-UNDO.
DEF VAR ccusto-ant  LIKE funcionario.cod_rh_ccusto         NO-UNDO.
def var c-texto     AS c form "x(30)"                      NO-UNDO.
Def var dt-ref      As Date                                No-undo.
DEF var i-mm-ref    LIKE control_seq_calc.num_mes_pagto_fp NO-UNDO.
DEF var i-aa-ref    LIKE control_seq_calc.num_ano_pagto_fp NO-UNDO.
DEF var i-es-ini    LIKE rh_estab.cdn_estab                NO-UNDO.
DEF var i-es-fim    LIKE rh_estab.cdn_estab                NO-UNDO.
DEF var c-cc-ini    LIKE rh_ccusto.cod_rh_ccusto           NO-UNDO.
DEF var c-cc-fim    LIKE rh_ccusto.cod_rh_ccusto           NO-UNDO.
DEF var dt-pto-ini  LIKE funcionario.dat_desligto_func     NO-UNDO.
DEF var dt-pto-fim  LIKE funcionario.dat_desligto_func     NO-UNDO.
DEF var dt-admis    LIKE funcionario.dat_admis_func        NO-UNDO.
DEF var i-ep-codigo LIKE empresa.ep-codigo                 NO-UNDO.
DEF var i-cc-ini    LIKE rh_ccusto.cod_rh_ccusto           NO-UNDO.
DEF var i-cc-fim    LIKE rh_ccusto.cod_rh_ccusto           NO-UNDO.
DEF var i-es-ant    LIKE rh_estab.cdn_estab                NO-UNDO.
DEF var i-cgc-ant   LIKE rh_estab.cod_alter_cgc_cei        NO-UNDO.
DEF var i-rel-ant   LIKE funcionario.num_relogio_pto       NO-UNDO.
DEF var i-chap-ant  LIKE funcionario.num_chap_cartao_pto   NO-UNDO.
DEF var c-cs-ini    LIKE categ_sal.cdn_categ_sal           NO-UNDO.
DEF var c-cs-fim    LIKE categ_sal.cdn_categ_sal           NO-UNDO.

DEF var l-sel-func AS log                 NO-UNDO.
DEF var i-pto-ini  AS int FORMAT ">>>>>9" NO-UNDO.
DEF var i-pto-fim  AS int FORMAT ">>>>>9" NO-UNDO.
DEF var i-ord-aux  AS INT                 NO-UNDO.
DEF var i-fc-ini   AS int FORMAT "99999999" initial 0     NO-UNDO.
DEF var i-fc-fim   AS int FORMAT "99999999" initial 99999999 NO-UNDO.
DEF var i-index    AS int initial 0   NO-UNDO.
DEF var l-primeiro AS log initial YES NO-UNDO.
Def Var dt-aux     As Date   Extent 6.

/*-------------- Array da etiquetas do ponto --------------*/
DEF var c-estab   AS char FORMAT "x(40)"      extent 3 NO-UNDO.
DEF var c-ccusto  AS char FORMAT "x(40)"      extent 3 NO-UNDO.
DEF var i-mat     LIKE funcionario.cdn_funciona form "zzzz9"   extent 3 NO-UNDO.
DEF var c-nome    LIKE funcionario.nom_pessoa_fisic extent 3 NO-UNDO.
DEF var c-dig     AS char FORMAT "x(02)"      extent 3 NO-UNDO.
DEF var c-periodo AS char FORMAT "x(40)"      extent 3 NO-UNDO.
Def Var rec-fer   As Recid.
/*Bloco principal do programa*/

FIND FIRST param_empres_rh NO-LOCK NO-ERROR.
ASSIGN i-ep-codigo = IF AVAIL param_empres_rh THEN param_empres_rh.cdn_empresa ELSE "1".
FIND empresa WHERE empresa.ep-codigo = i-ep-codigo NO-LOCK NO-ERROR.

ASSIGN c-programa     = "esfr0001RP"
       c-versao       = "2.08"
       c-revisao      = ".00.000"
       c-empresa      = empresa.razao-social
       c-sistema      = "FêRIAS/RESCIS«O"
       c-titulo-relat = "GERA PROGRAMAÄ«O DE FêRIAS COM BASE ÈLTIMO PER÷ODO"
       .

ASSIGN i-ord-aux   = tt-param.classifica
       l-sel-func  = tt-param.l-digita
       i-ep-codigo = tt-param.v_cdn_empres_usuar
       i-cc-ini    = tt-param.cc-codigo-ini
       i-cc-fim    = tt-param.cc-codigo-fim
       dt-admis    = tt-param.v_dt_admissao
       i-aa-ref    = tt-param.c-ano
       i-mm-ref    = tt-param.c-mes
       dt-pto-ini  = tt-param.v_dt_pt_ini
       dt-pto-fim  = tt-param.v_dt_pt_fim
       i-es-ini    = tt-param.i-es-ini
       i-es-fim    = tt-param.i-es-fim
       c-texto  = "___/___/_____  ____  ____  ____".
                                                                
/*include padrao para saida de relatorios*/

{include/i-rpout.i}

RUN utp/ut-acomp.p PERSISTENT SET v_han_acomp.
{utp/ut-liter.i "Listagem Programaá∆o de FÇrias" *}

RUN pi-inicializar IN v_han_acomp (INPUT RETURN-VALUE).
IF i-ord-aux < 9 THEN
DO:
   IF NOT l-sel-func THEN
   DO:
      FOR EACH tt-digita:
          DELETE tt-digita.
      END.
      CREATE tt-digita.
      ASSIGN tt-digita.v_cdn_estab = "1".
   END.

   /*MESSAGE "Passei " i-ord-aux VIEW-AS ALERT-BOX.  */
   FOR EACH tt-digita:
      IF l-sel-func 
      THEN ASSIGN i-es-ini = tt-digita.v_cdn_estab
                  i-es-fim = tt-digita.v_cdn_estab
                  c-cs-ini = tt-digita.v_cs_codigo
                  c-cs-fim = tt-digita.v_cs_codigo
                  i-fc-ini = tt-digita.v_cdn_funcionario
                  i-fc-fim = tt-digita.v_cdn_funcionario
                  dt-admis = date(01,01,0001).
      ELSE ASSIGN i-fc-ini = 0
                  i-fc-fim = 99999999.
      
      FOR EACH funcionario NO-LOCK /* USE-INDEX fncnr_py08504 */ WHERE
               funcionario.cdn_empresa      = i-ep-codigo 
           AND funcionario.cdn_estab       >= i-es-ini 
           AND funcionario.cdn_estab       <= i-es-fim 
           AND funcionario.cod_rh_ccusto   >= i-cc-ini 
           AND funcionario.cod_rh_ccusto   <= i-cc-fim 
           AND funcionario.cod_unid_lotac  >= tt-param.v_cod_unid_lotac_ini
           AND funcionario.cod_unid_lotac  <= tt-param.v_cod_unid_lotac_fim
           AND ( NOT l-sel-func 
           AND ((funcionario.cdn_categ_sal  = 1 AND tt-param.v_log_mensal)    OR 
                (funcionario.cdn_categ_sal  = 2 AND tt-param.v_log_horista)   OR                        
                (funcionario.cdn_categ_sal  = 3 AND tt-param.v_log_semanal)   OR                        
                (funcionario.cdn_categ_sal  = 4 AND tt-param.v_log_quinzenal) OR                        
                (funcionario.cdn_categ_sal  = 5 AND tt-param.v_log_tarefa)    OR                        
                (funcionario.cdn_categ_sal  = 6 AND tt-param.v_log_diarista)) OR 
              l-sel-func
          AND funcionario.cdn_categ_sal    >= c-cs-ini 
          AND funcionario.cdn_categ_sal    <= c-cs-fim)
          AND funcionario.cdn_funciona     >= i-fc-ini 
          AND funcionario.cdn_funciona     <= i-fc-fim 
          AND funcionario.dat_admis_func   >= dt-admis 
          AND funcionario.dat_desligto_func = ? /* OR 
              (year(funcionario.dat_desligto_func)  > i-aa-ref OR
              (year(funcionario.dat_desligto_func)  = i-aa-ref AND
              month(funcionario.dat_desligto_func) >= i-mm-ref))) */
          BREAK BY IF (i-ord-aux = 7  OR i-ord-aux = 8) THEN 
                      funcionario.cod_rh_ccusto
                   ELSE
                      string(funcionario.cdn_estab,"999")
                BY IF LOOKUP(string(i-ord-aux), "1,3,5,7,9" ) > 0 THEN
                      string(funcionario.cdn_funciona,"99999999")   
                   ELSE funcionario.nom_pessoa_fisic  :           
                  RUN pi-acompanhar IN v_han_acomp (INPUT "Estabel/Funcionario: " + 
                                               STRING(funcionario.cdn_estab) +
                                               "/" +
                                               STRING(funcionario.cdn_funcionario)).
    
            FIND rh_estab    OF funcionario NO-LOCK.
            FIND rh_ccusto   OF funcionario NO-LOCK.
            Find First period_aqst_ferias of funcionario  where
                       period_aqst_ferias.idi_sit_period_aqst_ferias = 1 No-error.
            If Not Avail period_aqst_ferias Then Next.
            Run pi_grava_programacao_periodo(Output rec-fer).
            IF (tt-param.classific = 7 OR tt-param.classific = 8) then 
               IF ccusto-ant <> funcionario.cod_rh_ccusto then
                  put "Centro de Custo: " at 25 funcionario.cod_rh_ccusto form "99999" "-"
                       rh_ccusto.des_rh_ccusto  skip(1).
               ELSE
                 if i-es-ant <> funcionario.cdn_estab then
                    PUT "Estabelecimento: " AT 25 funcionario.cdn_estab FORM "ZZ9" "-"
                        rh_estab.nom_pessoa_jurid SKIP(1).
            Find habilit_ferias Where Recid(habilit_ferias) = rec-fer Exclusive-lock No-error.
            
            If (rec-fer = ?  Or Not Avail habilit_ferias) Then Next.
            IF habilit_ferias.vli_perc_adiant_13o > 0 THEN
               ASSIGN l_cons_13 = "S".
            ELSE
               ASSIGN l_cons_13 = "N". 
            
            DISP funcionario.cdn_funcionario
            funcionario.nom_pessoa_fisic
            period_aqst_ferias.dat_inic_period_aqst_ferias    column-label "-------!Inicio"
            period_aqst_ferias.dat_term_period_aqst_ferias    column-label "Per°odo!TÇrmino"
            period_aqst_ferias.qtd_dias_direito_period_aqst   column-label "----!Dir"
            period_aqst_ferias.qtd_dias_ferias_concedid       column-label "- - !Conc"
            habilit_ferias.dat_inic_ferias                    column-label "Dias --  ---!Inicio"
            Entry(habilit_ferias.idi_tip_ferias,"Normal,Coletiva") column-label "------------!Tipo    "
            habilit_ferias.qtd_dias_ferias_gozar              column-label "Progra! Gozo"
            habilit_ferias.qtd_dias_licenc                    column-label "maá∆o !  Lic"
            habilit_ferias.qtd_dias_abono_ferias              column-label "------!Abono"
            /*  (period_aqst_ferias.qtd_dias_direito_period_aqst - period_aqst_ferias.qtd_dias_ferias_concedid) column-label "-----!Saldo" */
            l_cons_13                                         column-label "Dias!13S "
            (30 - period_aqst_ferias.qtd_dias_direito_period_aqst) column-label "   Adt!Faltas"
            /*      c-texto column-label "--- Programaá∆o  --- Adto!Inicio         Gozo  Abono 13s"  */
            with stream-io use-text width 230.

            assign ccusto-ant = funcionario.cod_rh_ccusto
                   i-es-ant   = funcionario.cdn_estab
                   rec-fer  = ?.
    
      END. /*** funcionario **/    
   END.    /** tt-digita **/
END. /* i-ord-aux **/

IF i-ord-aux > 9 THEN
DO:
   IF NOT l-sel-func THEN
   DO:
      FOR EACH tt-digita:
         DELETE tt-digita.
      END.
      CREATE tt-digita.
      ASSIGN tt-digita.v_cdn_estab = "1".
   END.
  
   FOR EACH tt-digita:
      IF l-sel-func 
      THEN ASSIGN i-es-ini = tt-digita.v_cdn_estab
                  i-es-fim = tt-digita.v_cdn_estab
                  c-cs-ini = tt-digita.v_cs_codigo
                  c-cs-fim = tt-digita.v_cs_codigo
                  i-fc-ini = tt-digita.v_cdn_funcionario
                  i-fc-fim = tt-digita.v_cdn_funcionario
                  dt-admis = date(01,01,0001).
      ELSE ASSIGN i-fc-ini = 1
                  i-fc-fim = 99999999.
    
      FOR EACH tt_lotac_funcionario NO-LOCK,
          EACH funcionario OF tt_lotac_funcionario NO-LOCK /*USE-INDEX fncnr_py08504 */
         WHERE funcionario.cdn_empresa      = i-ep-codigo 
           AND funcionario.cdn_estab       >= i-es-ini 
           AND funcionario.cdn_estab       <= i-es-fim 
           AND funcionario.cod_rh_ccusto   >= i-cc-ini 
           AND funcionario.cod_rh_ccusto   <= i-cc-fim 
           AND funcionario.cod_unid_lotac  >= tt-param.v_cod_unid_lotac_ini
           AND funcionario.cod_unid_lotac  <= tt-param.v_cod_unid_lotac_fim
           AND ( NOT l-sel-func 
           AND ((funcionario.cdn_categ_sal  = 1 AND tt-param.v_log_mensal)    OR 
                (funcionario.cdn_categ_sal  = 2 AND tt-param.v_log_horista)   OR                        
                (funcionario.cdn_categ_sal  = 3 AND tt-param.v_log_semanal)   OR                        
                (funcionario.cdn_categ_sal  = 4 AND tt-param.v_log_quinzenal) OR                        
                (funcionario.cdn_categ_sal  = 5 AND tt-param.v_log_tarefa)    OR                        
                (funcionario.cdn_categ_sal  = 6 AND tt-param.v_log_diarista)) OR 
               l-sel-func
           AND funcionario.cdn_categ_sal   >= c-cs-ini 
           AND funcionario.cdn_categ_sal   <= c-cs-fim)
           AND funcionario.cdn_funciona    >= i-fc-ini 
           AND funcionario.cdn_funciona    <= i-fc-fim 
           AND funcionario.dat_admis_func  >= dt-admis 
           AND funcionario.dat_desligto_func = ? /*OR
               (year(funcionario.dat_desligto_func)  > i-aa-ref  OR
               (year(funcionario.dat_desligto_func)   = i-aa-ref AND
                month(funcionario.dat_desligto_func) >= i-mm-ref))),
          EACH period_aqst_ferias of funcionario  where
               period_aqst_ferias.idi_sit_period_aqst_ferias = 1 and
              (period_aqst_ferias.qtd_dias_direito_period_aqst - period_aqst_ferias.qtd_dias_ferias_concedid) > 0,
          EACH habilit_ferias OF period_aqst_ferias WHERE
               habilit_ferias.dat_inic_ferias >= tt-param.v_dt_pt_ini AND
               habilit_ferias.dat_inic_ferias <= tt-param.v_dt_pt_fim   */
         BREAK BY funcionario.cdn_estab
               BY tt_lotac_funcionario.num_seq_unid_lotac
               BY tt_lotac_funcionario.num_niv_unid_lotac
               BY tt_lotac_funcionario.cod_unid_lotac
               BY IF i-ord-aux = 9
                  THEN string(funcionario.cdn_funciona,"99999999")
                  ELSE funcionario.nom_pessoa_fisic:
        
         RUN pi-acompanhar IN v_han_acomp (INPUT "Estabel/Funcionario: " + 
                                           STRING(funcionario.cdn_estab) +
                                           "/" +
                                           STRING(funcionario.cdn_funcionario)).
         Find First period_aqst_ferias of funcionario  where
                    period_aqst_ferias.idi_sit_period_aqst_ferias = 1 No-error.
         If Not Avail period_aqst_ferias Then Next.
         Run pi_grava_programacao_periodo(Output rec-fer).
         
         FIND rh_ccusto  OF funcionario NO-LOCK.
         FIND rh_estab   OF funcionario NO-LOCK.
         PUT "Estabelecimento: " AT 25 funcionario.cdn_estab FORM "ZZ9" "-"
              rh_estab.nom_pessoa_jurid SKIP(1).
         Find habilit_ferias Where Recid(habilit_ferias) = rec-fer Exclusive-lock No-error.
         If Not Avail habilit_ferias Then Next.
         IF habilit_ferias.vli_perc_adiant_13o > 0 THEN
            ASSIGN l_cons_13 = "S".
         ELSE
            ASSIGN l_cons_13 = "N". 

         DISP funcionario.cdn_funcionario
              funcionario.nom_pessoa_fisic
              period_aqst_ferias.dat_inic_period_aqst_ferias  column-label "-------!Inicio"
              period_aqst_ferias.dat_term_period_aqst_ferias  column-label "Per°odo!TÇrmino"
              period_aqst_ferias.qtd_dias_direito_period_aqst column-label "----!Dir"
              period_aqst_ferias.qtd_dias_ferias_concedid     column-label "- - !Conc"
              habilit_ferias.dat_inic_ferias                  column-label "Dias --  ---!Inicio"
              Entry(habilit_ferias.idi_tip_ferias,"Normal,Coletiva") column-label "------------!Tipo    "
              habilit_ferias.qtd_dias_ferias_gozar            column-label "Progra! Gozo"
              habilit_ferias.qtd_dias_licenc                  column-label "maá∆o !  Lic"
              habilit_ferias.qtd_dias_abono_ferias            column-label "------!Abono"
             /* (period_aqst_ferias.qtd_dias_direito_period_aqst - period_aqst_ferias.qtd_dias_ferias_concedid) column-label "-----!Saldo" */
              l_cons_13                                       column-label "Dias!13S "
              (30 - period_aqst_ferias.qtd_dias_direito_period_aqst) column-label "   Adt!Faltas"
              with stream-io use-text width 230.
          ASSIGN ccusto-ant = funcionario.cod_rh_ccusto
                 i-es-ant   = funcionario.cdn_estab
                 rec-fer = ?.
      END. /* funcionario **/   
   END.
END.
    

Procedure pi_grava_programacao_periodo:
    Define Output Parameter rec-fer As Recid. 
    Def Var i-ano As Int.
    
    /*Find Last b_habilit_ferias OF funcionario Exclusive-lock No-error. */
    Find Last b_habilit_ferias Where 
              b_habilit_ferias.cdn_empresa     = funcionario.cdn_empresa And
              b_habilit_ferias.cdn_estab       = funcionario.cdn_estab And
              b_habilit_ferias.cdn_funcionario = funcionario.cdn_funcionario Exclusive-lock No-error.
    If  Avail b_habilit_ferias Then
    Do:
      Assign i-ano     = Year(b_habilit_ferias.dat_inic_ferias) + 1
             dt-aux[1] = Date(Month(b_habilit_ferias.dat_inic_ferias),Day(b_habilit_ferias.dat_inic_ferias),i-ano).
             dt-ref    = dt-aux[1].
      Run pi_calcula_data.
      Assign dt-aux[1] = dt-ref.
      Assign dt-aux[2] = Date(Month(b_habilit_ferias.dat_inic_concess_ferias),Day(b_habilit_ferias.dat_inic_concess_ferias),i-ano).
              dt-ref    = dt-aux[2].                                                                                                                 
      Run pi_calcula_data.                                                                                                                          
      Assign dt-aux[2] = dt-ref.                                                                                                                    
      Assign dt-aux[3] = Date(Month(b_habilit_ferias.dat_inic_abono_ferias),Day(b_habilit_ferias.dat_inic_abono_ferias),i-ano).
              dt-ref    = dt-aux[3].                                                                                                                 
      Run pi_calcula_data.                                                         
      Assign dt-aux[3] = dt-ref.
      Assign dt-aux[4] = Date(Month(b_habilit_ferias.dat_solicit_abono),Day(b_habilit_ferias.dat_solicit_abono),i-ano).
             dt-ref    = dt-aux[4].                                                                                                                 
      Run pi_calcula_data.                                                                                                                          
      Assign dt-aux[4] = dt-ref.            
      Assign dt-aux[5] = Date(Month(b_habilit_ferias.dat_aviso_ferias),Day(b_habilit_ferias.dat_aviso_ferias),i-ano).
             dt-ref    = dt-aux[5].                                                                                                                 
      Run pi_calcula_data.                                                                                                                          
      Assign dt-aux[5] = dt-ref.
       Assign dt-aux[6] = Date(Month(b_habilit_ferias.dat_pagto_ferias),Day(b_habilit_ferias.dat_pagto_ferias),i-ano).
             dt-ref    = dt-aux[6].                                                                                                                 
      Run pi_calcula_data.                                                                                                                          
      Assign dt-aux[6] = dt-ref.  
      Find habilit_ferias Where habilit_ferias.cdn_empresa     = funcionario.cdn_empresa And
                                habilit_ferias.cdn_estab       = funcionario.cdn_estab And
                                habilit_ferias.cdn_funcionario = funcionario.cdn_funcionario And
                                habilit_ferias.dat_inic_ferias = dt-aux[1] And            
                                habilit_ferias.dat_inic_period_aqst_ferias = dt-aux[2] No-error.
      If Not Avail habilit_ferias Then
      Do:                      
         Buffer-copy b_habilit_ferias Except b_habilit_ferias.dat_inic_ferias             
                               b_habilit_ferias.dat_inic_period_aqst_ferias
                               b_habilit_ferias.dat_term_period_aqst_ferias
                               b_habilit_ferias.dat_inic_concess_ferias 
                               b_habilit_ferias.dat_term_concess_ferias 
                               b_habilit_ferias.dat_inic_abono_ferias   
                               b_habilit_ferias.dat_term_abono_ferias   
                               b_habilit_ferias.dat_solicit_abono       
                               b_habilit_ferias.dat_aviso_ferias        
                               b_habilit_ferias.dat_pagto_ferias
                               b_habilit_ferias.dat_ult_atualiz
                               b_habilit_ferias.idi_tip_ferias
                       To        habilit_ferias
                       ASSIGN
                              habilit_ferias.dat_inic_ferias             = dt-aux[1]
                              habilit_ferias.dat_inic_period_aqst_ferias = period_aqst_ferias.dat_inic_period_aqst_ferias
                              habilit_ferias.dat_term_period_aqst_ferias = period_aqst_ferias.dat_term_period_aqst_ferias
                              habilit_ferias.dat_inic_concess_ferias     = dt-aux[2]
                              habilit_ferias.dat_term_concess_ferias     = dt-aux[2] + b_habilit_ferias.qtd_dias_ferias_gozar
                              habilit_ferias.dat_inic_abono_ferias       = dt-aux[3]
                              habilit_ferias.dat_term_abono_ferias       = dt-aux[3] + b_habilit_ferias.qtd_dias_abono_ferias
                              habilit_ferias.dat_solicit_abono           = dt-aux[4]
                              habilit_ferias.dat_aviso_ferias            = dt-aux[5]
                              habilit_ferias.dat_pagto_ferias            = dt-aux[6]
                              habilit_ferias.dat_ult_atualiz             = Today
                              habilit_ferias.idi_tip_ferias              = 1
                              rec-fer = Recid(habilit_ferias).

          Find sit_afast_func Where
               sit_afast_func.cdn_empresa         = habilit_ferias.cdn_empresa And
               sit_afast_func.cdn_estab           = habilit_ferias.cdn_estab      And
               sit_afast_func.cdn_funcionario     = habilit_ferias.cdn_funcionario  And
               sit_afast_func.dat_inic_sit_afast  = habilit_ferias.dat_inic_ferias  And
               sit_afast_func.cdn_sit_afast_func  = 90 No-error.
          If Not Avail sit_afast_func Then
          Do:
              Create sit_afast_func.      
               ASSIGN
                 sit_afast_func.cdn_empresa                    = habilit_ferias.cdn_empresa
                 sit_afast_func.cdn_estab                      = habilit_ferias.cdn_estab
                 sit_afast_func.cdn_funcionario                = habilit_ferias.cdn_funcionario
                 sit_afast_func.dat_inic_sit_afast             = habilit_ferias.dat_inic_ferias
                 sit_afast_func.dat_inic_proces_sit_afast      = habilit_ferias.dat_inic_ferias
                 sit_afast_func.dat_term_sit_afast             = habilit_ferias.dat_term_concess_ferias
                 sit_afast_func.dat_term_proces_sit_afast      = habilit_ferias.dat_term_concess_ferias
                 sit_afast_func.dat_integr_sit_afast_func      = habilit_ferias.dat_term_concess_ferias
                 sit_afast_func.dat_ant_integr_sit_afast_func  = habilit_ferias.dat_term_concess_ferias
                 sit_afast_func.idi_sit_afast_func             = 2
                 sit_afast_func.log_sit_marcac_ptoelet_tratada = Yes
                 sit_afast_func.cdn_sit_afast_func             = 90
                 sit_afast_func.num_mes_ano_refer_fp           = string(Month(habilit_ferias.dat_inic_concess_ferias),"99") +
                                                                 String(Year(habilit_ferias.dat_inic_concess_ferias))
                 sit_afast_func.qti_dias_sit_func              = habilit_ferias.qtd_dias_ferias_gozar
                 sit_afast_func.cod_usuar_ult_atualiz          = tt-param.usuario
                 sit_afast_func.dat_ult_atualiz                = Today.
            
          End.
          /*    Message   " b_habilit_ferias.dat_inic_ferias" b_habilit_ferias.dat_inic_ferias Skip
                          "rec-fer Procedure " rec-fer Skip
                          "Data1" dt-aux[1] Skip
                          "Data2" dt-aux[2] Skip     View-as Alert-box.
            */
      End.
    End.
End Procedure.

Procedure pi_calcula_data:
    Repeat:
        Find  det_calend_turma_localid no-lock where
              det_calend_turma_localid.cdn_turno_trab = funcionario.cdn_turno_trab and
              det_calend_turma_localid.cdn_turma_trab = funcionario.cdn_turma_trab and
              det_calend_turma_localid.cod_pais       = funcionario.cod_pais                   and
              det_calend_turma_localid.cdn_localidade = funcionario.cdn_localidade             and
              det_calend_turma_localid.dat_refer_calend = dt-ref no-error.
        IF Avail det_calend_turma_localid And det_calend_turma_localid.idi_sit_dia_trab <> 1 Then
        Do:
           Assign dt-ref = dt-ref + 1.
        End.
        Else 
           Do:  Message "Calendario de localidade n∆o existe > Turno " funcionario.cdn_turno_trab " Turma" funcionario.cdn_turma_trab
               View-as Alert-box Information.
               Leave.
           End.
              
   End.

End Procedure.

/*Fechando saida*/
{include/i-rpclo.i}

RUN pi-finalizar IN v_han_acomp.

RETURN "OK":U.


