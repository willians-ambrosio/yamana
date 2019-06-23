/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esFP1661R1 LIBERADO}  /*** 010366 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esfp1661r1 MFP}
&ENDIF

/******************************************************************************
**
**       Programa: prghur/esp/esfp1661-1.P
**
**       Data....: Marco/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem dos Historicos de Salarios e Funcoes
*                  Classificacoes 5 a 10
*******************************************************************************/
{prghur/esp/esfp1661tt.i shared}
{prghur/fpp/fp9200.i10 shared}
{prghur/fpp/fp9200.i8}

find first tt-param no-error.    

/* chamada epc */             
{include/i-epc200.i esfp1661r1} 

define shared var i-ordem2              as int                           no-undo.
define shared var v_log_folha_educnal   as log                           no-undo.
define shared var v_cod_matr            as char format "x(11)"           no-undo.
define shared var v_des_motiv_liber_sal as char format "x(19)"           no-undo.

define var c-ordem-v1         as character format "x(16)"              no-undo.
define var c-codigo-v1        as character format "x(04)"              no-undo.
define var c-descri-v1        as character format "x(30)"              no-undo.
define var c-hifem-v1         as character format "x(01)"              no-undo.
define var c-ordem-v2         as character format "x(16)"              no-undo.
define var c-codigo-v2        as character format "x(04)"              no-undo.
define var c-descri-v2        as character format "x(23)"              no-undo.
define var c-hifem-v2         as character format "x(01)"              no-undo.
define var c-ordem-v3         as character format "x(16)"              no-undo.
define var c-codigo-v3        as character format "x(04)"              no-undo.
define var c-descri-v3        as character format "x(40)"              no-undo.
define var c-hifem-v3         as character format "x(01)"              no-undo.
define var l-lista            as logical                               no-undo.
define var c-hifem            as character format "x(01)" initial "-"  no-undo.
define var d-salar-anter      as decimal   format "zzz,zzz,zz9.999"    no-undo.
define var d-salar-prim       as decimal   format "zzz,zzz,zz9.999"    no-undo.
define var d-perc-aumento     as decimal   /*format "zzz9.99"*/             no-undo.
define var d-dt-corte         as date      initial "08011993"          no-undo.
define var l-real             as logical   initial no                  no-undo.
define var d-dt-urv           as date      initial "03011994"          no-undo.
define var l-urv              as logical   initial no                  no-undo.
define var d-val-urv          as dec       initial "647.50"            no-undo.
DEFINE VAR V_LABEL1           AS CHAR      FORMAT "X(4)"               NO-UNDO.
DEFINE VAR V_LABEL2           AS CHAR      FORMAT "X(50)"              NO-UNDO.
DEFINE VAR V_LABEL3           AS CHAR      FORMAT "X(11)"              NO-UNDO.
DEFINE VAR V_TRACO1           AS CHAR      FORMAT "X(4)"               NO-UNDO.
DEFINE VAR V_TRACO2           AS CHAR      FORMAT "X(50)"              NO-UNDO.
DEFINE VAR V_TRACO3           AS CHAR      FORMAT "X(11)"              NO-UNDO.

def var v_cdn_cargo           like histor_sal_func.cdn_cargo_basic     no-undo.
def var v_cdn_func            like histor_sal_func.cdn_funcionario     no-undo.
def var v_val_sal_hist        like histor_sal_func.val_salario_mensal  no-undo.

def var v_cdn_cargo_basic     as int  format ">>,>>9"                  no-undo. 
def var v_cdn_niv_cargo       as int  format ">>9"                     no-undo. 
def var v_des_cargo           as char format "x(30)"                   no-undo. 
def var v_log_imprime         as log  init yes                         no-undo.   
def var v_log_imprime_2       as log                                   no-undo.   
def var v_cargo_niv_des       as char format "x(30)"                   no-undo. 
def var v_des_tip_cargo_func    as char format "x(6)"                  no-undo. 
def var v_des_sit_histor_funcao as char format "x(4)"                  no-undo.

/* inicio sergio */
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
/* fi sergio */

DEFINE BUFFER bf_histor_sal_func FOR histor_sal_func.
DEFINE BUFFER bf_cargo FOR cargo.

DEF VAR i_tipo_ini             AS INT FORMAT 9                          NO-UNDO.
DEF VAR i_tipo_fim             AS INT FORMAT 9                          NO-UNDO.

/*  Cristina */
DEF VAR de_trunc_val_salario_categ    AS DECIMAL  NO-UNDO.
DEF VAR de_trunc_val_salario_mensal   AS DECIMAL  NO-UNDO.
/* ******** */

/**** d-val-urv =  URV do dia 01/03/1994 ****/
define var r-rec-hist         as rowid                        no-undo.

def shared buffer bhistsal for histor_sal_func.

form header
    c-ordem-v1                                        at 01
    c-codigo-v1                                       at 18
    c-hifem-v1                                        AT 22
    c-descri-v1                                       AT 24
    c-ordem-v2                                        at 60
    c-codigo-v2                                       at 77
    c-hifem-v2                                        at 82
    c-descri-v2                                       at 84
    "Cargo/Funá∆o:"                                   at 110
    tt-param.des_tip_cargo_funcao       format "x(6)" AT 124 skip    
    c-ordem-v3                                        at 01
    c-codigo-v3                                       at 18
    c-hifem-v3                                        at 22
    c-descri-v3                                       at 24 skip(1)
    with no-labels no-attr-space no-box page-top stream-io width 132 1 down frame f-cab.

form    
    "-------------Data-------------"                  AT 01
    "Cargo/"                                          AT 54
    "----------- Sal†rio ----------- Perc"            at 93
    "Alteraá∆o"                                       at 01
    "Vigància"                                        at 12
    "Validade"                                        at 23  
    "Mot"                                             at 34
    "Descriá∆o"                                       at 38
    "Funá∆o"                                          AT 54
    "N°v"                                             AT 61
    "Descriá∆o"                                       AT 65
    "Categoria"                                       at 93
    "Padr∆o Màs"                                      at 109
    "Aumento"                                         at 125
   "---------- ---------- ---------- --- --------------- ------ --- --------------------------- --------------- --------------- --------"
   with no-labels no-attr-space no-box page-top stream-io width 132 1 down frame f-cab2.

form
    "Matricula:"                            AT 01
    v_cod_matr                              at 11 
    funcionario.nom_pessoa_fisic            AT 23 SKIP
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-matric.

form
    histor_sal_func.dat_liber_sal           at 01                 
    histor_sal_func.dat_livre_1             AT 12
    histor_sal_func.dat_term_valid_funcao   AT 24
    histor_sal_func.cdn_motiv_liber_sal     AT 34
    v_des_motiv_liber_sal   format "x(10)"  AT 38
    cargo.cdn_cargo_basic   format "zzzz9"  AT 55
    cargo.cdn_niv_cargo     format "zz9"    AT 61
    cargo.des_cargo         format "x(30)"  AT 65
    de_trunc_val_salario_categ              AT 95
    de_trunc_val_salario_mensal               
    d-perc-aumento                          
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-dados.

form
    histor_sal_func.dat_liber_sal        at  01                 space(1)
    histor_sal_func.cdn_motiv_liber_sal                         space(1)
    v_des_motiv_liber_sal                format "x(10)"         space(1)
    v_cargo_niv_des                      format "x(24)"         space(1)
    v_des_tip_cargo_func                 format "x(6)"
    v_des_sit_histor_funcao              format "x(4)"
    histor_sal_func.dat_term_valid_funcao
    de_trunc_val_salario_categ           TO  99
    de_trunc_val_salario_mensal          TO 113
    d-perc-aumento                       TO 121
    histor_sal_func.dat_livre_1          TO 132
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-dados-epc.

run utp/ut-trfrrp.p (input frame f-cab:handle).
run utp/ut-trfrrp.p (input frame f-cab2:handle).
run utp/ut-trfrrp.p (input frame f-matric:handle).
run utp/ut-trfrrp.p (input frame f-dados:handle).
run utp/ut-trfrrp.p (input frame f-dados-epc:handle).


   view frame f-cab.
   view frame f-cab2.

   def shared var v_han_acomp as handle no-undo.

   RUN pi-abre-excel.

   if i-ordem2 = 05    
   or i-ordem2 = 06 then do:
    /*"Por Estabelecimento/Unidade Lotaá∆o/Matr°cula", 5,
     * "Por Estabelecimento/Unidade Lotaá∆o/Nome", 6*/

      if v_log_folha_educnal then
         assign tt-param.i-fc-ini = {prghur/dop/eng005.i &VAR="((tt-param.cdn_func_centrdor_ini * 100) + tt-param.cdn_tip_contrat_ini)"} 
                tt-param.i-fc-fim = {prghur/dop/eng005.i &VAR="((tt-param.cdn_func_centrdor_fim * 100) + tt-param.cdn_tip_contrat_fim)"}.

      {prghur/fpp/fp9200.i6}
      for each func_ccusto no-lock where
               func_ccusto.cdn_empresa             = tt-param.v_cdn_empres_usuar         and
               func_ccusto.cdn_estab              >= tt-param.i-es-ini                   and
               func_ccusto.cdn_estab              <= tt-param.i-es-fim                   and
               func_ccusto.cdn_funcionario        >= tt-param.i-fc-ini                   and
               func_ccusto.cdn_funcionario        <= tt-param.i-fc-fim                   and
               func_ccusto.dat_inic_lotac_func    <= tt-param.v_dat_valid                and
               func_ccusto.dat_fim_lotac_func     >= tt-param.v_dat_valid                AND
               func_ccusto.cod_rh_ccusto          >= c-cc-ini                            and
               func_ccusto.cod_rh_ccusto          <= c-cc-fim,
          each tt_lotac_funcionario no-lock where
               tt_lotac_funcionario.cdn_empresa     = func_ccusto.cdn_empresa     and
               tt_lotac_funcionario.cdn_estab       = func_ccusto.cdn_estab       and
               tt_lotac_funcionario.cdn_funcionario = func_ccusto.cdn_funcionario,
          each funcionario of tt_lotac_funcionario no-lock where
               funcionario.nom_pessoa_fisic   >= c-nm-ini               and
               funcionario.nom_pessoa_fisic   <= c-nm-fim               and
               /*funcionario.cdn_cargo_basic    >= tt-param.cdn_cargo_ini and
               funcionario.cdn_cargo_basic    <= tt-param.cdn_cargo_fim and*/
             ((funcionario.dat_desligto_func  <> ? and l-imp-desl) or
               funcionario.dat_desligto_func   = ?)
          break by funcionario.cdn_estab
                by tt_lotac_funcionario.num_seq_unid_lotac
                by tt_lotac_funcionario.num_niv_unid_lotac
                by tt_lotac_funcionario.cod_unid_lotac
                by if i-ordem2 = 5 then
                      string(funcionario.cdn_funcionario,"99999999")
                   else
                      funcionario.nom_pessoa_fisic:

                  if first-of(funcionario.cdn_estab) then do:
                     find rh_estab of funcionario no-lock.
                     assign c-ordem-v1   = "Estabelecimento:"
                            c-codigo-v1  = string(funcionario.cdn_estab,{prghur/dop/eng006.i})
                            c-hifem-v1   = "-"
                            c-descri-v1  = rh_estab.nom_pessoa_jurid.
                  end.

                  find rh_ccusto of func_ccusto no-lock.

                  if not v_log_folha_educnal then 
                     run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_funcionario)).            
                  else 
                     run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_func_centrdor)).            

                  assign l-real       = no
                         l-urv        = no
                         d-salar-prim = 0.

                  if first-of(tt_lotac_funcionario.cod_unid_lotac) then 
                    run pi_report_unid_lotac.

                  assign l-real = no
                         l-urv  = no.

                  if not tt-param.v_log_imp_ult_movto then do:

                      ASSIGN i_tipo_ini = tt-param.idi_tip_cargo_funcao 
                      i_tipo_fim = tt-param.idi_tip_cargo_funcao.
                      IF tt-param.idi_tip_cargo_funcao  = 3  THEN 
                         ASSIGN i_tipo_ini = 1.

                      assign l-lista = no.

                      for each histor_sal_func of funcionario no-lock where
                               histor_sal_func.dat_liber_sal >= tt-param.d-dt-ini and
                               histor_sal_func.dat_liber_sal <= tt-param.d-dt-fim and   
                               histor_sal_func.cdn_cargo_basic >= tt-param.cdn_cargo_ini and 
                               histor_sal_func.cdn_cargo_basic <= tt-param.cdn_cargo_fim AND
                               histor_sal_func.idi_tip_cargo_funcao >= i_tipo_ini AND
                               histor_sal_func.idi_tip_cargo_funcao <= i_tipo_fim
                               break by histor_sal_func.cdn_funcionario
                                     by histor_sal_func.dat_liber_sal
                                     by histor_sal_func.num_seq_histor_sal
                                     by histor_sal_func.cdn_motiv_liber_sal:
                          

                          if histor_sal_func.idi_sit_histor_funcao <> 0 then
                             assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 histor_sal_func.idi_sit_histor_funcao}.
                          else
                             assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 1}.

/*                           if histor_sal_func.idi_tip_cargo_funcao <> 0 then                                                    */
/*                              assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 histor_sal_func.idi_tip_cargo_funcao}. */
/*                           else                                                                                                 */
/*                              assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 1}.                                    */


                          if first-of(histor_sal_func.cdn_funcionario) then do:
                              assign d-perc-aumento = 0
                                     d-salar-anter  = 0
                                     r-rec-hist = rowid(histor_sal).
                              find bhistsal where rowid(bhistsal) = r-rec-hist NO-LOCK NO-ERROR.
                              find prev bhistsal of funcionario no-lock no-error.
    
                              assign d-salar-prim = if available bhistsal
                                                    then bhistsal.val_salario_mensal
                                                    else 0.
                    
                              if histor_sal_func.dat_liber_sal = d-dt-corte    and
                                 histor_sal_func.val_salario_mensal  < d-salar-anter and
                                 l-real             = no then
                                 assign l-real        = yes
                                        d-salar-anter = if d-salar-prim <> 0
                                                           then trunc(d-salar-prim / 1000,3)
                                                        else trunc(histor_sal_func.val_salario_mensal / 1000,3)
                                        d-salar-prim  = 0.
                              else
                              if histor_sal_func.dat_liber_sal   = d-dt-urv      and
                                 histor_sal_func.val_salario_mensal    < d-salar-anter and
                                 l-urv                = no then
                                 assign l-urv         = yes
                                        d-salar-anter = if d-salar-prim <> 0
                                                               then trunc(d-salar-prim / d-val-urv,3)
                                                        else trunc(histor_sal_func.val_salario_mensal / d-val-urv,3)
                                        d-salar-prim  = 0.
                              else
                                 assign d-salar-anter = if d-salar-prim <> 0
                                                        then d-salar-prim
                                                        else histor_sal_func.val_salario_mensal.
                              assign d-perc-aumento = truncate(round(
                                                      (((histor_sal_func.val_salario_mensal /
                                                         d-salar-anter) - 1) * 100),4),4)
                                     d-salar-anter = histor_sal_func.val_salario_mensal.
                              if not v_log_folha_educnal then
                                  assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
                              else 
                                  assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").
                          end.
                          else do:
                              if histor_sal_func.dat_liber_sal   = d-dt-corte    and
                                 histor_sal_func.val_salario_mensal    < d-salar-anter and
                                 l-real               = no then
                                  assign l-real        = yes
                                         d-salar-anter = truncate(d-salar-anter / 1000,3).
                              if histor_sal_func.dat_liber_sal   = d-dt-urv      and
                                 histor_sal_func.val_salario_mensal    < d-salar-anter and
                                 l-urv                = no then
                                  assign l-urv         = yes
                                         d-salar-anter = truncate(d-salar-anter / d-val-urv,3).
                              assign d-perc-aumento = truncate(round(
                                                      (((histor_sal_func.val_salario_mensal /
                                                         d-salar-anter) - 1) * 100),4),4)
                                     d-salar-anter = histor_sal_func.val_salario_mensal.
                          end.

                          run pi-imprime-historicos.

                     end.
                     if l-lista then
                         put "".
                 end.

                 else run pi-imprime-ult-movto.
       end.
    end.  

    if i-ordem2 = 07
    or i-ordem2 = 08 then do:
    /* "Por Centro de Custo/Estabelecimento/Turno/Matr°cula", 7, *
    * "Por Centro de Custo/Estabelecimento/Turno/Nome", 8       */

    for each func_ccusto no-lock where
             func_ccusto.cdn_empresa             = tt-param.v_cdn_empres_usuar         and
             func_ccusto.cdn_estab              >= tt-param.i-es-ini                   and
             func_ccusto.cdn_estab              <= tt-param.i-es-fim                   and
             func_ccusto.cdn_funcionario        >= tt-param.i-fc-ini                   and
             func_ccusto.cdn_funcionario        <= tt-param.i-fc-fim                   and
             func_ccusto.dat_inic_lotac_func    <= tt-param.v_dat_valid                and
             func_ccusto.dat_fim_lotac_func     >= tt-param.v_dat_valid                AND
             func_ccusto.cod_rh_ccusto          >= c-cc-ini                            and
             func_ccusto.cod_rh_ccusto          <= c-cc-fim,
        each funcionario of func_ccusto no-lock where
             funcionario.nom_pessoa_fisic  >= c-nm-ini                              and
             funcionario.nom_pessoa_fisic  <= c-nm-fim                              and
             funcionario.cdn_cargo_basic   >= tt-param.cdn_cargo_ini                and
             funcionario.cdn_cargo_basic   <= tt-param.cdn_cargo_fim                and
             funcionario.cdn_turno_trab       >= tt-param.cdn_turno_ini             AND
             funcionario.cdn_turno_trab       <= tt-param.cdn_turno_fim             AND  
            (funcionario.dat_desligto_func  = ?                                     or
            (funcionario.dat_desligto_func <> ?                                     and
             tt-param.l-imp-desl            = yes))                                 and
           ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func         > 0)  or
            (not v_log_folha_educnal))
        break by func_ccusto.cod_rh_ccusto
              by funcionario.cdn_estab
              by funcionario.cdn_turno_trab
              by if i-ordem2 = 7 then
                   string(funcionario.cdn_funcionario,"99999999")
                else
                   funcionario.nom_pessoa_fisic:

          find rh_ccusto of func_ccusto no-lock.
          assign c-ordem-v1   = "Centro de Custo:"
                 c-codigo-v1  = rh_ccusto.cod_rh_ccusto
                 c-hifem-v1   = "-"
                 c-descri-v1  = rh_ccusto.des_rh_ccusto.

          find rh_estab of funcionario no-lock.                          
          assign c-ordem-v2   = "Estabelecimento:"
                 c-codigo-v2  = string(rh_estab.cdn_estab,{prghur/dop/eng006.i}) 
                 c-hifem-v2   = "-"
                 c-descri-v2  = rh_estab.nom_pessoa_jurid.

          if not v_log_folha_educnal then 
             run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_funcionario)).            
          else 
             run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_func_centrdor)).            

          if first-of(func_ccusto.cod_rh_ccusto) 
              or first-of(funcionario.cdn_estab) 
              or first-of(funcionario.cdn_turno_trab)
              or line-counter > 63  then do: 
              find turno_trab of funcionario NO-LOCK where
                   turno_trab.cdn_turno_trab = funcionario.cdn_turno_trab no-error.

                  assign c-ordem-v3   = "Turno:"
                         c-codigo-v3  = string(turno_trab.cdn_turno_trab,"9999")
                         c-hifem-v3   = "-"
                         c-descri-v3  = turno_trab.des_turno_trab.
              page.  
          end.  

          if i-ordem2 = 7 then do:
             if v_log_folha_educnal then do:
                if funcionario.cdn_func_centrdor < tt-param.cdn_func_centrdor_ini or
                   funcionario.cdn_func_centrdor > tt-param.cdn_func_centrdor_fim then 
                   next.

                if funcionario.cdn_tip_contrat_func < tt-param.cdn_tip_contrat_ini or
                   funcionario.cdn_tip_contrat_func > tt-param.cdn_tip_contrat_fim then
                   next. 
             end.      
          end.

          assign l-real = no
                 l-urv  = no.
          
          if  not tt-param.v_log_imp_ult_movto then do:

              ASSIGN i_tipo_ini = tt-param.idi_tip_cargo_funcao
                     i_tipo_fim = tt-param.idi_tip_cargo_funcao.

              IF tt-param.idi_tip_cargo_funcao = 3  THEN
                  ASSIGN i_tipo_ini = 1.

              assign l-lista = no.

              for each histor_sal_func of funcionario no-lock where
                       histor_sal_func.dat_liber_sal >= tt-param.d-dt-ini and
                       histor_sal_func.dat_liber_sal <= tt-param.d-dt-fim and
                       histor_sal_func.cdn_cargo_basic >= tt-param.cdn_cargo_ini and 
                       histor_sal_func.cdn_cargo_basic <= tt-param.cdn_cargo_fim AND
                       histor_sal_func.idi_tip_cargo_funcao >= i_tipo_ini AND
                       histor_sal_func.idi_tip_cargo_funcao <= i_tipo_fim
                       break by histor_sal_func.cdn_funcionario
                             by histor_sal_func.dat_liber_sal
                             by histor_sal_func.num_seq_histor_sal
                             by histor_sal_func.cdn_motiv_liber_sal:

                  if histor_sal_func.idi_sit_histor_funcao <> 0 then
                     assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 histor_sal_func.idi_sit_histor_funcao}.
                  else
                     assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 1}.

                  /*if histor_sal_func.idi_tip_cargo_funcao <> 0 then
                     assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 histor_sal_func.idi_tip_cargo_funcao}.
                  else
                     assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 1}.
                    */
                  
                  if first-of(histor_sal_func.cdn_funcionario) then do:
                      assign d-perc-aumento = 0
                             d-salar-anter  = 0
                             r-rec-hist = rowid(histor_sal).
                      find bhistsal where rowid(bhistsal) = r-rec-hist NO-LOCK NO-ERROR.
                      find prev bhistsal of funcionario no-lock no-error.

                      assign d-salar-prim = if available bhistsal
                                            then bhistsal.val_salario_mensal
                                            else 0.
             
                      if histor_sal_func.dat_liber_sal       = d-dt-corte    and
                         histor_sal_func.val_salario_mensal  < d-salar-anter and
                         l-real                              = no then

                         assign l-real        = yes
                                d-salar-anter = if d-salar-prim <> 0
                                                   then trunc(d-salar-prim / 1000,3)
                                                else trunc(histor_sal_func.val_salario_mensal / 1000,3)
                                d-salar-prim  = 0.
                      else
                          if histor_sal_func.dat_liber_sal      = d-dt-urv            and
                             histor_sal_func.val_salario_mensal < d-salar-anter and
                             l-urv                              = no then

                              assign l-urv         = yes
                                     d-salar-anter = if d-salar-prim <> 0 then trunc(d-salar-prim / d-val-urv,3)
                                                     else trunc(histor_sal_func.val_salario_mensal / d-val-urv,3)
                                     d-salar-prim  = 0.
                          else
                              assign d-salar-anter = if  d-salar-prim <> 0 then d-salar-prim
                                                     else histor_sal_func.val_salario_mensal.

                      assign d-perc-aumento = truncate(round((((histor_sal_func.val_salario_mensal / d-salar-anter) - 1) * 100),4),4)
                             d-salar-anter = histor_sal_func.val_salario_mensal.

                      if  not v_log_folha_educnal then
                          assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
                      else 
                          assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").
                   end.

                   else do:
                       if histor_sal_func.dat_liber_sal   = d-dt-corte    and
                          histor_sal_func.val_salario_mensal    < d-salar-anter and
                          l-real               = no then
                           assign l-real        = yes
                                  d-salar-anter = truncate(d-salar-anter / 1000,3).
                       if histor_sal_func.dat_liber_sal   = d-dt-urv      and
                          histor_sal_func.val_salario_mensal    < d-salar-anter and
                          l-urv                = no then
                           assign l-urv         = yes
                                  d-salar-anter = truncate(d-salar-anter / d-val-urv,3).
                       assign d-perc-aumento = truncate(round(
                                               (((histor_sal_func.val_salario_mensal /
                                                  d-salar-anter) - 1) * 100),4),4)
                              d-salar-anter = histor_sal_func.val_salario_mensal.
                   end.

                   run pi-imprime-historicos.

              end. /* for each histor_sal_func */

              if l-lista then
                  put "".
          end. /* not tt-param.v_log_imp_ult_movto */

          else
              run pi-imprime-ult-movto.
   end.
   end.


RUN pi-encerra-excel.

return "OK".

procedure pi-imprime-historicos:

    if tt-param.idi_tip_cla_motiv = 2 then do:
        find tt-digita no-lock where
             tt-digita.cdn_motiv_liber_sal = histor_sal_func.cdn_motiv_liber_sal no-error.
        if not avail tt-digita then
            next.
        else
            assign v_des_motiv_liber_sal = tt-digita.des_motiv_liber_sal.
    end.
    else do:
        if histor_sal_func.cdn_motiv_liber_sal  >= i-mt-ini and
           histor_sal_func.cdn_motiv_liber_sal  <= i-mt-fim then do:
            find motiv_liber_sal no-lock where
                 motiv_liber_sal.cdn_motiv_liber_sal = histor_sal_func.cdn_motiv_liber_sal no-error.
            if avail motiv_liber_sal then
                assign v_des_motiv_liber_sal = motiv_liber_sal.des_motiv_liber_sal.
            else next.
        end.
        else next.
    end.
    /***/

    find tt-motivo-func where
         tt-motivo-func.cdn_empresa     = histor_sal_func.cdn_empresa     and
         tt-motivo-func.cdn_estab       = histor_sal_func.cdn_estab       and
         tt-motivo-func.cdn_motivo      = histor_sal_func.cdn_motiv_liber_sal no-lock no-error.
    if not available tt-motivo-func then do:
        create tt-motivo-func.
        assign tt-motivo-func.cdn_empresa     = histor_sal_func.cdn_empresa
               tt-motivo-func.cdn_estab       = histor_sal_func.cdn_estab
               tt-motivo-func.cdn_motivo      = histor_sal_func.cdn_motiv_liber_sal
               tt-motivo-func.qtde-func       = tt-motivo-func.qtde-func + 1.
    end.
     else
         assign tt-motivo-func.qtde-func = tt-motivo-func.qtde-func + 1.
    
    find first cargo 
         where cargo.cdn_cargo_basic      = histor_sal_func.cdn_cargo_basic
           and cargo.cdn_niv_cargo        = histor_sal_func.cdn_niv_cargo
           and (cargo.idi_tip_cargo_funcao = histor_sal_func.idi_tip_cargo_funcao or
                cargo.idi_tip_cargo_funcao = 0) no-lock no-error.
    if not avail cargo then
       next.

    if   histor_sal_func.dat_liber_sal > d-dt-corte then
         assign l-real = yes.
    if   histor_sal_func.dat_liber_sal > d-dt-urv then
         assign l-urv = yes.

    if d-perc-aumento < 0
    or d-perc-aumento = ?
    or d-perc-aumento > 9999 then
       assign d-perc-aumento = 0.

    if not l-lista and 
       line-counter <= 63
       then do:
       display v_cod_matr
               funcionario.nom_pessoa_fisic
               with frame f-matric.
       down with frame f-matric.
    end.   
    if line-counter > 63 then do:
       page.
       display v_cod_matr
               funcionario.nom_pessoa_fisic
               with frame f-matric.
       down with frame f-matric.
    end.   

    ASSIGN de_trunc_val_salario_categ    = 0
           de_trunc_val_salario_mensal   = 0.
    CASE tt-param.num_casas_dec:
        WHEN 2 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,2)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,2)    
                    de_trunc_val_salario_categ:FORMAT  IN FRAME f-dados = "zz,zzz,zz9.99"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zz,zzz,zz9.99".

           /******** Chamada EPC - 1 **********/
           for each tt-epc exclusive-lock where tt-epc.cod-event = "cargo_basico":
              delete tt-epc.
           end.
           create tt-epc.
           assign v_cdn_cargo_basic = histor_sal_func.cdn_cargo_basic 
                  v_cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo   
                  v_des_cargo       = cargo.des_cargo            
                  tt-epc.cod-event = "cargo_basico"
                  tt-epc.val-parameter = string(v_cdn_cargo_basic, ">>,>>9") +
                                         string(v_cdn_niv_cargo, ">>9") +
                                         string(v_des_cargo, "x(30)").
           {include/i-epc201.i "cargo_basico"}
           /*****/

           find first tt-epc where 
                      tt-epc.cod-event = "des_cargo" no-error. /*epc*/
           if avail tt-epc then do:
               assign v_cargo_niv_des = string(substr(tt-epc.val-parameter,01,30))
                      v_log_imprime = no. 

              
           end.
           for each tt-epc exclusive-lock where tt-epc.cod-event = "des_cargo":
              delete tt-epc.
           end.

             
        if v_log_imprime then do:
             if tt-param.idi_tip_cargo_funcao >= 1 and
                tt-param.idi_tip_cargo_funcao <= 3 then do:

                 RUN pi-dados-excel.
                 
                 ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                                                  
                        ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                                                    
                        ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                                          
                        ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                                            
                        ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                                                          
                        ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic      
                        ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo        
                        ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo            
                        ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ 
                        ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal
                        ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento             
                     .       

                 RUN pi-salario-anterior.

                 ASSIGN i-cont-linha = i-cont-linha + 1.

                end.
        END. /*v_log_imprime*/
    END.
        WHEN 3 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,3)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,3)    
                    de_trunc_val_salario_categ:FORMAT IN FRAME f-dados  = "zz,zzz,zz9.999"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zz,zzz,zz9.999".

            /******** Chamada EPC - 2 **********/
            for each tt-epc exclusive-lock where tt-epc.cod-event = "cargo_basico":
              delete tt-epc.
            end.
            create tt-epc.
            assign v_cdn_cargo_basic = histor_sal_func.cdn_cargo_basic 
                   v_cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo   
                   v_des_cargo       = cargo.des_cargo            
                   tt-epc.cod-event = "cargo_basico"
                   tt-epc.val-parameter = string(v_cdn_cargo_basic, ">>,>>9") +
                                         string(v_cdn_niv_cargo, ">>9") +
                                         string(v_des_cargo, "x(30)").
            {include/i-epc201.i "cargo_basico"}
            /*****/

            find first tt-epc where 
                      tt-epc.cod-event = "des_cargo" no-error. /*epc*/
            if avail tt-epc then do:
               assign v_cargo_niv_des = string(substr(tt-epc.val-parameter,01,30))
                      v_log_imprime = no. 
            end.

            if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                 RUN pi-dados-excel.
                 
                 ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                                                            
                        ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                                                              
                        ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                                                    
                        ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                                                      
                        ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                                                                    
                        ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic       
                        ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo         
                        ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo             
                        ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ  
                        ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal 
                        ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento              
                     .                                                                

                 RUN pi-salario-anterior.

                 ASSIGN i-cont-linha = i-cont-linha + 1.
                   
              end.
           end. /*v_log_imprime*/
        END.
        WHEN 4 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,4)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,4)
                    de_trunc_val_salario_categ:FORMAT IN FRAME f-dados  = "zz,zzz,zz9.9999"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zz,zzz,zz9.9999".

            /******** Chamada EPC - 3 **********/
            for each tt-epc exclusive-lock where tt-epc.cod-event = "cargo_basico":
              delete tt-epc.
            end.
            create tt-epc.
            assign v_cdn_cargo_basic = histor_sal_func.cdn_cargo_basic 
                   v_cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo   
                   v_des_cargo       = cargo.des_cargo            
                   tt-epc.cod-event = "cargo_basico"
                   tt-epc.val-parameter = string(v_cdn_cargo_basic, ">>,>>9") +
                                         string(v_cdn_niv_cargo, ">>9") +
                                         string(v_des_cargo, "x(30)").
            {include/i-epc201.i "cargo_basico"}
            /*****/

            find first tt-epc where 
                      tt-epc.cod-event = "des_cargo" no-error. /*epc*/
            if avail tt-epc then do:
               assign v_cargo_niv_des = string(substr(tt-epc.val-parameter,01,30))
                      v_log_imprime = no. 
              
            end.
              if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    RUN pi-dados-excel.

                    ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                                                                          
                           ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                                                                            
                           ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                                                                  
                           ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                                                                    
                           ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                                                                                  
                           ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic       
                           ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo         
                           ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo             
                           ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ  
                           ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal 
                           ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento              
                        .                                                              

                    RUN pi-salario-anterior.

                    ASSIGN i-cont-linha = i-cont-linha + 1.

                end.
           end. /*v_log_imprime*/
        END.

    END CASE.

    FOR EACH HISTOR_DET_REMUN_VAR_FUNC NO-LOCK WHERE 
             HISTOR_DET_REMUN_VAR_FUNC.CDN_EMPRESA         = HISTOR_SAL_FUNC.CDN_EMPRESA          AND 
             HISTOR_DET_REMUN_VAR_FUNC.CDN_ESTAB           = HISTOR_SAL_FUNC.CDN_ESTAB            AND
             HISTOR_DET_REMUN_VAR_FUNC.CDN_FUNCIONARIO     = HISTOR_SAL_FUNC.CDN_FUNCIONARIO      AND
             HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL       = HISTOR_SAL_FUNC.DAT_LIBER_SAL        AND
             HISTOR_DET_REMUN_VAR_FUNC.NUM_SEQ_HISTOR_SAL  = HISTOR_SAL_FUNC.NUM_SEQ_HISTOR_SAL   AND
             HISTOR_DET_REMUN_VAR_FUNC.CDN_MOTIV_LIBER_SAL = HISTOR_SAL_FUNC.CDN_MOTIV_LIBER_SAL,
       FIRST REMUN_VAR NO-LOCK OF HISTOR_DET_REMUN_VAR_FUNC
        BREAK BY HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL:

        IF FIRST-OF (HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL) THEN DO:

           DISP V_LABEL1
                V_LABEL2
                V_LABEL3
                WITH FRAME F-LABELS.
                DOWN WITH FRAME F-LABELS.

           DISP V_TRACO1
                V_TRACO2
                V_TRACO3    
                WITH FRAME F-TRACO.
                DOWN WITH FRAME F-TRACO.
        END.    

        DISP HISTOR_DET_REMUN_VAR_FUNC.CDN_REMUN_VAR
             REMUN_VAR.DES_REMUN_VAR
             HISTOR_DET_REMUN_VAR_FUNC.VAL_REMUN_VAR
             WITH FRAME F-EXPLODE.
             DOWN WITH FRAME F-EXPLODE.
    END.

    assign l-lista = yes.

end procedure.

procedure pi-imprime-ult-movto:

    assign l-lista = no.

    for each bhistsal of funcionario no-lock where
             bhistsal.dat_liber_sal >= tt-param.d-dt-ini and
             bhistsal.dat_liber_sal <= tt-param.d-dt-fim
    break by bhistsal.cdn_funcionario
          by bhistsal.dat_liber_sal
          by bhistsal.num_seq_histor_sal
          by bhistsal.cdn_motiv_liber_sal:

        if first-of(bhistsal.cdn_funcionario) then do:
            assign d-perc-aumento = 0
                   d-salar-anter  = 0
                   r-rec-hist     = rowid(bhistsal).
            find histor_sal_func where rowid(histor_sal_func) = r-rec-hist NO-LOCK NO-ERROR.
            find prev histor_sal_func of funcionario no-lock no-error.

            assign d-salar-prim = if available histor_sal_func
                                  then histor_sal_func.val_salario_mensal
                                  else 0.
    
            if bhistsal.dat_liber_sal       = d-dt-corte    and
               bhistsal.val_salario_mensal  < d-salar-anter and
               l-real = no then
               assign l-real        = yes
                      d-salar-anter = if d-salar-prim <> 0
                                         then trunc(d-salar-prim / 1000,3)
                                      else trunc(bhistsal.val_salario_mensal / 1000,3)
                      d-salar-prim  = 0.
            else
            if bhistsal.dat_liber_sal      = d-dt-urv      and
               bhistsal.val_salario_mensal < d-salar-anter and
               l-urv = no then
               assign l-urv         = yes
                      d-salar-anter = if d-salar-prim <> 0
                                             then trunc(d-salar-prim / d-val-urv,3)
                                      else trunc(bhistsal.val_salario_mensal / d-val-urv,3)
                      d-salar-prim  = 0.
            else
                assign d-salar-anter = if d-salar-prim <> 0
                                      then d-salar-prim
                                      else bhistsal.val_salario_mensal.
            assign d-perc-aumento = truncate(round(
                                    (((bhistsal.val_salario_mensal /
                                       d-salar-anter) - 1) * 100),4),4)
                   d-salar-anter = bhistsal.val_salario_mensal.
            if not v_log_folha_educnal then
                assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
            else
                assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").
        end.
        else do:
            if bhistsal.dat_liber_sal      = d-dt-corte    and
               bhistsal.val_salario_mensal < d-salar-anter and
               l-real = no then
                assign l-real        = yes
                       d-salar-anter = truncate(d-salar-anter / 1000,3).
             if bhistsal.dat_liber_sal      = d-dt-urv      and
                bhistsal.val_salario_mensal < d-salar-anter and
                l-urv = no then
                 assign l-urv         = yes
                        d-salar-anter = truncate(d-salar-anter / d-val-urv,3).
             assign d-perc-aumento = truncate(round(
                                     (((bhistsal.val_salario_mensal /
                                        d-salar-anter) - 1) * 100),4),4)
                    d-salar-anter = bhistsal.val_salario_mensal.
        end.
        
        case tt-param.idi_imp_ult_mov_tip:
            when 1 then do: /* ultimo cargo */
                if last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if   avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.cdn_cargo_basic <> bhistsal.cdn_cargo_basic then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
            when 2 then do: /* ultimo salario */
                if last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if   avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario    <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.val_salario_mensal <> bhistsal.val_salario_mensal then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
            when 3 then do: /* ambos */
                if last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if  avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.cdn_cargo_basic <> bhistsal.cdn_cargo_basic then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/

                    find first histor_sal_func of bhistsal no-lock no-error.
                    if  avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario    <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.val_salario_mensal <> bhistsal.val_salario_mensal then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
        end case.     
    end.

end procedure.

procedure pi-abre-excel:
   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".xls".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESFP1661".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"
          ch-excel:columns( "B"):NumberFormat = "@"
          ch-excel:columns( "C"):NumberFormat = "@"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "@"
          ch-excel:columns( "F"):NumberFormat = "@"           
          ch-excel:columns( "G"):NumberFormat = "@"           
          ch-excel:columns( "H"):NumberFormat = "@"    
          ch-excel:columns( "I"):NumberFormat = "@"    
          ch-excel:columns( "J"):NumberFormat = "@"    
          ch-excel:columns( "K"):NumberFormat = "@"
          ch-excel:columns( "L"):NumberFormat = "@"
          ch-excel:columns( "M"):NumberFormat = "@"
          ch-excel:columns( "N"):NumberFormat = "@"
          ch-excel:columns( "O"):NumberFormat = "#.##0"
          ch-excel:columns( "P"):NumberFormat = "@"           
          ch-excel:columns( "Q"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "R"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "S"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "T"):NumberFormat = "@"           
          ch-excel:columns( "U"):NumberFormat = "@"           
          ch-excel:columns( "V"):NumberFormat = "@"    
          ch-excel:columns( "W"):NumberFormat = "#.##0,00"
          ch-excel:columns( "X"):NumberFormat = "@"
          ch-excel:columns( "Y"):NumberFormat = "@"
          ch-excel:columns( "Z"):NumberFormat = "@"
          ch-excel:columns("AA"):NumberFormat = "@"
          ch-excel:columns("AB"):NumberFormat = "@"
          ch-excel:columns("AC"):NumberFormat = "@"
          ch-excel:columns("AD"):NumberFormat = "@"
          ch-excel:columns("AE"):NumberFormat = "@"
          ch-excel:columns("AF"):NumberFormat = "@"
          ch-excel:columns("AG"):NumberFormat = "@"
          ch-excel:columns("AH"):NumberFormat = "@"
          ch-excel:columns("AI"):NumberFormat = "@"
          ch-excel:columns("AJ"):NumberFormat = "@"
          ch-excel:columns("AK"):NumberFormat = "@".


   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Empresa" 
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Centro Custo" 
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o" 
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Estabelecimento" 
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o" 
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "Matr°cula" 
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "Funcion†rio"
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "CPF"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Alteraá∆o"    
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "Vigància"     
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Validade"     
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Motivo"       
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o"     
          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = "Cargo/Funá∆o" 
          ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = "Nivel"        
          ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o"    
          ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = "Categoria"    
          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = "Padr∆o Màs"   
          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = "Perc Aumento"
          ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = "Cargo/Funá∆o Ant" 
          ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = "Nivel Ant"        
          ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o Ant"    
          ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = "Padr∆o Màs Ant"    
       .           

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados-excel:                                             
   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = tt-param.v_cdn_empres_usuar
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = rh_ccusto.cod_rh_ccusto
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = rh_ccusto.des_rh_ccusto
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = string(rh_estab.cdn_estab,{prghur/dop/eng006.i})
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = rh_estab.nom_pessoa_jurid                       
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = v_cod_matr                    
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = funcionario.nom_pessoa_fisic
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "CPF".
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ch-excel:Range("A1:W" + string(i-cont-linha - 1)):autofilter(,,,).

   ch-excel:Range("A1:W" + string(i-cont-linha - 1)):select.

   case tt-param.destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,c-arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.

PROCEDURE pi-salario-anterior:
   FOR EACH bf_histor_sal_func OF funcionario
            WHERE ROWID(bf_histor_sal_func) <> ROWID(histor_sal_func)
            NO-LOCK,
            FIRST bf_cargo 
                  WHERE bf_cargo.cdn_cargo_basic      = bf_histor_sal_func.cdn_cargo_basic     AND
                        bf_cargo.cdn_niv_cargo        = bf_histor_sal_func.cdn_niv_cargo       AND
                       (bf_cargo.idi_tip_cargo_funcao = bf_histor_sal_func.idi_tip_cargo_funcao OR bf_cargo.idi_tip_cargo_funcao = 0) 
                  NO-LOCK:
      ASSIGN ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.cdn_cargo_basic       
             ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.cdn_niv_cargo         
             ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.des_cargo                                    
             ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = TRUNC(bf_histor_sal_func.val_salario_mensal,3).
   END.
END PROCEDURE.
