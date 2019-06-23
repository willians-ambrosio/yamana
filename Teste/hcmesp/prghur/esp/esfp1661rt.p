/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esFP1661RT 1.02.00.055}  /*** 010055 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esfp1661rt MFP}
&ENDIF

/******************************************************************************
**
**       Programa: prghur/esp/esfp1661.P
**
**       Data....: Marco/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem dos Historicos de Salarios e Funcoes
*******************************************************************************/

{include/i-rpvar.i}

&GLOBAL-DEFINE RTF YES

{prghur/esp/esfp1661tt.i shared}
&if "{&dthrpyc_version}" >= "2.06" &then /* programa dispon¡vel somente ap¢s 2.06 */


/* ++++++++++++++++++ (Definicao Variaveis) ++++++++++++++++++ */

define var c-ordem-v1         as character format "x(16)"              no-undo.
define var c-codigo-v1        as character format "x(06)"              no-undo.
define var c-descri-v1        as character format "x(40)"              no-undo.
define var c-hifem-v1         as character format "x(01)"              no-undo.
define var c-ordem-v2         as character format "x(16)"              no-undo.
define var c-codigo-v2        as character format "x(03)"              no-undo.
define var c-descri-v2        as character format "x(30)"              no-undo.
define var c-hifem-v2         as character format "x(01)"              no-undo.
define var l-lista            as logical                               no-undo.
define var c-hifem            as character format "x(01)" initial "-"  no-undo.
define var d-salar-anter      as decimal   format "zzz,zzz,zz9.999"    no-undo.
define var d-salar-prim       as decimal   format "zzz,zzz,zz9.999"    no-undo.
define var d-perc-aumento     as decimal   format "zzz9.9999"          no-undo.
define var d-dt-corte         as date      initial "08011993"          no-undo.
define var l-real             as logical   initial no                  no-undo.
define var d-dt-urv           as date      initial "03011994"          no-undo.
define var l-urv              as logical   initial no                  no-undo.
define var d-val-urv          as dec       initial "647.50"            no-undo.
define var r-rec-hist         as rowid                                 no-undo.
define var v_cod_matr         as char format "x(11)"                   no-undo.

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

define shared var v_log_folha_educnal   as log                         no-undo.
define shared var v_han_acomp           as handle                      no-undo.
define shared var v_des_motiv_liber_sal as char format "x(19)"         no-undo.


DEF VAR i_tipo_ini             AS INT FORMAT 9                          NO-UNDO.
DEF VAR i_tipo_fim             AS INT FORMAT 9                          NO-UNDO.

DEF VAR V_LABEL1 AS CHAR NO-UNDO.
DEF VAR V_LABEL2 AS CHAR NO-UNDO.
DEF VAR V_LABEL3 AS CHAR NO-UNDO.
DEF VAR V_TRACO1 AS CHAR NO-UNDO.
DEF VAR V_TRACO2 AS CHAR NO-UNDO.
DEF VAR V_TRACO3 AS CHAR NO-UNDO.

DEF VAR de_trunc_val_salario_categ    AS DECIMAL  NO-UNDO.
DEF VAR de_trunc_val_salario_mensal   AS DECIMAL  NO-UNDO.

/* DEF VAR i_tipo_ini             AS INT FORMAT 9                          NO-UNDO. */
/* DEF VAR i_tipo_fim             AS INT FORMAT 9                          NO-UNDO. */

/**** d-val-urv =  URV do dia 01/03/1994 ****/

def new shared buffer bhistsal for histor_sal_func.

find first tt-param.
&global-define other_clt_99 yes
{prghur/fpp/fp9200.i11}

/* chamada epc */             
{include/i-epc200.i esfp1661rt} 

/* ++++++++++++++++++ (Definicao Form) ++++++++++++++++++ */
form header
    c-ordem-v1                                        at 01
    c-codigo-v1                                       at 18
    c-hifem-v1                                        AT 29
    c-descri-v1                                       AT 30
    c-ordem-v2                                        at 60
    c-codigo-v2                                       at 76
    c-hifem-v2                                        at 82
    c-descri-v2                                       at 83
    "Cargo/Fun‡Æo:"                                   at 110
    tt-param.des_tip_cargo_funcao         format "x(6)" AT 124 SKIP(1)
    "-------------Data-------------"                  AT 01
    "Cargo/"                                          AT 54
    "----------- Sal rio ----------- Perc"            at 93 
    "Altera‡Æo"                                       at 01
    "Vigˆncia"                                        at 12
    "Validade"                                        at 23  
    "Mot"                                             at 34
    "Descri‡Æo"                                       at 38
    "Fun‡Æo"                                          AT 54
    "N¡v"                                             AT 61
    "Descri‡Æo"                                       AT 65
    "Categoria"                                       at 93  
    "PadrÆo Mˆs"                                      at 109 
    "Aumento"                                         at 125 
    "---------- ---------- ---------- --- --------------- ------ --- --------------------------- --------------- --------------- --------"
   with no-labels no-attr-space no-box page-top stream-io width 132 1 down frame f-cab.

form
    "Matricula:"                            AT 01
    v_cod_matr                              at 11 
    funcionario.nom_pessoa_fisic            AT 23 skip
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-matric.

form
   histor_sal_func.dat_liber_sal            at 01                 
    histor_sal_func.dat_livre_1             AT 12
    histor_sal_func.dat_term_valid_funcao   AT 23
    histor_sal_func.cdn_motiv_liber_sal     AT 34
    v_des_motiv_liber_sal   format "x(10)"  AT 38
    cargo.cdn_cargo_basic   format "zzzz9"  AT 55
    cargo.cdn_niv_cargo     format "zz9"    AT 61
    cargo.des_cargo         format "x(30)"  AT 65
    de_trunc_val_salario_categ              AT 93 
    de_trunc_val_salario_mensal             AT 109 
    d-perc-aumento                          AT 125 
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-dados.

form
    histor_sal_func.dat_liber_sal        at  13                 space(1)
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

find param_empres_rh where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.       
find mgcad.empresa  where  empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
assign c-empresa = string(empresa.ep-codigo) + " - " + empresa.razao-social.

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
assign v_log_folha_educnal = if avail param_folha_educnal then yes else no.
assign c-sistema      = "FOLHA DE PAGAMENTO"
       c-titulo-relat = "Listagem dos Hist¢ricos de Sal rios e Fun‡äes".


run utp/ut-trfrrp.p (input frame f-dados:handle).
run utp/ut-trfrrp.p (input frame f-dados-epc:handle).
run utp/ut-trfrrp.p (input frame f-matric:handle).
run utp/ut-trfrrp.p (input frame f-cab:handle).
   {include/i-rpcab.i}




   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-cabec.
   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-rodape.
   view frame f-cab.

   if tt-param.num_classif_terc = 1
   or tt-param.num_classif_terc = 2 then do:
      for each funcionario no-lock where
               funcionario.cdn_empresa           = v_cdn_empres_usuar                  and
               funcionario.cdn_estab            >= tt-param.i-es-ini                   and
               funcionario.cdn_estab            <= tt-param.i-es-fim                   and
               funcionario.cdn_funcionario      >= tt-param.i-fc-ini                   and
               funcionario.cdn_funcionario      <= tt-param.i-fc-fim                   and
               /*funcionario.cdn_cargo_basic      >= tt-param.cdn_cargo_ini              and
               funcionario.cdn_cargo_basic      <= tt-param.cdn_cargo_fim              and*/
               funcionario.nom_pessoa_fisic     >= c-nm-ini                            and
               funcionario.nom_pessoa_fisic     <= c-nm-fim                            and
               funcionario.cdn_tip_contrat_func >= tt-param.cdn_tip_contrat_ini        and
               funcionario.cdn_tip_contrat_func <= tt-param.cdn_tip_contrat_fim        and
              (funcionario.dat_desligto_func     = ?                                   or
              (funcionario.dat_desligto_func    <= tt-param.v_dat_valid                and
               tt-param.l-imp-desl               = yes))                               and
              (funcionario.dat_admis_transf_func = ?                                   or
               funcionario.dat_admis_transf_func <= tt-param.v_dat_valid)              and
             ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func         > 0)   or
              (not v_log_folha_educnal))
         break by funcionario.cdn_estab
               by funcionario.idi_orig_contratac_func
               by funcionario.cdn_prestdor_serv
               by if tt-param.num_classif_terc = 1 then
                     string(funcionario.cdn_funcionario,"99999999")
                   else
                     funcionario.nom_pessoa_fisic:
           {prghur/fpp/fp9240.i14}

         find rh_estab of funcionario no-lock.                          
         assign c-ordem-v1   = "Estabelecimento:"
                c-codigo-v1  = string(rh_estab.cdn_estab,{prghur/dop/eng006.i})
                c-hifem-v1   = "-"
                c-descri-v1  = rh_estab.nom_pessoa_jurid
                c-ordem-v2   = ""
                c-codigo-v2  = ""
                c-hifem-v2   = ""
                c-descri-v2  = "".

             if not v_log_folha_educnal then 
                run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_funcionario)).            
             else 
                run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_func_centrdor)).            

             if first-of(funcionario.cdn_estab)
             or line-counter > 63 then do:
                page.
             end.
             if classifica = 3 then do:
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

/*                      if histor_sal_func.idi_tip_cargo_funcao <> 0 then                                                    */
/*                         assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 histor_sal_func.idi_tip_cargo_funcao}. */
/*                      else                                                                                                 */
/*                         assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 1}.                                    */
/*                                                                                                                           */
                     
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

&endif 
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

               display histor_sal_func.dat_liber_sal
                       histor_sal_func.cdn_motiv_liber_sal
                       v_des_motiv_liber_sal
                       v_cargo_niv_des
                       v_des_tip_cargo_func            format "x(24)" 
                       v_des_sit_histor_funcao
                       histor_sal_func.dat_term_valid_funcao
                       de_trunc_val_salario_categ      FORMAT "zzzz,zz9.99"  
                       de_trunc_val_salario_mensal     FORMAT "zzzz,zz9.99"  
                       d-perc-aumento                  FORMAT "zzz9.99"         
                       histor_sal_func.dat_livre_1                             
                     with frame f-dados-epc.
                down with frame f-dados-epc.
           end.
           for each tt-epc exclusive-lock where tt-epc.cod-event = "des_cargo":
              delete tt-epc.
           end.

           if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    display histor_sal_func.dat_liber_sal
                            histor_sal_func.dat_livre_1
                            histor_sal_func.dat_term_valid_funcao
                            histor_sal_func.cdn_motiv_liber_sal
                            v_des_motiv_liber_sal
                            cargo.cdn_cargo_basic       format "zzzz9"
                            cargo.cdn_niv_cargo         format "zz9"
                            cargo.des_cargo
                            de_trunc_val_salario_categ  FORMAT "zz,zzz,zz9.99"
                            de_trunc_val_salario_mensal FORMAT "zz,zzz,zz9.99"
                            d-perc-aumento              FORMAT "zzz9.99"
                            with frame f-dados.
                            down with frame f-dados.
                end.
            end. /*v_log_imprime*/
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
               display histor_sal_func.dat_liber_sal
                       histor_sal_func.cdn_motiv_liber_sal
                       v_des_motiv_liber_sal
                       v_cargo_niv_des
                       v_des_tip_cargo_func            format "x(24)" 
                       v_des_sit_histor_funcao
                       histor_sal_func.dat_term_valid_funcao
                       de_trunc_val_salario_categ      FORMAT "zzzz,zz9.99"  
                       de_trunc_val_salario_mensal     FORMAT "zzzz,zz9.99"  
                       d-perc-aumento                  FORMAT "zzz9.99"         
                       histor_sal_func.dat_livre_1                             
                     with frame f-dados-epc.
                down with frame f-dados-epc.
            end.

           
            if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    display histor_sal_func.dat_liber_sal
                            histor_sal_func.dat_livre_1
                            histor_sal_func.dat_term_valid_funcao
                            histor_sal_func.cdn_motiv_liber_sal
                            v_des_motiv_liber_sal
                            cargo.cdn_cargo_basic       format "zzzz9"
                            cargo.cdn_niv_cargo         format "zz9"
                            cargo.des_cargo
                            de_trunc_val_salario_categ  FORMAT "zz,zzz,zz9.999" 
                            de_trunc_val_salario_mensal FORMAT "zz,zzz,zz9.999" 
                            d-perc-aumento              FORMAT "zzz9.99"
                            with frame f-dados.
                            down with frame f-dados.
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
               display histor_sal_func.dat_liber_sal
                       histor_sal_func.cdn_motiv_liber_sal
                       v_des_motiv_liber_sal
                       v_cargo_niv_des
                       v_des_tip_cargo_func            format "x(24)" 
                       v_des_sit_histor_funcao
                       histor_sal_func.dat_term_valid_funcao
                       de_trunc_val_salario_categ      FORMAT "zzzz,zz9.99"  
                       de_trunc_val_salario_mensal     FORMAT "zzzz,zz9.99"  
                       d-perc-aumento                  FORMAT "zzz9.99"         
                       histor_sal_func.dat_livre_1                             
                     with frame f-dados-epc.
                down with frame f-dados-epc.
            end.

            
           if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    display histor_sal_func.dat_liber_sal
                            histor_sal_func.dat_livre_1
                            histor_sal_func.dat_term_valid_funcao
                            histor_sal_func.cdn_motiv_liber_sal
                            v_des_motiv_liber_sal
                            cargo.cdn_cargo_basic       format "zzzz9"
                            cargo.cdn_niv_cargo         format "zz9"
                            cargo.des_cargo
                            de_trunc_val_salario_categ  FORMAT "zz,zzz,zz9.9999"
                            de_trunc_val_salario_mensal FORMAT "zz,zzz,zz9.9999"
                            d-perc-aumento              FORMAT "zzz9.99"
                            with frame f-dados.
                            down with frame f-dados.
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
                            if histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if histor_sal_func.cdn_cargo_basic <> bhistsal.cdn_cargo_basic then do:
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
                            if histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if histor_sal_func.val_salario_mensal <> bhistsal.val_salario_mensal then do:
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
                    if   avail histor_sal_func then do:
                        repeat:
                            assign v_val_sal_hist = histor_sal_func.val_salario_mensal
                                   v_cdn_func     = histor_sal_func.cdn_funcionario
                                   v_cdn_cargo    = histor_sal_func.cdn_cargo_basic.
                            find prev histor_sal_func no-lock no-error.
                            if histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if histor_sal_func.cdn_cargo_basic    <> v_cdn_cargo    and
                               histor_sal_func.val_salario_mensal <> v_val_sal_hist then do:
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
