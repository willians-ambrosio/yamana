/********************************************************************************
 ********************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFR0140RP 1.00.00.001}  /*** 010219 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ymfr0140rp MFR}
&ENDIF

{cfghur/cfghur.i}   /*dirf parte2 - Isencao Abono*/

/*******************************************************************************
**       Programa: prghur/esp/YMFR0140.P
**       Data....: Maio/2009.
**       Autor...: Datasul.
**       Objetivo: Emissao do Demonstrativo do Calculo em Excel
*******************************************************************************/
/*{include/i-rpvar.i}*/

&GLOBAL-DEFINE RTF YES

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.  

def buffer bfrctrcal   for habilit_ferias.
def buffer bfunciona   for funcionario.

def var c-titulo-p    as char format "x(1)"                   no-undo.   
def var c-titulo-s    as char format "x(1)"                   no-undo.
def var c-titulo-i    as char format "x(1)"                   no-undo.  
def var c-destino     as char format "x(15)"                  no-undo.
def var i-contador    as int  initial 0                       no-undo.
def var i-matr-ini    like funcionario.cdn_funcionario        no-undo.
def var i-matr-fim    like funcionario.cdn_funcionario        no-undo.

{esp/ymfr0140tt.i}  /* Parametro */  /* Parametro */

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
{prghur/fpp/fp9200.i11}

&if "{&function_isencao_abono}" = "yes" &then   /*dirf parte2*/
    def buffer bmovto_ferias_calcul for movto_ferias_calcul.
&endif

def var v_des_destino      as char format "x(30)"  no-undo.
def var i-ind               as int  format "999"        initial 0      no-undo.
def var c-sinal             as char format "x(01)"                     no-undo.
def var i-matr-func         as char format "99999999999"               no-undo.
def var c-hifen             as char format "x" initial "-"             no-undo.
def var c-barra             as char format "x" initial "/"             no-undo.
def var v_log_folha_educnal as log  initial no                         no-undo.
def var c-path-excel       as char format "x(256)" no-undo.
def var v_cont_lin         as int                                      no-undo.
def var v_cont_col         as int                                      no-undo.
def var v_mes              as int format "99"                          no-undo.
def var v_mes_prox         as int format "99"                          no-undo.
def var v_num_col          as int                                      no-undo.
def var v_num_col2         as int                                      no-undo.
def var v_num_col_max      as int                                      no-undo.
def var v_ano              as int format "9999"                        no-undo.
def var v_des_calc         as char format "x(12)"                      no-undo.         
def var v_coluna            as char initial 
    "A;B;C;D;E;F;G;H;I;J;K;L;M;N;O;P;Q;R;S;T;U;V;W;X;Y;Z;
    AA;AB;AC;AD;AE;AF;AG;AH;AI;AJ;AK;AL;AM;AN;AO;AP;AQ;AR;AS;AT;AU;AV;AW;AX;AY;AZ;
    BA;BB;BC;BD;BE;BF;BG;BH;BI;BJ;BK;BL;BM;BN;BO;BP;BQ;BR;BS;BT;BU;BV;BW;BX;BY;BZ;" no-undo.

def temp-table tt-event
    field cdn_event as int 
    field num_col   as int
    field num_mes         as int format "99"
    field num_ano         as int format "9999"
    field des_event as char format "x(20)"
    index ttvnt 
    as primary unique num_col
                      num_ano
                      num_mes.

def temp-table tt-event2
    field cdn_event as int 
    field num_col   as int
    field num_mes         as int format "99"
    field num_ano         as int format "9999"
    field des_event as char format "x(20)"
    index ttvnt 
    as primary unique num_col
                      num_ano
                      num_mes.


def temp-table tt-funcionario
    field cdn_empresa     like funcionario.cdn_empresa
    field cdn_estab       like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field cdn_event       as int
    field cdn_tip_cal     as int
    field num_mes         as int format "99"
    field num_ano         as int format "9999"
    field val_event       as dec
    field num_col         as int
    index ttfncnr
    as primary unique cdn_empresa    
                      cdn_estab      
                      cdn_funcionario
                      cdn_event 
                      cdn_tip_cal 
                      num_mes        
                      num_ano.

def temp-table tt-funcionario2
    field cdn_empresa     like funcionario.cdn_empresa
    field cdn_estab       like funcionario.cdn_estab
    field cdn_funcionario like funcionario.cdn_funcionario
    field cdn_event       as int
    field cdn_tip_cal     as int
    field num_mes         as int format "99"
    field num_ano         as int format "9999"
    field val_event       as dec
    field num_col         as int
    index ttfncnr
    as primary unique cdn_empresa    
                      cdn_estab      
                      cdn_funcionario
                      cdn_event 
                      cdn_tip_cal
                      num_mes        
                      num_ano.

/***** Excel *****/
&scoped-define xlCenter              -4108
&scoped-define xlRight               -4152
&scoped-define xlLeft                -4131
&scoped-define xlJustify             -4130  
&scoped-define xlTop                 -4160
&scoped-define xlDiagonalDown        5
&scoped-define xlDiagonalUp          6
&scoped-define xlEdgeLeft            7
&scoped-define xlContinuous          1
&scoped-define xlAutomatic           -4105
&scoped-define xlEdgeTop             8
&scoped-define xlEdgeBottom          9
&scoped-define xlEdgeRight           10
&scoped-define xlNone                -4142
&scoped-define xlmsoShapeRectangle   1
&scoped-define xlmsoFalse            0
&scoped-define xlmsoScaleFromTopLeft 0
&scoped-define xlLandscape           2
&scoped-define xlDiagonalDown        5
&scoped-define xlDiagonalUp          6
&scoped-define xlInsideVertical      11
&scoped-define xlInsideHorizontal    12
&scoped-define xlFillDefault         0
&scoped-define xlLightUp             14
&scoped-define xlPortrait            1
&scoped-define xlpaperA4             9        
&scoped-define xlMedium              -4138

define variable chExcel       as component-handle no-undo.
define variable chWBook       as component-handle no-undo.
define variable chWSheet      as component-handle no-undo.
define variable chwsheetrange as component-handle no-undo.

create 'Excel.Application' chExcel.
assign c-path-excel = chExcel:Path.

chExcel:SheetsInNewWorkbook = 1.
chWBook  = chExcel:Workbooks:Add().
chWSheet = chWBook:Sheets:Item(1).

chExcel:Workbooks:Item(1):Sheets(1):Name = "F‚rias".
chExcel:Windows:Item(1):DisplayGridlines = true.

if trim(session:printer-name) <> "" then do: 
  chWSheet:PageSetup:Orientation = {&xlLandScape}. /* Paisagem */
  chExcel:ActiveWindow:Zoom = 75.
  chWSheet:PageSetup:PaperSize = 9.   /*----relatorio com folha do tipo A4 ----*/     
end.

chExcel:visible = no.
/****************/

/*assign v_des_destino = replace(arquivo,'.tmp','-' + replace(string(time,'HH:MM:SS' ),':','') + '.xls').*/

find empresa where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-lock no-error.
find param_empres_rh where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.
find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.

if avail param_folha_educnal then
   assign v_log_folha_educnal = yes
          i-matr-ini = {prghur/dop/eng005.i &var="((tt-param.i-fc-ini * 100) + tt-param.i-contr-ini)"}
          i-matr-fim = {prghur/dop/eng005.i &var="((tt-param.i-fc-fim * 100) + tt-param.i-contr-fim)"}.
else
   assign i-matr-ini = tt-param.i-fc-ini  
          i-matr-fim = tt-param.i-fc-fim.          
                             
{utp/ut-liter.i Processando... MFP R}
run pi-inicializar in h-acomp (input return-value).

assign v_num_col_max  = 0.

for each rh_estab of param_empres_rh no-lock where
         rh_estab.cdn_estab >= i-es-ini  and
	  rh_estab.cdn_estab <= i-es-fim:

    {prghur/fpp/fp9200.i21 &tabela=habilit_ferias &funcionario=yes}

    for each bfrctrcal of rh_estab exclusive-lock where
  	      bfrctrcal.cdn_funcionario  >= i-matr-ini    and
	      bfrctrcal.cdn_funcionario  <= i-matr-fim    and
	      bfrctrcal.dat_inic_ferias  >= d-dt-ini      and
	      bfrctrcal.dat_inic_ferias  <= d-dt-fim: 
        find bfunciona of rh_estab where
            bfunciona.cdn_funcionario = bfrctrcal.cdn_funcionario no-lock no-error. 
        if not avail bfunciona then 
            next.
        assign i-contador = i-contador + 1.
        run pi-acompanhar in h-acomp (input i-contador).

        assign v_mes = month(dat_inic_ferias)
               v_ano =  year(dat_inic_ferias).

        if avail param_folha_educnal then 
            assign v_log_folha_educnal = yes
                 i-matr-func = string(bfunciona.cdn_func_centrdor) + c-barra + 
                               string(bfunciona.cdn_tip_contrat_func) + c-hifen + 
                               string(bfunciona.num_digito_verfdor_func).
        else assign i-matr-func = string(bfunciona.cdn_funcionario) + c-hifen + 
                                  string(bfunciona.num_digito_verfdor_func).

        find categ_sal of rh_estab no-lock where
             categ_sal.cdn_categ_sal = bfunciona.cdn_categ_sal
             no-error.
        for each movto_ferias_calcul of bfunciona no-lock where
                 movto_ferias_calcul.dat_inic_ferias = bfrctrcal.dat_inic_ferias and
                 movto_ferias_calcul.log_calc_efetd_movto_ferias = yes
                 break by movto_ferias_calcul.cdn_funcionario
                       by movto_ferias_calcul.cdn_tip_calc_ferias:
           
           /************************************************************/
           for each reg_det_ferias no-lock where
                    reg_det_ferias.cdn_empresa         = movto_ferias_calcul.cdn_empresa     and   
                    reg_det_ferias.cdn_estab           = movto_ferias_calcul.cdn_estab       and   
                    reg_det_ferias.cdn_funcionario     = movto_ferias_calcul.cdn_funcionario and   
                    reg_det_ferias.dat_inic_ferias     = movto_ferias_calcul.dat_inic_ferias and   
                    reg_det_ferias.cdn_tip_calc_ferias = movto_ferias_calcul.cdn_tip_calc_ferias:

               do i-ind = 1 to reg_det_ferias.qti_efp:
                  
                  if reg_det_ferias.cdn_efp_calc_origin[i-ind] = "000" then
                     next.
     
                  find event_fp no-lock where
                       event_fp.cdn_event_fp = reg_det_ferias.cdn_efp_calc_origin[i-ind] no-error.  
                  
                  if not can-find(first tt-event where
                                  tt-event.cdn_event = int(event_fp.cdn_event_fp) and
                                  tt-event.num_ano   = v_ano and
                                  tt-event.num_mes   = v_mes) then do:

                      find last tt-event 
                          where tt-event.num_ano   = v_ano  
                            and tt-event.num_mes   = v_mes no-error.
                      if avail tt-event  then do:
                          assign v_num_col = tt-event.num_col + 1.
                      end.
                      else
                          assign v_num_col = 1.

                      create tt-event.
                      assign tt-event.cdn_event = int(event_fp.cdn_event_fp)
                             tt-event.des_event = event_fp.des_abrev_efp
                             tt-event.num_col   = v_num_col
                             tt-event.num_ano   = v_ano
                             tt-event.num_mes   = v_mes.
                  end.
                  else do:
                      find last tt-event where
                          tt-event.cdn_event = int(event_fp.cdn_event_fp) and
                          tt-event.num_ano   = v_ano and
                          tt-event.num_mes   = v_mes no-error.
                      if avail tt-event then
                          assign v_num_col = tt-event.num_col.
                  end.

                  if v_num_col > v_num_col_max then
                      assign v_num_col_max = v_num_col.

                  if reg_det_ferias.qtd_dias_mes_inic_ferias[i-ind] > 0
                  or reg_det_ferias.val_base_mes_inic_ferias[i-ind] > 0
                  or reg_det_ferias.val_efp_mes_inic_ferias[i-ind] > 0 then do:
                     
                     if not can-find(first tt-funcionario where
                                     tt-funcionario.cdn_empresa     = reg_det_ferias.cdn_empresa    and
                                     tt-funcionario.cdn_estab       = reg_det_ferias.cdn_estab          and
                                     tt-funcionario.cdn_funcionario = reg_det_ferias.cdn_funcionario        and
                                     tt-funcionario.cdn_event       = int(reg_det_ferias.cdn_efp_calc_origin[i-ind]) and
                                     tt-funcionario.cdn_tip_cal     = reg_det_ferias.cdn_tip_calc_ferias        and
                                     tt-funcionario.num_mes         = v_mes  and
                                     tt-funcionario.num_ano         = v_ano) then do:

                         create tt-funcionario.
                         assign tt-funcionario.cdn_empresa     = reg_det_ferias.cdn_empresa               
                                tt-funcionario.cdn_estab       = reg_det_ferias.cdn_estab                 
                                tt-funcionario.cdn_funcionario = reg_det_ferias.cdn_funcionario           
                                tt-funcionario.cdn_event       = int(reg_det_ferias.cdn_efp_calc_origin[i-ind])
                                tt-funcionario.num_mes         = v_mes                                    
                                tt-funcionario.num_ano         = v_ano
                                tt-funcionario.num_col         = v_num_col
                                tt-funcionario.cdn_tip_cal     = reg_det_ferias.cdn_tip_calc_ferias.
                     end.
     
                     if reg_det_ferias.cdn_idx_efp_espcif_ferias[i-ind] <> 8 then do:
                        if event_fp.idi_tip_inciden_liq = 1 then 
                           assign c-sinal = "+".
                        else
                           if event_fp.idi_tip_inciden_liq = 2 then 
                              assign c-sinal = "-".
                           else
                              assign c-sinal = " ".                   

                        assign tt-funcionario.val_event = reg_det_ferias.val_efp_mes_inic_ferias[i-ind].
                     end.
                     else do:
                         assign tt-funcionario.val_event = reg_det_ferias.val_mes_inic_ferias[i-ind].
                     end.
                  end.
                 if reg_det_ferias.qtd_dias_seguinte_ferias[i-ind] > 0
                 or reg_det_ferias.val_base_seguinte_ferias[i-ind] > 0
                 or reg_det_ferias.val_efp_seguinte_ferias[i-ind] > 0 then do:
                    
                     if event_fp.idi_tip_inciden_liq = 1 then 
                        assign c-sinal = "+".
                     else
                        if event_fp.idi_tip_inciden_liq = 2 then 
                           assign c-sinal = "-".
                        else
                           assign c-sinal = " ".  

                     if not can-find(first tt-event2 where
                                     tt-event2.cdn_event = int(event_fp.cdn_event_fp) and
                                     tt-event2.num_ano   = v_ano and
                                     tt-event2.num_mes   = v_mes) then do:
    
                         find last tt-event2 
                             where tt-event2.num_ano   = v_ano  
                               and tt-event2.num_mes   = v_mes no-error.
                         if avail tt-event2  then do:
                             assign v_num_col2 = tt-event2.num_col + 1.
                         end.
                         else
                             assign v_num_col2 = 1.
    
                         create tt-event2.
                         assign tt-event2.cdn_event = int(event_fp.cdn_event_fp)
                                tt-event2.des_event = event_fp.des_abrev_efp
                                tt-event2.num_col   = v_num_col2
                                tt-event2.num_ano   = v_ano
                                tt-event2.num_mes   = v_mes.
                     end.
                     else do:
                         find last tt-event2 where
                             tt-event2.cdn_event = int(event_fp.cdn_event_fp) and
                             tt-event2.num_ano   = v_ano and
                             tt-event2.num_mes   = v_mes no-error.
                         if avail tt-event2 then
                             assign v_num_col2 = tt-event2.num_col.
                     end.    

                     if not can-find(first tt-funcionario2 where
                                     tt-funcionario2.cdn_empresa     = reg_det_ferias.cdn_empresa    and
                                     tt-funcionario2.cdn_estab       = reg_det_ferias.cdn_estab          and
                                     tt-funcionario2.cdn_funcionario = reg_det_ferias.cdn_funcionario        and
                                     tt-funcionario2.cdn_event       = int(reg_det_ferias.cdn_efp_calc_origin[i-ind]) and
                                     tt-funcionario2.cdn_tip_cal     = reg_det_ferias.cdn_tip_calc_ferias        and 
                                     tt-funcionario2.num_mes         = v_mes  and
                                     tt-funcionario2.num_ano         = v_ano) then do:

                         create tt-funcionario2.
                         assign tt-funcionario2.cdn_empresa     = reg_det_ferias.cdn_empresa               
                                tt-funcionario2.cdn_estab       = reg_det_ferias.cdn_estab                 
                                tt-funcionario2.cdn_funcionario = reg_det_ferias.cdn_funcionario           
                                tt-funcionario2.cdn_event       = int(reg_det_ferias.cdn_efp_calc_origin[i-ind])
                                tt-funcionario2.cdn_tip_cal     = reg_det_ferias.cdn_tip_calc_ferias        
                                tt-funcionario2.num_mes         = v_mes                                    
                                tt-funcionario2.num_ano         = v_ano
                                tt-funcionario2.num_col         = v_num_col2
                                tt-funcionario2.val_event       = reg_det_ferias.val_efp_seguinte_ferias[i-ind]
                                tt-funcionario2.cdn_tip_cal     = reg_det_ferias.cdn_tip_calc_ferias.
                     end.
                 end.
               end.
           end.
        end.
    end.
end.

{utp/ut-liter.i Gerando_Relat¢rio_Excel... MFP R}
run pi-inicializar in h-acomp (input return-value).

assign i-contador    = 1
       v_cont_lin    = -1
       v_num_col_max = v_num_col_max + 4.
for each tt-funcionario
    break by tt-funcionario.num_ano
          by tt-funcionario.num_mes
          by tt-funcionario.cdn_estab
          by tt-funcionario.cdn_funcionario
          by tt-funcionario.cdn_tip_cal:

    run pi-acompanhar in h-acomp (input i-contador).

    if first-of(tt-funcionario.num_mes) then do:
        assign v_cont_lin = v_cont_lin + 2.
        chWSheet:Range("A" + string(v_cont_lin)):font:bold = true.
        chWsheet:Range("A" + string(v_cont_lin)):Value = "Folha " + {database/inpy/i02py167.i 04 tt-funcionario.num_mes} 
                                                         + " - " + string(tt-funcionario.num_ano).

        /*** Imprime Texto Pr¢xima Folha se houver evento pro pr¢ximo mˆs ***/
        if can-find(first tt-event2 where
                    tt-event2.num_ano = tt-funcionario.num_ano and
                    tt-event2.num_mes = tt-funcionario.num_mes) then do:
            assign v_mes_prox = if tt-funcionario.num_mes = 12 then 1 else tt-funcionario.num_mes + 1.
            chWSheet:Range(entry(v_num_col_max + 1 ,v_coluna,';') + string(v_cont_lin)):font:bold = true.
            chWsheet:Range(entry(v_num_col_max + 1 ,v_coluna,';') + string(v_cont_lin)):Value = "Pr¢xima Folha " + {database/inpy/i02py167.i 04 v_mes_prox}.
        end.

        assign v_cont_lin = v_cont_lin + 1.
                                                                     
        chWSheet:Range("A" + string(v_cont_lin)):font:bold = true.
        chWsheet:Range("A" + string(v_cont_lin)):Value = "Estab".
        chWSheet:Range("B" + string(v_cont_lin)):font:bold = true.
        chWsheet:Range("B" + string(v_cont_lin)):Value = "Matr¡cula".
        chWSheet:Range("C" + string(v_cont_lin)):font:bold = true.
        chWsheet:Range("C" + string(v_cont_lin)):Value = "C lculo".

        for each tt-event
            where tt-event.num_ano = tt-funcionario.num_ano
              and tt-event.num_mes = tt-funcionario.num_mes
            break by tt-event.num_ano
                  by tt-event.num_mes
                  by tt-event.cdn_event:                 

            chWSheet:Range(entry(tt-event.num_col + 3,v_coluna,';') + string(v_cont_lin)):font:bold = true.
            chWsheet:Range(entry(tt-event.num_col + 3,v_coluna,';') + string(v_cont_lin)):Value = tt-event.cdn_event.

            find first tt-event2 of tt-event no-error.
            if avail tt-event2 then do:
                chWSheet:Range(entry(tt-event2.num_col + v_num_col_max,v_coluna,';') + string(v_cont_lin)):font:bold = true.
                chWsheet:Range(entry(tt-event2.num_col + v_num_col_max,v_coluna,';') + string(v_cont_lin)):Value = tt-event2.cdn_event.
            end.
        end.     
    end.

    if first-of(tt-funcionario.cdn_tip_cal) then do:
        assign i-contador = i-contador + 1
               v_cont_lin = v_cont_lin + 1
               v_des_calc = if tt-funcionario.cdn_tip_cal = 0
                            then "Original"
                            else "Complemento".
        chWsheet:Range("A" + string(v_cont_lin)):Value = tt-funcionario.cdn_estab.
        chWsheet:Range("B" + string(v_cont_lin)):Value = tt-funcionario.cdn_funcionario.
        chWsheet:Range("C" + string(v_cont_lin)):Value = v_des_calc.
    end.

    chWsheet:Range(entry(tt-funcionario.num_col + 3,v_coluna,';') + string(v_cont_lin)):NumberFormat = '########,00'.
    if tt-funcionario.val_event <> 0 then
        chWsheet:Range(entry(tt-funcionario.num_col + 3,v_coluna,';') + string(v_cont_lin)):Value = tt-funcionario.val_event.
    
    /*** Imprime res¡duos para o pr¢ximo mˆs ***/
    find first tt-funcionario2 of tt-funcionario no-error.
    if avail tt-funcionario2 then do:
        chWsheet:Range(entry(tt-funcionario2.num_col + v_num_col_max,v_coluna,';') + string(v_cont_lin)):NumberFormat = '########,00'.
        if tt-funcionario2.val_event <> 0 then
            chWsheet:Range(entry(tt-funcionario2.num_col + v_num_col_max,v_coluna,';') + string(v_cont_lin)):Value = tt-funcionario2.val_event.
    end.
end.

if tt-param.destino = 2 then do:

    assign v_des_destino = replace(arquivo,'.lst','-' + replace(string(time,'HH:MM:SS' ),':','') + '.xls').

    chWSheet:Range("A1"):Select.
    chExcel:Visible = false.
    chWbook:Close(yes, v_des_destino).
    chExcel:Quit().     
end.

if tt-param.destino = 3 then do:

    assign v_des_destino = replace(arquivo,'.tmp','-' + replace(string(time,'HH:MM:SS' ),':','') + '.xls').

    chWSheet:Range("A1"):Select.
    chExcel:Visible = false.
    chWbook:Close(yes, v_des_destino).
    chExcel:Quit().     

    run WinExec (input string(c-path-excel + "~\excel~.exe " + "~"" + v_des_destino + "~""), input 1).
end.

if valid-handle(chWSheet)      then release object chWSheet. 
if valid-handle(chwsheetrange) then release object chwsheetrange. 
if valid-handle(chWBook)       then release object chWBook.
if valid-handle(chExcel)       then release object chExcel.

run pi-finalizar in h-acomp.

return.

procedure WinExec external "kernel32.dll":
    define input parameter prog_name    as character.
    define input parameter visual_style as short.
end procedure.

/*     prghur/frp/FR0140.P     */


