/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FP3500RP 1.02.08.044 } /*** 010844 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fp3500rp MFP}
&ENDIF

{include/i_fnctrad.i}
/*********************************************************************************
**       Programa: prghur/fpp/FP3500.P
**
**       Data....: Maio/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem - Emissao Coletiva de Envelopes de Pagamento.
**
*******************************************************************************/

{include/i-rpvar.i}

def new shared var v_han_acomp as handle no-undo.
run utp/ut-acomp.p persistent set v_han_acomp.                            

define var c-forma-pgto     as char format "x(30)"                     no-undo.
define var c-tab-trans      as character initial "FP3500"              no-undo.
def var i-col-selec        as dec                                      no-undo.
def var i-col-classif      as dec                                      no-undo.
def var i-col-janela       as dec                                      no-undo.
def var c-titulo-g         as char format "x(20)"                      no-undo. 
def var c-titulo-p         as char format "x(30)"                      no-undo.
def var c-titulo-i         as char format "x(30)"                      no-undo.
def var c-titulo-s         as char format "x(30)"                      no-undo.
def var c-tipo-folha as char                 no-undo.
DEF VAR c_arquivo         AS CHAR FORMAT "x(120)"                      NO-UNDO.
def var c-selecao            as char format "x(20)"                     no-undo. 
def var c-classif            as char format "x(30)"                     no-undo.
def var c-impressao          as char format "x(30)"                     no-undo.
def var c-parametro          as char format "x(30)"                     no-undo.
DEF VAR c-nivel-salta-pg     AS CHAR FORMAT "x(30)"                     NO-UNDO.
DEF VAR c-nivel-quebra       AS CHAR FORMAT "x(30)"                     NO-UNDO.
DEF VAR c-forma-pg           AS CHAR FORMAT "x(30)"                     NO-UNDO.
DEF VAR c-tp-formula         AS CHAR FORMAT "x(30)"                     NO-UNDO.
DEF VAR c-desc-destino       AS CHAR FORMAT "x(30)"                     NO-UNDO.

def var c-demitidos            as char format "x(3)"                   no-undo.
def new shared var l-imprime   as logical                              no-undo.
def var c-msg              as char format "x(50)"                      no-undo.

define var l-retorno  as logical format "Sim/Nao"                      no-undo.

define new shared var i-ordem   as integer               no-undo.
define new shared var i-empresa like mgcad.empresa.ep-codigo   no-undo.
define new shared var i-ord-aux as int                   no-undo.
define new shared var l-origem  as log format "Coletiva/Individual" no-undo.
define new shared var v_log_folha_educnal as log initial no no-undo.

def new shared var c-imp as cha                      no-undo.
def new shared var c-emp as cha  format "x(40)"      no-undo.
def new shared var c-tit as cha  format "x(50)"      no-undo.
def new shared var i-num as int  format "ZZ"         no-undo.
def new shared var da-in as date format "99/99/9999" no-undo.
def new shared var da-fi as date format "99/99/9999" no-undo.
def new shared var c-rod as cha                      no-undo.
def new shared var c-sis as cha  format "x(25)"      no-undo.
def new shared var c-lay as cha                      no-undo.
def new shared var v_num as int                      no-undo.
def new shared var c-arq as cha                      no-undo.
def new shared var i-pag as int                      no-undo.

def new shared var v_mes_ini           as int                       no-undo.
def new shared var v_ano_ini           as int                       no-undo.
def new shared var v_mes_fim           as int                       no-undo.
def new shared var v_ano_fim           as int                       no-undo.


DEFINE NEW SHARED TEMP-TABLE tt-rel-erros no-undo
    FIELD cdn_empresa      LIKE funcionario.cdn_empresa
    FIELD cdn_estab        LIKE funcionario.cdn_estab
    FIELD cdn_funcionario  LIKE funcionario.cdn_funcionario
    FIELD nom_pessoa_fisic LIKE funcionario.nom_pessoa_fisic
    FIELD status_email     AS CHAR FORMAT "x(15)"
    FIELD email            AS CHAR FORMAT "x(60)".

FORM
    tt-rel-erros.cdn_estab SKIP(1)
    with stream-io SIDE-LABELS no-attr-space no-box width 132 frame f-estab.
    
    RUN utp/ut-trfrrp.p (INPUT FRAME f-estab:HANDLE).

FORM 
    tt-rel-erros.cdn_funcionario
    tt-rel-erros.nom_pessoa_fisic
    tt-rel-erros.status_email
    tt-rel-erros.email
    with stream-io DOWN no-attr-space no-box width 132 frame f-status.

    RUN utp/ut-trfrrp.p (INPUT FRAME f-status:HANDLE).


{prghur/fpp/fp3500tt.i new shared}
{prghur/fpp/fp9200.i10 new shared}
{prghur/fpp/fp9200.i8}

{utp/ut-liter.i Status_do_Envio MFP R}
ASSIGN tt-rel-erros.status_email:LABEL IN FRAME f-status = RETURN-VALUE.

{utp/ut-liter.i Descri‡Æo MFP R}
ASSIGN tt-rel-erros.email:LABEL IN FRAME f-status = RETURN-VALUE.

define buffer b-tt-digita for tt-digita.
def temp-table tt-raw-digita
    field raw-digita as raw.
def input parameter  raw-param as raw no-undo.
def input parameter  table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.
&global-define task_clt YES
{prghur/fpp/fp9200.i11}

{utp/ut-liter.i SELE€ÇO *}
assign c-selecao = return-value.
{utp/ut-liter.i CLASSIFICA€ÇO *}
assign c-classif = return-value.
{utp/ut-liter.i PAR¶METROS *}
assign c-parametro = return-value.
{utp/ut-liter.i IMPRESSÇO *}
assign c-impressao = return-value.


 FORM
      SKIP(5)
      "P gina de Parƒmetros"          AT 20              SKIP(2)

      c-classif                       AT 25  NO-LABEL    SKIP
        desc-classifica               AT 35  NO-LABEL    SKIP(2)
      c-parametro                     AT 25  NO-LABEL    SKIP
      "Data Refer Hist Lotac:"     COLON 40
        v_dat_valid                           NO-LABEL    SKIP
      "Expande Extrutura:"         COLON 40
        v_log_expande_estrut                  NO-LABEL    SKIP  
      "N¡vel Salta P gina:"        COLON 40
        v_num_salta_pg                        NO-LABEL    SKIP 
      "N¡vel Quebra:"              COLON 40
        v_num_quebra                          NO-LABEL    SKIP
      "Enviar e-mail:"             COLON 40
      v_log_enviar_email                      NO-LABEL    SKIP
      "Tipo Folha:"                COLON 40
        c-tipo-folha                          NO-LABEL    SKIP  
      "Forma Pagamento:"           COLON 40
        c-forma-pg                          NO-LABEL    SKIP
      "Parcela:"                   colon 40
        i-parcela                             NO-LABEL    SKIP 
      "Tipo Formul rio:"              COLON 40
        c-tp-formula                        NO-LABEL    SKIP(2)          
      "Categoria Salarial: "          AT 35               SKIP 
      "Mensal:"                    COLON 40
       l-mensal                               NO-LABEL    SKIP                  
      "Horista:"                   COLON 40                             
       l-horista                              NO-LABEL    SKIP              
      "Semanal:"                   COLON 40                             
       l-semanal                              NO-LABEL    SKIP              
      "Quinzenal:"                 COLON 40                            
       l-quinzenal                            NO-LABEL    SKIP              
      "Tarefa:"                    COLON 40                             
       l-tarefa                               NO-LABEL    SKIP           
      "Diarista:"                  COLON 40                            
       l-diarista                             NO-LABEL    SKIP(2)   
      "Emite Demitidos:"           COLON 40
      l-emite-demi                            NO-LABEL    SKIP
      "Emite Afastados:"           COLON 40
      l-emite-afast                           NO-LABEL    SKIP
      "Emite F‚rias:"              COLON 40
      l-emite-ferias                          NO-LABEL    SKIP
       with stream-io attr-space side-labels no-box width 132 frame f-param.

     RUN utp/ut-trfrrp.p (INPUT FRAME f-param:HANDLE).
&if "{&cd_rel_hr}" >= "2.11" &then
      FORM
          c-selecao                        AT 25  NO-LABEL    SKIP
            i-es-ini                    colon 50                        
            "                     |<  >| "                                        
            i-es-fim                    NO-LABEL              SKIP
            i-fc-ini                    colon 50                        
            "                  |<  >| "                                        
            i-fc-fim                    NO-LABEL              SKIP
            v_nom_func_ini              colon 50  FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz"
            " |<  >| "             
            v_nom_func_fim              FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-LABEL    SKIP
            v_niv_unid_lotac            colon 50  LABEL "N¡vel Unidade Lota‡Æo"        SKIP
            v_cod_unid_lotac_ini            colon 50  LABEL "Unidade de Lota‡Æo" 
            "               |<  >| "        
            v_cod_unid_lotac_fim            NO-LABEL
            i-cc-codigo-1               colon 50                      
            "      |<  >| "                                         
            i-cc-codigo-2               NO-LABEL              SKIP
            i-bc-codigo-1               colon 50                              
            "                       |<  >| "                                         
            i-bc-codigo-2               NO-LABEL              SKIP
            i-ag-codigo-1               colon 50                              
            "                      |<  >| "                                         
            i-ag-codigo-2               NO-LABEL              SKIP
            cdn_local_pagto_ini         colon 50                              
            "               |<  >| "                                          
            cdn_local_pagto_fim         NO-LABEL              SKIP(2)
          with stream-io attr-space side-labels no-box width 132 frame f-sel.
          RUN utp/ut-trfrrp.p (INPUT FRAME f-sel:HANDLE).
&else
      FORM
          c-selecao                        AT 25  NO-LABEL    SKIP
            i-es-ini                    colon 50                        
            "                       |<  >| "                                        
            i-es-fim                    NO-LABEL              SKIP
            i-fc-ini                    colon 50                        
            "                  |<  >| "                                        
            i-fc-fim                    NO-LABEL              SKIP
            v_nom_func_ini              colon 50  FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz"
            " |<  >| "             
            v_nom_func_fim              FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-LABEL    SKIP
            v_niv_unid_lotac            colon 50  LABEL "N¡vel Unidade Lota‡Æo"        SKIP
            v_cod_unid_lotac_ini            colon 50  LABEL "Unidade de Lota‡Æo" 
            "               |<  >| "        
            v_cod_unid_lotac_fim            NO-LABEL
            i-cc-codigo-1               colon 50                      
            "                  |<  >| "                                         
            i-cc-codigo-2               NO-LABEL              SKIP
            i-bc-codigo-1               colon 50                              
            "                       |<  >| "                                         
            i-bc-codigo-2               NO-LABEL              SKIP
            i-ag-codigo-1               colon 50                              
            "                      |<  >| "                                         
            i-ag-codigo-2               NO-LABEL              SKIP
            cdn_local_pagto_ini         colon 50                              
            "               |<  >| "                                          
            cdn_local_pagto_fim         NO-LABEL              SKIP(2)
          with stream-io attr-space side-labels no-box width 132 frame f-sel.
          RUN utp/ut-trfrrp.p (INPUT FRAME f-sel:HANDLE).
&endif


  FORM
      c-selecao                        AT 25  NO-LABEL    SKIP
        i-es-ini                    COLON 50                 
        "                       |<  >| "                                           
        i-es-fim                    NO-LABEL              SKIP
        i-fc-ini                    COLON 50                      
        "                  |<  >| "                                         
        i-fc-fim                    NO-LABEL              SKIP
        i-contr-ini                 COLON 50                      
        "                        |<  >| "                                        
        i-contr-fim                 NO-LABEL              SKIP
        v_nom_func_ini              COLON 50  FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz"                      
        " |<  >| "             
        v_nom_func_fim              FORMAT "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-LABEL    SKIP            
        v_niv_unid_lotac            COLON 50 LABEL "N¡vel Unidade Lota‡Æo:"         SKIP                  
        v_cod_unid_lotac_ini            COLON 50 LABEL "Unidade de Lota‡Æo"                     
        "               |<  >| "                                                   
        v_cod_unid_lotac_fim            NO-LABEL                      
        i-cc-codigo-1               COLON 50                      
        "                  |<  >| "                                         
        i-cc-codigo-2               NO-LABEL              SKIP
        i-bc-codigo-1               COLON 50                          
        "                       |<  >| "                                          
        i-bc-codigo-2               NO-LABEL              SKIP
        i-ag-codigo-1               COLON 50                      
        "                      |<  >| "                                        
        i-ag-codigo-2               NO-LABEL              SKIP
        cdn_local_pagto_ini         COLON 50                       
        "               |<  >| "                                         
        cdn_local_pagto_fim         NO-LABEL              SKIP(2)
      with stream-io attr-space side-labels no-box width 132 frame f-sel-educ.
      RUN utp/ut-trfrrp.p (INPUT FRAME f-sel-educ:HANDLE).
 
  FORM
  
      c-impressao                    AT 25 NO-LABEL   
      c-desc-destino              COLON 50 LABEL "Destino"
      arquivo                     COLON 50 LABEL "Arquivo"            
     with stream-io attr-space side-labels no-box width 132 frame f-imp.
     RUN utp/ut-trfrrp.p (INPUT FRAME f-imp:HANDLE).

assign c-titulo-s = "SELE€ÇO"
       c_arquivo  = tt-param.arquivo. 

  CASE i-tipo-folha:
      WHEN 1 THEN
          ASSIGN c-tipo-folha = "Normal". 
      WHEN 3 THEN                         
          ASSIGN c-tipo-folha = "13§ Sal rio". 
      WHEN 4 THEN                         
          ASSIGN c-tipo-folha = "Adt 13§ Sal rio". 
      OTHERWISE
          ASSIGN c-tipo-folha = "Normal".             
  END CASE.

  CASE r-forma-pgto:
      WHEN 1 THEN                         
          ASSIGN c-forma-pg = "Dep¢sito L¡quido". 
      WHEN 2 THEN                         
          ASSIGN c-forma-pg = "Cheque Sal rio". 
      WHEN 3 THEN                         
          ASSIGN c-forma-pg = "Caixa". 
      OTHERWISE
          ASSIGN c-forma-pg = "Todas".
  END CASE.

  CASE i-tipo-formula:
      WHEN 1 THEN
          ASSIGN c-tp-formula = "Espec¡fico".
      WHEN 3 THEN                            
          ASSIGN c-tp-formula = "Individual".
      WHEN 4 THEN                            
          ASSIGN c-tp-formula = "Individual Moore".
      WHEN 5 THEN                            
          ASSIGN c-tp-formula = "PDF Duplo".
      WHEN 6 THEN                            
          ASSIGN c-tp-formula = "PDF Individual".
      OTHERWISE
          ASSIGN c-tp-formula = "Duplo".
  END CASE.

  CASE destino:
      WHEN 1 THEN
          ASSIGN c-desc-destino = "Impressora".
      WHEN 2 THEN               
          ASSIGN c-desc-destino = "Arquivo".
      OTHERWISE
          ASSIGN c-desc-destino = "Terminal".
  END CASE.

find mgcad.empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

find param_empres_rh no-lock where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.

find param_folha_educnal no-lock where param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.

if avail param_folha_educnal then assign v_log_folha_educnal = yes.

assign c-versao       = "D.00"
       c-revisao      = "013"
       c-empresa      = empresa.razao-social
       c-sistema      = "FOLHA DE PAGAMENTO"
       c-titulo-relat = "EmissÆo Coletiva de Envelopes".
{utp/ut-liter.i EmissÆo_Coletiva_de_Envelopes *}

assign tt-param.l-mensal:format             in frame f-param = "Sim/NÆo"                          
       tt-param.l-horista:format            in frame f-param = "Sim/NÆo"               
       tt-param.l-semanal:format            in frame f-param = "Sim/NÆo"               
       tt-param.l-quinzenal:format          in frame f-param = "Sim/NÆo"             
       tt-param.l-tarefa:format             in frame f-param = "Sim/NÆo"                
       tt-param.l-diarista:format           in frame f-param = "Sim/NÆo"              
       tt-param.l-emite-demi:format         in frame f-param = "Sim/NÆo"
       tt-param.v_log_expande_estrut:format in frame f-param = "Sim/NÆo"
       tt-param.v_log_enviar_email:format   in frame f-param = "Sim/NÆo"
       tt-param.l-emite-afast:format        in frame f-param = "Sim/NÆo"
       tt-param.l-emite-ferias:format       in frame f-param = "Sim/NÆo".

  run pi-inicializar in v_han_acomp (input  Return-value ).

  assign i-ordem   = tt-param.classifica.
  assign i-empresa = empresa.ep-codigo
         i-ord-aux = i-ordem
         l-origem  = yes.

  assign c-imp = c-impressora
         c-emp = c-empresa
         c-tit = c-titulo-relat
         i-num = i-numper-x
         da-in = da-iniper-x
         da-fi = da-fimper-x
         c-rod = c-rodape
         c-sis = c-sistema
         c-lay = c-layout
         v_num = v_num_count
         c-arq = c-arq-control
         i-pag = i-page-size-rel.

  IF NOT tt-param.v_log_enviar_email THEN DO:

      if tt-param.l-layout-detalhado then do:
          if tt-param.destino = 1 then do: /* Impressora */

              assign c-impressora = substring(tt-param.arquivo,1,index(tt-param.arquivo,":") - 1).

              find first imprsor_usuar no-lock
                   where imprsor_usuar.nom_impressora = c-impressora
                     and imprsor_usuar.cod_usuario    = tt-param.usuario use-index imprsrsr_id no-error.

              output {&stream} to value(imprsor_usuar.nom_disposit_so) paged convert target "ISO8859-1".
          end.
          else do:
              output {&stream} to value(tt-param.arquivo) paged convert target "ISO8859-1".
          end.
      end.
      else do:
          {include/i-rpout.i &pagesize=0}
      end.

      if i-num-ped-exec-rpw > 0 then
          assign tt-param.arquivo-pdf = c-dir-spool-servid-exec + "/" + tt-param.arquivo-pdf.
  end.
  ELSE
     assign tt-param.arquivo = session:TEMP-DIRECTORY + "extrato.lst".

    if tt-param.classifica <> 99 then do:
        RUN VALUE("prghur/fpp/fp3500r" + STRING(tt-param.i-tipo-formula,"9") + ".p").
    end.
    if tt-param.classifica = 99 then do:
        run prghur/fpp/fp3500rt.p.
    end.

  IF tt-param.l-parametro THEN DO:
       DISP SKIP(3)  
        c-classif
        tt-param.desc-classifica
        c-parametro
        v_dat_valid            
        v_log_expande_estrut   
        v_num_salta_pg         
        v_num_quebra 
        v_log_enviar_email
        c-tipo-folha           
        c-forma-pg           
        i-parcela              
        c-tp-formula
        l-mensal            
        l-horista           
        l-semanal               
        l-quinzenal             
        l-tarefa           
        l-diarista
        l-emite-demi
        l-emite-afast
        l-emite-ferias WITH FRAME f-param.
       
       IF NOT v_log_folha_educnal THEN DO:
           DISP 
                c-selecao 
                i-es-ini                
                i-es-fim                
                i-fc-ini                
                i-fc-fim                      
                v_nom_func_ini                           
                v_nom_func_fim 
                v_niv_unid_lotac  
                v_cod_unid_lotac_ini       
                v_cod_unid_lotac_fim  
                i-cc-codigo-1               
                i-cc-codigo-2                      
                i-bc-codigo-1               
                i-bc-codigo-2               
                i-ag-codigo-1                       
                i-ag-codigo-2
                cdn_local_pagto_ini            
                cdn_local_pagto_fim 
                WITH FRAME f-sel.
       END.
       ELSE DO:
           DISP
                c-selecao          
                i-es-ini           
                i-es-fim           
                i-fc-ini           
                i-fc-fim
                i-contr-ini  
                i-contr-fim 
                v_niv_unid_lotac  
                v_cod_unid_lotac_ini  
                v_cod_unid_lotac_fim  
                v_nom_func_ini     
                v_nom_func_fim     
                i-cc-codigo-1      
                i-cc-codigo-2      
                i-bc-codigo-1      
                i-bc-codigo-2      
                i-ag-codigo-1      
                i-ag-codigo-2      
                cdn_local_pagto_ini
                cdn_local_pagto_fim
                WITH FRAME f-sel-educ. 
       END.

       DISP c-impressao     
            c-desc-destino            
            arquivo
            WITH FRAME f-imp.
  END.
   
  IF tt-param.v_log_enviar_email THEN DO:
      ASSIGN tt-param.arquivo = c_arquivo.
      {include/i-rpcab.i}
      {include/i-rpout.i}
      view frame f-cabec.
      view frame f-rodape.
      
      FOR EACH tt-rel-erros BREAK BY tt-rel-erros.cdn_estab BY tt-rel-erros.cdn_funcionario:
          IF FIRST-OF(tt-rel-erros.cdn_estab) THEN
              DISP tt-rel-erros.cdn_estab WITH FRAME f-estab.
          DISP tt-rel-erros.cdn_funcionario  
               tt-rel-erros.nom_pessoa_fisic 
               tt-rel-erros.status_email     
               tt-rel-erros.email WITH FRAME f-status.
          DOWN WITH FRAME f-status.
          IF LAST-OF(tt-rel-erros.cdn_estab) THEN
              PAGE.
      END.
  
      view frame f-cabec.
      view frame f-rodape.
      {include/i-rpclo.i}

  END.
  ELSE
    {include/i-rpclo.i}

  run pi-finalizar in v_han_acomp.

return "ok".

