/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YG0001RP 12.1.13.003}  /*** 010030 ***/

{include/i_fnctrad.i}
/******************************************************************************
**
**   Programa: yg0001rp.p
**
**   Data....: 17 de Abril de 2017.
**
**   Autor...: Sergio Luiz Neto da Silveira - DSC PRAXIS
**
**   Objetivo: Listagem da Verba da Ordem Investimento - Detalhado.
**  
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-epc200.i in0518rp}

{include/i-rpvar.i}

def var h-acomp as handle no-undo.
def var c-titulo-i       as char format "x(20)"  no-undo.
def var c-titulo-p       as char format "x(20)"  no-undo.
def var c-destino        as char format "x(7)"   no-undo.             
def var c-titulo-s       as char format "x(20)"  no-undo.   
def var c-titulo-c       as char format "x(20)"  no-undo.   
def var c-titulo-par     as char format "x(20)"  no-undo.
def var l-imprime        as log  initial yes     no-undo.
def var c-geral          as char format "x(20)"  no-undo.
def var c-verba          as char format "x(20)"  no-undo.
def var c-situacao       as char format "x(11)"  no-undo.
def var c-situacao-1     as char format "x(11)"  no-undo.
def var c-situacao-2     as char format "x(11)"  no-undo.
def var c-exib-moeda     as char format "x(20)"  no-undo.
def var c-lb-exib-moeda  as char format "x(25)"  no-undo.
def var c-emp-sel        as char format "x(10)"  no-undo.
def var c-est-sel        as char format "x(10)"  no-undo.
def var c-proj-sel       as char format "x(10)"  no-undo.
def var c-ord-sel        as char format "x(10)"  no-undo.
def var c-imprimindo     as char format "x(13)"  no-undo. 
def var c-processando    as char format "x(13)"  no-undo. 
def var c-moeda-controle as char format "x(15)"  no-undo.
def var c-moedas-inv     as char format "x(15)"  no-undo extent 3.
def var c-lb-proj        as char format "x(5)"   no-undo.
def var i-cod-moeda-par  as integer              no-undo.
def var c-lb-dest        as char                 no-undo.
def var c-lb-usuar       as char                 no-undo.
def var c-lb-atualiza    as char                 no-undo.
def var c-atualiza       as char                 no-undo.
def var c-lb-moeda-controle as char format "x(10)" no-undo.
def var c-lb-situacao    as char format "x(10)"  no-undo.
def var l-imprimiu       as logical              no-undo.
def var c-lb-total       as char format "x(10)" no-undo.
def var c-lb-dt-trans    as char format "x(10)" no-undo.
def var c-lb-tipo-doc    as char format "x(10)" no-undo.
def var c-lb-serie       as char format "x(05)" no-undo.
def var c-lb-docto       as char format "x(25)" no-undo.
def var c-lb-ent-comp    as char format "x(14)" no-undo.    
def var c-lb-sai-comp    as char format "x(14)" no-undo.
def var c-lb-ent-real    as char format "x(14)" no-undo.
def var c-lb-sai-real    as char format "x(14)" no-undo.
def var c-lb-acum-comp   as char format "x(16)" no-undo. 
def var c-lb-acum-real   as char format "x(16)" no-undo. 
def var c-lb-solicitacao as char                no-undo.
def var c-lb-ordem       as char                no-undo.
def var c-lb-pedido      as char                no-undo.
def var c-lb-especie     as char                no-undo.     
def var c-lb-item        as char                no-undo.
def var c-lb-num-ord-magnus as char             no-undo.
def var c-lbl-ep-codigo     as char format "x(08)" no-undo.
def var c-lbl-estab-exec    as char format "x(11)" no-undo.
def var c-lbl-num-projeto   as char format "x(12)" no-undo.
def var c-lbl-num-ordem     as char format "x(13)" no-undo.
def var c-lbl-situacao      as char format "x(09)" no-undo.
def var c-lbl-moeda         as char format "x(15)" no-undo.
def var c-lbl-vl-verba      as char format "x(12)" no-undo.
def var c-lbl-vl-verba-orig as char format "x(17)" no-undo.
def var c-lbl-vl-compromis  as char format "x(16)" no-undo.
def var c-lbl-vl-realizado  as char format "x(16)" no-undo.
def var c-lbl-total         as char format "x(06)" no-undo.

{cdp/cd9731.i}  /* l-integra-cn-in */
{cdp/cd9731.i1} /* l-integra-cn-in-medicao */
&if '{&bf_mat_versao_ems}' >= '2.062' &THEN
  {cdp/cd9590.i} /* Unidade de Negocio */
&endif

def buffer b-controle-verba for controle-verba.
def var de-acum-comp     as dec                 no-undo.
def var de-acum-real     as dec                 no-undo.
def var de-acum-tot      as dec                 no-undo.
def var de-total         as dec                 no-undo.
DEF VAR c-txt-unid-negoc AS CHAR FORMAT 'x(28)' EXTENT 33 NO-UNDO.
DEF VAR i-cnt-unid-negoc AS INT                 NO-UNDO.
DEF VAR c-lbl-unid-negoc AS CHAR FORMAT '(15)'  NO-UNDO.

def var c-data-sel       as char format "x(10)"  no-undo.
DEF VAR v_cod_lista_label AS CHAR NO-UNDO.
DEF VAR v_cod_cgc         AS CHAR NO-UNDO.
DEF STREAM s-planilha.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classif-1        as integer
    field desc-classifica  as char format "x(40)"
    field i-ep-ini         as char 
    field i-ep-fim         as char
    field c-est-ini        as char
    field c-est-fim        as char
    field i-proj-ini       as int 
    field i-proj-fim       as int
    field i-ord-ini        as int
    field i-ord-fim        as int
    FIELD d-data-ini       AS DATE
    FIELD d-data-fim       AS DATE
    FIELD cod-divisao-ini        LIKE proj-inv.cod-divisao
    FIELD cod-divisao-fim        LIKE proj-inv.cod-divisao
    FIELD cod-especialidade-ini  LIKE sub-div-ordem.cod-especialidade
    FIELD cod-especialidade-fim  LIKE sub-div-ordem.cod-especialidade
    FIELD cod-sub-espec-ini      LIKE sub-div-ordem.cod-sub-espec
    FIELD cod-sub-espec-fim      LIKE sub-div-ordem.cod-sub-espec
    FIELD num-ord-magnus-ini     LIKE sub-div-ordem.num-ord-magnus
    FIELD num-ord-magnus-fim     LIKE sub-div-ordem.num-ord-magnus
    field i-moeda-par      as int
    field atualiza         as logical
    field log-gera-excel   as logical
    field caminho-excel    as char format "x(100)"
    field delimitador      as char format "x(1)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem. 

define temp-table tt-impressao
    field linha-1 as char format "x(100)"
    field linha-2 as char format "x(100)"
    field linha-3 as char format "x(100)"
    field linha-4 as char format "x(100)"
    field linha-5 as char format "x(100)"
    field linha-6 as char format "x(100)"
    field linha-7 as char format "x(100)"
    field linha-8 as char format "x(100)"
    field linha-9 as char format "x(100)"
    field colunas as char format "x(100)".

def temp-table tt-raw-digita                   
    field raw-digita as raw.
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

{cdp/cdcfgmat.i}

/* Definiªío das frames e traduªío*/
{esp/yg0001.i2}

define buffer B-T-CONTROLE for T-CONTROLE.

find first param-global no-lock no-error.

def var de-vficha1    as decimal no-undo. 
def var de-vficha2    as decimal no-undo. 
def var de-vficha3    as decimal no-undo.
def var de-vficha     LIKE ficha-liber.vl-verba[1] no-undo. 

def var de-vordem1    as decimal no-undo. 
def var de-vordem2    as decimal no-undo. 
def var de-vordem3    as decimal no-undo.
def var de-vordem     LIKE ficha-liber.vl-verba[1] no-undo.

DEFINE VARIABLE v_dat_transacao    LIKE movto_tit_ap.dat_transacao    NO-UNDO.
DEFINE VARIABLE v_cod_tit_ap       LIKE tit_ap.cod_tit_ap             NO-UNDO.
DEFINE VARIABLE v_val_movto_ap     LIKE movto_tit_ap.val_movto_ap    NO-UNDO.


DEFINE VARIABLE d-vl-imposto-ir                     LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-pis-cofins-csll        LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-pis                    LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-cofins                 LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-csll                   LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-iss                    LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-inss                   LIKE  dupli-imp.vl-imposto  NO-UNDO.  
DEFINE VARIABLE d-vl-imposto-contrato               LIKE  dupli-imp.vl-imposto  NO-UNDO. 

{utp/ut-liter.i Listagem_Verba_Ordem_de_Investimento_-_Detalhada * r }
assign c-titulo-relat = return-value.
{utp/ut-liter.i INVESTIMENTO  * r }
assign c-programa     = "YG0001" 
       c-versao       = "12.1.13"
       c-revisao      = "000"
       c-sistema      = return-value.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN  c-destino = RETURN-VALUE.
&else
    ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.
&endif

run utp/ut-acomp.p persistent set h-acomp.

if not tt-param.log-gera-excel then do:
    run INIC-FORMS.  /* gera cabecalho e rodape de 233 cols */
    {include/i-rpout.i}

    view frame f-cabec.
    view frame f-rodape.
end.
/* Monta valores na temp-table */
/*******************************************************************************
* ESIN0518.I1 - Busca Valores
********************************************************************************/
{cdp/cdcfgmat.i}

run pi-inicializar in h-acomp(input c-processando).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:

      if first-of (controle-verba.ep-codigo) then do:
         find param-inv 
              where param-inv.ep-codigo = controle-verba.ep-codigo no-lock no-error.     
         if tt-param.i-moeda-par = 1 then
            assign i-cod-moeda-par = param-inv.moeda-inv.
         else if tt-param.i-moeda-par = 2 then
            assign i-cod-moeda-par = param-inv.moeda1.
         else
            assign i-cod-moeda-par = param-inv.moeda2.
      end.   

      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).
      for each plano-aprov
         where plano-aprov.ep-codigo = controle-verba.ep-codigo
           and plano-aprov.cod-est-exec = controle-verba.cod-est-exec
           and plano-aprov.num-projeto = controle-verba.num-projeto
           and plano-aprov.num-ordem = controle-verba.num-ordem
           and plano-aprov.cod-sit-solic <> " " 
           and plano-aprov.cod-sit-plano <> "E"
           and plano-aprov.num-solicitacao > 0 no-lock:

          if plano-aprov.num-ord-comp <> 0 then do:
              find ord-ped where 
                   ord-ped.ep-codigo = plano-aprov.ep-codigo and
                   ord-ped.cod-estabel = plano-aprov.cod-estabel and
                   ord-ped.cod-area = 0 and
                   ord-ped.num-ord-comp = plano-aprov.num-ord-comp and
                   ord-ped.seq-comp = plano-aprov.seq-comp no-lock no-error.
              if avail ord-ped then do:
                 if ord-ped.cod-sit-comp = "E" or
                    ord-ped.cod-sit-ped  = "E" then
                    next.
              end.     
           end.

           create t-controle.
           assign t-controle.seq          = 10
                  t-controle.ep-codigo    = plano-aprov.ep-codigo
                  t-controle.cod-est-exec = plano-aprov.cod-est-exec
                  t-controle.num-projeto  = plano-aprov.num-projeto
                  t-controle.num-ordem    = plano-aprov.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.dt-trans     = plano-aprov.dt-emiss-solic
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.ent-comp     = dec(substring(plano-aprov.char-1,1,15)) +
                                            (dec(substring(plano-aprov.char-1,16,2)) / 100).


           RUN pi-especialidade (plano-aprov.ep-codigo,   
                                 plano-aprov.cod-est-exec,
                                 plano-aprov.num-projeto, 
                                 plano-aprov.num-ordem,   
                                 plano-aprov.num-ord-magnus).



                  run cdp/cd0812.p(input  0,
                                 input  i-cod-moeda-par,
                                 input  t-controle.ent-comp,
                                 input  t-controle.dt-trans,
                                 output t-controle.ent-comp).                          

                  if plano-aprov.tp-solic = "1" then
                     assign t-controle.tipo-doc = "Requisiá∆o".
                  else
                     assign t-controle.tipo-doc = "Solicit".
        {esp/yg0001.i3}
      end. 

      for each ord-ped
         where ord-ped.ep-codigo = controle-verba.ep-codigo
           and ord-ped.cod-est-exec = controle-verba.cod-est-exec
           and ord-ped.num-projeto = controle-verba.num-projeto
           and ord-ped.num-ordem = controle-verba.num-ordem
           and ord-ped.cod-sit-comp <> "E"
           and ord-ped.cod-sit-ped <> "E"
           no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = ord-ped.ep-codigo and
                plano-aprov.cod-area = ord-ped.cod-area and
                plano-aprov.num-ord-comp = ord-ped.num-ord-comp and
                plano-aprov.seq-comp = ord-ped.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           create t-controle.
           assign t-controle.ep-codigo    = ord-ped.ep-codigo
                  t-controle.cod-est-exec = ord-ped.cod-est-exec
                  t-controle.num-projeto  = ord-ped.num-projeto
                  t-controle.num-ordem    = ord-ped.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Ordem"
                  t-controle.dt-trans     = ord-ped.data-1
                  t-controle.num-ord-comp = ord-ped.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.ent-comp     = ord-ped.dec-1 * ord-ped.quant-comp.

           RUN pi-especialidade (plano-aprov.ep-codigo,   
                                 plano-aprov.cod-est-exec,
                                 plano-aprov.num-projeto, 
                                 plano-aprov.num-ordem,   
                                 plano-aprov.num-ord-magnus).

           &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
               IF ord-ped.nr-contrato <> 0 THEN DO:
                 if l-integra-cn-in or 
                    l-integra-cn-in-medicao then do:
    
                    assign T-CONTROLE.NUM-DOCTO = string(ord-ped.nr-contrato).
    
                    if l-integra-cn-in-medicao THEN DO:
                       IF  ord-ped.num-seq-item <> 0 THEN DO: 
    
                          assign T-CONTROLE.NUM-DOCTO = string(ord-ped.nr-contrato)  + "/" + 
                                                        string(ord-ped.num-seq-item) + "/" + 
                                                        string(ord-ped.num-seq-medicao) + "/" +
                                                        string(ord-ped.num-seq-event).
    
                       END.
                    END.
                 END.
               end.
           &else
                 IF SUBSTRING(ORD-PED.CHAR-2,1,9) <> "" THEN DO:
                   if l-integra-cn-in or 
                      l-integra-cn-in-medicao then do:
    
                      assign T-CONTROLE.NUM-DOCTO = substring(ord-ped.char-2,1,9).
    
                      if l-integra-cn-in-medicao THEN DO:
                         IF SUBSTRING(ord-ped.char-2,10,4) <> "" THEN DO: 
    
                            assign T-CONTROLE.NUM-DOCTO = trim(substring(ord-ped.char-2,1,9))  + "/" + 
                                                          trim(SUBSTRING(ord-ped.char-2,10,4)) + "/" + 
                                                          trim(SUBSTRING(ord-ped.char-2,18,4)) + "/" +
                                                          trim(substring(ord-ped.char-2,14,4)).
    
                         END.
                      END.
                   END.
                 end.
           &ENDIF
           if plano-aprov.num-solicitacao <> 0 then           
              assign t-controle.sai-comp = dec(substring(plano-aprov.char-1,1,15)) +
                                               (dec(substring(plano-aprov.char-1,16,2)) / 100).


           run cdp/cd0812.p(input  0,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-comp,
                          input  t-controle.dt-trans,
                          output t-controle.ent-comp).

           if plano-aprov.num-solicitacao <> 0 then                
              run cdp/cd0812.p(input  0,
                             input  i-cod-moeda-par,
                             input  t-controle.sai-comp,
                             input  plano-aprov.dt-emiss-solic,
                             output t-controle.sai-comp).

           if plano-aprov.num-solicitacao <> 0 then             
              assign t-controle.seq = 10.
           else
              assign t-controle.seq = 20.                                 

         {esp/yg0001.i3}
      end.       


      for each ord-ped
         where ord-ped.ep-codigo = controle-verba.ep-codigo
           and ord-ped.cod-est-exec = controle-verba.cod-est-exec
           and ord-ped.num-projeto = controle-verba.num-projeto
           and ord-ped.num-ordem = controle-verba.num-ordem
           and ord-ped.num-pedido <> 0 
           and (ord-ped.cod-sit-ped = "A"
                or ord-ped.cod-sit-ped = "F") 
           and ord-ped.cod-sit-comp <> "E"  no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = ord-ped.ep-codigo and
                plano-aprov.cod-area = ord-ped.cod-area and
                plano-aprov.num-ord-comp = ord-ped.num-ord-comp and
                plano-aprov.seq-comp = ord-ped.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           create t-controle.
           assign t-controle.ep-codigo    = ord-ped.ep-codigo
                  t-controle.cod-est-exec = ord-ped.cod-est-exec
                  t-controle.num-projeto  = ord-ped.num-projeto
                  t-controle.num-ordem    = ord-ped.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Pedido"
                  t-controle.dt-trans     = ord-ped.dt-pedido
                  t-controle.num-ord-comp = ord-ped.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.num-pedido   = ord-ped.num-pedido
                  t-controle.ent-comp     = ord-ped.vl-item[1]
                  t-controle.sai-comp     = ord-ped.dec-1 * ord-ped.quant-comp.

           RUN pi-especialidade (plano-aprov.ep-codigo,   
                                 plano-aprov.cod-est-exec,
                                 plano-aprov.num-projeto, 
                                 plano-aprov.num-ordem,   
                                 plano-aprov.num-ord-magnus).

           /*Log339640*/
           find first B-T-CONTROLE no-lock
                where B-t-controle.tipo-doc     = "ORDEM" 
                and   B-T-CONTROLE.NUM-ORD-COMP = T-CONTROLE.NUM-ORD-COMP no-error.

           if avail B-T-CONTROLE then do:
               run utp/ut-liter.p (input "Item_Contrato",
                                   input "",
                                   input "") no-error.

               if (can-find (first controle-inv-esp 
                            where controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
                            and   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
                            and   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
                            and   controle-inv-esp.tipo-doc     = "Contrato"
                            and   controle-inv-esp.NRO-DOCTO    = ENTRY(1, B-T-CONTROLE.NUM-DOCTO, "/") )
                   or
                   can-find (first controle-inv-esp 
                            where controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
                            and   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
                            and   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
                            and   controle-inv-esp.tipo-doc     = return-value
                            and   ENTRY(1, controle-inv-esp.NRO-DOCTO, "/") = ENTRY(1, B-T-CONTROLE.NUM-DOCTO, "/") 
                            AND   ENTRY(2, controle-inv-esp.NRO-DOCTO, "/") = ENTRY(2, B-T-CONTROLE.NUM-DOCTO, "/") ))
               then do:
                   assign t-controle.sai-comp = t-controle.ent-comp.
               end.
           end.
           /*Log339640*/
           
        run cdp/cd0812.p(input  param-inv.moeda-inv,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-comp,
                          input  ord-ped.dt-pedido,
                          output t-controle.ent-comp).

           run cdp/cd0812.p(input  0,
                          input  i-cod-moeda-par,
                          input  t-controle.sai-comp,
                          input  ord-ped.data-1,
                          output t-controle.sai-comp).

           if plano-aprov.num-solicitacao <> 0 then             
              assign t-controle.seq = 10.
           else
              assign t-controle.seq = 20.                                 

           {esp/yg0001.i3}
      end.       

      for each movto-nf
         where movto-nf.ep-codigo = controle-verba.ep-codigo
           and movto-nf.cod-est-exec = controle-verba.cod-est-exec
           and movto-nf.num-projeto = controle-verba.num-projeto
           and movto-nf.num-ordem = controle-verba.num-ordem
           no-lock:

           find plano-aprov where
                plano-aprov.ep-codigo = movto-nf.ep-codigo and
                plano-aprov.cod-area = movto-nf.cod-area and
                plano-aprov.num-ord-comp = movto-nf.num-ord-comp and
                plano-aprov.seq-comp = movto-nf.seq-comp no-lock no-error.
           if not avail plano-aprov then next.     

           find ord-ped where 
                   ord-ped.ep-codigo = plano-aprov.ep-codigo and
                   ord-ped.cod-estabel = plano-aprov.cod-estabel and
                   ord-ped.cod-area = 0 and
                   ord-ped.num-ord-comp = plano-aprov.num-ord-comp and
                   ord-ped.seq-comp = plano-aprov.seq-comp no-lock no-error.
           if not avail ord-ped then next.  

           create t-controle.
           assign t-controle.ep-codigo    = movto-nf.ep-codigo
                  t-controle.cod-est-exec = movto-nf.cod-est-exec
                  t-controle.num-projeto  = movto-nf.num-projeto
                  t-controle.num-ordem    = movto-nf.num-ordem
                  t-controle.it-codigo    = plano-aprov.it-codigo
                  t-controle.num-ord-magnus = plano-aprov.num-ord-magnus
                  t-controle.tipo-doc     = "Nota Fiscal"
                  t-controle.dt-trans     = movto-nf.dt-trans
                  t-controle.num-ord-comp = movto-nf.num-ord-comp
                  t-controle.seq-comp     = ord-ped.seq-comp
                  t-controle.solicitacao  = plano-aprov.num-solicitacao
                  t-controle.num-pedido   = movto-nf.num-pedido
                  t-controle.num-docto    = movto-nf.serie-docto + "/" +
                                            substring(movto-nf.nro-docto,1,10) + "/" +
                                            movto-nf.nat-operacao + "/" +
                                            string(movto-nf.sequencia,">>>>9")
                  t-controle.ent-real     = movto-nf.vl-nota[1].

           RUN pi-especialidade (movto-nf.ep-codigo,
                                 movto-nf.cod-est-exec, 
                                 movto-nf.num-projeto,  
                                 movto-nf.num-ordem,    
                                 plano-aprov.num-ord-magnus).

            ASSIGN t-controle.r-rowid = ROWID(movto-nf).

            run cdp/cd0812.p (input  param-inv.moeda-inv,
                              input  i-cod-moeda-par,
                              input  t-controle.ent-real,
                              input  movto-nf.dt-trans,
                              output t-controle.ent-real).  

           if ord-ped.cod-sit-comp <> "E" and ord-ped.cod-sit-ped <> "E" then do:                  
                assign t-controle.sai-comp = movto-nf.quant-doc-ent * 
                                             (ord-ped.vl-item[1] / ord-ped.quant-comp).
                                             
                &if '{&BF_MAT_VERSAO_EMS}' = '2.04':U &then
                     if  can-find (funcao where funcao.cd-funcao = "spp-atu-comp-receb" 
                                            and funcao.ativo) THEN DO:
                     
                         assign t-controle.sai-comp = dec(substr(movto-nf.char-1,39,18)).
                     END.
                &endif
                
                
                &if '{&BF_MAT_VERSAO_EMS}' > '2.04':U and '{&BF_MAT_VERSAO_EMS}' <= '2.062':U &then
                     assign t-controle.sai-comp = dec(substr(movto-nf.char-1,39,18)).
                &endif
                 
                
                &if '{&BF_MAT_VERSAO_EMS}' > '2.062':U &then
                     assign t-controle.sai-comp = movto-nf.val-cpromis.
                &endif

             IF ord-ped.data-2 <> ? THEN
                 RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                                   INPUT  i-cod-moeda-par,
                                   INPUT  t-controle.sai-comp,
                                   INPUT  ord-ped.data-2,
                                   OUTPUT t-controle.sai-comp).
             ELSE                                              
                 RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                                   INPUT  i-cod-moeda-par,
                                   INPUT  t-controle.sai-comp,
                                   INPUT  ord-ped.data-1,
                                   OUTPUT t-controle.sai-comp).
           end.

           /* Devoluªío ao Fornecedor: Gera entrada no compromissado e sa≠da no realizado */
           &IF "{&bf_mat_versao_EMS}" >= "2.04" &THEN
               if  movto-nf.tipo-trans = 2 then
                   assign t-controle.sai-real = t-controle.ent-real
                          t-controle.ent-real = 0
                          t-controle.ent-comp = t-controle.sai-comp
                          t-controle.sai-comp = 0.
           &endif.

           if plano-aprov.num-solicitacao <> 0 then
               assign t-controle.seq = 10.
           else
               assign t-controle.seq = 20.

         {esp/yg0001.i3}
      end.                                          


        /** CHAMADA UPC **/   

        for each tt-epc:
            delete tt-epc.
        end.    

        create tt-epc.
        assign tt-epc.cod-event     = "apos-leitura-movto-nf":U
               tt-epc.cod-parameter = "controle-verba-rowid"
               tt-epc.val-parameter = string(rowid(controle-verba)). 

        {include/i-epc201.i "apos-leitura-movto-nf"}        

        if return-value = 'NOK' then do:
            undo,leave.
        end.

        /********************/      

     for each movto-apr
         where movto-apr.ep-codigo = controle-verba.ep-codigo
           and movto-apr.cod-est-exec = controle-verba.cod-est-exec
           and movto-apr.num-projeto = controle-verba.num-projeto
           and movto-apr.num-ordem = controle-verba.num-ordem
           no-lock:      
           if movto-apr.transacao = "IMD" and
              movto-apr.num-pedido <> 0 then next.             

           create t-controle.
           assign t-controle.ep-codigo    = movto-apr.ep-codigo
                  t-controle.cod-est-exec = movto-apr.cod-est-exec
                  t-controle.num-projeto  = movto-apr.num-projeto
                  t-controle.num-ordem    = movto-apr.num-ordem
                  t-controle.it-codigo    = movto-apr.it-codigo
                  t-controle.num-ord-magnus = movto-apr.num-ord-magnus
                  t-controle.dt-trans     = movto-apr.dt-trans 
                  t-controle.esp-docto    = movto-apr.esp-docto
                  t-controle.num-docto    = movto-apr.nro-docto
                  t-controle.ent-real     = movto-apr.vl-mat[1] + 
                                            movto-apr.vl-mob[1].


           RUN pi-especialidade (movto-apr.ep-codigo,   
                                 movto-apr.cod-est-exec,
                                 movto-apr.num-projeto, 
                                 movto-apr.num-ordem, 
                                 movto-apr.num-ord-magnus).


           if movto-apr.transacao = "IMD" or 
              movto-apr.transacao = "PEF" then
              assign t-controle.num-docto = movto-apr.serie-docto    + "/" + 
                                            movto-apr.nro-docto      + "/" + 
                                            movto-apr.parcela        + "/" +
                                            trim(string(movto-apr.cod-emitente,">>>>>>>>9")). 



           if movto-apr.tipo-trans = 2 then
              assign t-controle.ent-real = t-controle.ent-real * -1.    

           run cdp/cd0812.p(input  param-inv.moeda-inv,
                          input  i-cod-moeda-par,
                          input  t-controle.ent-real,
                          input  movto-apr.dt-trans,
                          output t-controle.ent-real).    

           if movto-apr.transacao = "REQ" then do:                                 
                if movto-apr.tipo-trans = 1 then                              
                   assign t-controle.tipo-doc = "Atend. REQ".
                else
                   assign t-controle.tipo-doc = "Devol. REQ".
           end.
           else do:
               assign t-controle.tipo-doc = movto-apr.transacao.
           end.        

           if movto-apr.transacao = "REQ" or movto-apr.transacao = "ACE" then do:
              assign t-controle.seq = 10.
              find plano-aprov where
                   plano-aprov.ep-codigo = movto-apr.ep-codigo and
                   plano-aprov.cod-estabel = movto-apr.cod-estabel and
                   plano-aprov.num-solicitacao = int(movto-apr.nro-docto) and
                   plano-aprov.seq-solic = int(movto-apr.parcela) and
                   plano-aprov.tp-solic = "1" 
                   no-lock no-error.
              if avail plano-aprov then     
                  assign t-controle.solicitacao = plano-aprov.num-solicitacao.   

              if movto-apr.log-1 then do: /* Executou o in2301 */                    
                if avail plano-aprov then do:                     
                     assign t-controle.sai-comp = dec(substring(plano-aprov.char-1,1,15)) +
                                                   (dec(substring(plano-aprov.char-1,16,2)) / 100).
                     assign t-controle.sai-comp = t-controle.sai-comp / plano-aprov.quant-solic.
                     assign t-controle.sai-comp = t-controle.sai-comp * movto-apr.quant-mov.                                 
                            t-controle.solicitacao  = plano-aprov.num-solicitacao.                        
                     run cdp/cd0812.p(input  0,
                                    input  i-cod-moeda-par,
                                    input  t-controle.sai-comp,
                                    input  plano-aprov.dt-emiss-solic,
                                    output t-controle.sai-comp).                  
                end.                
              end.

              if movto-apr.tipo-trans = 2 then do:                 
                 assign t-controle.ent-real = t-controle.ent-real * -1
                        t-controle.ent-comp = t-controle.sai-comp
                        t-controle.sai-comp = 0
                        t-controle.sai-real = t-controle.ent-real
                        t-controle.ent-real = 0.                          
              end.                             
           end.              
           else if movto-apr.transacao = "DIV" or movto-apr.transacao = "ACE" then 
              assign t-controle.seq = 40.
           else
              assign t-controle.seq = 50.   

        {esp/yg0001.i3}
      end.           


      for each ordem-man
         where ordem-man.ep-codigo = controle-verba.ep-codigo
           and ordem-man.cod-est-exec = controle-verba.cod-est-exec
           and ordem-man.num-projeto = controle-verba.num-projeto
           and ordem-man.num-ordem = controle-verba.num-ordem
           no-lock:                     

           create t-controle.
           assign t-controle.ep-codigo    = ordem-man.ep-codigo
                  t-controle.cod-est-exec = ordem-man.cod-est-exec
                  t-controle.num-projeto  = ordem-man.num-projeto
                  t-controle.num-ordem    = ordem-man.num-ordem
                  t-controle.it-codigo    = ordem-man.char-2
                  t-controle.num-ord-magnus = ordem-man.num-ord-magnus                  
                  t-controle.dt-trans     = ordem-man.dt-cadastro
                  t-controle.num-ord-comp = 0
                  t-controle.solicitacao  = 0
                  t-controle.num-pedido   = 0
                  t-controle.num-docto    = string(ordem-man.num-ord-man)
                  t-controle.ent-real     = ordem-man.vl-material[1] + ordem-man.vl-servico[1] + 
                                            ordem-man.vl-mob[1] + decimal(substring(ordem-man.char-1,1,18)).

           RUN pi-especialidade (ordem-man.ep-codigo,   
                                 ordem-man.cod-est-exec,
                                 ordem-man.num-projeto, 
                                 ordem-man.num-ordem,   
                                 ordem-man.num-ord-magnus).

           if ordem-man.log-2 then
              assign t-controle.tipo-doc = "Ord Manut".
           else   
              assign t-controle.tipo-doc = "Ord Prod".

            run cdp/cd0812.p (input  param-inv.moeda-inv,
                              input  i-cod-moeda-par,
                              input  t-controle.ent-real,
                              input  ordem-man.dt-cadastro,
                              output t-controle.ent-real).  



            assign t-controle.seq = 50.

        {esp/yg0001.i3}
      end.              

    {utp/ut-liter.i Apontamento_MOB}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo      = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec   = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto    = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = 0
               t-controle.sai-comp       = 0
               t-controle.ent-real       = controle-inv-esp.valor-origem
               t-controle.sai-real       = 0
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

           RUN pi-especialidade (controle-verba.ep-codigo ,  
                                 controle-verba.cod-est-exec,
                                 controle-verba.num-projeto, 
                                 controle-verba.num-ordem,   
                                 controle-inv-esp.num-ord-inv).

          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 60.

        {esp/yg0001.i3}
    END.

    {utp/ut-liter.i Despesa_de_Importaªío}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

        RUN pi-especialidade (controle-verba.ep-codigo ,  
                              controle-verba.cod-est-exec,
                              controle-verba.num-projeto, 
                              controle-verba.num-ordem,   
                              controle-inv-esp.num-ord-inv).
                           
          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        run cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 70.

        {esp/yg0001.i3}
    END.
    
    {utp/ut-liter.i Contrato}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

        RUN pi-especialidade (controle-verba.ep-codigo ,  
                              controle-verba.cod-est-exec,
                              controle-verba.num-projeto, 
                              controle-verba.num-ordem,   
                              controle-inv-esp.num-ord-inv).
               
          IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").

        RUN cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 80.

        {esp/yg0001.i3}
    END.
    
    {utp/ut-liter.i Item_Contrato}
    FOR EACH  controle-inv-esp NO-LOCK
        WHERE controle-inv-esp.ep-codigo    = controle-verba.ep-codigo   
        AND   controle-inv-esp.cod-est-exec = controle-verba.cod-est-exec
        AND   controle-inv-esp.num-projeto  = controle-verba.num-projeto 
          and   controle-inv-esp.num-ordem    = controle-verba.num-ordem
          and   controle-inv-esp.tipo-doc     = return-value:

        CREATE t-controle.
        ASSIGN t-controle.dt-trans       = controle-inv-esp.dt-trans
               t-controle.it-codigo      = STRING(controle-inv-esp.item)
               t-controle.num-ord-magnus = controle-inv-esp.num-ord-inv
               t-controle.tipo-doc       = controle-inv-esp.tipo-doc
               t-controle.solicitacao    = 0
               t-controle.num-ord-comp   = controle-inv-esp.numero-ordem
               t-controle.num-pedido     = controle-inv-esp.num-pedido
               t-controle.ent-comp       = controle-inv-esp.ent-comp
               t-controle.sai-comp       = controle-inv-esp.sai-comp
               t-controle.ent-real       = controle-inv-esp.ent-real
               t-controle.sai-real       = controle-inv-esp.sai-real
               t-controle.ep-codigo      = controle-inv-esp.ep-codigo
               t-controle.cod-est-exec   = controle-inv-esp.cod-est-exec
               t-controle.num-projeto    = controle-inv-esp.num-projeto
               t-controle.num-ordem      = controle-inv-esp.num-ordem.

       RUN pi-especialidade (controle-verba.ep-codigo ,  
                             controle-verba.cod-est-exec,
                             controle-verba.num-projeto, 
                             controle-verba.num-ordem,   
                             controle-inv-esp.num-ord-inv).

            IF  controle-inv-esp.nro-docto <> "" THEN
              ASSIGN t-controle.num-docto = controle-inv-esp.serie-docto  + "/" +
                                            controle-inv-esp.nro-docto    + "/" +
                                            controle-inv-esp.nat-operacao + "/" +
                                     STRING(controle-inv-esp.sequencia,">>>>9").
            
        run cdp/cd0812.p (INPUT  param-inv.moeda-inv,
                          INPUT  i-cod-moeda-par,
                          INPUT  t-controle.ent-real,
                          INPUT  t-controle.dt-trans,
                          OUTPUT t-controle.ent-real).  

        ASSIGN t-controle.seq = 80.

        {esp/yg0001.i3}
    END.
end.

RUN pi-principal.
if not tt-param.log-gera-excel then do:
    RUN pi-parametros.

    {include/i-rpclo.i}
 
/**********************************************************************
*  in0518.i5
*  Cabeªalho
***********************************************************************/

procedure INIC-FORMS.

  /****************************************************************************
   **  Identico: I-RPCAB.I - Form do Cabeªalho Padrío e Rodap≤ (ex-CD9500.F)
   **              {&STREAM} - indica o nome da stream (opcional)
   ****************************************************************************/

  /* Rodape */
  c-rodape = "DATASUL - " + c-sistema + " - " + c-prg-obj + " - V:" + c-prg-vrs.
  c-rodape = fill("-", 233 - length(c-rodape)) + c-rodape.
  form header
       c-rodape format 'x(233)'
       with stream-io width 233 no-labels no-box page-bottom frame f-rodape.

  /* Cabecalho */
  &IF "{&LANGUAGE-CODE}" = "ING" &THEN 
     &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number  at 229 format ">>>>9" skip
            "Period:" i-numper-x at 09 "-"
            da-iniper-x at 14 "to" da-fimper-x
            fill("-", 175) format "x(173)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number({&STREAM})  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Page:" at 224 page-number({&STREAM})  at 229 format ">>>>9" skip
            "Period:" i-numper-x at 10 "-"
            da-iniper-x at 14 "to" da-fimper-x
            fill("-", 175) format "x(173)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF

  &ELSEIF "{&LANGUAGE-CODE}" = "ESP" &THEN
     &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Pˇgina:" at 222 page-number  at 227 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Pˇgina:" at 222 page-number  at 227 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta" da-fimper-x
            fill("-", 167) format "x(165)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Pˇgina:" at 222 page-number({&STREAM})  at 227 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Pˇgina:" at 222 page-number({&STREAM})  at 227 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta" da-fimper-x
            fill("-", 167) format "x(165)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF

  &ELSE
     &IF "{&STREAM}" = "" &THEN
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:"  at 223 page-number  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number  at 229 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "a" da-fimper-x
            fill("-", 163) format "x(161)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ELSE
        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number({&STREAM})  at 229 format ">>>>9" skip
            fill("-", 213) format "x(211)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabec.

        form header
            fill("-", 233) format "x(233)" skip
            c-empresa c-titulo-relat at 50
            "Folha:" at 223 page-number({&STREAM})  at 229 format ">>>>9" skip
            "Periodo:" i-numper-x at 10 "-"
            da-iniper-x at 15 "a" da-fimper-x
            fill("-", 163) format "x(161)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS") skip(1)
            with stream-io width 233 no-labels no-box page-top frame f-cabper.
     &ENDIF
  &ENDIF

end procedure.

/* Fim esin0518.i5 *************************/
 
end.
ELSE DO:
    OUTPUT TO VALUE(SESSION:TEMP-DIR + "yg0001.tmp").

    IF i-num-ped-exec-rpw = 0 THEN
       os-command VALUE('start excel' + " " + CHR(34) + tt-param.caminho-excel + CHR(34)).

    put unformatted "Geraªío de Arquivo em Planilha do EXCEL!"               skip
                    "O arquivo gerado foi salvo em: " tt-param.caminho-excel skip.
    OUTPUT CLOSE.
END.

run pi-finalizar in h-acomp.
return 'ok'.

PROCEDURE pi-principal:

    if  c-nom-prog-dpc-mg97  <> ""
    or  c-nom-prog-appc-mg97 <> ""
    or  c-nom-prog-upc-mg97  <> "" then do:
        /* Chamada EPC */
        for each tt-epc
            where tt-epc.cod-event = "modify-report":U:
            delete tt-epc.
        end.
        
        /* Include i-epc200.i2: Criaá∆o de registro para Temp-Table tt-epc */
        {include/i-epc200.i2 &CodEvent='"modify-report"'
                             &CodParameter='"handle-tt-param"'
                             &ValueParameter="string(temp-table tt-param:handle)"}
        
        {include/i-epc200.i2 &CodEvent='"modify-report"'
                             &CodParameter='"handle-t-controle"'
                             &ValueParameter="string(temp-table t-controle:handle)"}
        
        /* Include i-epc201.i: Chamada do programa de EPC */
        {include/i-epc201.i "modify-report"}
          
        if return-value = "NOK":U then
            undo, leave.
    end.

    /* GIL - Elimina as informaªÑes que estío fora da faixa de datas */
  /*  FOR EACH t-controle
       WHERE t-controle.dt-trans < tt-param.d-data-ini
          OR t-controle.dt-trans > tt-param.d-data-fim:
        DELETE t-controle.
    END.*/

    if not tt-param.log-gera-excel then do:
        if tt-param.classif-1 = 1 THEN DO:
           /*******************************************************************************
* ESIN0518.I - Impressío
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:   
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {esp/yg0001.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.it-codigo by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {esp/yg0001.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {esp/yg0001.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        END.
        else if tt-param.classif-1 = 2 THEN DO:
           /*******************************************************************************
* ESIN0518.I - Impressío
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:   
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {esp/yg0001.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.dt-trans by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {esp/yg0001.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {esp/yg0001.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        end.
        else do:
           /*******************************************************************************
* ESIN0518.I - Impressío
********************************************************************************/
run pi-inicializar in h-acomp(input c-imprimindo).
for each controle-verba
    where controle-verba.ep-codigo    >= tt-param.i-ep-ini
      and controle-verba.ep-codigo    <= tt-param.i-ep-fim
      and controle-verba.cod-est-exec >= tt-param.c-est-ini
      and controle-verba.cod-est-exec <= tt-param.c-est-fim      
      and controle-verba.num-projeto  >= tt-param.i-proj-ini
      and controle-verba.num-projeto  <= tt-param.i-proj-fim
      and controle-verba.num-ordem    >= tt-param.i-ord-ini
      and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
      break by controle-verba.ep-codigo
            by controle-verba.cod-est-exec
            by controle-verba.num-projeto
            by controle-verba.num-ordem:   
            
     
      run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                         string(controle-verba.num-projeto) + "  " + 
                                         c-lb-ordem + 
                                         string(controle-verba.num-ordem))).     
      if first-of (controle-verba.ep-codigo) or
         not avail empresa then do:
         find empresa
              where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
         find param-inv 
              where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
         run inp/in9997.p (input 1).
         assign c-moedas-inv [1] = Trim(return-value).
         run inp/in9997.p (input 2).
         assign c-moedas-inv [2] = Trim(return-value).
         run inp/in9997.p (input 3).
         assign c-moedas-inv [3] = Trim(return-value). 
      end.
      if first-of (controle-verba.cod-est-exec) or
         not avail estabelec then    
         find estabelec
              where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
      if first-of (controle-verba.num-projeto) or 
         not avail proj-inv then         
         find proj-inv
             where proj-inv.ep-codigo = controle-verba.ep-codigo
               and proj-inv.cod-est-exec = controle-verba.cod-est-exec
               and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
      if first-of (controle-verba.num-ordem) or
         not avail ordem-inv then do:
         find ordem-inv
              where ordem-inv.ep-codigo = controle-verba.ep-codigo
                and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                and ordem-inv.num-projeto = controle-verba.num-projeto
                and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
         
         &if '{&bf_mat_versao_ems}' >= '2.062' &then
         ASSIGN i-cnt-unid-negoc = 1.
          DO  WHILE i-cnt-unid-negoc < 34:
              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
          END.
          
          FOR EACH mat-rat-med-inv NO-LOCK
              WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                AND mat-rat-med-inv.numero-ordem = 0
                AND mat-rat-med-inv.nr-contrato  = 0:
              IF  c-txt-unid-negoc[1] = "" THEN DO:
                  {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                         i-cnt-unid-negoc = 1.
              END.
              
              ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
              IF  i-cnt-unid-negoc < 34 THEN DO:
                 {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                 &if '{&bf_mat_versao_ems}' = '2.062' &then
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                 &else
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                 &endif
                  ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
              END.
          END.
         &endif
      end.
     
      if controle-verba.situacao = 1 then 
         assign c-situacao = c-situacao-1.
      else 
         assign c-situacao = c-situacao-2.      

      assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
 
      assign de-acum-comp = 0
             de-acum-real = 0.

      if tt-param.atualiza then do:
          for each t-controle
             where t-controle.ep-codigo    = controle-verba.ep-codigo
               and t-controle.cod-est-exec = controle-verba.cod-est-exec
               and t-controle.num-projeto  = controle-verba.num-projeto
               and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
               assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                      de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
          end.

          if de-acum-comp  < 0 then assign de-acum-comp = 0.
          if de-acum-real < 0 then assign de-acum-real = 0.

          find first b-controle-verba exclusive-lock
              where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
          if avail b-controle-verba then
            assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                   b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
      end.

      {esp/yg0001.i4}
           
      assign de-acum-comp = 0
             de-acum-real = 0
             l-imprimiu   = no.
                   
      for each t-controle
         where t-controle.ep-codigo    = controle-verba.ep-codigo
           and t-controle.cod-est-exec = controle-verba.cod-est-exec
           and t-controle.num-projeto  = controle-verba.num-projeto
           and t-controle.num-ordem    = controle-verba.num-ordem
           no-lock break by t-controle.tipo-doc by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                  t-controle.acum-comp = de-acum-comp
                  t-controle.acum-real = de-acum-real
                  l-imprimiu           = yes.
           if line-counter < 4 then do:
              {esp/yg0001.i4}
           end.
       
           IF t-controle.dt-trans > d-data-fim OR
              t-controle.dt-trans < d-data-ini THEN NEXT.
             
           disp t-controle.dt-trans   
                t-controle.it-codigo
                t-controle.num-ord-magnus
                t-controle.tipo-doc
                t-controle.solicitacao      
                t-controle.num-ord-comp
                t-controle.num-pedido
                t-controle.esp-docto
                t-controle.num-docto
                t-controle.ent-comp   
                t-controle.sai-comp   
                t-controle.ent-real   
                t-controle.sai-real 
                t-controle.acum-comp  
                t-controle.acum-real
                with frame f-controle.
                down with frame f-controle.


            /** CHAMADA UPC **/   
    
            for each tt-epc:
                delete tt-epc.
            end.    
             
            create tt-epc.
            assign tt-epc.cod-event     = "apos-display-frame-controle":U
                   tt-epc.cod-parameter = "controle-verba-rowid"
                   tt-epc.val-parameter = string(rowid(controle-verba)). 
                  
            {include/i-epc201.i "apos-display-frame-controle"}        
          
            if return-value = 'NOK' then do:
                undo,leave.
            end.
        
            /********************/    

      end.   
      if l-imprimiu = yes then do:
         if line-counter + 4 > page-size then do:
            page.
            {esp/yg0001.i4}
         end.
         if de-acum-comp  < 0 then assign de-acum-comp = 0.
         if de-acum-real < 0 then assign de-acum-real = 0.
         assign de-acum-tot = de-acum-comp + de-acum-real.
         disp c-lb-total
              de-acum-tot
              with frame f-total. 
      end.        
      page. 
end.
 
        
        end.
    end.
    else do:
        if tt-param.classif-1 = 1 THEN DO:
           /*******************************************************************************
            * ESIN0518.I - Impressío
            ********************************************************************************/
            
            OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.
            
            RUN pi-imprime-cabecalho-planilha.
            
            run pi-inicializar in h-acomp(input c-imprimindo).
            for each controle-verba
                where controle-verba.ep-codigo    >= tt-param.i-ep-ini
                  and controle-verba.ep-codigo    <= tt-param.i-ep-fim
                  and controle-verba.cod-est-exec >= tt-param.c-est-ini
                  and controle-verba.cod-est-exec <= tt-param.c-est-fim      
                  and controle-verba.num-projeto  >= tt-param.i-proj-ini
                  and controle-verba.num-projeto  <= tt-param.i-proj-fim
                  and controle-verba.num-ordem    >= tt-param.i-ord-ini
                  and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
                  break by controle-verba.ep-codigo
                        by controle-verba.cod-est-exec
                        by controle-verba.num-projeto
                        by controle-verba.num-ordem:   
                        
                 
                  run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                                     string(controle-verba.num-projeto) + "  " + 
                                                     c-lb-ordem + 
                                                     string(controle-verba.num-ordem))).     
                  if first-of (controle-verba.ep-codigo) or
                     not avail empresa then do:
                     find empresa
                          where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
                     find param-inv 
                          where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
                     run inp/in9997.p (input 1).
                     assign c-moedas-inv [1] = Trim(return-value).
                     run inp/in9997.p (input 2).
                     assign c-moedas-inv [2] = Trim(return-value).
                     run inp/in9997.p (input 3).
                     assign c-moedas-inv [3] = Trim(return-value). 
                  end.
                  if first-of (controle-verba.cod-est-exec) or
                     not avail estabelec then    
                     find estabelec
                          where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
                  if first-of (controle-verba.num-projeto) or 
                     not avail proj-inv then         
                     find proj-inv
                         where proj-inv.ep-codigo = controle-verba.ep-codigo
                           and proj-inv.cod-est-exec = controle-verba.cod-est-exec
                           and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
                  if first-of (controle-verba.num-ordem) or
                     not avail ordem-inv then do:
                     find ordem-inv
                          where ordem-inv.ep-codigo = controle-verba.ep-codigo
                            and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                            and ordem-inv.num-projeto = controle-verba.num-projeto
                            and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
                     
                     &if '{&bf_mat_versao_ems}' >= '2.062' &then
                     ASSIGN i-cnt-unid-negoc = 1.
                      DO  WHILE i-cnt-unid-negoc < 34:
                          ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
                          ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                      END.
                      
                      FOR EACH mat-rat-med-inv NO-LOCK
                          WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                            AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                            AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                            AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                            AND mat-rat-med-inv.numero-ordem = 0
                            AND mat-rat-med-inv.nr-contrato  = 0:
                          IF  c-txt-unid-negoc[1] = "" THEN DO:
                              {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                              ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                                     i-cnt-unid-negoc = 1.
                          END.
                          
                          ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                          IF  i-cnt-unid-negoc < 34 THEN DO:
                             {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                             &if '{&bf_mat_versao_ems}' = '2.062' &then
                              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                             &else
                              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                             &endif
                              ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
                          END.
                      END.
                     &endif
                  end.
                 
                  if controle-verba.situacao = 1 then 
                     assign c-situacao = c-situacao-1.
                  else 
                     assign c-situacao = c-situacao-2.      
            
                  assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
             
                  assign de-acum-comp = 0
                         de-acum-real = 0.
            
                  if tt-param.atualiza then do:
                      for each t-controle
                         where t-controle.ep-codigo    = controle-verba.ep-codigo
                           and t-controle.cod-est-exec = controle-verba.cod-est-exec
                           and t-controle.num-projeto  = controle-verba.num-projeto
                           and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
                           assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                                  de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
                      end.
            
                      if de-acum-comp  < 0 then assign de-acum-comp = 0.
                      if de-acum-real < 0 then assign de-acum-real = 0.
            
                      find first b-controle-verba exclusive-lock
                          where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                            and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                            and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                            and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
                      if avail b-controle-verba then
                        assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                               b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
                  end.
            
                  for each tt-impressao:
                      delete tt-impressao.
                  end.
                  /*******************************************************************************
            * ESIN0518-EXCEL.I4 - Impressío do cabeªalho no EXCEL
            ********************************************************************************/
            assign de-total = controle-verba.vl-comp[i-moeda-par] +
                              controle-verba.vl-real[i-moeda-par].
            
            
                if l-unidade-negocio and l-mat-unid-negoc then do:
                    create tt-impressao.
                    ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                                + tt-param.delimitador + string(controle-verba.ep-codigo)
                                                + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[01]
                           tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                                + tt-param.delimitador + controle-verba.cod-est-exec
                                                + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[02]
                                                + tt-param.delimitador + c-txt-unid-negoc[10]
                                                + tt-param.delimitador + c-txt-unid-negoc[18]
                                                + tt-param.delimitador + c-txt-unid-negoc[26]
                           tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                                + tt-param.delimitador + string(controle-verba.num-projeto)
                                                + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[03]
                                                + tt-param.delimitador + c-txt-unid-negoc[11]
                                                + tt-param.delimitador + c-txt-unid-negoc[19]
                                                + tt-param.delimitador + c-txt-unid-negoc[27]
                           tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                                + tt-param.delimitador + string(controle-verba.num-ordem)
                                                + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                                + tt-param.delimitador + c-txt-unid-negoc[04]
                                                + tt-param.delimitador + c-txt-unid-negoc[12]
                                                + tt-param.delimitador + c-txt-unid-negoc[20]
                                                + tt-param.delimitador + c-txt-unid-negoc[28]
                           tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                                + tt-param.delimitador + c-situacao
                                                + tt-param.delimitador
                                                + tt-param.delimitador + c-lbl-moeda
                                                + tt-param.delimitador + c-moeda-controle
                                                + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[05]
                                                + tt-param.delimitador + c-txt-unid-negoc[13]
                                                + tt-param.delimitador + c-txt-unid-negoc[21]
                                                + tt-param.delimitador + c-txt-unid-negoc[29]
                           tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                                + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                                + tt-param.delimitador + c-lbl-vl-verba-orig
                                                + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                                + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[06]
                                                + tt-param.delimitador + c-txt-unid-negoc[14]
                                                + tt-param.delimitador + c-txt-unid-negoc[22]
                                                + tt-param.delimitador + c-txt-unid-negoc[30]
                           tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                                + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[07]
                                                + tt-param.delimitador + c-txt-unid-negoc[15]
                                                + tt-param.delimitador + c-txt-unid-negoc[23]
                                                + tt-param.delimitador + c-txt-unid-negoc[31]
                           tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                                + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[08]
                                                + tt-param.delimitador + c-txt-unid-negoc[16]
                                                + tt-param.delimitador + c-txt-unid-negoc[24]
                                                + tt-param.delimitador + c-txt-unid-negoc[32]
                           tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                                + tt-param.delimitador + string(de-total)
                                                + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                                + tt-param.delimitador + c-txt-unid-negoc[09]
                                                + tt-param.delimitador + c-txt-unid-negoc[17]
                                                + tt-param.delimitador + c-txt-unid-negoc[25]
                                                + tt-param.delimitador + c-txt-unid-negoc[33]
                           tt-impressao.colunas = c-lb-dt-trans
                                                + tt-param.delimitador + c-lb-item
                                                + tt-param.delimitador + c-lb-num-ord-magnus
                                                + tt-param.delimitador + c-lb-tipo-doc
                                                + tt-param.delimitador + c-lb-solicitacao
                                                + tt-param.delimitador + c-lb-ordem
                                                + tt-param.delimitador + c-lb-pedido
                                                + tt-param.delimitador + c-lb-especie
                                                + tt-param.delimitador + c-lb-docto
                                                + tt-param.delimitador + c-lb-ent-comp
                                                + tt-param.delimitador + c-lb-sai-comp
                                                + tt-param.delimitador + c-lb-ent-real
                                                + tt-param.delimitador + c-lb-sai-real
                                                + tt-param.delimitador + c-lb-acum-comp
                                                + tt-param.delimitador + c-lb-acum-real.
                end.
                else do:
                    create tt-impressao.
                    ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                                + tt-param.delimitador + string(controle-verba.ep-codigo)
                                                + tt-param.delimitador + if avail empresa then empresa.nome else ""
                           tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                                + tt-param.delimitador + controle-verba.cod-est-exec
                                                + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                           tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                                + tt-param.delimitador + string(controle-verba.num-projeto)
                                                + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                           tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                                + tt-param.delimitador + string(controle-verba.num-ordem)
                                                + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                           tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                                + tt-param.delimitador + c-situacao
                                                + tt-param.delimitador 
                                                + tt-param.delimitador + c-lbl-moeda
                                                + tt-param.delimitador + c-moeda-controle
                           tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                                + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                                + tt-param.delimitador 
                                                + tt-param.delimitador + c-lbl-vl-verba-orig
                                                + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                           tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                                + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                           tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                                + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                           tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                                + tt-param.delimitador + string(de-total)
                           tt-impressao.colunas = c-lb-dt-trans
                                                + tt-param.delimitador + c-lb-item
                                                + tt-param.delimitador + c-lb-num-ord-magnus
                                                + tt-param.delimitador + c-lb-tipo-doc
                                                + tt-param.delimitador + c-lb-solicitacao
                                                + tt-param.delimitador + c-lb-ordem
                                                + tt-param.delimitador + c-lb-pedido
                                                + tt-param.delimitador + c-lb-especie
                                                + tt-param.delimitador + c-lb-docto
                                                + tt-param.delimitador + c-lb-ent-comp
                                                + tt-param.delimitador + c-lb-sai-comp
                                                + tt-param.delimitador + c-lb-ent-real
                                                + tt-param.delimitador + c-lb-sai-real
                                                + tt-param.delimitador + c-lb-acum-comp
                                                + tt-param.delimitador + c-lb-acum-real.
                end.
            
             
            /*       find first tt-impressao no-lock no-error. */
            /*       if avail tt-impressao then                */
            /*           PUT STREAM s-planilha UNFORMATTED     */
            /*                tt-impressao.linha-1 skip        */
            /*                tt-impressao.linha-2 skip        */
            /*                tt-impressao.linha-3 skip        */
            /*                tt-impressao.linha-4 skip        */
            /*                tt-impressao.linha-5 skip        */
            /*                tt-impressao.linha-6 skip        */
            /*                tt-impressao.linha-7 skip        */
            /*                tt-impressao.linha-8 skip        */
            /*                tt-impressao.linha-9 skip(2)     */
            /*                tt-impressao.colunas SKIP.       */
                       
                  assign de-acum-comp = 0
                         de-acum-real = 0
                         l-imprimiu   = no.
                               
                  for each t-controle
                     where t-controle.ep-codigo    = controle-verba.ep-codigo
                       and t-controle.cod-est-exec = controle-verba.cod-est-exec
                       and t-controle.num-projeto  = controle-verba.num-projeto
                       and t-controle.num-ordem    = controle-verba.num-ordem

                       AND  t-controle.cod-especialidade >= tt-param.cod-especialidade-ini 
                       AND  t-controle.cod-especialidade <= tt-param.cod-especialidade-fim 
                       AND  t-controle.cod-sub-espec     >= tt-param.cod-sub-espec-ini
                       AND  t-controle.cod-sub-espec     <= tt-param.cod-sub-espec-fim
                       no-lock break by t-controle.it-codigo by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
                       assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                              de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                              t-controle.acum-comp = de-acum-comp
                              t-controle.acum-real = de-acum-real
                              l-imprimiu           = yes.
            
                          RUN pi-imprime-linha-t-controle.
            
            /*            PUT STREAM s-planilha UNFORMATTED                  */
            /*                t-controle.dt-trans       tt-param.delimitador */
            /*                t-controle.it-codigo      tt-param.delimitador */
            /*                t-controle.num-ord-magnus tt-param.delimitador */
            /*                t-controle.tipo-doc       tt-param.delimitador */
            /*                t-controle.solicitacao    tt-param.delimitador */
            /*                t-controle.num-ord-comp   tt-param.delimitador */
            /*                t-controle.num-pedido     tt-param.delimitador */
            /*                t-controle.esp-docto      tt-param.delimitador */
            /*                t-controle.num-docto      tt-param.delimitador */
            /*                t-controle.ent-comp       tt-param.delimitador */
            /*                t-controle.sai-comp       tt-param.delimitador */
            /*                t-controle.ent-real       tt-param.delimitador */
            /*                t-controle.sai-real       tt-param.delimitador */
            /*                t-controle.acum-comp      tt-param.delimitador */
            /*                t-controle.acum-real      tt-param.delimitador */
            /*                SKIP.                                          */
                        /** CHAMADA UPC **/   
                
                        for each tt-epc:
                            delete tt-epc.
                        end.    
                         
                        create tt-epc.
                        assign tt-epc.cod-event     = "apos-display-frame-controle":U
                               tt-epc.cod-parameter = "controle-verba-rowid"
                               tt-epc.val-parameter = string(rowid(controle-verba)). 
                              
                        {include/i-epc201.i "apos-display-frame-controle"}        
                      
                        if return-value = 'NOK' then do:
                            undo,leave.
                        end.
                    
                        /********************/    
            
                  end.   
                  if l-imprimiu = yes then do:
                     if de-acum-comp  < 0 then assign de-acum-comp = 0.
                     if de-acum-real < 0 then assign de-acum-real = 0.
                     assign de-acum-tot = de-acum-comp + de-acum-real.
            /*          PUT STREAM s-planilha UNFORMATTED                                                                             */
            /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
            /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
            /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total                                */
            /*               tt-param.delimitador de-acum-tot SKIP(2).                                                                */
                  end.        
            /*       PUT STREAM s-planilha UNFORMATTED SKIP(10). */
            end.
            
            OUTPUT STREAM s-planilha CLOSE.
 
        END.
        else if tt-param.classif-1 = 2 THEN DO:
           /*******************************************************************************
           * ESIN0518.I - Impressío
           ********************************************************************************/
           
           OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.
           
           RUN pi-imprime-cabecalho-planilha.
           
           run pi-inicializar in h-acomp(input c-imprimindo).
           for each controle-verba
               where controle-verba.ep-codigo    >= tt-param.i-ep-ini
                 and controle-verba.ep-codigo    <= tt-param.i-ep-fim
                 and controle-verba.cod-est-exec >= tt-param.c-est-ini
                 and controle-verba.cod-est-exec <= tt-param.c-est-fim      
                 and controle-verba.num-projeto  >= tt-param.i-proj-ini
                 and controle-verba.num-projeto  <= tt-param.i-proj-fim
                 and controle-verba.num-ordem    >= tt-param.i-ord-ini
                 and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
                 break by controle-verba.ep-codigo
                       by controle-verba.cod-est-exec
                       by controle-verba.num-projeto
                       by controle-verba.num-ordem:   
                       
                
                 run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                                    string(controle-verba.num-projeto) + "  " + 
                                                    c-lb-ordem + 
                                                    string(controle-verba.num-ordem))).     
                 if first-of (controle-verba.ep-codigo) or
                    not avail empresa then do:
                    find empresa
                         where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
                    find param-inv 
                         where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
                    run inp/in9997.p (input 1).
                    assign c-moedas-inv [1] = Trim(return-value).
                    run inp/in9997.p (input 2).
                    assign c-moedas-inv [2] = Trim(return-value).
                    run inp/in9997.p (input 3).
                    assign c-moedas-inv [3] = Trim(return-value). 
                 end.
                 if first-of (controle-verba.cod-est-exec) or
                    not avail estabelec then    
                    find estabelec
                         where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
                 if first-of (controle-verba.num-projeto) or 
                    not avail proj-inv then         
                    find proj-inv
                        where proj-inv.ep-codigo = controle-verba.ep-codigo
                          and proj-inv.cod-est-exec = controle-verba.cod-est-exec
                          and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
                 if first-of (controle-verba.num-ordem) or
                    not avail ordem-inv then do:
                    find ordem-inv
                         where ordem-inv.ep-codigo = controle-verba.ep-codigo
                           and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                           and ordem-inv.num-projeto = controle-verba.num-projeto
                           and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
                    
                    &if '{&bf_mat_versao_ems}' >= '2.062' &then
                    ASSIGN i-cnt-unid-negoc = 1.
                     DO  WHILE i-cnt-unid-negoc < 34:
                         ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                     END.
                     
                     FOR EACH mat-rat-med-inv NO-LOCK
                         WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                           AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                           AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                           AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                           AND mat-rat-med-inv.numero-ordem = 0
                           AND mat-rat-med-inv.nr-contrato  = 0:
                         IF  c-txt-unid-negoc[1] = "" THEN DO:
                             {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                                    i-cnt-unid-negoc = 1.
                         END.
                         
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                         IF  i-cnt-unid-negoc < 34 THEN DO:
                            {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                            &if '{&bf_mat_versao_ems}' = '2.062' &then
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                            &else
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                            &endif
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
                         END.
                     END.
                    &endif
                 end.
                
                 if controle-verba.situacao = 1 then 
                    assign c-situacao = c-situacao-1.
                 else 
                    assign c-situacao = c-situacao-2.      
           
                 assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
            
                 assign de-acum-comp = 0
                        de-acum-real = 0.
           
                 if tt-param.atualiza then do:
                     for each t-controle
                        where t-controle.ep-codigo    = controle-verba.ep-codigo
                          and t-controle.cod-est-exec = controle-verba.cod-est-exec
                          and t-controle.num-projeto  = controle-verba.num-projeto
                          and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
                          assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                                 de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
                     end.
           
                     if de-acum-comp  < 0 then assign de-acum-comp = 0.
                     if de-acum-real < 0 then assign de-acum-real = 0.
           
                     find first b-controle-verba exclusive-lock
                         where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                           and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                           and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                           and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
                     if avail b-controle-verba then
                       assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                              b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
                 end.
           
                 for each tt-impressao:
                     delete tt-impressao.
                 end.
                 /*******************************************************************************
           * ESIN0518-EXCEL.I4 - Impressío do cabeªalho no EXCEL
           ********************************************************************************/
           assign de-total = controle-verba.vl-comp[i-moeda-par] +
                             controle-verba.vl-real[i-moeda-par].
           
           
               if l-unidade-negocio and l-mat-unid-negoc then do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[01]
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[02]
                                               + tt-param.delimitador + c-txt-unid-negoc[10]
                                               + tt-param.delimitador + c-txt-unid-negoc[18]
                                               + tt-param.delimitador + c-txt-unid-negoc[26]
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[03]
                                               + tt-param.delimitador + c-txt-unid-negoc[11]
                                               + tt-param.delimitador + c-txt-unid-negoc[19]
                                               + tt-param.delimitador + c-txt-unid-negoc[27]
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                               + tt-param.delimitador + c-txt-unid-negoc[04]
                                               + tt-param.delimitador + c-txt-unid-negoc[12]
                                               + tt-param.delimitador + c-txt-unid-negoc[20]
                                               + tt-param.delimitador + c-txt-unid-negoc[28]
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[05]
                                               + tt-param.delimitador + c-txt-unid-negoc[13]
                                               + tt-param.delimitador + c-txt-unid-negoc[21]
                                               + tt-param.delimitador + c-txt-unid-negoc[29]
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[06]
                                               + tt-param.delimitador + c-txt-unid-negoc[14]
                                               + tt-param.delimitador + c-txt-unid-negoc[22]
                                               + tt-param.delimitador + c-txt-unid-negoc[30]
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[07]
                                               + tt-param.delimitador + c-txt-unid-negoc[15]
                                               + tt-param.delimitador + c-txt-unid-negoc[23]
                                               + tt-param.delimitador + c-txt-unid-negoc[31]
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[08]
                                               + tt-param.delimitador + c-txt-unid-negoc[16]
                                               + tt-param.delimitador + c-txt-unid-negoc[24]
                                               + tt-param.delimitador + c-txt-unid-negoc[32]
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[09]
                                               + tt-param.delimitador + c-txt-unid-negoc[17]
                                               + tt-param.delimitador + c-txt-unid-negoc[25]
                                               + tt-param.delimitador + c-txt-unid-negoc[33]
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
               else do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
           
            
           /*       find first tt-impressao no-lock no-error. */
           /*       if avail tt-impressao then                */
           /*           PUT STREAM s-planilha UNFORMATTED     */
           /*                tt-impressao.linha-1 skip        */
           /*                tt-impressao.linha-2 skip        */
           /*                tt-impressao.linha-3 skip        */
           /*                tt-impressao.linha-4 skip        */
           /*                tt-impressao.linha-5 skip        */
           /*                tt-impressao.linha-6 skip        */
           /*                tt-impressao.linha-7 skip        */
           /*                tt-impressao.linha-8 skip        */
           /*                tt-impressao.linha-9 skip(2)     */
           /*                tt-impressao.colunas SKIP.       */
                      
                 assign de-acum-comp = 0
                        de-acum-real = 0
                        l-imprimiu   = no.
                              
                 for each t-controle
                    where t-controle.ep-codigo    = controle-verba.ep-codigo
                      and t-controle.cod-est-exec = controle-verba.cod-est-exec
                      and t-controle.num-projeto  = controle-verba.num-projeto
                      and t-controle.num-ordem    = controle-verba.num-ordem

                       AND  t-controle.cod-especialidade >= tt-param.cod-especialidade-ini 
                       AND  t-controle.cod-especialidade <= tt-param.cod-especialidade-fim 
                       AND  t-controle.cod-sub-espec     >= tt-param.cod-sub-espec-ini
                       AND  t-controle.cod-sub-espec     <= tt-param.cod-sub-espec-fim

                      no-lock break by t-controle.dt-trans by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
                      assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                             de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                             t-controle.acum-comp = de-acum-comp
                             t-controle.acum-real = de-acum-real
                             l-imprimiu           = yes.
           
                      RUN pi-imprime-linha-t-controle.
           
           /*            PUT STREAM s-planilha UNFORMATTED                  */
           /*                t-controle.dt-trans       tt-param.delimitador */
           /*                t-controle.it-codigo      tt-param.delimitador */
           /*                t-controle.num-ord-magnus tt-param.delimitador */
           /*                t-controle.tipo-doc       tt-param.delimitador */
           /*                t-controle.solicitacao    tt-param.delimitador */
           /*                t-controle.num-ord-comp   tt-param.delimitador */
           /*                t-controle.num-pedido     tt-param.delimitador */
           /*                t-controle.esp-docto      tt-param.delimitador */
           /*                t-controle.num-docto      tt-param.delimitador */
           /*                t-controle.ent-comp       tt-param.delimitador */
           /*                t-controle.sai-comp       tt-param.delimitador */
           /*                t-controle.ent-real       tt-param.delimitador */
           /*                t-controle.sai-real       tt-param.delimitador */
           /*                t-controle.acum-comp      tt-param.delimitador */
           /*                t-controle.acum-real      tt-param.delimitador */
           /*                SKIP.                                          */
                       /** CHAMADA UPC **/   
               
                       for each tt-epc:
                           delete tt-epc.
                       end.    
                        
                       create tt-epc.
                       assign tt-epc.cod-event     = "apos-display-frame-controle":U
                              tt-epc.cod-parameter = "controle-verba-rowid"
                              tt-epc.val-parameter = string(rowid(controle-verba)). 
                             
                       {include/i-epc201.i "apos-display-frame-controle"}        
                     
                       if return-value = 'NOK' then do:
                           undo,leave.
                       end.
                   
                       /********************/    
           
                 end.   
                 if l-imprimiu = yes then do:
                    if de-acum-comp  < 0 then assign de-acum-comp = 0.
                    if de-acum-real < 0 then assign de-acum-real = 0.
                    assign de-acum-tot = de-acum-comp + de-acum-real.
           /*          PUT STREAM s-planilha UNFORMATTED                                                                             */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total                                */
           /*               tt-param.delimitador de-acum-tot SKIP(2).                                                                */
                 end.        
           /*       PUT STREAM s-planilha UNFORMATTED SKIP(10). */
           end.
           
           OUTPUT STREAM s-planilha CLOSE.
 
        end.
        else IF tt-param.classif-1 = 3 THEN do:
           /*******************************************************************************
           * ESIN0518.I - Impressío
           ********************************************************************************/
           
           OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.
           
           RUN pi-imprime-cabecalho-planilha.
           
           run pi-inicializar in h-acomp(input c-imprimindo).
           for each controle-verba
               where controle-verba.ep-codigo    >= tt-param.i-ep-ini
                 and controle-verba.ep-codigo    <= tt-param.i-ep-fim
                 and controle-verba.cod-est-exec >= tt-param.c-est-ini
                 and controle-verba.cod-est-exec <= tt-param.c-est-fim      
                 and controle-verba.num-projeto  >= tt-param.i-proj-ini
                 and controle-verba.num-projeto  <= tt-param.i-proj-fim
                 and controle-verba.num-ordem    >= tt-param.i-ord-ini
                 and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
                 break by controle-verba.ep-codigo
                       by controle-verba.cod-est-exec
                       by controle-verba.num-projeto
                       by controle-verba.num-ordem:   
                       
                
                 run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                                    string(controle-verba.num-projeto) + "  " + 
                                                    c-lb-ordem + 
                                                    string(controle-verba.num-ordem))).     
                 if first-of (controle-verba.ep-codigo) or
                    not avail empresa then do:
                    find empresa
                         where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
                    find param-inv 
                         where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
                    run inp/in9997.p (input 1).
                    assign c-moedas-inv [1] = Trim(return-value).
                    run inp/in9997.p (input 2).
                    assign c-moedas-inv [2] = Trim(return-value).
                    run inp/in9997.p (input 3).
                    assign c-moedas-inv [3] = Trim(return-value). 
                 end.
                 if first-of (controle-verba.cod-est-exec) or
                    not avail estabelec then    
                    find estabelec
                         where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
                 if first-of (controle-verba.num-projeto) or 
                    not avail proj-inv then         
                    find proj-inv
                        where proj-inv.ep-codigo = controle-verba.ep-codigo
                          and proj-inv.cod-est-exec = controle-verba.cod-est-exec
                          and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
                 if first-of (controle-verba.num-ordem) or
                    not avail ordem-inv then do:
                    find ordem-inv
                         where ordem-inv.ep-codigo = controle-verba.ep-codigo
                           and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                           and ordem-inv.num-projeto = controle-verba.num-projeto
                           and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
                    
                    &if '{&bf_mat_versao_ems}' >= '2.062' &then
                    ASSIGN i-cnt-unid-negoc = 1.
                     DO  WHILE i-cnt-unid-negoc < 34:
                         ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                     END.
                     
                     FOR EACH mat-rat-med-inv NO-LOCK
                         WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                           AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                           AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                           AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                           AND mat-rat-med-inv.numero-ordem = 0
                           AND mat-rat-med-inv.nr-contrato  = 0:
                         IF  c-txt-unid-negoc[1] = "" THEN DO:
                             {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                                    i-cnt-unid-negoc = 1.
                         END.
                         
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                         IF  i-cnt-unid-negoc < 34 THEN DO:
                            {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                            &if '{&bf_mat_versao_ems}' = '2.062' &then
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                            &else
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                            &endif
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
                         END.
                     END.
                    &endif
                 end.
                
                 if controle-verba.situacao = 1 then 
                    assign c-situacao = c-situacao-1.
                 else 
                    assign c-situacao = c-situacao-2.      
           
                 assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
            
                 assign de-acum-comp = 0
                        de-acum-real = 0.
           
                 if tt-param.atualiza then do:
                     for each t-controle
                        where t-controle.ep-codigo    = controle-verba.ep-codigo
                          and t-controle.cod-est-exec = controle-verba.cod-est-exec
                          and t-controle.num-projeto  = controle-verba.num-projeto
                          and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
                          assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                                 de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
                     end.
           
                     if de-acum-comp  < 0 then assign de-acum-comp = 0.
                     if de-acum-real < 0 then assign de-acum-real = 0.
           
                     find first b-controle-verba exclusive-lock
                         where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                           and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                           and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                           and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
                     if avail b-controle-verba then
                       assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                              b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
                 end.
           
                 for each tt-impressao:
                     delete tt-impressao.
                 end.
                 /*******************************************************************************
           * ESIN0518-EXCEL.I4 - Impressío do cabeªalho no EXCEL
           ********************************************************************************/
           assign de-total = controle-verba.vl-comp[i-moeda-par] +
                             controle-verba.vl-real[i-moeda-par].
           
           
               if l-unidade-negocio and l-mat-unid-negoc then do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[01]
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[02]
                                               + tt-param.delimitador + c-txt-unid-negoc[10]
                                               + tt-param.delimitador + c-txt-unid-negoc[18]
                                               + tt-param.delimitador + c-txt-unid-negoc[26]
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[03]
                                               + tt-param.delimitador + c-txt-unid-negoc[11]
                                               + tt-param.delimitador + c-txt-unid-negoc[19]
                                               + tt-param.delimitador + c-txt-unid-negoc[27]
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                               + tt-param.delimitador + c-txt-unid-negoc[04]
                                               + tt-param.delimitador + c-txt-unid-negoc[12]
                                               + tt-param.delimitador + c-txt-unid-negoc[20]
                                               + tt-param.delimitador + c-txt-unid-negoc[28]
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[05]
                                               + tt-param.delimitador + c-txt-unid-negoc[13]
                                               + tt-param.delimitador + c-txt-unid-negoc[21]
                                               + tt-param.delimitador + c-txt-unid-negoc[29]
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[06]
                                               + tt-param.delimitador + c-txt-unid-negoc[14]
                                               + tt-param.delimitador + c-txt-unid-negoc[22]
                                               + tt-param.delimitador + c-txt-unid-negoc[30]
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[07]
                                               + tt-param.delimitador + c-txt-unid-negoc[15]
                                               + tt-param.delimitador + c-txt-unid-negoc[23]
                                               + tt-param.delimitador + c-txt-unid-negoc[31]
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[08]
                                               + tt-param.delimitador + c-txt-unid-negoc[16]
                                               + tt-param.delimitador + c-txt-unid-negoc[24]
                                               + tt-param.delimitador + c-txt-unid-negoc[32]
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[09]
                                               + tt-param.delimitador + c-txt-unid-negoc[17]
                                               + tt-param.delimitador + c-txt-unid-negoc[25]
                                               + tt-param.delimitador + c-txt-unid-negoc[33]
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
               else do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
           
            
           /*       find first tt-impressao no-lock no-error.  */
           /*       if avail tt-impressao then                 */
           /*           PUT STREAM s-planilha UNFORMATTED      */
           /*                tt-impressao.linha-1 skip         */
           /*                tt-impressao.linha-2 skip         */
           /*                tt-impressao.linha-3 skip         */
           /*                tt-impressao.linha-4 skip         */
           /*                tt-impressao.linha-5 skip         */
           /*                tt-impressao.linha-6 skip         */
           /*                tt-impressao.linha-7 skip         */
           /*                tt-impressao.linha-8 skip         */
           /*                tt-impressao.linha-9 skip(2)      */
           /*                tt-impressao.colunas SKIP.        */
                      
                 assign de-acum-comp = 0
                        de-acum-real = 0
                        l-imprimiu   = no.
                              
                 for each t-controle
                    where t-controle.ep-codigo    = controle-verba.ep-codigo
                      and t-controle.cod-est-exec = controle-verba.cod-est-exec
                      and t-controle.num-projeto  = controle-verba.num-projeto
                      and t-controle.num-ordem    = controle-verba.num-ordem

                       AND  t-controle.cod-especialidade >= tt-param.cod-especialidade-ini 
                       AND  t-controle.cod-especialidade <= tt-param.cod-especialidade-fim 
                       AND  t-controle.cod-sub-espec     >= tt-param.cod-sub-espec-ini
                       AND  t-controle.cod-sub-espec     <= tt-param.cod-sub-espec-fim

                      no-lock break by t-controle.tipo-doc by t-controle.seq by t-controle.solicitacao by t-controle.num-ord-comp by t-controle.seq-comp by t-controle.num-pedido by t-controle.esp-docto by t-controle.num-docto:
                      assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                             de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                             t-controle.acum-comp = de-acum-comp
                             t-controle.acum-real = de-acum-real
                             l-imprimiu           = yes.
           
                         RUN pi-imprime-linha-t-controle.
           
           /*            PUT STREAM s-planilha UNFORMATTED                  */
           /*                t-controle.dt-trans       tt-param.delimitador */
           /*                t-controle.it-codigo      tt-param.delimitador */
           /*                t-controle.num-ord-magnus tt-param.delimitador */
           /*                t-controle.tipo-doc       tt-param.delimitador */
           /*                t-controle.solicitacao    tt-param.delimitador */
           /*                t-controle.num-ord-comp   tt-param.delimitador */
           /*                t-controle.num-pedido     tt-param.delimitador */
           /*                t-controle.esp-docto      tt-param.delimitador */
           /*                t-controle.num-docto      tt-param.delimitador */
           /*                t-controle.ent-comp       tt-param.delimitador */
           /*                t-controle.sai-comp       tt-param.delimitador */
           /*                t-controle.ent-real       tt-param.delimitador */
           /*                t-controle.sai-real       tt-param.delimitador */
           /*                t-controle.acum-comp      tt-param.delimitador */
           /*                t-controle.acum-real      tt-param.delimitador */
           /*                SKIP.                                          */
                       /** CHAMADA UPC **/   
               
                       for each tt-epc:
                           delete tt-epc.
                       end.    
                        
                       create tt-epc.
                       assign tt-epc.cod-event     = "apos-display-frame-controle":U
                              tt-epc.cod-parameter = "controle-verba-rowid"
                              tt-epc.val-parameter = string(rowid(controle-verba)). 
                             
                       {include/i-epc201.i "apos-display-frame-controle"}        
                     
                       if return-value = 'NOK' then do:
                           undo,leave.
                       end.
                   
                       /********************/    
           
                 end.   
                 if l-imprimiu = yes then do:
                    if de-acum-comp  < 0 then assign de-acum-comp = 0.
                    if de-acum-real < 0 then assign de-acum-real = 0.
                    assign de-acum-tot = de-acum-comp + de-acum-real.
           /*          PUT STREAM s-planilha UNFORMATTED                                                                             */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total                                */
           /*               tt-param.delimitador de-acum-tot SKIP(2).                                                                */
                 end.        
           /*       PUT STREAM s-planilha UNFORMATTED SKIP(10).  */
           end.
           
           OUTPUT STREAM s-planilha CLOSE.
 
        
        end.
        ELSE IF tt-param.classif-1 = 4 THEN DO:
           /*******************************************************************************
           * ESIN0518.I - Impressío
           ********************************************************************************/
           
           OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.
           
           RUN pi-imprime-cabecalho-planilha.
           
           run pi-inicializar in h-acomp(input c-imprimindo).
           for each controle-verba
               where controle-verba.ep-codigo    >= tt-param.i-ep-ini
                 and controle-verba.ep-codigo    <= tt-param.i-ep-fim
                 and controle-verba.cod-est-exec >= tt-param.c-est-ini
                 and controle-verba.cod-est-exec <= tt-param.c-est-fim      
                 and controle-verba.num-projeto  >= tt-param.i-proj-ini
                 and controle-verba.num-projeto  <= tt-param.i-proj-fim
                 and controle-verba.num-ordem    >= tt-param.i-ord-ini
                 and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
                 break by controle-verba.ep-codigo
                       by controle-verba.cod-est-exec
                       by controle-verba.num-projeto
                       by controle-verba.num-ordem:   
                       
                
                 run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                                    string(controle-verba.num-projeto) + "  " + 
                                                    c-lb-ordem + 
                                                    string(controle-verba.num-ordem))).     
                 if first-of (controle-verba.ep-codigo) or
                    not avail empresa then do:
                    find empresa
                         where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
                    find param-inv 
                         where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
                    run inp/in9997.p (input 1).
                    assign c-moedas-inv [1] = Trim(return-value).
                    run inp/in9997.p (input 2).
                    assign c-moedas-inv [2] = Trim(return-value).
                    run inp/in9997.p (input 3).
                    assign c-moedas-inv [3] = Trim(return-value). 
                 end.
                 if first-of (controle-verba.cod-est-exec) or
                    not avail estabelec then    
                    find estabelec
                         where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
                 if first-of (controle-verba.num-projeto) or 
                    not avail proj-inv then         
                    find proj-inv
                        where proj-inv.ep-codigo = controle-verba.ep-codigo
                          and proj-inv.cod-est-exec = controle-verba.cod-est-exec
                          and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
                 if first-of (controle-verba.num-ordem) or
                    not avail ordem-inv then do:
                    find ordem-inv
                         where ordem-inv.ep-codigo = controle-verba.ep-codigo
                           and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                           and ordem-inv.num-projeto = controle-verba.num-projeto
                           and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
                    
                    &if '{&bf_mat_versao_ems}' >= '2.062' &then
                    ASSIGN i-cnt-unid-negoc = 1.
                     DO  WHILE i-cnt-unid-negoc < 34:
                         ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                     END.
                     
                     FOR EACH mat-rat-med-inv NO-LOCK
                         WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                           AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                           AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                           AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                           AND mat-rat-med-inv.numero-ordem = 0
                           AND mat-rat-med-inv.nr-contrato  = 0:
                         IF  c-txt-unid-negoc[1] = "" THEN DO:
                             {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                                    i-cnt-unid-negoc = 1.
                         END.
                         
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                         IF  i-cnt-unid-negoc < 34 THEN DO:
                            {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                            &if '{&bf_mat_versao_ems}' = '2.062' &then
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                            &else
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                            &endif
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
                         END.
                     END.
                    &endif
                 end.
                
                 if controle-verba.situacao = 1 then 
                    assign c-situacao = c-situacao-1.
                 else 
                    assign c-situacao = c-situacao-2.      
           
                 assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
            
                 assign de-acum-comp = 0
                        de-acum-real = 0.
           
                 if tt-param.atualiza then do:
                     for each t-controle
                        where t-controle.ep-codigo    = controle-verba.ep-codigo
                          and t-controle.cod-est-exec = controle-verba.cod-est-exec
                          and t-controle.num-projeto  = controle-verba.num-projeto
                          and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
                          assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                                 de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
                     end.
           
                     if de-acum-comp  < 0 then assign de-acum-comp = 0.
                     if de-acum-real < 0 then assign de-acum-real = 0.
           
                     find first b-controle-verba exclusive-lock
                         where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                           and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                           and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                           and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
                     if avail b-controle-verba then
                       assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                              b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
                 end.
           
                 for each tt-impressao:
                     delete tt-impressao.
                 end.
                 /*******************************************************************************
           * ESIN0518-EXCEL.I4 - Impressío do cabeªalho no EXCEL
           ********************************************************************************/
           assign de-total = controle-verba.vl-comp[i-moeda-par] +
                             controle-verba.vl-real[i-moeda-par].
           
           
               if l-unidade-negocio and l-mat-unid-negoc then do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[01]
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[02]
                                               + tt-param.delimitador + c-txt-unid-negoc[10]
                                               + tt-param.delimitador + c-txt-unid-negoc[18]
                                               + tt-param.delimitador + c-txt-unid-negoc[26]
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[03]
                                               + tt-param.delimitador + c-txt-unid-negoc[11]
                                               + tt-param.delimitador + c-txt-unid-negoc[19]
                                               + tt-param.delimitador + c-txt-unid-negoc[27]
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                               + tt-param.delimitador + c-txt-unid-negoc[04]
                                               + tt-param.delimitador + c-txt-unid-negoc[12]
                                               + tt-param.delimitador + c-txt-unid-negoc[20]
                                               + tt-param.delimitador + c-txt-unid-negoc[28]
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[05]
                                               + tt-param.delimitador + c-txt-unid-negoc[13]
                                               + tt-param.delimitador + c-txt-unid-negoc[21]
                                               + tt-param.delimitador + c-txt-unid-negoc[29]
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[06]
                                               + tt-param.delimitador + c-txt-unid-negoc[14]
                                               + tt-param.delimitador + c-txt-unid-negoc[22]
                                               + tt-param.delimitador + c-txt-unid-negoc[30]
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[07]
                                               + tt-param.delimitador + c-txt-unid-negoc[15]
                                               + tt-param.delimitador + c-txt-unid-negoc[23]
                                               + tt-param.delimitador + c-txt-unid-negoc[31]
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[08]
                                               + tt-param.delimitador + c-txt-unid-negoc[16]
                                               + tt-param.delimitador + c-txt-unid-negoc[24]
                                               + tt-param.delimitador + c-txt-unid-negoc[32]
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[09]
                                               + tt-param.delimitador + c-txt-unid-negoc[17]
                                               + tt-param.delimitador + c-txt-unid-negoc[25]
                                               + tt-param.delimitador + c-txt-unid-negoc[33]
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
               else do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
           
            
           /*       find first tt-impressao no-lock no-error.  */
           /*       if avail tt-impressao then                 */
           /*           PUT STREAM s-planilha UNFORMATTED      */
           /*                tt-impressao.linha-1 skip         */
           /*                tt-impressao.linha-2 skip         */
           /*                tt-impressao.linha-3 skip         */
           /*                tt-impressao.linha-4 skip         */
           /*                tt-impressao.linha-5 skip         */
           /*                tt-impressao.linha-6 skip         */
           /*                tt-impressao.linha-7 skip         */
           /*                tt-impressao.linha-8 skip         */
           /*                tt-impressao.linha-9 skip(2)      */
           /*                tt-impressao.colunas SKIP.        */
                      
                 assign de-acum-comp = 0
                        de-acum-real = 0
                        l-imprimiu   = no.
                              
                 for each t-controle
                    where t-controle.ep-codigo    = controle-verba.ep-codigo
                      and t-controle.cod-est-exec = controle-verba.cod-est-exec
                      and t-controle.num-projeto  = controle-verba.num-projeto
                      and t-controle.num-ordem    = controle-verba.num-ordem

                       AND  t-controle.cod-especialidade >= tt-param.cod-especialidade-ini 
                       AND  t-controle.cod-especialidade <= tt-param.cod-especialidade-fim 
                       AND  t-controle.cod-sub-espec     >= tt-param.cod-sub-espec-ini
                       AND  t-controle.cod-sub-espec     <= tt-param.cod-sub-espec-fim

                      no-lock break by t-controle.cod-especialidade:
                      assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                             de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                             t-controle.acum-comp = de-acum-comp
                             t-controle.acum-real = de-acum-real
                             l-imprimiu           = yes.
           
                         RUN pi-imprime-linha-t-controle.
           
           /*            PUT STREAM s-planilha UNFORMATTED                  */
           /*                t-controle.dt-trans       tt-param.delimitador */
           /*                t-controle.it-codigo      tt-param.delimitador */
           /*                t-controle.num-ord-magnus tt-param.delimitador */
           /*                t-controle.tipo-doc       tt-param.delimitador */
           /*                t-controle.solicitacao    tt-param.delimitador */
           /*                t-controle.num-ord-comp   tt-param.delimitador */
           /*                t-controle.num-pedido     tt-param.delimitador */
           /*                t-controle.esp-docto      tt-param.delimitador */
           /*                t-controle.num-docto      tt-param.delimitador */
           /*                t-controle.ent-comp       tt-param.delimitador */
           /*                t-controle.sai-comp       tt-param.delimitador */
           /*                t-controle.ent-real       tt-param.delimitador */
           /*                t-controle.sai-real       tt-param.delimitador */
           /*                t-controle.acum-comp      tt-param.delimitador */
           /*                t-controle.acum-real      tt-param.delimitador */
           /*                SKIP.                                          */
                       /** CHAMADA UPC **/   
               
                       for each tt-epc:
                           delete tt-epc.
                       end.    
                        
                       create tt-epc.
                       assign tt-epc.cod-event     = "apos-display-frame-controle":U
                              tt-epc.cod-parameter = "controle-verba-rowid"
                              tt-epc.val-parameter = string(rowid(controle-verba)). 
                             
                       {include/i-epc201.i "apos-display-frame-controle"}        
                     
                       if return-value = 'NOK' then do:
                           undo,leave.
                       end.
                   
                       /********************/    
           
                 end.   
                 if l-imprimiu = yes then do:
                    if de-acum-comp  < 0 then assign de-acum-comp = 0.
                    if de-acum-real < 0 then assign de-acum-real = 0.
                    assign de-acum-tot = de-acum-comp + de-acum-real.
           /*          PUT STREAM s-planilha UNFORMATTED                                                                             */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total                                */
           /*               tt-param.delimitador de-acum-tot SKIP(2).                                                                */
                 end.        
           /*       PUT STREAM s-planilha UNFORMATTED SKIP(10).  */
           end.
           
           OUTPUT STREAM s-planilha CLOSE.
 
        
        END.
        ELSE DO:
           /*******************************************************************************
           * ESIN0518.I - Impressío
           ********************************************************************************/
           
           OUTPUT STREAM s-planilha TO VALUE(tt-param.caminho-excel) CONVERT TARGET 'iso8859-1'.
           
           RUN pi-imprime-cabecalho-planilha.
           
           run pi-inicializar in h-acomp(input c-imprimindo).
           for each controle-verba
               where controle-verba.ep-codigo    >= tt-param.i-ep-ini
                 and controle-verba.ep-codigo    <= tt-param.i-ep-fim
                 and controle-verba.cod-est-exec >= tt-param.c-est-ini
                 and controle-verba.cod-est-exec <= tt-param.c-est-fim      
                 and controle-verba.num-projeto  >= tt-param.i-proj-ini
                 and controle-verba.num-projeto  <= tt-param.i-proj-fim
                 and controle-verba.num-ordem    >= tt-param.i-ord-ini
                 and controle-verba.num-ordem    <= tt-param.i-ord-fim no-lock
                 break by controle-verba.ep-codigo
                       by controle-verba.cod-est-exec
                       by controle-verba.num-projeto
                       by controle-verba.num-ordem:   
                       
                
                 run pi-acompanhar in h-acomp(input (c-lb-proj + 
                                                    string(controle-verba.num-projeto) + "  " + 
                                                    c-lb-ordem + 
                                                    string(controle-verba.num-ordem))).     
                 if first-of (controle-verba.ep-codigo) or
                    not avail empresa then do:
                    find empresa
                         where empresa.ep-codigo = controle-verba.ep-codigo no-lock no-error.
                    find param-inv 
                         where param-inv.ep-codigo = empresa.ep-codigo no-lock no-error.     
                    run inp/in9997.p (input 1).
                    assign c-moedas-inv [1] = Trim(return-value).
                    run inp/in9997.p (input 2).
                    assign c-moedas-inv [2] = Trim(return-value).
                    run inp/in9997.p (input 3).
                    assign c-moedas-inv [3] = Trim(return-value). 
                 end.
                 if first-of (controle-verba.cod-est-exec) or
                    not avail estabelec then    
                    find estabelec
                         where estabelec.cod-estabel = controle-verba.cod-est-exec no-lock no-error.
                 if first-of (controle-verba.num-projeto) or 
                    not avail proj-inv then         
                    find proj-inv
                        where proj-inv.ep-codigo = controle-verba.ep-codigo
                          and proj-inv.cod-est-exec = controle-verba.cod-est-exec
                          and proj-inv.num-projeto = controle-verba.num-projeto no-lock no-error.
                 if first-of (controle-verba.num-ordem) or
                    not avail ordem-inv then do:
                    find ordem-inv
                         where ordem-inv.ep-codigo = controle-verba.ep-codigo
                           and ordem-inv.cod-est-exec = controle-verba.cod-est-exec
                           and ordem-inv.num-projeto = controle-verba.num-projeto
                           and ordem-inv.num-ordem = controle-verba.num-ordem no-lock no-error.     
                    
                    &if '{&bf_mat_versao_ems}' >= '2.062' &then
                    ASSIGN i-cnt-unid-negoc = 1.
                     DO  WHILE i-cnt-unid-negoc < 34:
                         ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = "".
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                     END.
                     
                     FOR EACH mat-rat-med-inv NO-LOCK
                         WHERE mat-rat-med-inv.ep-codigo    = controle-verba.ep-codigo
                           AND mat-rat-med-inv.cod-est-exec = controle-verba.cod-est-exec
                           AND mat-rat-med-inv.num-projeto  = controle-verba.num-projeto
                           AND mat-rat-med-inv.num-ordem    = controle-verba.num-ordem
                           AND mat-rat-med-inv.numero-ordem = 0
                           AND mat-rat-med-inv.nr-contrato  = 0:
                         IF  c-txt-unid-negoc[1] = "" THEN DO:
                             {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[1] = RETURN-VALUE
                                    i-cnt-unid-negoc = 1.
                         END.
                         
                         ASSIGN i-cnt-unid-negoc = i-cnt-unid-negoc + 1.
                         IF  i-cnt-unid-negoc < 34 THEN DO:
                            {utp/ut-liter.i Matriz_Unidade_de_NegΩcio}
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = TRIM(c-lbl-unid-negoc).
                            &if '{&bf_mat_versao_ems}' = '2.062' &then
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + SUBSTR(mat-rat-med-inv.cod-livre-1,1,3).
                            &else
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " " + mat-rat-med-inv.cod-unid-negoc.
                            &endif
                             ASSIGN c-txt-unid-negoc[i-cnt-unid-negoc] = c-txt-unid-negoc[i-cnt-unid-negoc] + " - " + STRING(mat-rat-med-inv.perc-rateio, ">>9.9999") + " %".
                         END.
                     END.
                    &endif
                 end.
                
                 if controle-verba.situacao = 1 then 
                    assign c-situacao = c-situacao-1.
                 else 
                    assign c-situacao = c-situacao-2.      
           
                 assign c-moeda-controle = c-moedas-inv [tt-param.i-moeda-par].   
            
                 assign de-acum-comp = 0
                        de-acum-real = 0.
           
                 if tt-param.atualiza then do:
                     for each t-controle
                        where t-controle.ep-codigo    = controle-verba.ep-codigo
                          and t-controle.cod-est-exec = controle-verba.cod-est-exec
                          and t-controle.num-projeto  = controle-verba.num-projeto
                          and t-controle.num-ordem    = controle-verba.num-ordem no-lock:
                          assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                                 de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real.
                     end.
           
                     if de-acum-comp  < 0 then assign de-acum-comp = 0.
                     if de-acum-real < 0 then assign de-acum-real = 0.
           
                     find first b-controle-verba exclusive-lock
                         where b-controle-verba.ep-codigo    = controle-verba.ep-codigo 
                           and b-controle-verba.cod-est-exec = controle-verba.cod-est-exec 
                           and b-controle-verba.num-projeto  = controle-verba.num-projeto 
                           and b-controle-verba.num-ordem    = controle-verba.num-ordem no-error.
                     if avail b-controle-verba then
                       assign b-controle-verba.vl-comp [i-moeda-par] = de-acum-comp
                              b-controle-verba.vl-real [i-moeda-par] = de-acum-real.                 
                 end.
           
                 for each tt-impressao:
                     delete tt-impressao.
                 end.
                 /*******************************************************************************
           * ESIN0518-EXCEL.I4 - Impressío do cabeªalho no EXCEL
           ********************************************************************************/
           assign de-total = controle-verba.vl-comp[i-moeda-par] +
                             controle-verba.vl-real[i-moeda-par].
           
           
               if l-unidade-negocio and l-mat-unid-negoc then do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[01]
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[02]
                                               + tt-param.delimitador + c-txt-unid-negoc[10]
                                               + tt-param.delimitador + c-txt-unid-negoc[18]
                                               + tt-param.delimitador + c-txt-unid-negoc[26]
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[03]
                                               + tt-param.delimitador + c-txt-unid-negoc[11]
                                               + tt-param.delimitador + c-txt-unid-negoc[19]
                                               + tt-param.delimitador + c-txt-unid-negoc[27]
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador 
                                               + tt-param.delimitador + c-txt-unid-negoc[04]
                                               + tt-param.delimitador + c-txt-unid-negoc[12]
                                               + tt-param.delimitador + c-txt-unid-negoc[20]
                                               + tt-param.delimitador + c-txt-unid-negoc[28]
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[05]
                                               + tt-param.delimitador + c-txt-unid-negoc[13]
                                               + tt-param.delimitador + c-txt-unid-negoc[21]
                                               + tt-param.delimitador + c-txt-unid-negoc[29]
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[06]
                                               + tt-param.delimitador + c-txt-unid-negoc[14]
                                               + tt-param.delimitador + c-txt-unid-negoc[22]
                                               + tt-param.delimitador + c-txt-unid-negoc[30]
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[07]
                                               + tt-param.delimitador + c-txt-unid-negoc[15]
                                               + tt-param.delimitador + c-txt-unid-negoc[23]
                                               + tt-param.delimitador + c-txt-unid-negoc[31]
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[08]
                                               + tt-param.delimitador + c-txt-unid-negoc[16]
                                               + tt-param.delimitador + c-txt-unid-negoc[24]
                                               + tt-param.delimitador + c-txt-unid-negoc[32]
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                                               + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador + tt-param.delimitador
                                               + tt-param.delimitador + c-txt-unid-negoc[09]
                                               + tt-param.delimitador + c-txt-unid-negoc[17]
                                               + tt-param.delimitador + c-txt-unid-negoc[25]
                                               + tt-param.delimitador + c-txt-unid-negoc[33]
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
               else do:
                   create tt-impressao.
                   ASSIGN tt-impressao.linha-1 = tt-param.delimitador + c-lbl-ep-codigo
                                               + tt-param.delimitador + string(controle-verba.ep-codigo)
                                               + tt-param.delimitador + if avail empresa then empresa.nome else ""
                          tt-impressao.linha-2 = tt-param.delimitador + c-lbl-estab-exec
                                               + tt-param.delimitador + controle-verba.cod-est-exec
                                               + tt-param.delimitador + if avail estabelec then estabelec.nome else ""
                          tt-impressao.linha-3 = tt-param.delimitador + c-lbl-num-projeto
                                               + tt-param.delimitador + string(controle-verba.num-projeto)
                                               + tt-param.delimitador + if avail proj-inv then proj-inv.descricao else ""
                          tt-impressao.linha-4 = tt-param.delimitador + c-lbl-num-ordem
                                               + tt-param.delimitador + string(controle-verba.num-ordem)
                                               + tt-param.delimitador + if avail ordem-inv then ordem-inv.descricao else ""
                          tt-impressao.linha-5 = tt-param.delimitador + c-lbl-situacao
                                               + tt-param.delimitador + c-situacao
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-moeda
                                               + tt-param.delimitador + c-moeda-controle
                          tt-impressao.linha-6 = tt-param.delimitador + c-lbl-vl-verba
                                               + tt-param.delimitador + string(controle-verba.vl-verba[i-moeda-par])
                                               + tt-param.delimitador 
                                               + tt-param.delimitador + c-lbl-vl-verba-orig
                                               + tt-param.delimitador + string(controle-verba.vl-verba-orig[i-moeda-par])
                          tt-impressao.linha-7 = tt-param.delimitador + c-lbl-vl-compromis
                                               + tt-param.delimitador + string(controle-verba.vl-comp[i-moeda-par])
                          tt-impressao.linha-8 = tt-param.delimitador + c-lbl-vl-realizado
                                               + tt-param.delimitador + string(controle-verba.vl-real[i-moeda-par])
                          tt-impressao.linha-9 = tt-param.delimitador + c-lbl-total
                                               + tt-param.delimitador + string(de-total)
                          tt-impressao.colunas = c-lb-dt-trans
                                               + tt-param.delimitador + c-lb-item
                                               + tt-param.delimitador + c-lb-num-ord-magnus
                                               + tt-param.delimitador + c-lb-tipo-doc
                                               + tt-param.delimitador + c-lb-solicitacao
                                               + tt-param.delimitador + c-lb-ordem
                                               + tt-param.delimitador + c-lb-pedido
                                               + tt-param.delimitador + c-lb-especie
                                               + tt-param.delimitador + c-lb-docto
                                               + tt-param.delimitador + c-lb-ent-comp
                                               + tt-param.delimitador + c-lb-sai-comp
                                               + tt-param.delimitador + c-lb-ent-real
                                               + tt-param.delimitador + c-lb-sai-real
                                               + tt-param.delimitador + c-lb-acum-comp
                                               + tt-param.delimitador + c-lb-acum-real.
               end.
           
            
           /*       find first tt-impressao no-lock no-error.  */
           /*       if avail tt-impressao then                 */
           /*           PUT STREAM s-planilha UNFORMATTED      */
           /*                tt-impressao.linha-1 skip         */
           /*                tt-impressao.linha-2 skip         */
           /*                tt-impressao.linha-3 skip         */
           /*                tt-impressao.linha-4 skip         */
           /*                tt-impressao.linha-5 skip         */
           /*                tt-impressao.linha-6 skip         */
           /*                tt-impressao.linha-7 skip         */
           /*                tt-impressao.linha-8 skip         */
           /*                tt-impressao.linha-9 skip(2)      */
           /*                tt-impressao.colunas SKIP.        */
                      
                 assign de-acum-comp = 0
                        de-acum-real = 0
                        l-imprimiu   = no.
                              
                 for each t-controle
                    where t-controle.ep-codigo    = controle-verba.ep-codigo
                      and t-controle.cod-est-exec = controle-verba.cod-est-exec
                      and t-controle.num-projeto  = controle-verba.num-projeto
                      and t-controle.num-ordem    = controle-verba.num-ordem

                       AND  t-controle.cod-especialidade >= tt-param.cod-especialidade-ini 
                       AND  t-controle.cod-especialidade <= tt-param.cod-especialidade-fim 
                       AND  t-controle.cod-sub-espec     >= tt-param.cod-sub-espec-ini
                       AND  t-controle.cod-sub-espec     <= tt-param.cod-sub-espec-fim

                      no-lock break by t-controle.cod-sub-espec:
                      assign de-acum-comp = de-acum-comp + t-controle.ent-comp - t-controle.sai-comp
                             de-acum-real = de-acum-real + t-controle.ent-real - t-controle.sai-real
                             t-controle.acum-comp = de-acum-comp
                             t-controle.acum-real = de-acum-real
                             l-imprimiu           = yes.
           
                         RUN pi-imprime-linha-t-controle.
           
           /*            PUT STREAM s-planilha UNFORMATTED                  */
           /*                t-controle.dt-trans       tt-param.delimitador */
           /*                t-controle.it-codigo      tt-param.delimitador */
           /*                t-controle.num-ord-magnus tt-param.delimitador */
           /*                t-controle.tipo-doc       tt-param.delimitador */
           /*                t-controle.solicitacao    tt-param.delimitador */
           /*                t-controle.num-ord-comp   tt-param.delimitador */
           /*                t-controle.num-pedido     tt-param.delimitador */
           /*                t-controle.esp-docto      tt-param.delimitador */
           /*                t-controle.num-docto      tt-param.delimitador */
           /*                t-controle.ent-comp       tt-param.delimitador */
           /*                t-controle.sai-comp       tt-param.delimitador */
           /*                t-controle.ent-real       tt-param.delimitador */
           /*                t-controle.sai-real       tt-param.delimitador */
           /*                t-controle.acum-comp      tt-param.delimitador */
           /*                t-controle.acum-real      tt-param.delimitador */
           /*                SKIP.                                          */
                       /** CHAMADA UPC **/   
               
                       for each tt-epc:
                           delete tt-epc.
                       end.    
                        
                       create tt-epc.
                       assign tt-epc.cod-event     = "apos-display-frame-controle":U
                              tt-epc.cod-parameter = "controle-verba-rowid"
                              tt-epc.val-parameter = string(rowid(controle-verba)). 
                             
                       {include/i-epc201.i "apos-display-frame-controle"}        
                     
                       if return-value = 'NOK' then do:
                           undo,leave.
                       end.
                   
                       /********************/    
           
                 end.   
                 if l-imprimiu = yes then do:
                    if de-acum-comp  < 0 then assign de-acum-comp = 0.
                    if de-acum-real < 0 then assign de-acum-real = 0.
                    assign de-acum-tot = de-acum-comp + de-acum-real.
           /*          PUT STREAM s-planilha UNFORMATTED                                                                             */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador tt-param.delimitador */
           /*               tt-param.delimitador tt-param.delimitador tt-param.delimitador c-lb-total                                */
           /*               tt-param.delimitador de-acum-tot SKIP(2).                                                                */
                 end.        
           /*       PUT STREAM s-planilha UNFORMATTED SKIP(10).  */
           end.
           
           OUTPUT STREAM s-planilha CLOSE.
 
        
        END.
    end.

    RETURN "OK":U.
END PROCEDURE.
    
PROCEDURE pi-parametros:
    put 
        c-titulo-s           skip(1)
        c-emp-sel            at 5  ": "
        tt-param.i-ep-ini    at 30 "|< >| " at 43 tt-param.i-ep-fim
        c-est-sel            at 5  ": "
        tt-param.c-est-ini   at 30 "|< >| " at 43 tt-param.c-est-fim
        c-proj-sel           at 5  ": "
        tt-param.i-proj-ini  at 30 "|< >| " at 43 tt-param.i-proj-fim
        c-ord-sel         at 5  ": "
        tt-param.i-ord-ini   at 30 "|< >| " at 43 tt-param.i-ord-fim 
        c-data-sel           at 5  ": "
        tt-param.d-data-ini FORMAT "99/99/9999"  at 30 "|< >| " at 43 tt-param.d-data-fim FORMAT "99/99/9999" skip(2)
        
        c-titulo-c           skip(1)
        c-geral              at 5  ": " tt-param.desc-classifica skip(2)
        
        c-titulo-p           skip(1)
        c-lb-exib-moeda      at 5  ": " c-exib-moeda
        c-lb-atualiza        at 5  ": " c-atualiza skip(2)
        
        c-titulo-i           skip(1)
        c-lb-dest            at 5  ": " c-destino + " - " + tt-param.arquivo
        c-lb-usuar           at 5  ": " tt-param.usuario skip(2).    
    RETURN "OK":U.
END.

PROCEDURE pi-imprime-cabecalho-planilha:
   PUT STREAM s-planilha
       "Empresa                             ;"
       "Raz∆o Social                        ;"
       "Estab Exec                          ;"
       "Projeto / ∑rea                      ;"
       "Descriá∆o do Projeto                ;"
       "Situaá∆o Projeto                    ;"
       "Moeda                               ;"
       "Valor Liberado Projeto              ;"
       "Divis∆o                             ;"
       "Descriá∆o Divis∆o                   ;"
       "Ordem                               ;"
       "Descriá∆o Ordem                     ;"
       "Unidade de Negocio                  ;"
       "C¢digo Respons†vel                  ;"
       "Nome do Responsavel                 ;"
       "Data Inicio Ordem                   ;"
       "Data Final Ordem                    ;"
       "Situaá∆o Ordem                      ;"
       "Valor Verba Ordem Original          ;"
       "Valor Verba Ordem                   ;"
       "Seá∆o                               ;"
       "Especialidade/µrea                  ;"
       "Descriá∆o Especialidade/µrea        ;"
       "Subespecialidade/Aplicaá∆o          ;"
       "Descriá∆o Subespecialidade/Aplicaá∆o;"
       "Numero OI                           ;"
       "Data Trans                          ;"
       "Item                                ;"
       "Descriá∆o Item                      ;"
       "Tipo                                ;"
       "Doc Suprimentos                     ;"
/*        "Req Estoque                         ;" */
/*        "Req Manut                           ;" */
/*        "Req Prod                            ;" */
/*        "Manual                              ;"  */
       "Ordem                               ;"
       "Pedido                              ;"
       "Contrato/Item Contrato              ;"
       "C¢digo Fornecedor                   ;"
       "Nome Fornecedor                     ;"
       "Esp                                 ;"
       "Documento                           ;"
       "Ent Compromissado                   ;"
       "Sai Compromissado                   ;"
       "Ent Realizado                       ;"
       "Sai Realizado                       ;"
       "Compromissado Acumulado             ;"
       "Realizado Acumulado                 ;"
       "ICMS NF                             ;"
       "IPI NF                              ;"
       "PIS NF                              ;"
       "Cofins NF                           ;"
       "Data Pagamento                      ;"
       "Titulo                              ;"
       "Valor Pago                          ;"
       "IR - Retencao                       ;"
       "PIS/COFINS/CSLL - Retencao          ;"
       "ISS - Retencao                      ;"
       "INSS - Retencao                     ;"
       "Retená∆o Contrato                   ;" SKIP.
END PROCEDURE.

PROCEDURE pi-imprime-linha-t-controle:
   ASSIGN de-vordem                    = 0
          d-vl-imposto-ir              = 0
          d-vl-imposto-pis-cofins-csll = 0
          d-vl-imposto-pis             = 0
          d-vl-imposto-cofins          = 0
          d-vl-imposto-csll            = 0
          d-vl-imposto-iss             = 0
          d-vl-imposto-inss            = 0
          d-vl-imposto-contrato        = 0
          v_dat_transacao              = ?
          v_cod_tit_ap                 = ""
          v_val_movto_ap               = 0.
       
   FIND FIRST empresa 
        WHERE empresa.ep-codigo = t-controle.ep-codigo
        NO-LOCK NO-ERROR.

   FIND FIRST proj-inv
        WHERE proj-inv.ep-codigo    = t-controle.ep-codigo     AND
              proj-inv.cod-est-exec = t-controle.cod-est-exec  AND
              proj-inv.num-projeto  = t-controle.num-projeto 
        NO-LOCK NO-ERROR.

   FIND FIRST div-inv OF proj-inv 
        NO-LOCK NO-ERROR.

   FIND FIRST ordem-inv
        WHERE ordem-inv.ep-codigo    = t-controle.ep-codigo     AND
              ordem-inv.cod-est-exec = t-controle.cod-est-exec  AND
              ordem-inv.num-projeto  = t-controle.num-projeto   AND
              ordem-inv.num-ordem    = t-controle.num-ordem
        NO-LOCK NO-ERROR.
   IF AVAILABLE(ordem-inv) THEN DO:
      CASE tt-param.i-moeda-par:
           WHEN 1 THEN ASSIGN de-vordem = ordem-inv.vl-verba[1].
           WHEN 2 THEN ASSIGN de-vordem = ordem-inv.vl-verba[2].
           WHEN 3 THEN ASSIGN de-vordem = ordem-inv.vl-verba[3].
      END CASE.

      FIND FIRST usuar_mestre
           WHERE usuar_mestre.cod_usuario = ordem-inv.usuario-atu
           NO-LOCK NO-ERROR.

      FIND FIRST controle-verba
           WHERE controle-verba.ep-codigo    = ordem-inv.ep-codigo    AND
                 controle-verba.cod-est-exec = ordem-inv.cod-est-exec AND
                 controle-verba.num-projeto  = ordem-inv.num-projeto  AND
                 controle-verba.num-ordem    = ordem-inv.num-ordem
           NO-LOCK NO-ERROR.
   END.

   IF t-controle.tipo-doc = "Solicit" THEN DO:
      FIND FIRST ITEM
           WHERE ITEM.it-codigo = t-controle.it-codigo 
           NO-LOCK NO-ERROR.
   END.
   
   RUN pi-valores (proj-inv.ep-codigo,   
                   proj-inv.cod-est-exec,
                   proj-inv.num-projeto). 

   CASE tt-param.i-moeda-par:
        WHEN 1 THEN ASSIGN de-vficha = de-vficha1.
        WHEN 2 THEN ASSIGN de-vficha = de-vficha2.
        WHEN 3 THEN ASSIGN de-vficha = de-vficha3.
   END CASE.

   FIND FIRST moeda
        WHERE moeda.mo-codigo = i-cod-moeda-par
        NO-LOCK NO-ERROR.

   IF t-controle.num-ord-comp <> 0 THEN DO:
      FIND FIRST ordem-compra
           WHERE ordem-compra.numero-ordem = t-controle.num-ord-comp
           NO-LOCK NO-ERROR.
      IF AVAILABLE(ordem-compra) THEN DO:
         FIND FIRST pedido-compr
              WHERE pedido-compr.num-pedido = ordem-compra.num-pedido
              NO-LOCK NO-ERROR.
         IF AVAILABLE(pedido-compr) THEN DO:
            FIND FIRST emitente OF pedido-compr
                 NO-LOCK NO-ERROR.
         END.
      END.
   END.

   IF t-controle.tipo-doc = "Nota Fiscal" THEN DO:
      FIND FIRST movto-nf
           WHERE ROWID(movto-nf) = t-controle.r-rowid
           NO-LOCK NO-ERROR.
      IF AVAILABLE(movto-nf) THEN DO:
         FOR EACH item-doc-est
                  WHERE item-doc-est.serie-docto  = movto-nf.serie-docto      AND
                        item-doc-est.nro-docto    = movto-nf.nro-docto        AND
                        item-doc-est.cod-emitente = movto-nf.cod-emitente     AND
                        item-doc-est.nat-operacao = movto-nf.nat-operacao     AND
                        (item-doc-est.sequencia    = movto-nf.num-seq-item-nf OR
                         item-doc-est.sequencia    = movto-nf.sequencia)
                  NO-LOCK,
                  FIRST docum-est OF item-doc-est
                        NO-LOCK:
            FOR EACH dupli-apagar
                     WHERE dupli-apagar.serie-docto  = docum-est.serie-docto      AND          
                           dupli-apagar.nro-docto    = docum-est.nro-docto        AND         
                           dupli-apagar.cod-emitente = docum-est.cod-emitente     AND         
                           dupli-apagar.nat-operacao = docum-est.nat-operacao     
                     NO-LOCK:

               FIND FIRST tit_ap
                    WHERE tit_ap.cod_estab         = dupli-apagar.cod-estabel  AND
                          tit_ap.cdn_fornecedor    = dupli-apagar.cod-emitente AND
                          tit_ap.cod_espec_docto   = dupli-apagar.cod-esp      AND
                          tit_ap.cod_ser_docto     = dupli-apagar.serie        AND
                          tit_ap.cod_tit_ap        = dupli-apagar.nro-docto    AND
                          tit_ap.cod_parcela       = dupli-apagar.parcela 
                    NO-LOCK NO-ERROR.
               IF AVAILABLE(tit_ap) THEN DO:
                  ASSIGN v_cod_tit_ap = tit_ap.cod_tit_ap.

                  FIND LAST  movto_tit_ap OF tit_ap
                       WHERE movto_tit_ap.ind_trans_ap        = "Baixa" AND 
                             movto_tit_ap.log_movto_estordo   = NO    
                       NO-LOCK NO-ERROR. 
                  IF AVAILABLE(movto_tit_ap) THEN DO:
                     ASSIGN v_dat_transacao = movto_tit_ap.dat_transacao.
                  END.
               END.

               ASSIGN v_val_movto_ap  = 0.
               FOR EACH tit_ap
                        WHERE tit_ap.cod_estab         = dupli-apagar.cod-estabel  AND
                              tit_ap.cdn_fornecedor    = dupli-apagar.cod-emitente AND
                              tit_ap.cod_espec_docto   = dupli-apagar.cod-esp      AND
                              tit_ap.cod_ser_docto     = dupli-apagar.serie        AND
                              tit_ap.cod_tit_ap        = dupli-apagar.nro-docto
                        NO-LOCK:
                  FOR EACH movto_tit_ap OF tit_ap
                           WHERE movto_tit_ap.ind_trans_ap        = "Baixa" AND 
                                 movto_tit_ap.log_movto_estordo   = NO    
                           NO-LOCK :
                     ASSIGN v_val_movto_ap  = v_val_movto_ap + movto_tit_ap.val_movto_ap.
                  END.
               END.

               /* impostos */
               FOR EACH dupli-imp 
                        WHERE dupli-imp.cod-emitente = dupli-apagar.cod-emitente AND
                              dupli-imp.nat-operacao = dupli-apagar.nat-operacao AND
                              dupli-imp.serie-docto  = dupli-apagar.serie-docto  AND
                              dupli-imp.nro-docto    = dupli-apagar.nro-docto    AND
                              dupli-imp.parcela      = dupli-apagar.parcela
                        NO-LOCK:
      
                  CASE dupli-imp.cod-esp:
                       WHEN "IR" THEN ASSIGN d-vl-imposto-ir               = d-vl-imposto-ir              + dupli-imp.vl-imposto.
                       WHEN "CP" THEN ASSIGN d-vl-imposto-pis-cofins-csll  = d-vl-imposto-pis-cofins-csll + dupli-imp.vl-imposto.
                       WHEN ""   THEN ASSIGN d-vl-imposto-pis              = d-vl-imposto-pis             + dupli-imp.vl-imposto.
                       WHEN ""   THEN ASSIGN d-vl-imposto-cofins           = d-vl-imposto-cofins          + dupli-imp.vl-imposto.
                       WHEN ""   THEN ASSIGN d-vl-imposto-csll             = d-vl-imposto-csll            + dupli-imp.vl-imposto.
                       WHEN "IS" THEN ASSIGN d-vl-imposto-iss              = d-vl-imposto-iss             + dupli-imp.vl-imposto.
                       WHEN "IN" THEN ASSIGN d-vl-imposto-inss             = d-vl-imposto-inss            + dupli-imp.vl-imposto.
                       WHEN "RC" THEN ASSIGN d-vl-imposto-contrato         = d-vl-imposto-contrato        + dupli-imp.vl-imposto.
                  END CASE.
               END.
            END.
         END.
      END.
   END.

   IF t-controle.tipo-doc = "Contrato" THEN DO:
      FIND FIRST contrato-for
           WHERE contrato-for.nr-contrato = INTEGER(ENTRY(2,t-controle.num-docto,"/"))
           NO-LOCK NO-ERROR.
      IF AVAILABLE(contrato-for) THEN
         FIND FIRST emitente
              WHERE emitente.cod-emitente = contrato-for.cod-emitente
              NO-LOCK NO-ERROR.
   END.
 
   IF t-controle.tipo-doc = "Item Contrato" THEN DO:
      FIND FIRST item-contrat
           WHERE item-contrat.nr-contrato  = INTEGER(ENTRY(2,t-controle.num-docto,"/")) AND
                 item-contrat.num-seq-item = INTEGER(ENTRY(3,t-controle.num-docto,"/"))
           NO-LOCK NO-ERROR.
      IF AVAILABLE(item-contrat) THEN DO:
         FIND FIRST ITEM
              WHERE ITEM.it-codigo = item-contrat.it-codigo
              NO-LOCK NO-ERROR.

         FIND FIRST contrato-for
              WHERE contrato-for.nr-contrato = item-contrat.nr-contrato
              NO-LOCK NO-ERROR.
         IF AVAILABLE(contrato-for) THEN
            FIND FIRST emitente
                 WHERE emitente.cod-emitente = contrato-for.cod-emitente
                 NO-LOCK NO-ERROR.
      END.
   END.
      

   IF t-controle.tipo-doc = "IMD" THEN DO:
      FIND FIRST emitente                                          
           WHERE emitente.cod-emitente = INTEGER(ENTRY(4,t-controle.num-docto,"/")) 
           NO-LOCK NO-ERROR. 
   END.

   IF t-controle.tipo-doc = "ACE" THEN 
      ASSIGN t-controle.tipo-doc = "ACE - Manual".

   IF t-controle.tipo-doc = "IMD" THEN 
      ASSIGN t-controle.tipo-doc = "IMD - APB".

   PUT STREAM s-planilha
       t-controle.ep-codigo                                                                                            ";"    /*       "Empresa                             ;"    */
       empresa.razao-social                                                                                            ";"    /*       "Raz∆o Social                        ;"    */
       t-controle.cod-est-exec                                                                                         ";"    /*       "Estab Exec                          ;"    */
       t-controle.num-projeto                                                                                          ";"    /*       "Projeto / ∑rea                      ;"    */
       proj-inv.descricao                                                                                              ";"    /*       "Descriá∆o do Projeto                ;"    */
       ENTRY(proj-inv.cod-situacao-proj,{ivinc/i01iv038.i 03}) FORMAT "X(40)"  ""                                      ";"    /*       "Situaá∆o Projeto                    ;"    */
       moeda.descricao                                                                                                 ";"    /*       "Moeda                               ;"    */
       de-vficha                                                                                                       ";"    /*       "Valor Liberado Projeto              ;"    */
       proj-inv.cod-divisao                                                                                            ";"    /*       "Divis∆o                             ;"    */
       div-inv.descricao                                                                                               ";"    /*       "Descriá∆o Divis∆o                   ;"    */
       t-controle.num-ordem                                                                                            ";"    /*       "Ordem                               ;"    */
       IF AVAILABLE (ordem-inv)      THEN ordem-inv.descricao      ELSE "" FORMAT "x(40)"                              ";"    /*       "Descriá∆o Ordem                     ;"    */
       "'" ENTRY(3,c-txt-unid-negoc[02]," ")                                                                           ";"    /*       "Unidade de Negocio                  ;"    */
       IF AVAILABLE (ordem-inv)      THEN ordem-inv.usuario-atu    ELSE "" FORMAT "x(12)"                              ";"    /*       "C¢digo Respons†vel                  ;"    */
       IF AVAILABLE (usuar_mestre)   THEN usuar_mestre.nom_usuario ELSE "" FORMAT "x(50)"                              ";"    /*       "Nome do Responsavel                 ;"    */
       IF AVAILABLE (controle-verba) THEN controle-verba.dt-ini-validade ELSE ? FORMAT "99/99/9999"                                       ";"    /*       "Data Inicio Ordem                   ;"    */
       IF AVAILABLE (controle-verba) THEN controle-verba.dt-fim-validade ELSE ? FORMAT "99/99/9999"                    ";"    /*       "Data Final Ordem                    ;"    */
       IF AVAILABLE (ordem-inv) THEN ENTRY(ordem-inv.cod-situacao-inv,{ivinc/i01iv029.i 03})   ELSE "" FORMAT "x(30)"  ";"    /*       "Situaá∆o Ordem                      ;"    */
       de-vordem                                                                                                       ";"    /*       "Valor Verba Ordem Original          ;"    */
       de-vordem                                                                                                       ";"    /*       "Valor Verba Ordem                   ;"    */
       t-controle.num-secao                                                                                            ";"    /*       "Seá∆o                               ;"    */
       t-controle.cod-especialidade                                                                                    ";"    /*       "Especialidade/µrea                  ;"    */
       t-controle.descricao-subespecialidade                                                                           ";"    /*       "Descriá∆o Especialidade/µrea        ;"    */
       t-controle.cod-sub-espec                                                                                        ";"    /*       "Subespecialidade/Aplicaá∆o          ;"    */
       t-controle.descricao-sub-espec                                                                                  ";"    /*       "Descriá∆o Subespecialidade/Aplicaá∆o;"    */
       t-controle.num-ord-magnus                                                                                       ";"    /*       "Numero OI                           ;"    */
       t-controle.dt-trans               FORMAT "99/99/9999"                                                                              ";"    /*       "Data Trans                          ;"    */
       IF AVAILABLE(ITEM) THEN ITEM.it-codigo ELSE "" FORMAT "x(12)"                                                   ";"    /*       "Item                                ;"    */
       IF AVAILABLE(ITEM) THEN ITEM.desc-item ELSE "" FORMAT "x(60)"                                                   ";"    /*       "Descriá∆o Item                      ;"    */
       t-controle.tipo-doc                                                                                             ";"    /*       "Tipo                                ;"    */
       t-controle.solicitacao                                                                                          ";"    /*       "Doc Suprimentos                     ;"    */
       t-controle.num-ord-comp                                                                                         ";"    /*       "Ordem                               ;"    */
       t-controle.num-pedido                                                                                           ";"    /*       "Pedido                              ;"    */
       IF AVAILABLE(item-contrat) THEN STRING(item-contrat.nr-contrato) + "/" + item-contrat.it-codigo ELSE "" FORMAT "x(50)"  ";"    /*       "Contrato/Item Contrato              ;"    */
       IF AVAILABLE(emitente)     THEN emitente.cod-emitente             ELSE 0                                        ";"    /*       "C¢digo Fornecedor                   ;"    */
       IF AVAILABLE(emitente)     THEN emitente.nome-emit                ELSE ""  FORMAT "x(80)"                       ";"    /*       "Nome Fornecedor                     ;"    */
       t-controle.esp-docto                                                                                            ";"    /*       "Esp                                 ;"    */
       t-controle.num-docto                                                                                            ";"    /*       "Documento                           ;"    */
       t-controle.ent-comp                                                                                             ";"    /*       "Ent Compromissado                   ;"    */
       t-controle.sai-comp                                                                                             ";"    /*       "Sai Compromissado                   ;"    */
       t-controle.ent-real                                                                                             ";"    /*       "Ent Realizado                       ;"    */
       t-controle.sai-real                                                                                             ";"    /*       "Sai Realizado                       ;"    */
       t-controle.acum-comp                                                                                            ";"    /*       "Compromissado Acumulado             ;"    */
       t-controle.acum-real                                                                                            ";"    /*       "Realizado Acumulado                 ;"    */
       IF AVAILABLE(item-doc-est) THEN (item-doc-est.valor-icm[1] + item-doc-est.icm-complem[1]) ELSE 0                ";"    /*       "ICMS NF                             ;"    */
       IF AVAILABLE(item-doc-est) THEN item-doc-est.valor-ipi[1]   ELSE 0                                              ";"    /*       "IPI NF                              ;"    */
       IF AVAILABLE(item-doc-est) THEN item-doc-est.valor-pis      ELSE 0                                              ";"    /*       "PIS NF                              ;"    */
       IF AVAILABLE(item-doc-est) THEN item-doc-est.val-cofins     ELSE 0                                              ";"    /*       "Cofins NF                           ;"    */
       v_dat_transacao                                                                                                 ";"    /*       "Data Pagamento                      ;"    */
       v_cod_tit_ap                                                                                                    ";"    /*       "Titulo                              ;"    */
       v_val_movto_ap                                                                                                  ";"    /*       "Valor Pago                          ;"    */
       d-vl-imposto-ir                                                                                                 ";"    /*       "IR - Retencao                       ;"    */
       d-vl-imposto-pis-cofins-csll                                                                                    ";"    /*       "PIS/COFINS/CSLL - Retencao          ;"    */
       d-vl-imposto-iss                                                                                                ";"    /*       "ISS - Retencao                      ;"    */
       d-vl-imposto-inss                                                                                               ";"    /*       "INSS - Retencao                     ;"    */
       d-vl-imposto-contrato                                                                                           ";"    /*       "Retená∆o Contrato                   ;"    */
          .

   PUT STREAM s-planilha SKIP.


   RELEASE item-contrat.
   RELEASE emitente.
   RELEASE ITEM.
END PROCEDURE.

PROCEDURE pi-valores:
   DEFINE INPUT PARAMETER ip-ep-codigo      LIKE proj-inv.ep-codigo     NO-UNDO.
   DEFINE INPUT PARAMETER ip-cod-est-exec   LIKE proj-inv.cod-est-exec  NO-UNDO.
   DEFINE INPUT PARAMETER ip-num-projeto    LIKE proj-inv.num-projeto   NO-UNDO.

   ASSIGN de-vficha1 = 0
          de-vficha2 = 0
          de-vficha3 = 0.

   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FIND FIRST param-inv 
        WHERE param-inv.ep-codigo = param-global.empresa-prin 
        NO-LOCK NO-ERROR.
  
   FOR EACH ficha-liber
            WHERE ficha-liber.ep-codigo    = ip-ep-codigo    AND
                  ficha-liber.cod-est-exec = ip-cod-est-exec AND
                  ficha-liber.num-projeto  = ip-num-projeto 
            NO-LOCK:
      ASSIGN de-vficha1 = de-vficha1 + ficha-liber.vl-verba[1].  
      
      IF param-inv.tem-moeda1 THEN 
         ASSIGN de-vficha2 = de-vficha2 + ficha-liber.vl-verba[2].
    
      IF param-inv.tem-moeda2 THEN 
         ASSIGN de-vficha3 = de-vficha3 + ficha-liber.vl-verba[3]. 
  END.
END.


PROCEDURE pi-especialidade:
   DEFINE INPUT PARAMETER ip-ep-codigo       LIKE plano-aprov.ep-codigo      NO-UNDO.
   DEFINE INPUT PARAMETER ip-cod-est-exec    LIKE plano-aprov.cod-est-exec   NO-UNDO.
   DEFINE INPUT PARAMETER ip-num-projeto     LIKE plano-aprov.num-projeto    NO-UNDO.
   DEFINE INPUT PARAMETER ip-num-ordem       LIKE plano-aprov.num-ordem      NO-UNDO.
   DEFINE INPUT PARAMETER ip-num-ord-magnus  LIKE plano-aprov.num-ord-magnus NO-UNDO.

   FIND FIRST ordem-inv 
        WHERE ordem-inv.ep-codigo     = ip-ep-codigo    AND
              ordem-inv.cod-est-exec  = ip-cod-est-exec AND
              ordem-inv.num-projeto   = ip-num-projeto  AND
              ordem-inv.num-ordem     = ip-num-ordem   
        NO-LOCK NO-ERROR.
   IF AVAILABLE(ordem-inv) THEN DO:
      FIND FIRST sub-div-ordem OF ordem-inv
           WHERE sub-div-ordem.num-ord-magnus = ip-num-ord-magnus
           NO-LOCK NO-ERROR.
      IF AVAILABLE(sub-div-ordem) THEN DO:
         ASSIGN t-controle.cod-especialidade = sub-div-ordem.cod-especialidade
                t-controle.cod-sub-espec     = sub-div-ordem.cod-sub-espec
                t-controle.num-secao         = sub-div-ordem.num-secao.

         FIND FIRST especialidade OF sub-div-ordem
              NO-LOCK NO-ERROR.
         IF AVAILABLE(especialidade) THEN DO:
            ASSIGN t-controle.descricao-subespecialidade = especialidade.descricao.

            FIND FIRST sub-espec OF especialidade
                 NO-LOCK NO-ERROR.
            IF AVAILABLE(sub-espec) THEN
               ASSIGN t-controle.descricao-sub-espec = sub-espec.descricao.
         END.
      
         FIND FIRST secao-inv OF sub-div-ordem
              NO-LOCK NO-ERROR.
      END.
   END.
END PROCEDURE.


