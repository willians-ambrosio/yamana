/*****************************************************************************
*   Programa: es0001rp.p
*   Data....: abril/06
*   Autor...: Raimundo C. Soares
*   Cliente : Fazenda Brasileiro
*   Objetivo: Relat¢rio de Despesa de ProvisÆo
*   VersÆo..: 2.09.001                            
*   OBS.....: 
*******************************************************************************/
/*---------------- Include de controle de Versão ------------------*/ 
{include/buffers_RH.i}

{include/i-prgvrs.i es0001rp 2.09.00.000}

/******** Definição Temp-table para Recebimento de Parametro **********************/
def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field v_cdn_empres_usuar   like param_empres_rh.cdn_empresa
    field v_num_tip_aces_usuar as integer format "9" 
    field data-exec            as date
    field hora-exec            as integer
    field classific            as integer
    field i-empresa            as char
    field i-est-ini            like funcionario.cdn_estab
    field i-est-fim            like funcionario.cdn_estab
    FIELD i-mes-ref            AS INTEGER
    FIELD i-ano-ref            AS INTEGER
    field i-tipo               as INTEGER.

DEF VAR i-cont AS INT NO-UNDO.    

DEF TEMP-TABLE tt-dados NO-UNDO
    FIELD cdn_funcionario  LIKE   movto_calcul_func.cdn_funcionario
    FIELD nom_pessoa_fisic like   funcionario.nom_pessoa_fisic
    FIELD i-mes            AS INT
    FIELD i-ano            AS INT
    FIELD centro-custo     LIKE   movto_calcul_func.cod_rh_ccusto
    FIELD vl-902           AS DEC
    FIELD vl-904           AS DEC
    FIELD vl-906           AS DEC
    FIELD vl-908           AS DEC
    FIELD vl-inicial       AS DEC  FORMAT "->>>>,>>9.99".

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.


create tt-param.
raw-transfer raw-param to tt-param.


/****************** Definiçao de Variáveis do Relatório Não Pedidas em Tela ******************/ 
DEF VAR d-total           AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-inicial   AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-geral-902 AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-geral-904 AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-geral-906 AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-geral-908 AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-geral-todos AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".



/****************** Definiçao de Variáveis de Processamento do Relatório *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.

/****************** Definiçao de  Frames e Forms do Relatório 132 Colunas ***************************************/ 
/*-------- include padrão para variaveis de relatorio--------- */
  {include/i-rpvar.i}
/*------------------------------------------------------------ */
 DEF VAR c-periodo AS CHAR FORMAT "x(7)" NO-UNDO .
 FIND FIRST tt-param NO-ERROR.

FORM HEADER 
     "Referente:" c-periodo FORMAT "x(7)" SKIP (1)

    WITH NO-LABELS STREAM-IO PAGE-TOP FRAME f-periodo.
             
find empresa no-lock where
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

FIND param_empres_rh WHERE param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar NO-LOCK NO-ERROR.

assign c-programa     = "es0001RP"
       c-versao       = "2.09"
       c-revisao      = "000"
       c-titulo-relat = "Relat¢rio de Provisao de 13o.Salario "
       c-sistema      = "Folha de Pagamento"
       c-empresa      = empresa.razao-social.

/*---------------- Abertura do arquivo de saida ----------------------*/

/*------------ Definição parametro &stream para os includes
               da frame de cabeçalho e rodape para 132 colunas ---------*/
{include/i-rpcab.i}
/*--------------------------------------------------------------------*/

/*------------ Include Padrao para output de relatorios --------------*/
{include/i-rpout.i}
/*--------------------------------------------------------------------*/

view frame f-cabec.
view frame f-rodape.
VIEW FRAME f-periodo.

ASSIGN c-periodo = string(tt-param.i-mes-ref, "99") + "/" + string(tt-param.i-ano-ref, "9999") .

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo * I}

run pi-inicializar in h-acomp (input "Processando").  

EMPTY TEMP-TABLE tt-dados.

FIND FIRST tt-param NO-ERROR.

RUN pi-acha-dados.

IF tt-param.i-tipo  = 1 THEN
   RUN pi-funcionario.

ELSE IF tt-param.i-tipo  = 2 THEN
   RUN pi-centro-custo.

ELSE 
   RUN pi-total.
/*----------------- Fechamento do output do relatorio -----------------*/
{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK":U.


PROCEDURE pi-acha-dados:

DEF VAR f-risco AS DEC NO-UNDO.
    FIND FIRST tt-param.

IF  tt-param.i-empresa = "201" THEN
    ASSIGN f-risco = 26.8 / 100.
ELSE 
    ASSIGN f-risco = 28.8 / 100.

   FOR EACH  movto_calcul_func
       WHERE movto_calcul_func.cdn_empresa       =  tt-param.i-empresa
       AND   movto_calcul_func.cdn_estab         >= tt-param.i-est-ini 
       AND   movto_calcul_func.cdn_estab         <= tt-param.i-est-fim 
       AND   movto_calcul_func.num_ano_refer_fp   = tt-param.i-ano-ref
       AND   movto_calcul_func.num_mes_refer_fp   <= tt-param.i-mes-ref
       AND   movto_calcul_func.idi_tip_fp         = 1
       NO-LOCK:

       FIND FIRST funcionario OF movto_calcul_func
            NO-LOCK NO-ERROR.

       IF YEAR (funcionario.dat_desligto_func) < tt-param.i-ano-ref THEN NEXT.

       IF month(funcionario.dat_desligto_func) < tt-param.i-mes-ref AND  
          YEAR (funcionario.dat_desligto_func) = tt-param.i-ano-ref THEN NEXT.

       run pi-acompanhar in h-acomp(input "Lendo Funcion rio: " + STRING(movto_calcul_func.cdn_funcionario)).

       FIND FIRST tt-dados
            WHERE tt-dados.cdn_funcionario   = movto_calcul_func.cdn_funcionario
            NO-ERROR.

       IF NOT AVAIL tt-dados THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.cdn_funcionario   = movto_calcul_func.cdn_funcionario   
                 tt-dados.centro-custo      = movto_calcul_func.cod_rh_ccusto     
                 tt-dados.i-mes             = movto_calcul_func.num_ano_refer_fp  
                 tt-dados.i-ano             = movto_calcul_func.num_mes_refer_fp 
                 tt-dados.nom_pessoa_fisic  = IF AVAIL funcionario THEN funcionario.nom_pessoa_fisic ELSE "NÆo cadastrado". 
       END.

       ASSIGN i-cont = 1.
       DO WHILE i-cont <= 30:

           
           IF movto_calcul_func.num_mes_refer_fp   = tt-param.i-mes-re  THEN do:
             CASE movto_calcul_func.cdn_event_fp[i-cont]: 
                 WHEN "381" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "389" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "902" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 + movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "013"  THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont].

                 WHEN "373" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "376" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "904" THEN ASSIGN tt-dados.vl-904 = tt-dados.vl-904 + movto_calcul_func.val_calcul_efp [i-cont].

                 WHEN "532" THEN ASSIGN tt-dados.vl-906 = tt-dados.vl-906 - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "536" THEN ASSIGN tt-dados.vl-906 = tt-dados.vl-906 - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "542" THEN ASSIGN tt-dados.vl-906 = tt-dados.vl-906 - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "549" THEN ASSIGN tt-dados.vl-906 = tt-dados.vl-906 - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "906" THEN ASSIGN tt-dados.vl-906 = tt-dados.vl-906 + movto_calcul_func.val_calcul_efp [i-cont].

                 WHEN "908" THEN ASSIGN tt-dados.vl-908 = tt-dados.vl-908 + movto_calcul_func.val_calcul_efp [i-cont].
                 

/*                  WHEN 371 THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]. */
/*                  WHEN 374 THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]. */

                 WHEN "377" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "380" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "383" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).

                 WHEN "386" THEN ASSIGN tt-dados.vl-902 = tt-dados.vl-902 - movto_calcul_func.val_calcul_efp [i-cont]
                                      tt-dados.vl-904 = tt-dados.vl-904 - (movto_calcul_func.val_calcul_efp [i-cont] * f-risco ).
             END CASE.
           END.

           IF movto_calcul_func.num_mes_refer_fp   < tt-param.i-mes-re  THEN DO:

              CASE movto_calcul_func.cdn_event_fp[i-cont]: 
                 WHEN "902" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "904" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "906" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "908" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
                                                         
                 WHEN "532" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "549" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "373" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "376" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont].
                 WHEN "542" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont]. 
                 WHEN "536" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - movto_calcul_func.val_calcul_efp [i-cont].
                          
                 WHEN "377" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - ( movto_calcul_func.val_calcul_efp [i-cont] + (movto_calcul_func.val_calcul_efp [i-cont] * f-risco)).
                 WHEN "380" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - ( movto_calcul_func.val_calcul_efp [i-cont] + (movto_calcul_func.val_calcul_efp [i-cont] * f-risco)).
                 WHEN "383" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - ( movto_calcul_func.val_calcul_efp [i-cont] + (movto_calcul_func.val_calcul_efp [i-cont] * f-risco)).
                 WHEN "386" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial - ( movto_calcul_func.val_calcul_efp [i-cont] + (movto_calcul_func.val_calcul_efp [i-cont] * f-risco)).


             END CASE.

               
           END.


          ASSIGN i-cont = i-cont + 1.
       END.
   END.
END PROCEDURE.



PROCEDURE pi-funcionario:
    ASSIGN d-total-inicial     = 0
           d-total-geral-902   = 0
           d-total-geral-904   = 0
           d-total-geral-906   = 0
           d-total-geral-908   = 0
           d-total-geral-todos = 0.

    FOR EACH tt-dados 
        WHERE  (tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR 
                tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 OR 
                tt-dados.vl-inicial <> 0)
        BREAK BY tt-dados.cdn_funcionario
              BY  tt-dados.centro-custo:

       run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

        ASSIGN d-total             = tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 +
                                     IF tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR  tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 THEN
                                     tt-dados.vl-inicial ELSE 0
               d-total-geral-902   = d-total-geral-902 + tt-dados.vl-902
               d-total-geral-904   = d-total-geral-904 + tt-dados.vl-904
               d-total-geral-906   = d-total-geral-906 + tt-dados.vl-906
               d-total-geral-908   = d-total-geral-908 + tt-dados.vl-908
               d-total-geral-todos = tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 + tt-dados.vl-inicial.

        DISP tt-dados.cdn_funcionario    COLUMN-LABEL "C¢digo"
             tt-dados.nom_pessoa_fisic   COLUMN-LABEL "Funcion rio"
             tt-dados.centro-custo       COLUMN-LABEL "C. Custo"
             tt-dados.vl-inicial         COLUMN-LABEL "Saldo Inicial" (TOTAL)
             tt-dados.vl-902             COLUMN-LABEL "13 Salario" (TOTAL)
             tt-dados.vl-906             COLUMN-LABEL "FGTS"       (TOTAL)
             tt-dados.vl-904             COLUMN-LABEL "INSS"       (TOTAL)
             tt-dados.vl-908             COLUMN-LABEL "PIS"        (TOTAL)
             d-total                     COLUMN-LABEL "Total"      (TOTAL)
             WITH WIDTH 150 STREAM-IO.

     END.
END.

PROCEDURE pi-centro-custo:
    ASSIGN d-total-geral-902   = 0
           d-total-geral-904   = 0
           d-total-geral-906   = 0
           d-total-geral-908   = 0
           d-total-geral-todos = 0.

    FOR EACH tt-dados 
        WHERE  (tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR 
                tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 OR 
                tt-dados.vl-inicial <> 0)
        BREAK BY tt-dados.centro-custo
              BY tt-dados.cdn_funcionario:

       run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

        ASSIGN d-total             = tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 + 
                                     IF tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR  tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 THEN
                                     tt-dados.vl-inicial ELSE 0

               d-total-geral-902   = d-total-geral-902 + tt-dados.vl-902
               d-total-geral-904   = d-total-geral-904 + tt-dados.vl-904
               d-total-geral-906   = d-total-geral-906 + tt-dados.vl-906
               d-total-geral-908   = d-total-geral-908 + tt-dados.vl-908
               d-total-geral-todos = tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 + tt-dados.vl-inicial .

        DISP tt-dados.cdn_funcionario    COLUMN-LABEL "C¢digo"
             tt-dados.nom_pessoa_fisic   COLUMN-LABEL "Funcion rio"
             tt-dados.centro-custo       COLUMN-LABEL "C. Custo"
             tt-dados.vl-inicial         COLUMN-LABEL "Saldo Inicial" (TOTAL BY tt-dados.centro-custo)
             tt-dados.vl-902             COLUMN-LABEL "13 Salario"    (TOTAL BY tt-dados.centro-custo)
             tt-dados.vl-906             COLUMN-LABEL "FGTS"          (TOTAL BY tt-dados.centro-custo)
             tt-dados.vl-904             COLUMN-LABEL "INSS"          (TOTAL BY tt-dados.centro-custo)
             tt-dados.vl-908             COLUMN-LABEL "PIS"           (TOTAL BY tt-dados.centro-custo)
             d-total                     COLUMN-LABEL "Total"         (TOTAL BY tt-dados.centro-custo)
             WITH WIDTH 150 STREAM-IO.
    END.
END PROCEDURE.
/* fim do programa */


PROCEDURE pi-total :

    ASSIGN d-total-inicial     = 0
           d-total-geral-902   = 0
           d-total-geral-904   = 0
           d-total-geral-906   = 0
           d-total-geral-908   = 0
           d-total-geral-todos = 0.

    FOR EACH tt-dados 
        WHERE  (tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR 
                tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 OR 
                tt-dados.vl-inicial <> 0):

       run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

        ASSIGN d-total             = tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 + 
                                     IF tt-dados.vl-902 <> 0 OR tt-dados.vl-906 <> 0 OR  tt-dados.vl-904 <> 0 OR tt-dados.vl-908 <> 0 THEN
                                     tt-dados.vl-inicial ELSE 0

               d-total-inicial     = d-total-inicial   + tt-dados.vl-inicial
               d-total-geral-902   = d-total-geral-902 + tt-dados.vl-902
               d-total-geral-904   = d-total-geral-904 + tt-dados.vl-904
               d-total-geral-906   = d-total-geral-906 + tt-dados.vl-906
               d-total-geral-908   = d-total-geral-908 + tt-dados.vl-908
               d-total-geral-todos = d-total-geral-todos + tt-dados.vl-902 + tt-dados.vl-906 + tt-dados.vl-904 + tt-dados.vl-908 + tt-dados.vl-inicial .

           
    END.

   
    DISP d-total-inicial       COLUMN-LABEL "Saldo Inicial"
         d-total-geral-902     COLUMN-LABEL "13 Salario"   
         d-total-geral-906     COLUMN-LABEL "FGTS"         
         d-total-geral-904     COLUMN-LABEL "INSS"         
         d-total-geral-908     COLUMN-LABEL "PIS"          
         d-total-geral-todos   COLUMN-LABEL "Total"
         WITH WIDTH 150 STREAM-IO.        

END PROCEDURE.
