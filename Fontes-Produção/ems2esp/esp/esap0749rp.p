/********************************************************************************
**   Autor: Thiago Coutinho
** Empresa: CSX Solution
**    Data: March/2012
**Objetivo: ImpressÆo de titulos contas a pagar com Grupo e 
**          Data de Emissao dos Titulos.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESAP0749RP 2.06.00.000}  
{utp/ut-glob.i}
{include/i_fnctrad.i}

/* Miniflexibilizacao */
{cdp/cdcfgfin.i}
/* ------------------ */

/*--VARIAVIES--*/
{esp/esap0749rp.i2}                         
DEF VAR de-vl-liquido      AS DEC format "->>>>>>>>>,>>9.99" NO-UNDO.
DEF VAR de-tot-liquido     AS DEC format "->>>>>>>>>,>>9.99" NO-UNDO.
DEF VAR de-tot-liquido-ger AS DEC format "->>>>>>>>>,>>9.99" NO-UNDO.
DEF VAR l-vl-liq-impto     AS LOG                            NO-UNDO.
DEF VAR l-selec-ref        AS LOGICAL                        NO-UNDO.

DEFINE VARIABLE i-cod-gr-forn LIKE grp_fornec.cod_grp_fornec   NO-UNDO.
DEFINE VARIABLE c-descricao   LIKE grp_fornec.des_grp_fornec     NO-UNDO.

/*** Verificar se a funcao especial esta habilitada ***/
{include/getdefinedfunction.i}

assign l-selec-ref = &if defined(BF_FIN_SELEC_REF) &then YES &else GetDefinedFunction("SPP-SELEC-REF") &endif.

/*--TEMP-TABLES--*/
{esp/esap0749rp.i1}

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/*----FORMS----*/
IF tt-param.tg-excel = NO  THEN
DO:
{esp/esap0749rp.i3}
END.

/*---LABEL'S---*/
{esp/esap0749rp.i4}

{include/i-rpvar.i}

assign l-vl-liq-impto = no.
&if defined(BF_FIN_VL_LIQ_IMPTO_DOCTOS) &then
    assign l-vl-liq-impto = yes.
&else
    if can-find(first funcao
                where funcao.cd-funcao = 'spp-vl-liq-impto'
                AND   funcao.ativo     = YES) THEN
        assign l-vl-liq-impto = yes.
&endif

find empresa no-lock where empresa.ep-codigo = tt-param.ep-codigo no-error.
if  not avail empresa then
    return.

assign c-programa = "ES0749RP"
       c-empresa  = empresa.razao-social.

{utp/ut-liter.i T¡tulos_Impl_Contas_a_Pagar * R}
assign c-titulo-relat = return-value.

{utp/ut-liter.i CONTAS_A_PAGAR MCR R}
assign c-sistema = return-value.

{include/i-rpcab.i &WIDTH=256}
{include/i-rpout.i}
IF tt-param.tg-excel = NO THEN
DO:
    
    view frame f-cabec.
    view frame f-rodape.
END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Acompanhamento_Relat¢rio * R}
run pi-inicializar in h-acomp (input return-value).

FOR EACH movto_tit_ap NO-LOCK WHERE
         movto_tit_ap.cod_empresa      = v_cod_empres_usuar         AND
         movto_tit_ap.cod_estab       >= tt-param.cod-estabel-ini   AND
         movto_tit_ap.cod_estab       <= tt-param.cod-estabel-fim   AND
         movto_tit_ap.dat_gerac_movto >= tt-param.dt-maq-ini        AND
         movto_tit_ap.dat_gerac_movto <= tt-param.dt-maq-fim        AND
         movto_tit_ap.cod_refer       >= tt-param.referencia-ini    AND
         movto_tit_ap.cod_refer       <= tt-param.referencia-fim    AND
         movto_tit_ap.dat_transacao   >= tt-param.dt-trans-ini      AND
         movto_tit_ap.dat_transacao   <= tt-param.dt-trans-fim,
   FIRST tit_ap NO-LOCK WHERE
         tit_ap.cod_estab              = movto_tit_ap.cod_estab     AND
         tit_ap.num_id_tit_ap          = movto_tit_ap.num_id_tit_ap AND
         tit_ap.cod_espec_docto       >= tt-param.cod-esp-ini       AND
         tit_ap.cod_espec_docto       <= tt-param.cod-esp-fim       AND
         tit_ap.cdn_fornecedor        >= tt-param.cod-emit-ini      AND
         tit_ap.cdn_fornecedor        <= tt-param.cod-emit-fim      AND
         tit_ap.dat_vencto_tit_ap     >= tt-param.dt-venci-ini      AND
         tit_ap.dat_vencto_tit_ap     <= tt-param.dt-venci-fim      AND
        (tit_ap.ind_origin_tit_ap      = tt-param.c-origem          OR
         tt-param.c-origem             = "Todos"):

    create tt-mov-ap.
    buffer-copy movto_tit_ap to tt-mov-ap.
    ASSIGN tt-mov-ap.cod_tit_ap  = tit_ap.cod_tit_ap
           tt-mov-ap.cod_parcela = tit_ap.cod_parcela.
         
END.

/*IF tt-param.dt-trans-fim - tt-param.dt-trans-ini <= 31 THEN DO:
    DO da-data-trans = tt-param.dt-trans-ini TO tt-param.dt-trans-fim:
      &if "{&mgadm_version}" < "2.06b" &then
         IF l-selec-ref = NO THEN DO:
            for each mov-ap no-lock
               where mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.dt-transacao   = da-data-trans 
               and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini 
               and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim 
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini 
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim 
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini     
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim 
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini 
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim 
               AND   mov-ap.dt-today      >= tt-param.dt-maq-ini
               AND    mov-ap.dt-today     <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac)) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                     ((mov-ap.origem      >=  1 
                  and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) 
                  or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
                create tt-mov-ap.

                buffer-copy mov-ap to tt-mov-ap.

            end.
         END.
         ELSE DO:
            for each mov-ap no-lock
               where mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.dt-transacao   = da-data-trans
               and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini
               and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
               AND   mov-ap.referencia    >= tt-param.referencia-ini
               AND   mov-ap.referencia    <= tt-param.referencia-fim
               AND   mov-ap.dt-today      >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today      <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                     ((mov-ap.origem      >=  1
                  and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
                  or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
                create tt-mov-ap.
                buffer-copy mov-ap to tt-mov-ap.

            end.
         END.
      &else
         for each mov-ap no-lock
            where mov-ap.ep-codigo      = tt-param.ep-codigo
            and   mov-ap.dt-transacao   = da-data-trans
            and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini
            and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim
            and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
            and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
            and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
            and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
            and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
            and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
            AND   mov-ap.referencia    >= tt-param.referencia-ini
            AND   mov-ap.referencia    <= tt-param.referencia-fim
            AND   mov-ap.dt-today      >= tt-param.dt-maq-ini
            AND   mov-ap.dt-today      <= tt-param.dt-maq-fim
            and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                   (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                   (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                   (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                   (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
               and (mov-ap.transacao    =  1 or
                   (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                   (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                  ((mov-ap.origem      >=  1
               and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
               and (mov-ap.transacao    =  1 or
                   (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
               or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
                create tt-mov-ap.
                buffer-copy mov-ap to tt-mov-ap.

         end.
      &endif
    END.
END.
ELSE DO: 
    if  tt-param.cod-emit-ini = tt-param.cod-emit-fim then do:
      &if "{&mgadm_version}" < "2.06b" &then
        IF l-selec-ref = NO THEN DO:
            for each mov-ap no-lock
              where  mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.cod-fornec     = tt-param.cod-emit-ini 
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini 
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim 
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini     
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim 
               and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini 
               and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim 
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini 
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim 
               AND   mov-ap.dt-today      >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today      <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac)) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                     ((mov-ap.origem      >=  1 
                  and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) 
                  or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):

                create tt-mov-ap.
                buffer-copy mov-ap to tt-mov-ap.

            end.
        END.
        ELSE DO:
            for each mov-ap no-lock
              where  mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.cod-fornec     = tt-param.cod-emit-ini
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
               and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini
               and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
               AND   mov-ap.referencia    >= tt-param.referencia-ini
               AND   mov-ap.referencia    <= tt-param.referencia-fim
               AND   mov-ap.dt-today      >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today      <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                      ((mov-ap.origem      >=  1
                   and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
                   and (mov-ap.transacao    =  1 or
                       (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
                   or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):

                   create tt-mov-ap.
                   buffer-copy mov-ap to tt-mov-ap.

            end.
        END.
      &else
        for each mov-ap no-lock
            where  mov-ap.ep-codigo      = tt-param.ep-codigo
             and   mov-ap.cod-fornec     = tt-param.cod-emit-ini
             and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
             and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
             and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
             and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
             and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini
             and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim
             and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
             and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
             AND   mov-ap.referencia    >= tt-param.referencia-ini
             AND   mov-ap.referencia    <= tt-param.referencia-fim
            AND   mov-ap.dt-today  >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today  <= tt-param.dt-maq-fim
             and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                    (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                    (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                    (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                    (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
                and (mov-ap.transacao    =  1 or
                    (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                    (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                   ((mov-ap.origem      >=  1
                and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
                and (mov-ap.transacao    =  1 or
                    (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
                or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
            create tt-mov-ap.
            buffer-copy mov-ap to tt-mov-ap.
        end.
      &endif
    end.
    else do:
      &if "{&mgadm_version}" < "2.06b" &then
        IF l-selec-ref = NO THEN DO:
            for each mov-ap no-lock
               where mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini 
               and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim 
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini 
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini     
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim 
               and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini 
               and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim 
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini 
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim 
               AND   mov-ap.dt-today  >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today  <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac)) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                     ((mov-ap.origem      >=  1 
                  and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos) 
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) 
                  or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
                create tt-mov-ap.
                buffer-copy mov-ap to tt-mov-ap.
            end.
        END.
        ELSE DO:
            for each mov-ap no-lock
               where mov-ap.ep-codigo      = tt-param.ep-codigo
               and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini
               and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim
               and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
               and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
               and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
               and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
               and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini
               and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim
               and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
               and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
               AND   mov-ap.referencia    >= tt-param.referencia-ini
               AND   mov-ap.referencia    <= tt-param.referencia-fim
               AND   mov-ap.dt-today  >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today  <= tt-param.dt-maq-fim
               and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                      (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                      (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                      (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                      (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                      (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                     ((mov-ap.origem      >=  1
                  and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
                  and (mov-ap.transacao    =  1 or
                      (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
                  or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):
                create tt-mov-ap.
                buffer-copy mov-ap to tt-mov-ap.

            end.
           END.
      &else
        for each mov-ap no-lock
           where mov-ap.ep-codigo      = tt-param.ep-codigo
           and   mov-ap.cod-fornec    >= tt-param.cod-emit-ini
           and   mov-ap.cod-fornec    <= tt-param.cod-emit-fim
           and   mov-ap.cod-estabel   >= tt-param.cod-estabel-ini
           and   mov-ap.cod-estabel   <= tt-param.cod-estabel-fim
           and   mov-ap.cod-esp       >= tt-param.cod-esp-ini
           and   mov-ap.cod-esp       <= tt-param.cod-esp-fim
           and   mov-ap.dt-transacao  >= tt-param.dt-trans-ini
           and   mov-ap.dt-transacao  <= tt-param.dt-trans-fim
           and   mov-ap.dt-vencimen   >= tt-param.dt-venci-ini
           and   mov-ap.dt-vencimen   <= tt-param.dt-venci-fim
           AND   mov-ap.referencia    >= tt-param.referencia-ini
           AND   mov-ap.referencia    <= tt-param.referencia-fim
            AND   mov-ap.dt-today  >= tt-param.dt-maq-ini
               AND   mov-ap.dt-today  <= tt-param.dt-maq-fim
           and ((((mov-ap.origem       =  3 and trim(tt-param.c-origem)  = c-multiplanta) or
                  (mov-ap.origem       =  2 and trim(tt-param.c-origem)  =  c-inventario) or
                  (mov-ap.origem       =  4 and trim(tt-param.c-origem)  =      c-outros) or
                  (mov-ap.origem       =  1 and trim(tt-param.c-origem)  =       c-conta) or
                  (mov-ap.origem       =  5 and trim(tt-param.c-origem)  =         c-rac))
              and (mov-ap.transacao    =  1 or
                  (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes) or
                  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes))) or
                 ((mov-ap.origem      >=  1
              and  mov-ap.origem      <=  5 and trim(tt-param.c-origem)  = c-todos)
              and (mov-ap.transacao    =  1 or
                  (mov-ap.transacao    =  6 and mov-ap.lancamento        = 2 and tt-param.l-tit-subs  = yes)
              or  (mov-ap.transacao    = 16 and tt-param.l-nota-db-cr    = yes)))):

            create tt-mov-ap.
            buffer-copy mov-ap to tt-mov-ap.

        end.
      &endif
    end.
END.*/

CASE tt-param.classifica:
	WHEN 1 THEN
		run pi-class-dt-trans.
	WHEN 2 THEN  
	       run pi-class-cod-forn.
END CASE.

PROCEDURE pi-class-dt-trans:    
        IF tt-param.tg-excel = YES THEN
        DO:
            {esp/esap0749rp.i6 tt-mov-ap.dat_transacao tt-mov-ap.cdn_fornecedor}.
        END.

    IF  tt-param.tg-excel = NO THEN
    DO:
        {esp/esap0749rp.i5 tt-mov-ap.dat_transacao tt-mov-ap.cdn_fornecedor}   .
    END.

END.

PROCEDURE pi-class-cod-forn:

    IF tt-param.tg-excel = YES THEN
    DO:
        {esp/esap0749rp.i6 tt-mov-ap.cdn_fornecedor tt-mov-ap.dat_transacao}.
    END.

    IF tt-param.tg-excel = NO THEN
    DO:
        {esp/esap0749rp.i5 tt-mov-ap.cdn_fornecedor tt-mov-ap.dat_transacao}.
    END.

END.


PROCEDURE pi-imp-par:
IF tt-param.tg-excel = NO  THEN DO:

PAGE.
IF NOT l-vl-liq-impto THEN
    HIDE frame f-cabec-1.
ELSE 
    HIDE frame f-cabec-2.

    &if "{&mgadm_version}" < "2.06b" &then
      IF l-selec-ref = NO THEN DO:
        disp c-par
             tt-param.l-tit-subs
             tt-param.l-nota-db-cr
             tt-param.c-origem 
             c-sel
             tt-param.cod-emit-ini 
             tt-param.cod-emit-fim 
             tt-param.cod-esp-ini      
             tt-param.cod-esp-fim      
             tt-param.cod-estabel-ini 
             tt-param.cod-estabel-fim  
             tt-param.dt-trans-ini  
             tt-param.dt-trans-fim   
             tt-param.dt-venci-ini   
             tt-param.dt-venci-fim
             tt-param.dt-maq-ini
             tt-param.dt-maq-fim
             c-cla 
             c-cla-2
             c-imp
             c-destino
             tt-param.arquivo                  
             tt-param.usuario    
             with frame f-imp-par.
      END.
      ELSE DO:
        disp c-par
             tt-param.l-tit-subs
             tt-param.l-nota-db-cr
             tt-param.c-origem
             c-sel
             tt-param.cod-emit-ini
             tt-param.cod-emit-fim
             tt-param.cod-esp-ini
             tt-param.cod-esp-fim
             tt-param.cod-estabel-ini
             tt-param.cod-estabel-fim
             tt-param.dt-trans-ini
             tt-param.dt-trans-fim
             tt-param.dt-venci-ini
             tt-param.dt-venci-fim
             tt-param.referencia-ini
             tt-param.referencia-fim
             tt-param.dt-maq-ini
             tt-param.dt-maq-fim
             c-cla
             c-cla-2
             c-imp
             c-destino
             tt-param.arquivo
             tt-param.usuario
             with frame f-imp-par-2.
        END.
    &else
        disp c-par
             tt-param.l-tit-subs
             tt-param.l-nota-db-cr
             tt-param.c-origem
             c-sel
             tt-param.cod-emit-ini
             tt-param.cod-emit-fim
             tt-param.cod-esp-ini
             tt-param.cod-esp-fim
             tt-param.cod-estabel-ini
             tt-param.cod-estabel-fim
             tt-param.dt-trans-ini
             tt-param.dt-trans-fim
             tt-param.dt-venci-ini
             tt-param.dt-venci-fim
             tt-param.referencia-ini
             tt-param.referencia-fim
             tt-param.dt-maq-ini
             tt-param.dt-maq-fim
             c-cla
             c-cla-2
             c-imp
             c-destino
             tt-param.arquivo
             tt-param.usuario
             with frame f-imp-par-2.
    &endif
END.        
END.

{include/i-rpclo.i}
run pi-finalizar in h-acomp.          
