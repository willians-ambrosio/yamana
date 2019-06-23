/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*{include/i-prgvrs.i TD-DIST-EMITENTE-00 2.00.00.001} /*** 010001 ***/*/
/*******************************************************************************
**  Programa: <name>
**  Objetivo: <comment>
**  Autor...: Datasul Logistica - <who>
**  Data....: <date>
*******************************************************************************/
DEF PARAM BUFFER b-dist-emitente FOR dist-emitente.

{utp/ut-glob.i}
{mpp/mp9901.i}

def temp-table tt-param no-undo
    field c-param as char
    index ix as primary unique c-param.

def var c-conecta    as char no-undo.
def var i-erro       as int  no-undo.
def var c-banco-cont as char no-undo.

ASSIGN c-banco-cont = "".

empty temp-table tt-param.

for each bco_empres no-lock
   where bco_empres.cod_bco_dados = "ems2cademp"
     and bco_empres.log_bco_ativ
     and bco_empres.cod_empresa   < "A":

    if  bco_empres.cod_empresa = v_cdn_empres_usuar then
        run pi-param.

    find first tt-param
         where tt-param.c-param = bco_empres.cod_param_conex no-error.
    if  not avail tt-param then
        run pi-param.
    else
        next.

    empty temp-table tt-erro-conexao.

    assign c-banco-cont = "DBdistEmit" + bco_empres.cod_empresa.

    run pi-desconecta.
    run pi-conecta.

    if  not can-find(first tt-erro-conexao) then do:
        create alias table-alias for database value(c-banco-cont).

        run trigger/td-dist-emitente-01.p(buffer b-dist-emitente).
    end.

    run pi-desconecta.
    delete alias table-alias.

    for first tt-erro-conexao:
        message "ERRO DE CONEXÇO BANCO " skip
                "NÆo foram replicadas informa‡äes da tabela DIST-EMITENTE. Verifique programa MP0110." skip(2)
                tt-erro-conexao.cod-erro "-"
                tt-erro-conexao.desc-erro skip(1)
                "Nome F¡sico: " tt-erro-conexao.db-erro  skip
                "Parametros: " tt-erro-conexao.param-erro skip(1)
                view-as alert-box warning buttons ok.
    end.
end.

procedure pi-conecta:
    if  not connected(c-banco-cont) then do:
        assign c-conecta = "-db " + bco_empres.cod_bco_fisic +
                           " -ld " + c-banco-cont +
                           " " + bco_empres.cod_param_conex.

        connect value(c-conecta) no-error.

        if  error-status:error or
           (error-status:get-number(1) <> 138 and
            error-status:num-messages  <> 0) then do:
            do  i-erro = 1 to 5
            while(error-status:get-message(i-erro) <> ""):
                create tt-erro-conexao.
                assign tt-erro-conexao.cod-erro     = ERROR-STATUS:GET-NUMBER(i-erro)                                           
                       tt-erro-conexao.desc-erro    = ERROR-STATUS:GET-MESSAGE(i-erro)
                       tt-erro-conexao.db-erro      = bco_empres.cod_bco_fisic
                       tt-erro-conexao.lb-erro      = c-banco-cont
                       tt-erro-conexao.service-erro = ""
                       tt-erro-conexao.param-erro   = bco_empres.cod_param_conex
                       tt-erro-conexao.dt-erro      = today 
                       tt-erro-conexao.hora-erro    = time 
                       tt-erro-conexao.tp-erro-cad  = no.

                message "erro connect" error-status:get-message(i-erro)
                    view-as alert-box info buttons ok.
            end.
        end.
    end.
end procedure.

procedure pi-desconecta :
    if  connected(c-banco-cont) then
        disconnect value(c-banco-cont) no-error.
end procedure.

procedure pi-param :
    create tt-param no-error.
    assign tt-param.c-param = bco_empres.cod_param_conex.
end procedure.

return "OK":U.

