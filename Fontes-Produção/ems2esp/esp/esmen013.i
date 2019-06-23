/*****************************************************************************************
**    Programa: esmen013.i.p
**       Autor: DPC
**        Data: 03/05/2018
**    Objetivo: Unifica‡Æo de c¢digo - Relatorio valida‡Æo corretiva - conflito de acessos
**    Uso: Include usada em esmen013rp.p   
******************************************************************************************/
        
RUN pi-acompanhar in h-acomp (input "Conflito: " + CAPS(usuar_grp_usuar.cod_grp_usuar ) + ' X ' + CAPS(prog_dtsul_segur.cod_grp_usuar)).

 FIND tt-report 
    WHERE tt-report.cod_usuario          = usuar_grp_usuar.cod_usuario      
     AND  tt-report.cod_grp_usuar_base   = tt-conflito.cod_grp_usuar
     AND  tt-report.cod_prog_dtsul_base  = tt-conflito.cod_prog_dtsul
     AND  tt-report.cod_grp_usuar        = usuar_grp_usuar.cod_grp_usuar 
     AND  tt-report.cod_prog_dtsul       = ctrl-conflito-it.cod_prog_dtsul NO-ERROR.
 IF NOT AVAIL tt-report THEN DO:
     CREATE tt-report.
     ASSIGN tt-report.chave                = (IF tt-param.tipo = 1 THEN usuar_grp_usuar.cod_usuario ELSE usuar_grp_usuar.cod_grp_usuar)
            tt-report.cod_usuario          = usuar_grp_usuar.cod_usuario             
            tt-report.cod_grp_usuar_base   = tt-conflito.cod_grp_usuar 
            tt-report.cod_prog_dtsul_base  = tt-conflito.cod_prog_dtsul
            tt-report.cod_grp_usuar        = usuar_grp_usuar.cod_grp_usuar      
            tt-report.cod_prog_dtsul       = ctrl-conflito-it.cod_prog_dtsul
            tt-report.observacao           = ctrl-conflito-it.observacao 
            tt-report.razao                = ctrl-conflito-it.razao.
 END.

