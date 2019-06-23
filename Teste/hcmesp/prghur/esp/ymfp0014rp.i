/******************************************************************************
** Programa       : ymfp0014rp.i
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Importa‡Æo Planilha PLR
*****************************************************************************/
for each tt{&tip_func_indiv_factor} 
    break by tt{&tip_func_indiv_factor}.cdn_empresa    
          by tt{&tip_func_indiv_factor}.cdn_estab      
          by tt{&tip_func_indiv_factor}.cdn_funcionario
          by tt{&tip_func_indiv_factor}.log_corporativo
          by tt{&tip_func_indiv_factor}.dt_inicio :

    if last-of(tt{&tip_func_indiv_factor}.dt_inicio) then do:

        for each {&tiph_func_indiv_factor} exclusive-lock
            where {&tiph_func_indiv_factor}.cdn_empresa     = tt{&tip_func_indiv_factor}.cdn_empresa    
              and {&tiph_func_indiv_factor}.cdn_estab       = tt{&tip_func_indiv_factor}.cdn_estab      
              and {&tiph_func_indiv_factor}.cdn_funcionario = tt{&tip_func_indiv_factor}.cdn_funcionario
              and {&tiph_func_indiv_factor}.log_corporativo = tt{&tip_func_indiv_factor}.log_corporativo
              and {&tiph_func_indiv_factor}.dt_termino      = 12/31/9999 :

            assign {&tiph_func_indiv_factor}.dt_termino = max({&tiph_func_indiv_factor}.dt_inicio,
                                                              tt{&tip_func_indiv_factor}.dt_inicio - 1).

        end.


        for each {&tip_func_indiv_factor} exclusive-lock
            where {&tip_func_indiv_factor}.cdn_empresa      = tt{&tip_func_indiv_factor}.cdn_empresa    
              and {&tip_func_indiv_factor}.cdn_estab        = tt{&tip_func_indiv_factor}.cdn_estab      
              and {&tip_func_indiv_factor}.cdn_funcionario  = tt{&tip_func_indiv_factor}.cdn_funcionario
              and {&tip_func_indiv_factor}.log_corporativo  = tt{&tip_func_indiv_factor}.log_corporativo
              and {&tip_func_indiv_factor}.cdn_indiv_factor = tt{&tip_func_indiv_factor}.cdn_indiv_factor
              and {&tip_func_indiv_factor}.dt_inicio        = tt{&tip_func_indiv_factor}.dt_inicio :

            delete {&tip_func_indiv_factor}.

        end.

        create {&tip_func_indiv_factor}.

        buffer-copy tt{&tip_func_indiv_factor} to {&tip_func_indiv_factor}.

        assign {&tip_func_indiv_factor}.cod_usuario  = tt-param.usuario
               {&tip_func_indiv_factor}.dt_inclusao  = today
               {&tip_func_indiv_factor}.hr_inclusao  = string(time, 'hh:mm:ss').

        create {&tiph_func_indiv_factor}.

        buffer-copy {&tip_func_indiv_factor} to {&tiph_func_indiv_factor}.

    end.

end.
