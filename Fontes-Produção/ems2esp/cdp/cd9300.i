/****************************************************************************
**
**     CD9300.i - Calculo de Precos
**
****************************************************************************/

assign
  {1}.pre-unit-for = {1}.preco-fornec.

if {1}.taxa-financ then do:
    if {1}.codigo-ipi then do:

       if {1}.perc-descto > 0 then assign
           {1}.pre-unit-for = {1}.pre-unit-for *
                              (1 - ({1}.perc-descto / 100)).
    end.
    else do:

         if l-preco-bruto = no then do:
            if {1}.perc-descto > 0 then assign
               {1}.pre-unit-for = {1}.pre-unit-for
                                * (1 - ({1}.perc-descto / 100)).
            assign {1}.pre-unit-for = {1}.pre-unit-for
                                    * (1 + ({1}.aliquota-ipi / 100)).
         end.
         else do:

              assign {1}.pre-unit-for = {1}.pre-unit-for
                                      * (1 + ({1}.aliquota-ipi / 100)).
              if {1}.perc-descto > 0 then assign
                 {1}.pre-unit-for = {1}.pre-unit-for
                                  * (1 - ({1}.perc-descto / 100)).
         end.
    end.
end.
else do:
    if {1}.codigo-ipi then do:

       if l-preco-bruto = no then do:
          if {1}.perc-descto  > 0 then assign
             {1}.pre-unit-for = {1}.pre-unit-for
                              * (1 - ({1}.perc-descto / 100)).

          run ccp/cc9020.p (input yes,
                            input {1}.cod-cond-pag,
                            input {1}.valor-taxa,
                            input {1}.nr-dias-taxa,
                            input {1}.pre-unit-for,
                           output {1}.pre-unit-for).
       end.
       else do:
            run ccp/cc9020.p (input yes,
                              input {1}.cod-cond-pag,
                              input {1}.valor-taxa,
                              input {1}.nr-dias-taxa,
                              input {1}.pre-unit-for,
                             output {1}.pre-unit-for).

            if {1}.perc-descto  > 0 then assign
               {1}.pre-unit-for = {1}.pre-unit-for
                                * (1 - ({1}.perc-descto / 100)).
       end.
    end.
    else do:

       if l-preco-bruto = no then do:

          if {1}.perc-descto > 0 then assign
             {1}.pre-unit-for = {1}.pre-unit-for
                              * (1 - ({1}.perc-descto / 100)).

          run ccp/cc9020.p (input yes,
                            input {1}.cod-cond-pag,
                            input {1}.valor-taxa,
                            input {1}.nr-dias-taxa,
                            input {1}.pre-unit-for,
                           output {1}.pre-unit-for).

          assign {1}.pre-unit-for = {1}.pre-unit-for
                                  * (1 + ({1}.aliquota-ipi / 100)).
       end.
       else do:
            run ccp/cc9020.p (input yes,
                              input {1}.cod-cond-pag,
                              input {1}.valor-taxa,
                              input {1}.nr-dias-taxa,
                              input {1}.pre-unit-for,
                             output {1}.pre-unit-for).

            assign {1}.pre-unit-for = {1}.pre-unit-for
                                    * (1 + ({1}.aliquota-ipi / 100)).

            if {1}.perc-descto > 0 then assign
               {1}.pre-unit-for = {1}.pre-unit-for
                                * (1 - ({1}.perc-descto / 100)).
       end.
    end.
end.

/* Fim do Include */
