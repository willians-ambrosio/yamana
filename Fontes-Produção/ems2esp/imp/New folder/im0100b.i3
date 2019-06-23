&if '{&BF_MAT_VERSAO_EMS}' < '2.05' &then
 IF  NOT CAN-FIND(FIRST funcao
                  WHERE funcao.cd-funcao = "spp-decimal-imp":u
                    AND funcao.ativo) THEN DO:
     RUN utp/ut-msgs.p (INPUT "show":U,
                        INPUT 34048,
                        INPUT "").
     RETURN {1}.
 END.
&endif
