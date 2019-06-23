FOR EACH mguni.empresa /*WHERE ep-codigo = 201*/ NO-LOCK:
    FOR EACH rh_estab WHERE rh_estab.cdn_empresa = empresa.ep-codigo NO-LOCK:
        FOR EACH funcionario WHERE funcionario.cdn_empresa = empresa.ep-codigo
                               AND funcionario.cdn_estab = rh_estab.cdn_estab NO-LOCK:
            
            FIND FIRST cargo WHERE cargo.cdn_cargo_basic = funcionario.cdn_cargo_basic
                               AND cargo.cdn_niv_cargo = funcionario.cdn_niv_cargo NO-LOCK NO-ERROR. 
                
                FIND last histor_sal_func WHERE histor_sal_func.cdn_empresa = empresa.ep-codigo
                                            AND histor_sal_func.cdn_estab = rh_estab.cdn_estab
                                            AND histor_sal_func.cdn_funcionario = funcionario.cdn_funcionario
                                            AND histor_sal_func.cdn_niv_cargo = funcionario.cdn_niv_cargo NO-LOCK NO-ERROR.

                    FIND FIRST unid_lotac WHERE unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac NO-LOCK NO-ERROR.

                        FIND FIRST cargo_basic WHERE cargo_basic.cdn_cargo_basic = cargo.cdn_cargo_basic NO-LOCK NO-ERROR.
                            
                            FIND LAST func_ccusto WHERE func_ccusto.cdn_empresa      =   funcionario.cdn_empresa     
                                                    AND func_ccusto.cod_rh_ccusto    =   funcionario.cod_rh_ccusto   
                                                    AND func_ccusto.cdn_estab        =   funcionario.cdn_estab       
                                                    AND func_ccusto.cdn_funcionario  =   funcionario.cdn_funcionario NO-LOCK NO-ERROR.
                                
                               FIND FIRST rh_ccusto WHERE rh_ccusto.cdn_empresa = funcionario.cdn_empresa
                                                       AND rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.


                                    FIND rh_pessoa_fisic WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-LOCK NO-ERROR.
                                
                                                

        
                                  DISP empresa.ep-codigo                LABEL "Empresa"
                                       rh_estab.cdn_estab               LABEL "Estabelecimento"
                                       rh_estab.cod_id_feder            LABEL "CNPJ"
                                       funcionario.cdn_funcionario      LABEL "Matricula"
                                       funcionario.cdn_cargo_basic      LABEL "Cargo_ID"
                                       cargo.des_cargo                  LABEL "Cargo_Nome"
                                       cargo_basic.cod_classif_ocupac   LABEL "CBO"
                                       histor_sal_func.dat_liber_sal    LABEL "Data_Mud_Cargo"
                                       funcionario.cod_unid_lotac
                                       unid_lotac.des_unid_lotac
                                       func_ccusto.dat_inic_lotac_func  LABEL "Data_Mud_Area"
                                       funcionario.cod_rh_ccusto        LABEL "Setor_ID"
                                       rh_ccusto.des_rh_ccusto          LABEL "Setor_Nome"
                                       funcionario.nom_pessoa_fisic
                                       funcionario.dat_nascimento
                                       funcionario.dat_admis_func
                                       rh_pessoa_fisic.idi_sexo
                                       rh_pessoa_fisic.idi_estado_civil LABEL "Est_Civil"
                                       rh_pessoa_fisic.cod_id_feder
                                       rh_pessoa_fisic.cod_id_estad_fisic
                                       rh_pessoa_fisic.cod_orgao_emis_id_estad
                                       rh_pessoa_fisic.cod_unid_federac_emis_estad
                                       funcionario.cod_pis
                                       funcionario.cod_cart_trab
                                       funcionario.cod_ser_cart_trab
                                       funcionario.cod_unid_federac_cart_trab
                                       funcionario.dat_cart_trab
                                       funcionario.dat_valid_cart_trab
                                       rh_pessoa_fisic.log_pessoa_fisic_doador
                                       WITH 1 COL WIDTH 300.


        END.

    END.

END.
