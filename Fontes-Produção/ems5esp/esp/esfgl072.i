        FOR EACH tt_es_cons_lotes BY {1} BY {2}:

            DISP STREAM str-rp tt_es_cons_lotes.cod_empresa_dest
                               tt_es_cons_lotes.num_lote_orig
                               tt_es_cons_lotes.des_lote_orig
                               tt_es_cons_lotes.num_lancto_orig
                               tt_es_cons_lotes.num_lote_dest
                               tt_es_cons_lotes.des_lote_dest
                               tt_es_cons_lotes.num_lancto_dest
                               tt_es_cons_lotes.cod_mod_orig WITH FRAME f-lote.

            IF tt_param.rs_tipo_log = "Detalhado" THEN DO:
                FOR EACH tt_msg_erro_es_lote WHERE tt_msg_erro_es_lote.ROW_es_cons_lotes = tt_es_cons_lotes.ROW_es_cons_lotes NO-LOCK:
                    DISP STREAM str-rp tt_es_cons_lotes.dt_lote_dest
                                       tt_es_cons_lotes.dt_lancto_orig
                                       tt_msg_erro_es_lote.des_msg_erro FORMAT 'X(100)' WITH FRAME f-detalhe-lote.
                END.

                /*run pi_print_editor(tt_es_cons_lotes.resultado, 105).*/

                if  can-find(first tt-editor) then
                    put stream str-rp space(3) c-resultado.

                for each tt-editor:
                    put stream str-rp tt-editor.conteudo format "x(150)" at 15.
                end.

                if  can-find(first tt-editor) then
                    put stream str-rp skip(1).
            end.
        end.

