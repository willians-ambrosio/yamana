/*FOR EACH ilha.es_centro_custo EXCLUSIVE-LOCK.
    DELETE ilha.es_centro_custo.
END.

FOR EACH ilha.es_conta EXCLUSIVE-LOCK.
    DELETE ILHA.es_conta.
END.

FOR EACH ilha.es_contrato_for EXCLUSIVE-LOCK.
    DELETE ilha.es_contrato_for.                          
END.

FOR EACH ilha.es_departamento EXCLUSIVE-LOCK.
    DELETE ilha.es_departamento.
END.


FOR EACH es_it_departamento EXCLUSIVE-LOCK.
    DELETE es_it_departamento.
END.

FOR EACH ilha.es_deposito EXCLUSIVE-LOCK.
    DELETE ilha.es_deposito.
END.

FOR EACH ilha.es_familia EXCLUSIVE-LOCK.
    DELETE ilha.es_familia.
END.
*/

/*FOR EACH es_importacao EXCLUSIVE-LOCK.
    DELETE es_importacao.
END.


FOR EACH ilha.es_importacao_imposto EXCLUSIVE-LOCK.
    DELETE ilha.es_importacao_imposto.
END.

FOR EACH es_item EXCLUSIVE-LOCK.
    DELETE es_item.
END.

FOR EACH es_item_uni_estab EXCLUSIVE-LOCK.
    DELETE es_item_uni_estab.
END.

FOR EACH es_moeda EXCLUSIVE-LOCK.
    DELETE es_moeda.
END.

FOR EACH es_movimentaca_estoque EXCLUSIVE-LOCK.
    DELETE es_movimentaca_estoque.
END.

FOR EACH es_natur_oper EXCLUSIVE-LOCK.
    DELETE es_natur_oper.
END.


FOR EACH es_ordem_compra EXCLUSIVE-LOCK.
    DELETE es_ordem_compra.
END.


FOR EACH es_ordem_embarque EXCLUSIVE-LOCK.
    Delete es_ordem_embarque.
END.

FOR EACH es_ordem_inv EXCLUSIVE-LOCK.
    DELETE  es_ordem_inv.
END.


FOR EACH es_ped_compr EXCLUSIVE-LOCK.
    DELETE es_ped_compr.
END.

FOR EACH es_prazo_compr EXCLUSIVE-LOCK.
    DELETE es_prazo_compr.
END.


FOR EACH es_recebimento EXCLUSIVE-LOCK.
    DELETE es_recebimento.
END.
*/

FOR EACH es_sub_conta EXCLUSIVE-LOCK.
    DELETE es_sub_conta.
END.


FOR EACH es_tipo_transacao EXCLUSIVE-LOCK.
    DELETE es_tipo_transacao.

END.



FOR EACH es_tab_unidade EXCLUSIVE-LOCK.
    DELETE es_tab_unidade.
END.


FOR EACH es_usuar_mater EXCLUSIVE-LOCK.
    DELETE es_usuar_mater.
END.
