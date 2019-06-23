/****************************************************************************
**
**  cc0305.i3 - Display do cabecalho do pedido (contratos)
**
****************************************************************************/
do:
 assign c-desc-frete   = {ininc/i03in295.i 04 pedido-compr.frete}.       
 if avail cotacao-item then
    assign c-desc-destino = {ininc/i01in082.i 04 cotacao-item.codigo-icm}.
    disp c-desc-contrato
         pedido-compr.num-pedido
         c-natureza
         pedido-compr.data-pedido
         pedido-compr.cod-cond-pag
         c-desc-var-1
         /*c-desc-var-2*/
         pedido-compr.cod-transp
         transporte.nome-abrev   when avail transporte
         transporte.telefone     when avail transporte
         c-desc-frete @ pedido-compr.frete
         c-desc-destino @ cotacao-item.codigo-icm
         emitente.cod-emitente
         emitente.nome-emit
         emitente.nome-abrev
         emitente.endereco
         emitente.bairro
         emitente.cep
         emitente.cidade
         emitente.estado
         emitente.pais
         emitente.caixa-postal
         emitente.telefone[1]
         emitente.ramal[1]
         emitente.telefone[2]
         emitente.ramal[2]
         emitente.telef-fac
         emitente.telefax
         emitente.telex
         c-cgc-emit @ emitente.cgc
         emitente.ins-estadual
         estabelec.cod-estabel
         estabelec.nome
         estabelec.endereco
         estabelec.bairro
         estabelec.cep
         estabelec.cidade
         estabelec.estado
         estabelec.pais
         c-cgc @ estabelec.cgc
         estabelec.ins-estadual
         with frame f-contrato.
end.         

/* Fim do Include */
