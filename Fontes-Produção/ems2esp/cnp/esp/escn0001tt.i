define temp-table tt-contrato-cc00 no-undo
    field im-nr-contrat        like contrato-for.nr-contrato
    field im-cod-emitente      like contrato-for.cod-emitente
    field im-impr-contrat      like contrato-for.impr-contrat
    field im-cod-tipo-contrat  like contrato-for.cod-tipo-contrat
    field im-natureza          like contrato-for.natureza
    field im-gestor-tecnico    like contrato-for.gestor-tecnico
    field im-frete             like contrato-for.frete
    field im-variacao-qtd      like contrato-for.variacao-qtd
    field im-variacao-preco    like contrato-for.variacao-preco
    field im-cod-mensagem      like pedido-compr.cod-mensagem
    field im-cod-cond-pag      like contrato-for.cod-cond-pag
    field im-cod-transp        like pedido-compr.cod-transp
    field im-via-transp        like pedido-compr.via-transp
    field im-val-total         like contrato-for.val-total
    field im-cod-comprado      like contrato-for.cod-comprado
    field im-ind-sit-contrat   like contrato-for.ind-sit-contrat
    field im-qtd-total         like contrato-for.qtd-total
    field im-sld-qtd           like contrato-for.sld-qtd
    field im-sld-val           like contrato-for.sld-val
    field im-acum-rec-qtd      like contrato-for.acum-rec-qtd
    field im-acum-rec-val      like contrato-for.acum-rec-val
    field im-sld-qtd-liber     like contrato-for.sld-qtd-liber
    field im-sld-val-liber     like contrato-for.sld-val-liber
    field im-val-fatur-minimo  like contrato-for.val-fatur-minimo
    field im-dat-ini-validade  like contrato-for.dt-ini-validade
    field im-dat-fim-validade  like contrato-for.dt-ter-validade
    field im-dat-contrat       like contrato-for.dt-contrato
    field im-dec-2             like contrato-for.dec-2
    field im-cod-estabel       like contrato-for.cod-estabel
    field im-cod-estab-cobr    like contrato-for.cod-estab-cobr
    field im-cod-estab-orig    like contrato-for.cod-estab-orig
    field im-cod-estab-entr    like contrato-for.cod-estab-entr
    INDEX contrato AS PRIMARY UNIQUE
        im-nr-contrat.

define temp-table tt-contrato-cc01 no-undo
    field im-nr-contrat        like contrato-for.nr-contrato
    field im-cod-emitente      like contrato-for.cod-emitente
    field im-des-contrat       like contrato-for.des-contrat
    field im-acum-val-pago     like contrato-for.acum-val-pago
    field im-dat-revisao       like contrato-for.dat-revisao
    field im-mo-codigo         like contrato-for.mo-codigo
    field im-log-libera        like contrato-for.log-libera
    field im-tp-fornecim       like contrato-for.tp-fornecim
    field im-contato           like contrato-for.contato
    field im-ind-control-rec   like contrato-for.ind-control-rec
    field im-sld-qtd-med       like contrato-for.sld-qtd-med
    field im-sld-val-med       like contrato-for.sld-val-med
    field im-sld-qtd-liber-med like contrato-for.sal-qtd-liber-med
    field im-sld-val-liber-med like contrato-for.sld-val-liber-med
    field im-cod-projeto       like contrato-for.cod-projeto
    field im-cod-cond-fatur    like contrato-for.cod-cond-fatur
    field im-sld-val-receb     like contrato-for.sld-val-receb
    field im-narrat-contrat    like contrato-for.narrat-contrat
    field im-num-ord-inv       like contrato-for.num-ord-inv
    INDEX contrato AS PRIMARY UNIQUE
        im-nr-contrat.

define temp-table tt-item-contrato-ic00 no-undo
    field im-nr-contrat           like contrato-for.nr-contrato
    field im-num-seq-item         like item-contrat.num-seq-item
    field im-cod-emitente         like contrato-for.cod-emitente
    field im-it-codigo            like item-contrat.it-codigo
    field im-preco-unit           like item-contrat.preco-unit
    field im-qtd-minima           like item-contrat.qtd-minima
    field im-sld-val              like contrato-for.sld-val
    field im-val-fatur-minimo     like contrato-for.val-fatur-minimo
    field im-mo-codigo            like contrato-for.mo-codigo
    field im-log-libera           like contrato-for.log-libera
    field im-val-total            like contrato-for.val-total
    field im-cod-refer            like item-contrat.cod-refer
    field im-codigo-ipi           like item-contrat.codigo-ipi
    field im-codigo-icm           like item-contrat.codigo-icm
    field im-un                   like item-contrat.un
    field im-contato              like contrato-for.contato
    field im-frequencia           like item-contrat.frequencia
    field im-ind-sit-item         like item-contrat.ind-sit-item
    field im-qtd-total            like contrato-for.qtd-total
    field im-ind-un-contrato      like item-contrat.ind-un-contrato
    field im-sld-qtd              like contrato-for.sld-qtd
    field im-acum-rec-val         like contrato-for.acum-rec-val
    field im-acum-rec-qtd         like contrato-for.acum-rec-qtd
    field im-ind-tipo-control-val like item-contrat.ind-tipo-control-val
    field im-sld-qtd-liber        like contrato-for.sld-qtd-liber
    field im-sld-val-liber        like contrato-for.sld-val-liber
    field im-log-control-event    like item-contrat.log-control-event
    field im-ind-caract-item      like item-contrat.ind-caract-item
    field im-log-caract-item      like item-contrat.ind-caract-item
    field im-log-obrig-item       like item-contrat.log-obrig-item
    field im-log-ind-multa        like item-contrat.log-ind-multa
    field im-perc-multa-dia       like item-contrat.perc-multa-dia
    field im-perc-multa-limite    like item-contrat.perc-multa-limite
    field im-cod-depos            like item-contrat.cod-depos
    field im-aliquota-icm         like item-contrat.aliquota-icm
    field im-aliquota-ipi         like item-contrat.aliquota-ipi
    field im-aliquota-iss         like item-contrat.aliquota-iss
    field im-tp-despesa           like item-contrat.tp-despesa
    field im-cod-cond-pag         like contrato-for.cod-cond-pag
    field im-frete-ped            like item-contrat.frete
    INDEX item-contrat AS PRIMARY UNIQUE
        im-nr-contrat
        im-num-seq-item.

define temp-table tt-item-contrato-ic01 no-undo
    field im-nr-contrat           like contrato-for.nr-contrato
    field im-num-seq-item         like item-contrat.num-seq-item
    field im-cod-emitente         like contrato-for.cod-emitente
    field im-it-codigo            like item-contrat.it-codigo
    field im-preco-fornec         like item-contrat.preco-fornec
    field im-taxa-financ          like item-contrat.taxa-financ
    field im-val-frete            like item-contrat.val-frete      
    field im-val-taxa             like item-contrat.val-taxa        
    field im-prazo-ent            like item-contrat.prazo-ent  
    field im-dat-cotac            like item-contrat.dat-cotac     
    field im-preco-base           like item-contrat.preco-base   
    field im-cod-comprado         like contrato-for.cod-comprado
    field im-perc-desconto        like item-contrat.perc-desconto
    field im-narrat-compra        like item-contrat.narrat-compra
    field im-ind-tipo-control     like item-contrat.ind-tipo-control
    field im-pre-unit-for         like item-contrat.pre-unit-for  
    field im-dat-base             like item-contrat.dat-base         
    field im-sld-qtd-receb        like item-contrat.sld-qtd-rece
    field im-sld-val-receb        like contrato-for.sld-val-receb
    field im-narrat-item          like item-contrat.narrat-item
    FIELD im-num-ord-invest       LIKE item-contrat.num-ord-inv
    field im-ordem-base           like item-contrat.ordem-base 
    FIELD im-perc-compra          AS   DECIMAL
    INDEX item-contrat AS PRIMARY UNIQUE
        im-nr-contrat
        im-num-seq-item.
