define temp-table tt-pedido no-undo
    field im-num-pedido            like pedido-compr.num-pedido
    field im-condicao-ped          like pedido-compr.cod-cond-pag
    field im-natureza-ped          like pedido-compr.natureza
    field im-emitente-ped          like pedido-compr.cod-emitente
    field im-data-pedido           like pedido-compr.data-pedido
    field im-situacao-ped          like pedido-compr.situacao 
    field im-responsavel           like pedido-compr.responsavel 
    field im-end-cobranca          like pedido-compr.end-cobranca
    field im-end-entrega           like pedido-compr.end-entrega
    field im-frete-ped             like pedido-compr.frete 
    field im-cod-transp            like pedido-compr.cod-transp
    field im-via-transp            like pedido-compr.via-transp
    field im-cod-mensagem          like pedido-compr.cod-mensagem
    field im-cargo-ass             like pedido-compr.cargo-ass
    field im-nome-ass              like pedido-compr.nome-ass
    field im-mot-elimina           like pedido-compr.mot-elimina 
    field im-impr-pedido           like pedido-compr.impr-pedido
    field im-emergencial           like pedido-compr.emergencial
    field im-nr-prox-ped           like pedido-compr.nr-prox-ped
    field im-contr-forn            like pedido-compr.contr-forn
    field im-nr-processo           like pedido-compr.nr-processo
    field im-comentario            like pedido-compr.comentario
    field im-frete                 as char format "x(01)"
    field im-nr-contrato           like pedido-compr.nr-contrato.

define temp-table tt-contrato no-undo
    field im-cod-transp        like pedido-compr.cod-transp
    field im-via-transp        like pedido-compr.via-transp 
    field im-cod-mensagem      like pedido-compr.cod-mensagem 
    field de-tipo              as character format "x(2)" 
    field de-sequencia         as integer format "99"
    field c-linha              as character format "x(180)"
    field im-cod-emitente      like contrato-for.cod-emitente
    field im-nr-contrat        like contrato-for.nr-contrato
    field im-impr-contrat      like contrato-for.impr-contrat
    field im-cod-tipo-contrat  like contrato-for.cod-tipo-contrat
    field im-natureza          like contrato-for.natureza
    field im-gestor-tecnico    like contrato-for.gestor-tecnico
    field im-frete             like contrato-for.frete
    field im-variacao-qtd      like contrato-for.variacao-qtd
    field im-variacao-preco    like contrato-for.variacao-preco
    field im-cod-cond-pag      like contrato-for.cod-cond-pag
    field im-cod-estabel       like contrato-for.cod-estabel
    field im-cod-estab-cobr    like contrato-for.cod-estab-cobr
    field im-cod-estab-orig    like contrato-for.cod-estab-orig
    field im-val-total         like contrato-for.val-total
    field im-cod-comprado      like contrato-for.cod-comprado
    field im-cod-estab-entr    like contrato-for.cod-estab-entr
    field im-dat-ini-validade  like contrato-for.dt-ini-validade
    field im-dat-fim-validade  like contrato-for.dt-ter-validade
    field im-dat-contrat       like contrato-for.dt-contrato
    field im-ind-sit-contrat   like contrato-for.ind-sit-contrat
    field im-qtd-total         like contrato-for.qtd-total
    field im-sld-qtd           like contrato-for.sld-qtd
    field im-sld-val           like contrato-for.sld-val
    field im-acum-rec-qtd      like contrato-for.acum-rec-qtd
    field im-acum-rec-val      like contrato-for.acum-rec-val
    field im-sld-qtd-liber     like contrato-for.sld-qtd-liber
    field im-sld-val-liber     like contrato-for.sld-val-liber
    field im-val-fatur-minimo  like contrato-for.val-fatur-minimo
    field im-des-contrat       like contrato-for.des-contrat
    field im-acum-val-pago     like contrato-for.acum-val-pago
    field im-dat-revisao       like contrato-for.dat-revisao
    field im-mo-codigo         like contrato-for.mo-codigo
    field im-log-libera        like contrato-for.log-libera
    field im-tp-fornecim       like contrato-for.tp-fornecim
    field im-contato           like contrato-for.contato
    field im-sld-qtd-med       like contrato-for.sld-qtd-med
    field im-sld-val-med       like contrato-for.sld-val-med
    field im-sld-qtd-liber-med like contrato-for.sal-qtd-liber-med
    field im-sld-val-liber-med like contrato-for.sld-val-liber-med
    field im-cod-projeto       like contrato-for.cod-projeto
    field im-cod-cond-fatur    like contrato-for.cod-cond-fatur
    field im-ind-control-rec   like contrato-for.ind-control-rec
    field im-sld-val-receb     like contrato-for.sld-val-receb
    field im-narrat-contrat    like contrato-for.narrat-contrat
    field im-num-ord-inv       like contrato-for.num-ord-inv
    field im-dec-2             like contrato-for.dec-2
    field im-perc-alert-saldo  like contrato-for.perc-alert-saldo
    field im-email-alert       like contrato-for.email-alert.

define temp-table tt-item-contrato no-undo
    field im-preco-unit           like item-contrat.preco-unit
    field im-qtd-minima           like item-contrat.qtd-minima
    field im-it-codigo            like item-contrat.it-codigo   
    field im-cod-refer            like item-contrat.cod-refer     
    field im-codigo-ipi           like item-contrat.codigo-ipi           
    field im-codigo-icm           like item-contrat.codigo-icm     
    field im-un                   like item-contrat.un      
    field im-num-seq-item         like item-contrat.num-seq-item
    field im-frequencia           like item-contrat.frequencia    
    field im-ind-sit-item         like item-contrat.ind-sit-item    
    field im-ind-un-contrato      like item-contrat.ind-un-contrato
    field im-ind-tipo-control-val like item-contrat.ind-tipo-control-val
    field im-log-control-event    like item-contrat.log-control-event          
    field im-log-caract-item      like item-contrat.ind-caract-item
    field im-log-obrig-item       like item-contrat.log-obrig-item          
    field im-log-ind-multa        like item-contrat.log-ind-multa
    field im-perc-multa-dia       like item-contrat.perc-multa-dia
    field im-perc-multa-limite    like item-contrat.perc-multa-limite
    field im-cod-depos            like item-contrat.cod-depos
    field im-aliquota-icm         like item-contrat.aliquota-icm
    field im-aliquota-ipi         like item-contrat.aliquota-ipi
    field im-aliquota-iss         like item-contrat.aliquota-iss
    field im-preco-fornec         like item-contrat.preco-fornec
    field im-taxa-financ          like item-contrat.taxa-financ
    field im-tp-despesa           like item-contrat.tp-despesa
    field im-val-frete            like item-contrat.val-frete      
    field im-val-taxa             like item-contrat.val-taxa        
    field im-prazo-ent            like item-contrat.prazo-ent  
    field im-dat-cotac            like item-contrat.dat-cotac     
    field im-preco-base           like item-contrat.preco-base   
    field im-perc-desconto        like item-contrat.perc-desconto
    field im-narrat-compra        like item-contrat.narrat-compra
    field im-ind-tipo-control     like item-contrat.ind-tipo-control
    field im-pre-unit-for         like item-contrat.pre-unit-for  
    field im-dat-base             like item-contrat.dat-base         
    field im-ordem-base           like item-contrat.ordem-base 
    field im-cod-emitente         like contrato-for.cod-emitente
    field im-sld-qtd-receb        like item-contrat.sld-qtd-rece
    field im-nr-contrat           like contrato-for.nr-contrato
    field im-sld-val              like contrato-for.sld-val
    field im-val-fatur-minimo     like contrato-for.val-fatur-minimo
    field im-mo-codigo            like contrato-for.mo-codigo
    field im-log-libera           like contrato-for.log-libera
    field im-val-total            like contrato-for.val-total
    field im-contato              like contrato-for.contato
    field im-qtd-total            like contrato-for.qtd-total
    field im-acum-rec-val         like contrato-for.acum-rec-val
    field im-acum-rec-qtd         like contrato-for.acum-rec-qtd
    field im-sld-qtd-liber        like contrato-for.sld-qtd-liber
    field im-sld-qtd              like contrato-for.sld-qtd
    field im-sld-val-liber        like contrato-for.sld-val-liber
    field im-ind-caract-item      like item-contrat.ind-caract-item
    field im-cod-cond-pag         like contrato-for.cod-cond-pag
    field im-frete-ped            like item-contrat.frete
    field im-cod-comprado         like contrato-for.cod-comprado
    field im-sld-val-receb        like contrato-for.sld-val-receb
    field im-narrat-item          like item-contrat.narrat-item
    FIELD im-perc-compra          AS   DECIMAL
    FIELD im-num-ord-invest       LIKE item-contrat.num-ord-invest.

define temp-table tt-matriz-contrato no-undo
    field im-nr-contrat   like contrato-for.nr-contrato
    field im-perc-rateio  like matriz-rat-contr.perc-rateio
    field im-ct-codigo     AS CHAR
    FIELD im-sc-codigo     AS CHAR
    field im-cod-unid-negoc like matriz-rat-contr.cod-unid-negoc.

define temp-table tt-matriz-item no-undo
    field im-nr-contrat     like contrato-for.nr-contrato
    field im-ct-codigo     AS CHAR
    FIELD im-sc-codigo     AS CHAR
    field im-perc-rateio    like matriz-rat-contr.perc-rateio
    field im-num-seq-item   like item-contrat.num-seq-item
    field im-it-codigo      like item-contrat.it-codigo
    field im-cod-unid-negoc like matriz-rat-contr.cod-unid-negoc.

define temp-table tt-formula no-undo
    field im-nr-contrat     like contrato-for.nr-contrato
    field im-dat-preco-base like formula-reaj.dat-preco-base
    field im-num-seq-item   like item-contrat.num-seq-item
    field im-mo-codigo      like contrato-for.mo-codigo
    field im-dat-prox-reaj  like formula-reaj.dat-prox-reaj 
    field im-periodo        like formula-reaj.periodo     
    field im-log-reajuste   like formula-reaj.log-reajuste     
    field im-dat-indice     like formula-reaj.dat-indice
    field im-perc-reaj      like formula-reaj.perc-reaj
    field im-mo-codigo1     like contrato-for.mo-codigo extent 5.

define temp-table tt-receb no-undo
    field im-cod-emitente  like recebimento.cod-emitente
    field im-codigo-rejei  like recebimento.codigo-rejei
    field im-data-atualiz  like recebimento.data-atualiz
    field im-data-movto    like recebimento.data-movto
    field im-data-nota     like recebimento.data-nota
    field im-hora-atualiz  like recebimento.hora-atualiz
    field im-it-codigo     like recebimento.it-codigo
    field im-num-pedido    like recebimento.num-pedido
    field im-numero-nota   like recebimento.numero-nota
    field im-numero-ordem  like recebimento.numero-ordem
    field im-parcela       like recebimento.parcela
    field im-pre-unit-for  like recebimento.pre-unit-for
    field im-preco-unit    like recebimento.pre-unit
    field im-qtd-rec-forn  like recebimento.qtd-rec-forn
    field im-qtd-rej-forn  like recebimento.qtd-rej-forn
    field im-quant-receb   like recebimento.quant-receb
    field im-quant-rejeit  like recebimento.quant-rejeit
    field im-recebedor     like recebimento.recebedor
    field im-serie-nota    like recebimento.serie-nota
    field im-usuario       like recebimento.usuario
    field im-valor-total   like recebimento.valor-total
    field im-aliquota-icm  like recebimento.aliquota-icm
    field im-aliquota-ipi  like recebimento.aliquota-ipi
    field im-aliquota-iss  like recebimento.aliquota-iss
    field im-valor-icm     like recebimento.valor-icm
    field im-valor-ipi     like recebimento.valor-ipi
    field im-valor-iss     like recebimento.valor-iss
    field im-sit-ord       like ordem-compra.situacao
    field im-sit-par       like prazo-compra.situacao
    field im-cod-movto     like recebimento.cod-movto.
  
define temp-table tt-modelo-evento no-undo
    field im-nr-contrat        like contrato-for.nr-contrato
    field im-cod-cond-pag      like contrato-for.cod-cond-pag
    field im-cod-comprado      like contrato-for.cod-comprado
    field im-dat-event         like event-mod-contrat.dat-event
    field im-qtd-prevista      like event-mod-contrat.qtd-prevista
    field im-val-previsto      like event-mod-contrat.val-previsto
    field im-perc-lib-previsto like event-mod-contrat.perc-lib-previsto
    field im-log-dest-event    like event-mod-contrat.ind-dest-event
    field im-num-seq-event-mod like event-mod-contrat.num-seq-event-mod
    field im-num-seq-item      like item-contrat.num-seq-item
    field im-it-codigo         like item-contrat.it-codigo
    field im-prazo-dat-realiz  like event-mod-contrat.prazo-dat-realiz.  

define temp-table tt-condicao no-undo
    field im-ce-np-condicao     like cond-especif.num-pedido
    field im-ce-perc-pagto      like cond-especif.perc-pagto
    field im-ce-data-pagto      like cond-especif.data-pagto
    field im-ce-coment-cond     like cond-especif.comentarios
    field im-condicao-ped       like pedido-compr.cod-cond-pag.

define temp-table tt-ordem-compra no-undo
    field im-or-preco-orig   like ordem-compra.preco-orig
    field im-or-ind-reajuste like ordem-compra.ind-reajuste
    field im-or-linha        like ordem-compra.linha
    field im-or-cod-refer    like ordem-compra.cod-refer
    field im-or-nr-processo  like ordem-compra.nr-processo
    field im-or-ind-extrac   like ordem-compra.ind-extrac
    field im-or-reaj-tabela  like ordem-compra.reaj-tabela
    field im-or-num-ordem    like ordem-compra.numero-ordem 
    field im-or-it-codigo    like ordem-compra.it-codigo
    field im-or-cod-estabel  like ordem-compra.cod-estabel
    field im-or-ep-codigo    like estabelec.ep-codigo
    field im-or-dep-almoxar  like ordem-compra.dep-almoxar
    field im-or-ct-codigo    like ordem-compra.ct-codigo
    field im-or-sc-codigo    like ordem-compra.sc-codigo
    field im-or-natureza     like ordem-compra.natureza
    field im-or-situacao     like ordem-compra.situacao
    field im-or-origem       like ordem-compra.origem
    field im-or-data-emissao like ordem-compra.data-emissao
    field im-or-codigo-icm   like ordem-compra.codigo-icm
    field im-or-requisitante like ordem-compra.requisitante 
    field im-or-ordem-servic like ordem-compra.ordem-servic   
    field im-or-cod-comprado like ordem-compra.cod-comprado 
    field im-or-impr-ficha   like ordem-compra.impr-ficha 
    field im-or-aliquota-icm like ordem-compra.aliquota-icm   
    field im-or-aliquota-ipi like ordem-compra.aliquota-ipi   
    field im-or-aliquota-iss like ordem-compra.aliquota-iss   
    field im-or-cod-cond-pag like ordem-compra.cod-cond-pag  
    field im-or-cod-emitente like ordem-compra.cod-emitente   
    field im-or-codigo-ipi   like ordem-compra.codigo-ipi      
    field im-or-comentarios  like ordem-compra.comentarios    
    field im-or-contato      like ordem-compra.contato        
    field im-or-data-atualiz like ordem-compra.data-atualiz 
    field im-or-hora-atualiz like ordem-compra.hora-atualiz format "x(08)"
    field im-or-data-cotacao as date format "99/99/9999"
    field im-or-data-pedido  like ordem-compra.data-pedido format "99/99/9999"
    field im-or-frete        like ordem-compra.frete   
    field im-or-mo-codigo    like ordem-compra.mo-codigo      
    field im-or-nr-alt-preco like ordem-compra.nr-alt-preco   
    field im-or-nr-ord-orig  like ordem-compra.nr-ord-orig    
    field im-or-num-pedido   like ordem-compra.num-pedido     
    field im-or-op-codigo    like ordem-compra.op-codigo      
    field im-or-perc-descto  like ordem-compra.perc-descto    
    field im-or-prazo-entreg like ordem-compra.prazo-entreg   
    field im-or-pre-unit-for like ordem-compra.pre-unit-for   
    field im-or-preco-fornec like ordem-compra.preco-fornec   
    field im-or-preco-unit   like ordem-compra.preco-unit      
    field im-or-taxa-financ  like ordem-compra.taxa-financ      
    field im-or-usuario      like ordem-compra.usuario        
    field im-or-valor-frete  like ordem-compra.valor-frete    
    field im-or-valor-taxa   like ordem-compra.valor-taxa     
    field im-or-narrativa    like ordem-compra.narrativa
    field im-or-sit-oc       like pedido-compr.situacao
    field im-or-num-seq      like item-contrat.num-seq-item
    field im-nr-contrato     like ordem-compra.nr-contrato. 

define  temp-table tt-parcela no-undo
    field im-pa-qtd-do-forn     like prazo-compra.qtd-do-forn 
    field im-pa-qtd-rec-forn    like prazo-compra.qtd-rec-forn
    field im-pa-qtd-rej-forn    like prazo-compra.qtd-rej-forn
    field im-pa-qtd-sal-forn    like prazo-compra.qtd-sal-forn
    field im-pa-quant-receb     like prazo-compra.quant-receb
    field im-pa-quant-rejeit    like prazo-compra.quant-rejeit
    field im-pa-quant-saldo     like prazo-compra.quant-saldo 
    field im-pa-quantid-orig    like prazo-compra.quantid-orig 
    field im-pa-cod-alter       like prazo-compra.cod-alter 
    field im-pa-inte-entrega    as integer format "99999999" 
    field im-pa-data-orig       like prazo-compra.data-orig
    field im-pa-data-alter      like prazo-compra.data-alter
    field im-pa-usuario-alt     like prazo-compra.usuario-alt
    field im-pa-nr-alt-data     like prazo-compra.nr-alt-data
    field im-pa-nr-alt-quant    like prazo-compra.nr-alt-quant
    field im-pa-natureza        like prazo-compra.natureza
    field im-pa-cod-refer       like prazo-compra.cod-refer
    field im-pa-parcela         like prazo-compra.parcela 
    field im-pa-situacao        like prazo-compra.situacao 
    field im-pa-un              like prazo-compra.un 
    field im-pa-quantidade      like prazo-compra.quantidade 
    field im-pa-data-entrega    like prazo-compra.data-entrega 
    field im-pa-nome-abrev      like prazo-compra.nome-abrev 
    field im-pa-pedido-clien    like prazo-compra.pedido-clien 
    field im-or-num-ordem       like ordem-compra.numero-ordem  
    field im-or-it-codigo       like ordem-compra.it-codigo. 

define temp-table tt-cotacao no-undo
    field im-co-preco-unit   like cotacao-item.preco-unit
    field im-co-pre-unit-for like cotacao-item.pre-unit-for
    field im-co-codigo-icm   like cotacao-item.codigo-icm
    field im-co-aliquota-icm like cotacao-item.aliquota-icm
    field im-co-dt-atualiz   like cotacao-item.data-atualiz
    field im-co-usuario      like cotacao-item.usuario
    field im-co-hora-atualiz like cotacao-item.hora-atualiz
    field im-co-ind-reajuste like cotacao-item.ind-reajuste
    field im-co-possui-reaj  like cotacao-item.possui-reaj
    field im-co-reaj-tabela  like cotacao-item.reaj-tabela
    field im-co-narrativa    like cotacao-item.narrativa
    field im-co-cod-emitente like cotacao-item.cod-emitente   
    field im-co-data-cotacao like cotacao-item.data-cotacao   
    field im-co-un           like cotacao-item.un             
    field im-co-preco-fornec like cotacao-item.preco-fornec   
    field im-co-mo-codigo    like cotacao-item.mo-codigo      
    field im-co-codigo-ipi   like cotacao-item.codigo-ipi      
    field im-co-aliquota-ipi like cotacao-item.aliquota-ipi    
    field im-co-aliquota-iss like cotacao-item.aliquota-iss   
    field im-co-frete        like cotacao-item.frete      
    field im-co-valor-frete  like cotacao-item.valor-frete    
    field im-co-taxa-financ  like cotacao-item.taxa-financ      
    field im-co-valor-taxa   like cotacao-item.valor-taxa     
    field im-co-perc-descto  like cotacao-item.perc-descto    
    field im-co-cod-cond-pag like cotacao-item.cod-cond-pag  
    field im-co-prazo-entreg like cotacao-item.prazo-entreg   
    field im-co-contato      like cotacao-item.contato         
    field im-co-cod-comprado like cotacao-item.cod-comprado  
    field im-co-cot-aprovada like cotacao-item.cot-aprovada     
    field im-co-aprovador    like cotacao-item.aprovador      
    field im-co-motivo-apr   like cotacao-item.motivo-apr     
    field im-or-num-ordem    like ordem-compra.numero-ordem
    field im-or-it-codigo    like ordem-compra.it-codigo. 

define temp-table tt-item-contrat-estab no-undo
    field im-acum-rec-qtd  like item-contrat-estab.acum-rec-qtd
    field im-acum-rec-val  like item-contrat-estab.acum-rec-val
    field im-cod-emitente  like item-contrat-estab.cod-emitente
    field im-cod-estabel   like item-contrat-estab.cod-estabel
    field im-it-codigo     like item-contrat-estab.it-codigo
    field im-nr-contrato   like item-contrat-estab.nr-contrato
    field im-num-seq-item  like item-contrat-estab.num-seq-item
    field im-qtd-total     like item-contrat-estab.qtd-total
    field im-sld-qtd       like item-contrat-estab.sld-qtd
    field im-sld-qtd-liber like item-contrat-estab.sld-qtd-liber
    field im-sld-qtd-receb like item-contrat-estab.sld-val-receb
    field im-sld-val       like item-contrat-estab.sld-val
    field im-sld-val-liber like item-contrat-estab.sld-val-liber
    field im-sld-val-receb like item-contrat-estab.sld-val-receb 
    field im-val-total     like item-contrat-estab.val-total.

DEFINE TEMP-TABLE tt-desp-item-contrat NO-UNDO
    field im-nr-contrato        LIKE desp-item-contrat.nr-contrato
    field im-num-seq-item       LIKE desp-item-contrat.num-seq-item
    field im-num-seq-desp       LIKE desp-item-contrat.num-seq-desp
    field im-cod-despesa        LIKE desp-item-contrat.cod-despesa
    field im-val-despesa        LIKE desp-item-contrat.val-despesa
    field im-val-per-desp       LIKE desp-item-contrat.val-per-desp
    field im-dsl-narrativa      AS CHAR FORMAT "x(2000)" LABEL "Narrativa".

DEFINE TEMP-TABLE tt-desp-cotacao-item NO-UNDO
    field im-numero-ordem       LIKE desp-cotacao-item.numero-ordem
    field im-cod-emitente       LIKE desp-cotacao-item.cod-emitente
    field im-it-codigo          LIKE desp-cotacao-item.it-codigo
    field im-seq-cotac          LIKE desp-cotacao-item.seq-cotac
    field im-num-seq-desp       LIKE desp-cotacao-item.num-seq-desp
    field im-cod-despesa        LIKE desp-cotacao-item.cod-despesa
    field im-val-despesa        LIKE desp-cotacao-item.val-despesa
    field im-val-per-desp       LIKE desp-cotacao-item.val-per-desp
    field im-dsl-narrativa      AS CHAR FORMAT "x(2000)" LABEL "Narrativa".

define temp-table tt-anexo-contrat no-undo
    field i-num-seq-anexo            like anexo-contrat.num-seq-anexo
    field i-nr-contrato              like anexo-contrat.nr-contrato
    field c-usuario                  like anexo-contrat.usuario
    field c-des-anexo                like anexo-contrat.des-anexo
    field c-narrat-anexo             like anexo-contrat.narrat-anexo.

define temp-table tt-pedido-auto no-undo
    field im-num-pedido            like pedido-compr.num-pedido
    field im-nr-contrato           like pedido-compr.nr-contrato
    field im-end-entrega           like pedido-compr.end-entrega
    field im-end-cobranca          like pedido-compr.end-cobranca
    field im-condicao-ped          like pedido-compr.cod-cond-pag
    field im-cod-emit-terc         like pedido-compr.cod-emit-terc.

def var l-descr-prob as character format "x(68)" extent 100 init        
       ["01-Contrato de Compra nao Cadastrado                                ", 
        "02-Emitente nao Cadastrado                                          ",
        "03-Moeda nao Cadastrada                                             ",
        "04-Item nao Cadastrado                                              ",
        "05-Unidade de Medida nao Cadastrada                                 ",
        "06-Item do Contrato ja Cadastrado                                   ", 
        "07-Item do Contrato nao Cadastrado                                  ",
        "08-Deposito nao Cadastrado                                          ",
        "09-Comprador nao Cadastrado                                         ",
        "10-Condicao de Pagamento nao Cadastrada                             ",
        "11-Pedido de Compra nao Cadastrado                                  ",
        "12-Estabelecimento nao Cadastrado                                   ",
        "13-Conta Contabil nao Cadastrada                                    ",
        "14-Tipo da Conta Improprio para Movimentacao                        ",
        "15-Ordem de Compra nao Cadastrada                                   ",
        "16-Tabela Invalida: Controle p/ Recebimento da NF deve ser 1 - Total",
        "17-Tabela Invalida: Indicador do Item no Contr. deve ser 1 - Medio  ",
        "18-Contrato de Compra ja Cadastrado                                 ",
        "19-Formula de Reajuste ja Cadastrada                                ",
        "20-Pedido do Contrato ja Cadastrado                                 ",
        "21-Modelo do Contrato ja Cadastrado                                 ",
        "22-Matriz do Rateio do Item do Contrato ja Cadastrada               ",
        "23-Ordem do Contrato ja Cadastrada                                  ",
        "24-Natureza deve ser C ou S ou B                                    ",
        "25-Situacao do Pedido deve ser I ou N ou E                          ",
        "26-Transportador nao Cadastrado                                     ",
        "27-Via de Transporte Invalida                                       ",
        "28-Pedido de Compra deve ser de Contrato e nao de Processo          ",
        "29-Pedido de Compra ja Cadastrado                                   ",
        "30-Percentual Acumulado deve ser 100%                               ",
        "31-Pedido de Compra nao possui Condicao Especifica                  ",
        "32-Condicao Especifica ja Cadastrada                                ",
        "33-Condicao Especifica nao Cadastrada                               ",
        "34-Item esta Obsoleto                                               ",
        "35-Conta deve ser de Ativo, Passivo ou Ativo e de Sistema           ",
        "36-Natureza deve ser C ou S ou B                                    ",
        "37-Situacao da Ordem/Parcela Invalida                               ",
        "38-Origem da Ordem Invalida                                         ",
        "39-Ordem de Compra ja Cadastrada                                    ",
        "40-Unidade de Medida esta diferente do Item                         ",
        "41-Parcela ja Cadastrada                                            ",
        "42-Quantidade deve ser maior que zero                               ",
        "43-Unidade de Medida da Parcela nao Cadastrada                      ",
        "44-Fornecedor da Cotacao nao Cadastrada                             ",
        "45-Cotacao do Item ja Cadastrada                                    ",
        "46-Cotacao do Item nao Cadastrada                                   ",
        "47-Matriz do Rateio do Contrato ja Cadastrada                       ",
        "48-Contrato de Compra ja Cadastrado                                 ",
        "49-Item j† cadastrado em outro contrato do mesmo fornecedor         ", 
        "50-Estabelecimento de entrega ja cadastrado                         ",
        "51-Despesa n∆o cadastrada                                           ",
        "52-Despesa da cotacao do item j† cadastrada                         ",
        "53-Despesa do item do contrato j† cadastrada                        ",
        "54-O percentual da despesa deve ser maior que 0(zero)               ",
        "55-O valor da despesa deve ser maior que 0(zero)                    ",
        "56-O estabelecimento n∆o tem acesso a utilizar unidade de neg¢cio.  ",
        "57-Usu†rio n∆o tem acesso a utilizar unidade de neg¢cio.            ",
        "58-Unidade de Neg¢cio n∆o cadastrada.                               ",
        "59-Conta nao utiliza centro de custo.                               ",
        "60-Percentual de compra do fornecedor inv†lido.                     ",
        "61-Percentual de compra do fornecedor Ç diferente do cadastrado.    ",
        "62-Item possui Ordem de Investimentos mas n∆o possui valor.         ",
        "63-Contrato possui Ordem de Investimentos mas n∆o possui valor.     ",
        "64-Percentual de compra do fornecedor Ç diferente de 100%.          ",
        "65-Contrato j† possui Ordem de Investimentos informada.             ",
        "66-N∆o h† n£mero de pedido dispon°vel.                              ",
        "67-N∆o existe numeraá∆o de ordem dispon°vel.                        ",
        "68-Percentual de alerta de saldo deve ser maior que zero.           ",
        "69-Endereáo de e-mail deve ser informado.                           ",
        "70-Estabelecimento n∆o est† relacionado Ö empresa.                  ",
        "71-Necess†rio informar valor total para itens com Ordem de Investimento.",
        "72-Tipo de controle do contrato incompativel com tipo de controle do item.",
        "73-Necess†rio informar se item est† ativo ou inativo."].
