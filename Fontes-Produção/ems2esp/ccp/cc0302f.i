/*************************************************************************
** 
**      CC0302f.I - Definicao Temp-Table TT-Cotacao - 
**      utilizada no envio B2B , e no adpter
** 
*************************************************************************/

def temp-table tt-cotacao
    field numero-ordem       like cotacao-item.numero-ordem
    field cod-estabel        like ordem-compra.cod-estabel
  /*field cod-emitente       like cotacao-item.cod-emitente*/
    field nome-abrev         like emitente.nome-abrev
    field data-emissao       like ordem-compra.data-emissao
    field it-codigo          like cotacao-item.it-codigo
    field referencia         as char
    field ean-code           &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like item-mat.cod-ean
                             &else
                             as char
                             &endif
    field desc-item          like item.desc-item
    field narrativa-item     like item.narrativa
    field endereco-orig      like estabelec.endereco
    field bairro-orig        like emitente.bairro
    field cidade-orig        like emitente.cidade
    field uf-orig            like emitente.estado
    field cep-orig           like estabelec.cep
    field pais-orig          like emitente.pais
  /*field endereco-dest      like estabelec.endereco
    field bairro-dest        like emitente.bairro
    field cidade-dest        like emitente.cidade
    field uf-dest            like emitente.estado
    field cep-dest           like emitente.cep
    field pais-dest          like emitente.pais*/
    field narrativa          like ordem-compra.narrativa
    field narrativa-processo like proc-compra.narrativa
    field contato-orig-nome  like usuar-mater.cod-usuario
    field contato-orig-fone  like usuar-mater.telefone extent 0
    field contato-orig-ramal like usuar-mater.ramal    extent 0
    field contato-orig-fax   like usuar-mater.telefax
    field contato-orig-email like usuar-mater.e-mail
  /*field contato-dest-nome  like ordem-compra.contato
    field contato-dest-fone  like cont-emit.telefone
    field contato-dest-email like cont-emit.e-mail*/
  /********  DatasulNet ********/
    field ind-aberta         &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.log-cot-aberta
                             &else
                             as logical initial false format "Sim/N∆o" column-label "Aberta"
                             &endif
    field ind-rfq            &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.log-leilao
                             &else
                             as logical initial false format "Sim/N∆o" column-label "Leil∆o"
                             &endif
    field data-abertura      &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.dat-inicio-leilao-rfq
                             &else
                             as date    initial ?
                             &endif
    field data-fechamento    &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.dat-fim-leilao-rfq
                             &else
                             as date    initial ?
                             &endif
    field hora-abertura      &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.hra-inicio-leilao-rfq
                             &else
                             as char    initial ""
                             &endif
    field hora-fechamento    &if  '{&BF_DIS_VERSAO_EMS}' >= '2.05' &then
                             like ordem-compra.hra-fim-leilao-rfq
                             &else
                             as char    initial ""
                             &endif
  /*field ind-aberta         as logical initial false format "Sim/N∆o" column-label "Aberta"
    field ind-rfq            as logical initial false format "Sim/N∆o" column-label "Leil∆o"
    field data-abertura      as date    initial ?
    field hora-abertura      as char    initial ""
    field data-fechamento    as date    initial ?
    field hora-fechamento    as char    initial ""*/
    field nr-processo        like ordem-compra.nr-processo
    field condicao-pagamento as char
    field comentarios        as char
    index ordem-emitente is primary unique
          numero-ordem.
        /*cod-emitente.*/

def temp-table tt-fornecedores-envio
    field numero-ordem       like ordem-compra.numero-ordem
    field cod-emitente       like ordem-compra.cod-emitente
    field nome-abrev         like emitente.nome-abrev
    field nome-emit          like emitente.nome-emit
  /*field contato-dest-nome  like cont-emit.nome*/
    field endereco           like emitente.endereco
    field cidade             like emitente.cidade
    field bairro             like emitente.bairro
    field estado             like emitente.estado
    field pais               like emitente.pais
    field cep                like emitente.cep
    field contato-dest-fone  like emitente.telefone extent 0
    field contato-dest-ramal like emitente.ramal    extent 0
    field contato-dest-email like emitente.e-mail
    field contato-dest-fax   like emitente.telefax
    index ix_tt-fornecedores-envio is primary numero-ordem.

/********  DatasulNet ***********/

def temp-table tt-prazo-compra
    field numero-ordem    like prazo-compra.numero-ordem
  /*field cod-emitente    like cotacao-item.cod-emitente*/
    field parcela         like prazo-compra.parcela
    field data-entrega    like prazo-compra.data-entrega
    field frete           as char format "x(3)"
    field narrativa       like ordem-compra.narrativa
    field quantidade      like prazo-compra.quantidade
    field un              like prazo-compra.un
    field cod-transp      like ordem-compra.cod-transp
    field nome-transp     like transporte.nome-abrev
    field contato-transp  like cont-tran.nome
    field e-mail-transp   like cont-tran.e-mail
    field telefone-transp like cont-tran.telefone
    index ordem-emitente is primary unique
          numero-ordem
        /*cod-emitente*/
          parcela.
