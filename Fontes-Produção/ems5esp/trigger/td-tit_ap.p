/* ------- Defini‡Æo de Parƒmetros ------ */
def param buffer bf-tit_ap for tit_ap.

{utp/ut-glob.i}

create hist-tit-eliminados.
assign hist-tit-eliminados.sequencia           = recid(hist-tit-eliminados)
       hist-tit-eliminados.ep-codigo           = bf-tit_ap.cod_empresa
       hist-tit-eliminados.cod-estabel         = bf-tit_ap.cod_estab
       hist-tit-eliminados.cod-esp             = bf-tit_ap.cod_espec_docto
       hist-tit-eliminados.cod-fornec          = bf-tit_ap.cdn_fornecedor
       hist-tit-eliminados.serie               = bf-tit_ap.cod_ser_docto
       hist-tit-eliminados.nr-docto            = bf-tit_ap.cod_tit_ap
       hist-tit-eliminados.parcela             = bf-tit_ap.cod_parcela
       hist-tit-eliminados.vl-original         = bf-tit_ap.val_origin_tit_ap
       hist-tit-eliminados.valor-saldo         = bf-tit_ap.val_sdo_tit_ap
       hist-tit-eliminados.usuario-elim-canc   = c-seg-usuario
       hist-tit-eliminados.data-elim-canc      = today
       hist-tit-eliminados.hora-elim-canc      = string(time,"hh:mm:ss")
       hist-tit-eliminados.ind-pend-atualizado = 2 /* Atualizado */
       hist-tit-eliminados.ind-implan-baixa    = 2 /* Implanta‡Æo Atualizado */
       hist-tit-eliminados.data-1              = bf-tit_ap.dat_emis_docto
       hist-tit-eliminados.data-2              = bf-tit_ap.dat_transacao
       hist-tit-eliminados.char-1              = "AP"
       hist-tit-eliminados.ind-modul-dtsul     = 2. /* AP */
