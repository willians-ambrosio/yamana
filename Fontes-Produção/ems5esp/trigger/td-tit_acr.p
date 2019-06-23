/* ------- Defini‡Æo de Parƒmetros ------ */
def param buffer bf-tit_acr for tit_acr.

{utp/ut-glob.i}

create hist-tit-eliminados.
assign hist-tit-eliminados.sequencia           = recid(hist-tit-eliminados)
       hist-tit-eliminados.ep-codigo           = bf-tit_acr.cod_empresa
       hist-tit-eliminados.cod-estabel         = bf-tit_acr.cod_estab
       hist-tit-eliminados.cod-esp             = bf-tit_acr.cod_espec_docto
       hist-tit-eliminados.cod-fornec          = bf-tit_acr.cdn_cliente
       hist-tit-eliminados.serie               = bf-tit_acr.cod_ser_docto
       hist-tit-eliminados.nr-docto            = bf-tit_acr.cod_tit_acr
       hist-tit-eliminados.parcela             = bf-tit_acr.cod_parcela
       hist-tit-eliminados.vl-original         = bf-tit_acr.val_origin_tit_acr
       hist-tit-eliminados.valor-saldo         = bf-tit_acr.val_sdo_tit_acr
       hist-tit-eliminados.usuario-elim-canc   = c-seg-usuario
       hist-tit-eliminados.data-elim-canc      = today
       hist-tit-eliminados.hora-elim-canc      = string(time,"hh:mm:ss")
       hist-tit-eliminados.data-1              = bf-tit_acr.dat_emis_docto
       hist-tit-eliminados.data-2              = bf-tit_acr.dat_transacao
       hist-tit-eliminados.char-1              = "CR"
       hist-tit-eliminados.ind-modul-dtsul     = 1. /* CR */
