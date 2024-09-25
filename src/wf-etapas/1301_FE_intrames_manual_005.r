#!/usr/bin/env Rscript

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )
#------------------------------------------------------------------------------

atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )
  
  return(  length( atributos ) == length( comun ) )
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas
  
  # el mes 1,2, ..12
  if( atributos_presentes( c("foto_mes") ))
    dataset[, kmes := foto_mes %% 100]
  
  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  if( atributos_presentes( c("ctrx_quarter") ))
    dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
  
  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  
  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  
  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[
      cliente_antiguedad == 3,
      ctrx_quarter_normalizado := ctrx_quarter * 1.2
    ]
  
  # variable extraida de una tesis de maestria de Irlanda
  if( atributos_presentes( c("mpayroll", "cliente_edad") ))
    dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]
  
  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  if( atributos_presentes( c("Master_status", "Visa_status") ))
  {
    dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
    dataset[, vm_status02 := Master_status + Visa_status]
    
    dataset[, vm_status03 := pmax(
      ifelse(is.na(Master_status), 10, Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status)
    )]
    
    dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
            + ifelse(is.na(Visa_status), 10, Visa_status)]
    
    dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
            + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]
    
    dataset[, vm_status06 := ifelse(is.na(Visa_status),
                                    ifelse(is.na(Master_status), 10, Master_status),
                                    Visa_status
    )]
    
    dataset[, mv_status07 := ifelse(is.na(Master_status),
                                    ifelse(is.na(Visa_status), 10, Visa_status),
                                    Master_status
    )]
  }
  
  
  # combino MasterCard y Visa
  if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
    dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_Fvencimiento", "Visa_Fvencimiento") ))
    dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_Finiciomora", "Visa_Finiciomora") ))
    dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_msaldototal", "Visa_msaldototal") ))
    dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_msaldopesos", "Visa_msaldopesos") ))
    dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_msaldodolares", "Visa_msaldodolares") ))
    dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mconsumospesos", "Visa_mconsumospesos") ))
    dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mconsumosdolares", "Visa_mconsumosdolares") ))
    dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mlimitecompra", "Visa_mlimitecompra") ))
    dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_madelantopesos", "Visa_madelantopesos") ))
    dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_madelantodolares", "Visa_madelantodolares") ))
    dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_fultimo_cierre", "Visa_fultimo_cierre") ))
    dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mpagado", "Visa_mpagado") ))
    dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mpagospesos", "Visa_mpagospesos") ))
    dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mpagosdolares", "Visa_mpagosdolares") ))
    dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_fechaalta", "Visa_fechaalta") ))
    dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mconsumototal", "Visa_mconsumototal") ))
    dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_cconsumos", "Visa_cconsumos") ))
    dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_cadelantosefectivo", "Visa_cadelantosefectivo") ))
    dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  
  if( atributos_presentes( c("Master_mpagominimo", "Visa_mpagominimo") ))
    dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]
  
  # a partir de aqui juego con la suma de Mastercard y Visa
  if( atributos_presentes( c("Master_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  
  if( atributos_presentes( c("Visa_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_msaldototal", "vm_mlimitecompra") ))
    dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_msaldopesos", "vm_mlimitecompra") ))
    dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_msaldopesos", "vm_msaldototal") ))
    dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  
  if( atributos_presentes( c("vm_msaldodolares", "vm_mlimitecompra") ))
    dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_msaldodolares", "vm_msaldototal") ))
    dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  
  if( atributos_presentes( c("vm_mconsumospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mconsumosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_madelantopesos", "vm_mlimitecompra") ))
    dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_madelantodolares", "vm_mlimitecompra") ))
    dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mpagado", "vm_mlimitecompra") ))
    dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mpagospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mpagosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mconsumototal", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  
  if( atributos_presentes( c("vm_mpagominimo", "vm_mlimitecompra") ))
    dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]
  
  # Aqui debe usted agregar sus propias nuevas variables
  
  
  
  # Primera tanda
  if( atributos_presentes(c("mcaja_ahorro", "mcuenta_corriente" ,"mplazo_fijo_pesos","mplazo_fijo_dolares",
                            "minversion1_pesos" ,"minversion1_dolares" ,"minversion2")))
    dataset[,activos_cliente := mcaja_ahorro + mcuenta_corriente + mplazo_fijo_pesos + mplazo_fijo_dolares +
              minversion1_pesos + minversion1_dolares + minversion2]
  
  if( atributos_presentes(c("mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios")))
    dataset[,deuda_cliente_prestamos := mprestamos_personales + mprestamos_prendarios + 
              mprestamos_hipotecarios]
  
  if( atributos_presentes(c("activos_cliente","deuda_cliente_prestamos")))
    dataset[, ratio_deudas_activos := deuda_cliente_prestamos / activos_cliente]
  
  
  # Segunda tanda
  
  
  if(atributos_presentes(c("ctarjeta_debito_transacciones", "ctarjeta_master_transacciones", 
                           "ctarjeta_visa_transacciones")))
    dataset[, comparacion_debito_credito := ctarjeta_debito_transacciones / 
              (ctarjeta_visa_transacciones + ctarjeta_master_transacciones)]
  
  if(atributos_presentes(c("mtarjeta_visa_consumo", "mtarjeta_master_consumo", 
                           "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones")))
    dataset[, promedio_transacciones_credito := (mtarjeta_visa_consumo + mtarjeta_master_consumo) / 
              (ctarjeta_visa_transacciones + ctarjeta_master_transacciones)]
  
  if(atributos_presentes(c("cliente_antiguedad", "cproductos")))
    dataset[, adopcion_productos := cliente_antiguedad / cproductos]
  
  if(atributos_presentes(c("Master_Finiciomora", "Master_Fvencimiento")))
    dataset[, tendencia_morosidad_mastercard := Master_Finiciomora - Master_Fvencimiento]
  
  if(atributos_presentes(c("Visa_Finiciomora", "Visa_Fvencimiento")))
    dataset[, tendencia_morosidad_visa := Visa_Finiciomora - Visa_Fvencimiento]
  
  if(atributos_presentes(c("Master_msaldo_total", "Master_mlimitecompra")))
    dataset[, saldo_master_credito := Master_msaldo_total / Master_mlimitecompra]
  
  if(atributos_presentes(c("Visa_msaldo_total", "Visa_mlimitecompra")))
    dataset[, saldo_visa_credito := Visa_msaldo_total / Visa_mlimitecompra]
  
  if(atributos_presentes(c("Master_msaldo_total", "Visa_msaldo_total", "Master_mlimitecompra", "Visa_mlimitecompra")))
    dataset[, saldo_total_tarjetas_credito := (Master_msaldo_total + Visa_msaldo_total) / 
              (Master_mlimitecompra + Visa_mlimitecompra)]
  
  if(atributos_presentes(c("mtarjeta_master_descuentos", "Master_msaldo_total")))
    dataset[, descuentos_sobre_gastos_master := mtarjeta_master_descuentos / Master_msaldo_total]
  
  if(atributos_presentes(c("mtarjeta_visa_descuentos", "Visa_msaldo_total")))
    dataset[, descuentos_sobre_gastos_visa := mtarjeta_visa_descuentos / Visa_msaldo_total]
  
  if(atributos_presentes(c("mtarjeta_master_descuentos", "mtarjeta_visa_descuentos", "Master_msaldo_total", "Visa_msaldo_total")))
    dataset[, descuentos_totales_gastos := (mtarjeta_master_descuentos + mtarjeta_visa_descuentos) / 
              (Master_msaldo_total + Visa_msaldo_total)]
  
  
  # Tercera tanda
  
  if(atributos_presentes(c("cproductos")))
    dataset[, flag_cproductos := ifelse(cproductos > mean(cproductos),1,0)]
  
  if(atributos_presentes(c("cliente_antiguedad", "cproductos")))
    dataset[, cproductos_vs_antiguedad := cproductos / cliente_antiguedad]
  
  if(atributos_presentes(c("ccaja_ahorro", "ccuenta_corriente","cliente_antiguedad")))
    dataset[, ccuentas_vs_antiguedad := (ccaja_ahorro + ccuenta_corriente) / cliente_antiguedad]
  
  if(atributos_presentes(c("ctarjeta_visa_transacciones")))
    dataset[ ,flag_ctrans_visa := ifelse(ctarjeta_visa_transacciones > mean(ctarjeta_visa_transacciones),1,0)]
  
  if(atributos_presentes(c("ctarjeta_master_transacciones")))
    dataset[ ,flag_ctrans_master := ifelse(ctarjeta_master_transacciones > mean(ctarjeta_master_transacciones),1,0)]
  
  if(atributos_presentes(c("ctarjeta_debito_transacciones")))
    dataset[ ,flag_ctrans_debito := ifelse(ctarjeta_debito_transacciones > mean(ctarjeta_debito_transacciones),1,0)]
  
  if(atributos_presentes(c("cprestamos_hipotecarios")))
    dataset[, flag_cprestamos_hipotecarios := ifelse(cprestamos_hipotecarios > 0, 1, 0)]
  
  if(atributos_presentes(c("cprestamos_prendarios")))
    dataset[, flag_cprestamos_prendarios := ifelse(cprestamos_prendarios > 0, 1, 0)]
  
  if(atributos_presentes(c("cprestamos_personales")))
    dataset[, flag_cprestamos_personales := ifelse(cprestamos_personales > 0, 1, 0)]
  
  if(atributos_presentes(c("cseguro_vida")))
    dataset[, flag_cseguro_vida := ifelse(cseguro_vida > 0, 1, 0)]
  
  if(atributos_presentes(c("cseguro_auto")))
    dataset[, flag_cseguro_auto := ifelse(cseguro_auto > 0, 1, 0)]
  
  if(atributos_presentes(c("cseguro_vivienda")))
    dataset[, flag_cseguro_vivienda := ifelse(cseguro_vivienda > 0, 1, 0)]
  
  if(atributos_presentes(c("cpayroll2_trx")))
    dataset[, flag_cpayroll2_trx := ifelse(cpayroll2_trx > 0, 1, 0)]
  
  if(atributos_presentes(c("cpayroll_trx")))
    dataset[, flag_cpayroll_trx := ifelse(cpayroll_trx > 5, 1, 0)]
  
  if(atributos_presentes(c("ccuenta_debitos_automaticos")))
    dataset[, flag_ccuenta_debitos_automaticos := ifelse(ccuenta_debitos_automaticos > 0, 1, 0)]
  
  if(atributos_presentes(c("ccuenta_debitos_automaticos")))
    dataset[, flag_ccuenta_debitos_automaticos_exagerado := ifelse(ccuenta_debitos_automaticos > 9, 1, 0)]
  
  if(atributos_presentes(c("ctarjeta_visa_debitos_automaticos")))
    dataset[, flag_ctarjeta_visa_debitos_automaticos := ifelse(ctarjeta_visa_debitos_automaticos > 0, 1, 0)]
  
  if(atributos_presentes(c("ctarjeta_visa_debitos_automaticos")))
    dataset[, flag_ctarjeta_visa_debitos_automaticos_exagerado := ifelse(ctarjeta_visa_debitos_automaticos > 9, 1, 0)]
  
  if(atributos_presentes(c("ctarjeta_master_debitos_automaticos")))
    dataset[, flag_ctarjeta_master_debitos_automaticos := ifelse(ctarjeta_master_debitos_automaticos > 0, 1, 0)]
  
  if(atributos_presentes(c("ctarjeta_master_debitos_automaticos")))
    dataset[, flag_ctarjeta_master_debitos_automaticos_exagerado := ifelse(ctarjeta_master_debitos_automaticos > 9, 1, 0)]
  
  if(atributos_presentes(c("ctarjeta_master_debitos_automaticos","ctarjeta_visa_debitos_automaticos",
                           "ccuenta_debitos_automaticos")))
    dataset[, flag_total_debitos_automaticos_exagerado := 
              ifelse(ctarjeta_master_debitos_automaticos + ctarjeta_visa_debitos_automaticos + 
                       ccuenta_debitos_automaticos > 15,1,0)]
  
  if(atributos_presentes(c("cpagodeservicios")))
    dataset[, flag_paga_ventanilla := ifelse(cpagodeservicios > 0, 1, 0)]
  
  
  if(atributos_presentes(c("cpagodeservicios","cliente_edad")))
    dataset[, flag_ventanilla_viejito := ifelse(cpagodeservicios > 0 & cliente_edad >49, 1, 0)]
  
  if(atributos_presentes(c("cpagomiscuentas")))
    dataset[, flag_cpagomiscuentas := ifelse(cpagomiscuentas > 0, 1, 0)]
  
  if(atributos_presentes(c("cpagomiscuentas")))
    dataset[, flag_cpagomiscuentas_masquemedia := ifelse(cpagomiscuentas > mean(cpagomiscuentas), 1, 0)]
  
  
  
  # Cuarta tanda
  
  if(atributos_presentes(c("ctarjeta_debito_transacciones","ctarjeta_visa_transacciones","ctarjeta_master_transacciones",
                           "cpagodeservicios","cpagomiscuentas","cforex","ctransferencias_recibidas",
                           "ctransferencias_emitidas","cextraccion_autoservicio","ccheques_depositados",
                           "ccallcenter_transacciones","chomebanking_transacciones","ccajas_transacciones",
                           "ccajas_depositos","catm_trx","catm_trx_other","cmobile_app_trx","Master_cconsumos",
                           "Master_cadelantosefectivo","Visa_cconsumos","Visa_cadelantosefectivo")))
    dataset[, cantidad_total_transacciones := ctarjeta_debito_transacciones + ctarjeta_visa_transacciones + 
              ctarjeta_master_transacciones + cpagodeservicios + cpagomiscuentas + cforex + ctransferencias_recibidas + 
              ctransferencias_emitidas + cextraccion_autoservicio + ccheques_depositados + ccallcenter_transacciones + 
              chomebanking_transacciones + ccajas_transacciones + ccajas_depositos + catm_trx + catm_trx_other + 
              cmobile_app_trx + Master_cconsumos + Master_cadelantosefectivo + Visa_cconsumos + Visa_cadelantosefectivo]
  
  if(atributos_presentes(c("cliente_edad")))
    dataset[, segmento_edad_joven := ifelse(cliente_edad <= 35, 1, 0)]
  
  if(atributos_presentes(c("cliente_edad")))
    dataset[, segmento_edad_adulto := ifelse(cliente_edad > 35 & cliente_edad <= 60, 1, 0)]
  
  if(atributos_presentes(c("cliente_edad")))
    dataset[, segmento_edad_senior := ifelse(cliente_edad >= 60, 1, 0)]
  
  if(atributos_presentes(c("Master_mpagominimo","Visa_mpagominimo","mpayroll","mpayroll2")))
    dataset[, responsabilidad_comprador := (Master_mpagominimo + Visa_mpagominimo) / (mpayroll + mpayroll2)]
  
  if(atributos_presentes(c("Master_mconsumospesos","Visa_mconsumospesos","mpayroll","mpayroll2")))
    dataset[, responsabilidad_comprador2 := (Master_mconsumospesos + Visa_mconsumospesos) / (mpayroll + mpayroll2)]
  
  if(atributos_presentes(c("ccomisiones_mantenimiento","ccomisiones_otras")))
    dataset[, cantidad_total_comisiones := ccomisiones_mantenimiento + ccomisiones_otras]
  
  if(atributos_presentes(c("mcomisiones_mantenimiento","mcomisiones_otras")))
    dataset[, monto_total_comisiones := mcomisiones_mantenimiento + mcomisiones_otras]
  
  if(atributos_presentes(c("mcomisiones_mantenimiento","mcomisiones_otras","cproductos")))
    dataset[, ratio_comisiones_productos := (mcomisiones_mantenimiento +  mcomisiones_otras) / cproductos]
  
  
  
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )
  
  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }
  
  
  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )
  
  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )
    
    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }
  
  
  
  # Aca agregamos los primeros 20 componentes principales que salen haciendo PCA
  
  
  cat("Aca empieza a correr PCA. Suerte.\n")
  
  datasetsinNA <- dataset
  datasetsinNA[is.na(datasetsinNA)] <- 0
  pca_datos <- prcomp(datasetsinNA[,1:154],center=TRUE,scale=TRUE)
  rm(datasetsinNA) # borramos este dataset creado para ahorrar espacio
  autovec_pca <- as.data.table(pca_datos$x[,1:20])
  head(autovec_pca)
  rm(pca_datos) # borramos para ahorrar espacio
  dataset <-- dataset[, names(autovec_pca) := autovec_pca]
  head(dataset)
  rm(autovec_pca) # borramos para ahorrar espacio
  
  cat("Si llegaste hasta aca es porque PCA no te hizo volar por los aires la corrida. Felicitaciones.\n")
  
  
  
  
  
  
  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "1301_FE_intrames_manual_005.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
       file = "dataset.csv.gz",
       logical01 = TRUE,
       sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
       file = "dataset.campos.txt",
       sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "1301_FE_intrames_manual_005.r  END\n")
