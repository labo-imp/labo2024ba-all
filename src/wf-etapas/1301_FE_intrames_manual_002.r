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

  
  # Genero la totalidad de los debitos automaticos
  
  if( atributos_presentes( c("ccuenta_debitos_automaticos", "mcuenta_debitos_automaticos") ))
    dataset[, da_ctas := ccuenta_debitos_automaticos + mcuenta_debitos_automaticos]
  
  if( atributos_presentes( c("ctarjeta_visa_debitos_automaticos", "mttarjeta_visa_debitos_automaticos") ))
    dataset[, da_visa := ctarjeta_visa_debitos_automaticos + mttarjeta_visa_debitos_automaticos]
  
  if( atributos_presentes( c("ctarjeta_master_debitos_automaticos", "mttarjeta_master_debitos_automaticos") ))
    dataset[, da_master := ctarjeta_master_debitos_automaticos + mttarjeta_master_debitos_automaticos]
  
  if( atributos_presentes( c("da_ctas", "da_visa", "da_master") ))
    dataset[, da_total := da_ctas + da_visa + da_master]
  
  # Comparo esta variable con el resto de las variables
  
  # Verificación y creación de la variable da_total_numero_de_cliente
  if(atributos_presentes(c("da_total", "numero_de_cliente"))) {
    dataset[, da_total_numero_de_cliente := numero_de_cliente / da_total]
  }
  
  # Verificación y creación de la variable da_total_foto_mes
  if(atributos_presentes(c("da_total", "foto_mes"))) {
    dataset[, da_total_foto_mes := foto_mes / da_total]
  }
  
  # Verificación y creación de la variable da_total_active_quarter
  if(atributos_presentes(c("da_total", "active_quarter"))) {
    dataset[, da_total_active_quarter := active_quarter / da_total]
  }
  
  # Verificación y creación de la variable da_total_cliente_vip
  if(atributos_presentes(c("da_total", "cliente_vip"))) {
    dataset[, da_total_cliente_vip := cliente_vip / da_total]
  }
  
  # Verificación y creación de la variable da_total_internet
  if(atributos_presentes(c("da_total", "internet"))) {
    dataset[, da_total_internet := internet / da_total]
  }
  
  # Verificación y creación de la variable da_total_cliente_edad
  if(atributos_presentes(c("da_total", "cliente_edad"))) {
    dataset[, da_total_cliente_edad := cliente_edad / da_total]
  }
  
  # Verificación y creación de la variable da_total_cliente_antiguedad
  if(atributos_presentes(c("da_total", "cliente_antiguedad"))) {
    dataset[, da_total_cliente_antiguedad := cliente_antiguedad / da_total]
  }
  
  # Verificación y creación de la variable da_total_mrentabilidad
  if(atributos_presentes(c("da_total", "mrentabilidad"))) {
    dataset[, da_total_mrentabilidad := mrentabilidad / da_total]
  }
  
  # Verificación y creación de la variable da_total_mrentabilidad_annual
  if(atributos_presentes(c("da_total", "mrentabilidad_annual"))) {
    dataset[, da_total_mrentabilidad_annual := mrentabilidad_annual / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcomisiones
  if(atributos_presentes(c("da_total", "mcomisiones"))) {
    dataset[, da_total_mcomisiones := mcomisiones / da_total]
  }
  
  # Verificación y creación de la variable da_total_mactivos_margen
  if(atributos_presentes(c("da_total", "mactivos_margen"))) {
    dataset[, da_total_mactivos_margen := mactivos_margen / da_total]
  }
  
  # Verificación y creación de la variable da_total_mpasivos_margen
  if(atributos_presentes(c("da_total", "mpasivos_margen"))) {
    dataset[, da_total_mpasivos_margen := mpasivos_margen / da_total]
  }
  
  # Verificación y creación de la variable da_total_cproductos
  if(atributos_presentes(c("da_total", "cproductos"))) {
    dataset[, da_total_cproductos := cproductos / da_total]
  }
  
  # Verificación y creación de la variable da_total_tcuentas
  if(atributos_presentes(c("da_total", "tcuentas"))) {
    dataset[, da_total_tcuentas := tcuentas / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccuenta_corriente
  if(atributos_presentes(c("da_total", "ccuenta_corriente"))) {
    dataset[, da_total_ccuenta_corriente := ccuenta_corriente / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcuenta_corriente_adicional
  if(atributos_presentes(c("da_total", "mcuenta_corriente_adicional"))) {
    dataset[, da_total_mcuenta_corriente_adicional := mcuenta_corriente_adicional / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcuenta_corriente
  if(atributos_presentes(c("da_total", "mcuenta_corriente"))) {
    dataset[, da_total_mcuenta_corriente := mcuenta_corriente / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccaja_ahorro
  if(atributos_presentes(c("da_total", "ccaja_ahorro"))) {
    dataset[, da_total_ccaja_ahorro := ccaja_ahorro / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcaja_ahorro
  if(atributos_presentes(c("da_total", "mcaja_ahorro"))) {
    dataset[, da_total_mcaja_ahorro := mcaja_ahorro / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcaja_ahorro_adicional
  if(atributos_presentes(c("da_total", "mcaja_ahorro_adicional"))) {
    dataset[, da_total_mcaja_ahorro_adicional := mcaja_ahorro_adicional / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcaja_ahorro_dolares
  if(atributos_presentes(c("da_total", "mcaja_ahorro_dolares"))) {
    dataset[, da_total_mcaja_ahorro_dolares := mcaja_ahorro_dolares / da_total]
  }
  
  # Verificación y creación de la variable da_total_cdescubierto_preacordado
  if(atributos_presentes(c("da_total", "cdescubierto_preacordado"))) {
    dataset[, da_total_cdescubierto_preacordado := cdescubierto_preacordado / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcuentas_saldo
  if(atributos_presentes(c("da_total", "mcuentas_saldo"))) {
    dataset[, da_total_mcuentas_saldo := mcuentas_saldo / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_debito
  if(atributos_presentes(c("da_total", "ctarjeta_debito"))) {
    dataset[, da_total_ctarjeta_debito := ctarjeta_debito / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_debito_transacciones
  if(atributos_presentes(c("da_total", "ctarjeta_debito_transacciones"))) {
    dataset[, da_total_ctarjeta_debito_transacciones := ctarjeta_debito_transacciones / da_total]
  }
  
  # Verificación y creación de la variable da_total_mautoservicio
  if(atributos_presentes(c("da_total", "mautoservicio"))) {
    dataset[, da_total_mautoservicio := mautoservicio / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa
  if(atributos_presentes(c("da_total", "ctarjeta_visa"))) {
    dataset[, da_total_ctarjeta_visa := ctarjeta_visa / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa_transacciones
  if(atributos_presentes(c("da_total", "ctarjeta_visa_transacciones"))) {
    dataset[, da_total_ctarjeta_visa_transacciones := ctarjeta_visa_transacciones / da_total]
  }
  
  # Verificación y creación de la variable da_total_mtarjeta_visa_consumo
  if(atributos_presentes(c("da_total", "mtarjeta_visa_consumo"))) {
    dataset[, da_total_mtarjeta_visa_consumo := mtarjeta_visa_consumo / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master
  if(atributos_presentes(c("da_total", "ctarjeta_master"))) {
    dataset[, da_total_ctarjeta_master := ctarjeta_master / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master_transacciones
  if(atributos_presentes(c("da_total", "ctarjeta_master_transacciones"))) {
    dataset[, da_total_ctarjeta_master_transacciones := ctarjeta_master_transacciones / da_total]
  }
  
  # Verificación y creación de la variable da_total_mtarjeta_master_consumo
  if(atributos_presentes(c("da_total", "mtarjeta_master_consumo"))) {
    dataset[, da_total_mtarjeta_master_consumo := mtarjeta_master_consumo / da_total]
  }
  
  # Verificación y creación de la variable da_total_cprestamos_personales
  if(atributos_presentes(c("da_total", "cprestamos_personales"))) {
    dataset[, da_total_cprestamos_personales := cprestamos_personales / da_total]
  }
  
  # Verificación y creación de la variable da_total_mprestamos_personales
  if(atributos_presentes(c("da_total", "mprestamos_personales"))) {
    dataset[, da_total_mprestamos_personales := mprestamos_personales / da_total]
  }
  
  # Verificación y creación de la variable da_total_cprestamos_prendarios
  if(atributos_presentes(c("da_total", "cprestamos_prendarios"))) {
    dataset[, da_total_cprestamos_prendarios := cprestamos_prendarios / da_total]
  }
  
  # Verificación y creación de la variable da_total_mprestamos_prendarios
  if(atributos_presentes(c("da_total", "mprestamos_prendarios"))) {
    dataset[, da_total_mprestamos_prendarios := mprestamos_prendarios / da_total]
  }
  
  # Verificación y creación de la variable da_total_cprestamos_hipotecarios
  if(atributos_presentes(c("da_total", "cprestamos_hipotecarios"))) {
    dataset[, da_total_cprestamos_hipotecarios := cprestamos_hipotecarios / da_total]
  }
  
  # Verificación y creación de la variable da_total_mprestamos_hipotecarios
  if(atributos_presentes(c("da_total", "mprestamos_hipotecarios"))) {
    dataset[, da_total_mprestamos_hipotecarios := mprestamos_hipotecarios / da_total]
  }
  
  # Verificación y creación de la variable da_total_cplazo_fijo
  if(atributos_presentes(c("da_total", "cplazo_fijo"))) {
    dataset[, da_total_cplazo_fijo := cplazo_fijo / da_total]
  }
  
  # Verificación y creación de la variable da_total_mplazo_fijo_dolares
  if(atributos_presentes(c("da_total", "mplazo_fijo_dolares"))) {
    dataset[, da_total_mplazo_fijo_dolares := mplazo_fijo_dolares / da_total]
  }
  
  # Verificación y creación de la variable da_total_mplazo_fijo_pesos
  if(atributos_presentes(c("da_total", "mplazo_fijo_pesos"))) {
    dataset[, da_total_mplazo_fijo_pesos := mplazo_fijo_pesos / da_total]
  }
  
  # Verificación y creación de la variable da_total_cinversion1
  if(atributos_presentes(c("da_total", "cinversion1"))) {
    dataset[, da_total_cinversion1 := cinversion1 / da_total]
  }
  
  # Verificación y creación de la variable da_total_minversion1_pesos
  if(atributos_presentes(c("da_total", "minversion1_pesos"))) {
    dataset[, da_total_minversion1_pesos := minversion1_pesos / da_total]
  }
  
  # Verificación y creación de la variable da_total_minversion1_dolares
  if(atributos_presentes(c("da_total", "minversion1_dolares"))) {
    dataset[, da_total_minversion1_dolares := minversion1_dolares / da_total]
  }
  
  # Verificación y creación de la variable da_total_cinversion2
  if(atributos_presentes(c("da_total", "cinversion2"))) {
    dataset[, da_total_cinversion2 := cinversion2 / da_total]
  }
  
  # Verificación y creación de la variable da_total_minversion2
  if(atributos_presentes(c("da_total", "minversion2"))) {
    dataset[, da_total_minversion2 := minversion2 / da_total]
  }
  
  # Verificación y creación de la variable da_total_cseguro_vida
  if(atributos_presentes(c("da_total", "cseguro_vida"))) {
    dataset[, da_total_cseguro_vida := cseguro_vida / da_total]
  }
  
  # Verificación y creación de la variable da_total_cseguro_auto
  if(atributos_presentes(c("da_total", "cseguro_auto"))) {
    dataset[, da_total_cseguro_auto := cseguro_auto / da_total]
  }
  
  # Verificación y creación de la variable da_total_cseguro_vivienda
  if(atributos_presentes(c("da_total", "cseguro_vivienda"))) {
    dataset[, da_total_cseguro_vivienda := cseguro_vivienda / da_total]
  }
  
  # Verificación y creación de la variable da_total_cseguro_accidentes_personales
  if(atributos_presentes(c("da_total", "cseguro_accidentes_personales"))) {
    dataset[, da_total_cseguro_accidentes_personales := cseguro_accidentes_personales / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccaja_seguridad
  if(atributos_presentes(c("da_total", "ccaja_seguridad"))) {
    dataset[, da_total_ccaja_seguridad := ccaja_seguridad / da_total]
  }
  
  # Verificación y creación de la variable da_total_cpayroll_trx
  if(atributos_presentes(c("da_total", "cpayroll_trx"))) {
    dataset[, da_total_cpayroll_trx := cpayroll_trx / da_total]
  }
  
  # Verificación y creación de la variable da_total_mpayroll
  if(atributos_presentes(c("da_total", "mpayroll"))) {
    dataset[, da_total_mpayroll := mpayroll / da_total]
  }
  
  # Verificación y creación de la variable da_total_mpayroll2
  if(atributos_presentes(c("da_total", "mpayroll2"))) {
    dataset[, da_total_mpayroll2 := mpayroll2 / da_total]
  }
  
  # Verificación y creación de la variable da_total_cpayroll2_trx
  if(atributos_presentes(c("da_total", "cpayroll2_trx"))) {
    dataset[, da_total_cpayroll2_trx := cpayroll2_trx / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccuenta_debitos_automaticos
  if(atributos_presentes(c("da_total", "ccuenta_debitos_automaticos"))) {
    dataset[, da_total_ccuenta_debitos_automaticos := ccuenta_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcuenta_debitos_automaticos
  if(atributos_presentes(c("da_total", "mcuenta_debitos_automaticos"))) {
    dataset[, da_total_mcuenta_debitos_automaticos := mcuenta_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa_debitos_automaticos
  if(atributos_presentes(c("da_total", "ctarjeta_visa_debitos_automaticos"))) {
    dataset[, da_total_ctarjeta_visa_debitos_automaticos := ctarjeta_visa_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mttarjeta_visa_debitos_automaticos
  if(atributos_presentes(c("da_total", "mttarjeta_visa_debitos_automaticos"))) {
    dataset[, da_total_mttarjeta_visa_debitos_automaticos := mttarjeta_visa_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master_debitos_automaticos
  if(atributos_presentes(c("da_total", "ctarjeta_master_debitos_automaticos"))) {
    dataset[, da_total_ctarjeta_master_debitos_automaticos := ctarjeta_master_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mttarjeta_master_debitos_automaticos
  if(atributos_presentes(c("da_total", "mttarjeta_master_debitos_automaticos"))) {
    dataset[, da_total_mttarjeta_master_debitos_automaticos := mttarjeta_master_debitos_automaticos / da_total]
  }
  
  # Verificación y creación de la variable da_total_cpagodeservicios
  if(atributos_presentes(c("da_total", "cpagodeservicios"))) {
    dataset[, da_total_cpagodeservicios := cpagodeservicios / da_total]
  }
  
  # Verificación y creación de la variable da_total_mpagodeservicios
  if(atributos_presentes(c("da_total", "mpagodeservicios"))) {
    dataset[, da_total_mpagodeservicios := mpagodeservicios / da_total]
  }
  
  # Verificación y creación de la variable da_total_cpagomiscuentas
  if(atributos_presentes(c("da_total", "cpagomiscuentas"))) {
    dataset[, da_total_cpagomiscuentas := cpagomiscuentas / da_total]
  }
  
  # Verificación y creación de la variable da_total_mpagomiscuentas
  if(atributos_presentes(c("da_total", "mpagomiscuentas"))) {
    dataset[, da_total_mpagomiscuentas := mpagomiscuentas / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccajeros_propios_descuentos
  if(atributos_presentes(c("da_total", "ccajeros_propios_descuentos"))) {
    dataset[, da_total_ccajeros_propios_descuentos := ccajeros_propios_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcajeros_propios_descuentos
  if(atributos_presentes(c("da_total", "mcajeros_propios_descuentos"))) {
    dataset[, da_total_mcajeros_propios_descuentos := mcajeros_propios_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_visa_descuentos"))) {
    dataset[, da_total_ctarjeta_visa_descuentos := ctarjeta_visa_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mtarjeta_visa_descuentos
  if(atributos_presentes(c("da_total", "mtarjeta_visa_descuentos"))) {
    dataset[, da_total_mtarjeta_visa_descuentos := mtarjeta_visa_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_master_descuentos"))) {
    dataset[, da_total_ctarjeta_master_descuentos := ctarjeta_master_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mtarjeta_master_descuentos
  if(atributos_presentes(c("da_total", "mtarjeta_master_descuentos"))) {
    dataset[, da_total_mtarjeta_master_descuentos := mtarjeta_master_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ccajeros_ajenos_descuentos
  if(atributos_presentes(c("da_total", "ccajeros_ajenos_descuentos"))) {
    dataset[, da_total_ccajeros_ajenos_descuentos := ccajeros_ajenos_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_mcajeros_ajenos_descuentos
  if(atributos_presentes(c("da_total", "mcajeros_ajenos_descuentos"))) {
    dataset[, da_total_mcajeros_ajenos_descuentos := mcajeros_ajenos_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_visa_descuentos"))) {
    dataset[, da_total_ctarjeta_visa_descuentos := ctarjeta_visa_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_master_descuentos"))) {
    dataset[, da_total_ctarjeta_master_descuentos := ctarjeta_master_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_cforex
  if(atributos_presentes(c("da_total", "cforex"))) {
    dataset[, da_total_cforex := cforex / da_total]
  }
  
  # Verificación y creación de la variable da_total_mforex
  if(atributos_presentes(c("da_total", "mforex"))) {
    dataset[, da_total_mforex := mforex / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctesoreria
  if(atributos_presentes(c("da_total", "ctesoreria"))) {
    dataset[, da_total_ctesoreria := ctesoreria / da_total]
  }
  
  # Verificación y creación de la variable da_total_mtesoreria
  if(atributos_presentes(c("da_total", "mtesoreria"))) {
    dataset[, da_total_mtesoreria := mtesoreria / da_total]
  }
  
  # Verificación y creación de la variable da_total_cseguros
  if(atributos_presentes(c("da_total", "cseguros"))) {
    dataset[, da_total_cseguros := cseguros / da_total]
  }
  
  # Verificación y creación de la variable da_total_mseguros
  if(atributos_presentes(c("da_total", "mseguros"))) {
    dataset[, da_total_mseguros := mseguros / da_total]
  }
  
  # Verificación y creación de la variable da_total_cmulticanal
  if(atributos_presentes(c("da_total", "cmulticanal"))) {
    dataset[, da_total_cmulticanal := cmulticanal / da_total]
  }
  
  # Verificación y creación de la variable da_total_mmulticanal
  if(atributos_presentes(c("da_total", "mmulticanal"))) {
    dataset[, da_total_mmulticanal := mmulticanal / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_visa_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_visa_descuentos"))) {
    dataset[, da_total_ctarjeta_visa_descuentos := ctarjeta_visa_descuentos / da_total]
  }
  
  # Verificación y creación de la variable da_total_ctarjeta_master_descuentos
  if(atributos_presentes(c("da_total", "ctarjeta_master_descuentos"))) {
    dataset[, da_total_ctarjeta_master_descuentos := ctarjeta_master_descuentos / da_total]
  }
  
  
  
  # Busco dif activos y pasivos
  if(atributos_presentes(c("mactivos_margen", "mpasivos_margen"))) {
    dataset[, mactivos_margenpasivosmargen := mactivos_margen - mpasivos_margen ]
  }
  
  # Quiero comprender si cantidad de prod se relaciona con antiguedad
  if(atributos_presentes(c("cproductos", "cliente_antiguedad"))) {
    dataset[, cproductos_antiguedad := cproductos     / cliente_antiguedad ]
  }
  
  # Quiero comprender si cantidad de prod se relaciona con edad
  if(atributos_presentes(c("cproductos", "cliente_edad"))) {
    dataset[, cproductos_edad := cproductos     / cliente_edad ]
  }
  
  # Sumo transacciones de tarjeta
  if(atributos_presentes(c("ctarjeta_debito_transacciones", "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones"))) {
    dataset[, transacciones_tarjetas := ctarjeta_debito_transacciones   + ctarjeta_visa_transacciones + ctarjeta_master_transacciones ]
  }
  
  # Sumo seguros
  if(atributos_presentes(c("cseguro_vida", "cseguro_auto", "cseguro_vivienda", "cseguro_accidentes_personales"))) {
    dataset[, total_seguros := cseguro_vida   + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales ]
  }
  
  # Sumo productos que generan mas fidelidad
  if(atributos_presentes(c("cpayroll_trx", "cprestamos_hipotecarios", "cprestamos_personales"))) {
    dataset[, productosgancho := cpayroll_trx   + cprestamos_hipotecarios + cprestamos_personales ]
  }
  
  # debitos vs pagos
  if(atributos_presentes(c("cpagodeservicios", "da_total"))) {
    dataset[, debitossobrepagos := cpagodeservicios     / da_total ]
  }
  
  
  
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

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "z1301_FE_intrames_manual.r  START\n")
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
cat( "z1301_FE_intrames_manual.r  END\n")
