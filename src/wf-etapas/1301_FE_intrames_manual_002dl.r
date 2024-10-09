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

  
  # Sumo las pendientes derivada 1ra
  
  # Verificación y creación de la derivada primera de da_total
  if(atributos_presentes(c("da_total", "da_total_lag1"))) {
    dataset[, pendiente_da_total := da_total - da_total_lag1]
  }
  
  # Verificación y creación de la derivada primera de numero_de_cliente
  if(atributos_presentes(c("numero_de_cliente", "numero_de_cliente_lag1"))) {
    dataset[, pendiente_numero_de_cliente := numero_de_cliente - numero_de_cliente_lag1]
  }
  
  # Verificación y creación de la derivada primera de active_quarter
  if(atributos_presentes(c("active_quarter", "active_quarter_lag1"))) {
    dataset[, pendiente_active_quarter := active_quarter - active_quarter_lag1]
  }
  
  # Verificación y creación de la derivada primera de cliente_vip
  if(atributos_presentes(c("cliente_vip", "cliente_vip_lag1"))) {
    dataset[, pendiente_cliente_vip := cliente_vip - cliente_vip_lag1]
  }
  
  # Verificación y creación de la derivada primera de internet
  if(atributos_presentes(c("internet", "internet_lag1"))) {
    dataset[, pendiente_internet := internet - internet_lag1]
  }
  
  # Verificación y creación de la derivada primera de cliente_edad
  if(atributos_presentes(c("cliente_edad", "cliente_edad_lag1"))) {
    dataset[, pendiente_cliente_edad := cliente_edad - cliente_edad_lag1]
  }
  
  # Verificación y creación de la derivada primera de cliente_antiguedad
  if(atributos_presentes(c("cliente_antiguedad", "cliente_antiguedad_lag1"))) {
    dataset[, pendiente_cliente_antiguedad := cliente_antiguedad - cliente_antiguedad_lag1]
  }
  
  # Verificación y creación de la derivada primera de mrentabilidad
  if(atributos_presentes(c("mrentabilidad", "mrentabilidad_lag1"))) {
    dataset[, pendiente_mrentabilidad := mrentabilidad - mrentabilidad_lag1]
  }
  
  # Verificación y creación de la derivada primera de mrentabilidad_annual
  if(atributos_presentes(c("mrentabilidad_annual", "mrentabilidad_annual_lag1"))) {
    dataset[, pendiente_mrentabilidad_annual := mrentabilidad_annual - mrentabilidad_annual_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcomisiones
  if(atributos_presentes(c("mcomisiones", "mcomisiones_lag1"))) {
    dataset[, pendiente_mcomisiones := mcomisiones - mcomisiones_lag1]
  }
  
  # Verificación y creación de la derivada primera de mactivos_margen
  if(atributos_presentes(c("mactivos_margen", "mactivos_margen_lag1"))) {
    dataset[, pendiente_mactivos_margen := mactivos_margen - mactivos_margen_lag1]
  }
  
  # Verificación y creación de la derivada primera de mpasivos_margen
  if(atributos_presentes(c("mpasivos_margen", "mpasivos_margen_lag1"))) {
    dataset[, pendiente_mpasivos_margen := mpasivos_margen - mpasivos_margen_lag1]
  }
  
  # Verificación y creación de la derivada primera de cproductos
  if(atributos_presentes(c("cproductos", "cproductos_lag1"))) {
    dataset[, pendiente_cproductos := cproductos - cproductos_lag1]
  }
  
  # Verificación y creación de la derivada primera de tcuentas
  if(atributos_presentes(c("tcuentas", "tcuentas_lag1"))) {
    dataset[, pendiente_tcuentas := tcuentas - tcuentas_lag1]
  }
  
  # Verificación y creación de la derivada primera de ccuenta_corriente
  if(atributos_presentes(c("ccuenta_corriente", "ccuenta_corriente_lag1"))) {
    dataset[, pendiente_ccuenta_corriente := ccuenta_corriente - ccuenta_corriente_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcuenta_corriente_adicional
  if(atributos_presentes(c("mcuenta_corriente_adicional", "mcuenta_corriente_adicional_lag1"))) {
    dataset[, pendiente_mcuenta_corriente_adicional := mcuenta_corriente_adicional - mcuenta_corriente_adicional_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcuenta_corriente
  if(atributos_presentes(c("mcuenta_corriente", "mcuenta_corriente_lag1"))) {
    dataset[, pendiente_mcuenta_corriente := mcuenta_corriente - mcuenta_corriente_lag1]
  }
  
  # Verificación y creación de la derivada primera de ccaja_ahorro
  if(atributos_presentes(c("ccaja_ahorro", "ccaja_ahorro_lag1"))) {
    dataset[, pendiente_ccaja_ahorro := ccaja_ahorro - ccaja_ahorro_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcaja_ahorro
  if(atributos_presentes(c("mcaja_ahorro", "mcaja_ahorro_lag1"))) {
    dataset[, pendiente_mcaja_ahorro := mcaja_ahorro - mcaja_ahorro_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcaja_ahorro_adicional
  if(atributos_presentes(c("mcaja_ahorro_adicional", "mcaja_ahorro_adicional_lag1"))) {
    dataset[, pendiente_mcaja_ahorro_adicional := mcaja_ahorro_adicional - mcaja_ahorro_adicional_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcaja_ahorro_dolares
  if(atributos_presentes(c("mcaja_ahorro_dolares", "mcaja_ahorro_dolares_lag1"))) {
    dataset[, pendiente_mcaja_ahorro_dolares := mcaja_ahorro_dolares - mcaja_ahorro_dolares_lag1]
  }
  
  # Verificación y creación de la derivada primera de cdescubierto_preacordado
  if(atributos_presentes(c("cdescubierto_preacordado", "cdescubierto_preacordado_lag1"))) {
    dataset[, pendiente_cdescubierto_preacordado := cdescubierto_preacordado - cdescubierto_preacordado_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcuentas_saldo
  if(atributos_presentes(c("mcuentas_saldo", "mcuentas_saldo_lag1"))) {
    dataset[, pendiente_mcuentas_saldo := mcuentas_saldo - mcuentas_saldo_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_debito
  if(atributos_presentes(c("ctarjeta_debito", "ctarjeta_debito_lag1"))) {
    dataset[, pendiente_ctarjeta_debito := ctarjeta_debito - ctarjeta_debito_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_debito_transacciones
  if(atributos_presentes(c("ctarjeta_debito_transacciones", "ctarjeta_debito_transacciones_lag1"))) {
    dataset[, pendiente_ctarjeta_debito_transacciones := ctarjeta_debito_transacciones - ctarjeta_debito_transacciones_lag1]
  }
  
  # Verificación y creación de la derivada primera de mautoservicio
  if(atributos_presentes(c("mautoservicio", "mautoservicio_lag1"))) {
    dataset[, pendiente_mautoservicio := mautoservicio - mautoservicio_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_visa
  if(atributos_presentes(c("ctarjeta_visa", "ctarjeta_visa_lag1"))) {
    dataset[, pendiente_ctarjeta_visa := ctarjeta_visa - ctarjeta_visa_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_visa_transacciones
  if(atributos_presentes(c("ctarjeta_visa_transacciones", "ctarjeta_visa_transacciones_lag1"))) {
    dataset[, pendiente_ctarjeta_visa_transacciones := ctarjeta_visa_transacciones - ctarjeta_visa_transacciones_lag1]
  }
  
  # Verificación y creación de la derivada primera de mtarjeta_visa_consumo
  if(atributos_presentes(c("mtarjeta_visa_consumo", "mtarjeta_visa_consumo_lag1"))) {
    dataset[, pendiente_mtarjeta_visa_consumo := mtarjeta_visa_consumo - mtarjeta_visa_consumo_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_master
  if(atributos_presentes(c("ctarjeta_master", "ctarjeta_master_lag1"))) {
    dataset[, pendiente_ctarjeta_master := ctarjeta_master - ctarjeta_master_lag1]
  }
  
  # Verificación y creación de la derivada primera de ctarjeta_master_transacciones
  if(atributos_presentes(c("ctarjeta_master_transacciones", "ctarjeta_master_transacciones_lag1"))) {
    dataset[, pendiente_ctarjeta_master_transacciones := ctarjeta_master_transacciones - ctarjeta_master_transacciones_lag1]
  }
  
  # Verificación y creación de la derivada primera de mtarjeta_master_consumo
  if(atributos_presentes(c("mtarjeta_master_consumo", "mtarjeta_master_consumo_lag1"))) {
    dataset[, pendiente_mtarjeta_master_consumo := mtarjeta_master_consumo - mtarjeta_master_consumo_lag1]
  }
  
  # Verificación y creación de la derivada primera de cprestamos_personales
  if(atributos_presentes(c("cprestamos_personales", "cprestamos_personales_lag1"))) {
    dataset[, pendiente_cprestamos_personales := cprestamos_personales - cprestamos_personales_lag1]
  }
  
  # Verificación y creación de la derivada primera de mprestamos_personales
  if(atributos_presentes(c("mprestamos_personales", "mprestamos_personales_lag1"))) {
    dataset[, pendiente_mprestamos_personales := mprestamos_personales - mprestamos_personales_lag1]
  }
  
  # Verificación y creación de la derivada primera de cprestamos_prendarios
  if(atributos_presentes(c("cprestamos_prendarios", "cprestamos_prendarios_lag1"))) {
    dataset[, pendiente_cprestamos_prendarios := cprestamos_prendarios - cprestamos_prendarios_lag1]
  }
  
  # Verificación y creación de la derivada primera de mprestamos_prendarios
  if(atributos_presentes(c("mprestamos_prendarios", "mprestamos_prendarios_lag1"))) {
    dataset[, pendiente_mprestamos_prendarios := mprestamos_prendarios - mprestamos_prendarios_lag1]
  }
  
  # Verificación y creación de la derivada primera de cprestamos_hipotecarios
  if(atributos_presentes(c("cprestamos_hipotecarios", "cprestamos_hipotecarios_lag1"))) {
    dataset[, pendiente_cprestamos_hipotecarios := cprestamos_hipotecarios - cprestamos_hipotecarios_lag1]
  }
  
  # Verificación y creación de la derivada primera de mprestamos_hipotecarios
  if(atributos_presentes(c("mprestamos_hipotecarios", "mprestamos_hipotecarios_lag1"))) {
    dataset[, pendiente_mprestamos_hipotecarios := mprestamos_hipotecarios - mprestamos_hipotecarios_lag1]
  }
  
  # Verificación y creación de la derivada primera de cplazo_fijo
  if(atributos_presentes(c("cplazo_fijo", "cplazo_fijo_lag1"))) {
    dataset[, pendiente_cplazo_fijo := cplazo_fijo - cplazo_fijo_lag1]
  }
  
  # Verificación y creación de la derivada primera de mplazo_fijo_dolares
  if(atributos_presentes(c("mplazo_fijo_dolares", "mplazo_fijo_dolares_lag1"))) {
    dataset[, pendiente_mplazo_fijo_dolares := mplazo_fijo_dolares - mplazo_fijo_dolares_lag1]
  }
  
  # Verificación y creación de la derivada primera de mplazo_fijo_pesos
  if(atributos_presentes(c("mplazo_fijo_pesos", "mplazo_fijo_pesos_lag1"))) {
    dataset[, pendiente_mplazo_fijo_pesos := mplazo_fijo_pesos - mplazo_fijo_pesos_lag1]
  }
  
  # Verificación y creación de la derivada primera de cinversion1
  if(atributos_presentes(c("cinversion1", "cinversion1_lag1"))) {
    dataset[, pendiente_cinversion1 := cinversion1 - cinversion1_lag1]
  }
  
  # Verificación y creación de la derivada primera de minversion1_pesos
  if(atributos_presentes(c("minversion1_pesos", "minversion1_pesos_lag1"))) {
    dataset[, pendiente_minversion1_pesos := minversion1_pesos - minversion1_pesos_lag1]
  }
  
  # Verificación y creación de la derivada primera de minversion1_dolares
  if(atributos_presentes(c("minversion1_dolares", "minversion1_dolares_lag1"))) {
    dataset[, pendiente_minversion1_dolares := minversion1_dolares - minversion1_dolares_lag1]
  }
  
  # Verificación y creación de la derivada primera de cinversion2
  if(atributos_presentes(c("cinversion2", "cinversion2_lag1"))) {
    dataset[, pendiente_cinversion2 := cinversion2 - cinversion2_lag1]
  }
  
  # Verificación y creación de la derivada primera de minversion2
  if(atributos_presentes(c("minversion2", "minversion2_lag1"))) {
    dataset[, pendiente_minversion2 := minversion2 - minversion2_lag1]
  }
  
  # Verificación y creación de la derivada primera de cseguro_vida
  if(atributos_presentes(c("cseguro_vida", "cseguro_vida_lag1"))) {
    dataset[, pendiente_cseguro_vida := cseguro_vida - cseguro_vida_lag1]
  }
  
  # Verificación y creación de la derivada primera de cseguro_auto
  if(atributos_presentes(c("cseguro_auto", "cseguro_auto_lag1"))) {
    dataset[, pendiente_cseguro_auto := cseguro_auto - cseguro_auto_lag1]
  }
  
  # Verificación y creación de la derivada primera de cseguro_vivienda
  if(atributos_presentes(c("cseguro_vivienda", "cseguro_vivienda_lag1"))) {
    dataset[, pendiente_cseguro_vivienda := cseguro_vivienda - cseguro_vivienda_lag1]
  }
  
  # Verificación y creación de la derivada primera de cseguro_accidentes_personales
  if(atributos_presentes(c("cseguro_accidentes_personales", "cseguro_accidentes_personales_lag1"))) {
    dataset[, pendiente_cseguro_accidentes_personales := cseguro_accidentes_personales - cseguro_accidentes_personales_lag1]
  }
  
  # Verificación y creación de la derivada primera de ccaja_seguridad
  if(atributos_presentes(c("ccaja_seguridad", "ccaja_seguridad_lag1"))) {
    dataset[, pendiente_ccaja_seguridad := ccaja_seguridad - ccaja_seguridad_lag1]
  }
  
  # Verificación y creación de la derivada primera de cpayroll_trx
  if(atributos_presentes(c("cpayroll_trx", "cpayroll_trx_lag1"))) {
    dataset[, pendiente_cpayroll_trx := cpayroll_trx - cpayroll_trx_lag1]
  }
  
  # Verificación y creación de la derivada primera de mpayroll
  if(atributos_presentes(c("mpayroll", "mpayroll_lag1"))) {
    dataset[, pendiente_mpayroll := mpayroll - mpayroll_lag1]
  }
  
  # Verificación y creación de la derivada primera de mpayroll2
  if(atributos_presentes(c("mpayroll2", "mpayroll2_lag1"))) {
    dataset[, pendiente_mpayroll2 := mpayroll2 - mpayroll2_lag1]
  }
  
  # Verificación y creación de la derivada primera de cpayroll2_trx
  if(atributos_presentes(c("cpayroll2_trx", "cpayroll2_trx_lag1"))) {
    dataset[, pendiente_cpayroll2_trx := cpayroll2_trx - cpayroll2_trx_lag1]
  }
  
  # Verificación y creación de la derivada primera de ccuenta_debitos_automaticos
  if(atributos_presentes(c("ccuenta_debitos_automaticos", "ccuenta_debitos_automaticos_lag1"))) {
    dataset[, pendiente_ccuenta_debitos_automaticos := ccuenta_debitos_automaticos - ccuenta_debitos_automaticos_lag1]
  }
  
  # Verificación y creación de la derivada primera de mcuenta_debitos_automaticos
  if(atributos_presentes(c("mcuenta_debitos_automaticos", "mcuenta_debitos_automaticos_lag1"))) {
    dataset[, pendiente_mcuenta_debitos_automaticos := mcuenta_debitos_automaticos - mcuenta_debitos_automaticos_lag1]
  }
  
  #Sumo la aceleracion de las variables
  
  # Verificación y creación de la variable numero_de_cliente
  if(atributos_presentes(c("da_total", "da_total_lag1", "da_total_lag2"))) {
    dataset[, aceleracion_da_total := (da_total - 2 * da_total_lag1 + da_total_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable foto_mes
  if(atributos_presentes(c("foto_mes", "foto_mes_lag1", "foto_mes_lag2"))) {
    dataset[, aceleracion_foto_mes := (foto_mes - 2 * foto_mes_lag1 + foto_mes_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable active_quarter
  if(atributos_presentes(c("active_quarter", "active_quarter_lag1", "active_quarter_lag2"))) {
    dataset[, aceleracion_active_quarter := (active_quarter - 2 * active_quarter_lag1 + active_quarter_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cliente_vip
  if(atributos_presentes(c("cliente_vip", "cliente_vip_lag1", "cliente_vip_lag2"))) {
    dataset[, aceleracion_cliente_vip := (cliente_vip - 2 * cliente_vip_lag1 + cliente_vip_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable internet
  if(atributos_presentes(c("internet", "internet_lag1", "internet_lag2"))) {
    dataset[, aceleracion_internet := (internet - 2 * internet_lag1 + internet_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cliente_edad
  if(atributos_presentes(c("cliente_edad", "cliente_edad_lag1", "cliente_edad_lag2"))) {
    dataset[, aceleracion_cliente_edad := (cliente_edad - 2 * cliente_edad_lag1 + cliente_edad_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cliente_antiguedad
  if(atributos_presentes(c("cliente_antiguedad", "cliente_antiguedad_lag1", "cliente_antiguedad_lag2"))) {
    dataset[, aceleracion_cliente_antiguedad := (cliente_antiguedad - 2 * cliente_antiguedad_lag1 + cliente_antiguedad_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mrentabilidad
  if(atributos_presentes(c("mrentabilidad", "mrentabilidad_lag1", "mrentabilidad_lag2"))) {
    dataset[, aceleracion_mrentabilidad := (mrentabilidad - 2 * mrentabilidad_lag1 + mrentabilidad_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mrentabilidad_annual
  if(atributos_presentes(c("mrentabilidad_annual", "mrentabilidad_annual_lag1", "mrentabilidad_annual_lag2"))) {
    dataset[, aceleracion_mrentabilidad_annual := (mrentabilidad_annual - 2 * mrentabilidad_annual_lag1 + mrentabilidad_annual_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcomisiones
  if(atributos_presentes(c("mcomisiones", "mcomisiones_lag1", "mcomisiones_lag2"))) {
    dataset[, aceleracion_mcomisiones := (mcomisiones - 2 * mcomisiones_lag1 + mcomisiones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mactivos_margen
  if(atributos_presentes(c("mactivos_margen", "mactivos_margen_lag1", "mactivos_margen_lag2"))) {
    dataset[, aceleracion_mactivos_margen := (mactivos_margen - 2 * mactivos_margen_lag1 + mactivos_margen_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mpasivos_margen
  if(atributos_presentes(c("mpasivos_margen", "mpasivos_margen_lag1", "mpasivos_margen_lag2"))) {
    dataset[, aceleracion_mpasivos_margen := (mpasivos_margen - 2 * mpasivos_margen_lag1 + mpasivos_margen_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cproductos
  if(atributos_presentes(c("cproductos", "cproductos_lag1", "cproductos_lag2"))) {
    dataset[, aceleracion_cproductos := (cproductos - 2 * cproductos_lag1 + cproductos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable tcuentas
  if(atributos_presentes(c("tcuentas", "tcuentas_lag1", "tcuentas_lag2"))) {
    dataset[, aceleracion_tcuentas := (tcuentas - 2 * tcuentas_lag1 + tcuentas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccuenta_corriente
  if(atributos_presentes(c("ccuenta_corriente", "ccuenta_corriente_lag1", "ccuenta_corriente_lag2"))) {
    dataset[, aceleracion_ccuenta_corriente := (ccuenta_corriente - 2 * ccuenta_corriente_lag1 + ccuenta_corriente_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcuenta_corriente_adicional
  if(atributos_presentes(c("mcuenta_corriente_adicional", "mcuenta_corriente_adicional_lag1", "mcuenta_corriente_adicional_lag2"))) {
    dataset[, aceleracion_mcuenta_corriente_adicional := (mcuenta_corriente_adicional - 2 * mcuenta_corriente_adicional_lag1 + mcuenta_corriente_adicional_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcuenta_corriente
  if(atributos_presentes(c("mcuenta_corriente", "mcuenta_corriente_lag1", "mcuenta_corriente_lag2"))) {
    dataset[, aceleracion_mcuenta_corriente := (mcuenta_corriente - 2 * mcuenta_corriente_lag1 + mcuenta_corriente_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccaja_ahorro
  if(atributos_presentes(c("ccaja_ahorro", "ccaja_ahorro_lag1", "ccaja_ahorro_lag2"))) {
    dataset[, aceleracion_ccaja_ahorro := (ccaja_ahorro - 2 * ccaja_ahorro_lag1 + ccaja_ahorro_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcaja_ahorro
  if(atributos_presentes(c("mcaja_ahorro", "mcaja_ahorro_lag1", "mcaja_ahorro_lag2"))) {
    dataset[, aceleracion_mcaja_ahorro := (mcaja_ahorro - 2 * mcaja_ahorro_lag1 + mcaja_ahorro_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcaja_ahorro_adicional
  if(atributos_presentes(c("mcaja_ahorro_adicional", "mcaja_ahorro_adicional_lag1", "mcaja_ahorro_adicional_lag2"))) {
    dataset[, aceleracion_mcaja_ahorro_adicional := (mcaja_ahorro_adicional - 2 * mcaja_ahorro_adicional_lag1 + mcaja_ahorro_adicional_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcaja_ahorro_dolares
  if(atributos_presentes(c("mcaja_ahorro_dolares", "mcaja_ahorro_dolares_lag1", "mcaja_ahorro_dolares_lag2"))) {
    dataset[, aceleracion_mcaja_ahorro_dolares := (mcaja_ahorro_dolares - 2 * mcaja_ahorro_dolares_lag1 + mcaja_ahorro_dolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cdescubierto_preacordado
  if(atributos_presentes(c("cdescubierto_preacordado", "cdescubierto_preacordado_lag1", "cdescubierto_preacordado_lag2"))) {
    dataset[, aceleracion_cdescubierto_preacordado := (cdescubierto_preacordado - 2 * cdescubierto_preacordado_lag1 + cdescubierto_preacordado_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcuentas_saldo
  if(atributos_presentes(c("mcuentas_saldo", "mcuentas_saldo_lag1", "mcuentas_saldo_lag2"))) {
    dataset[, aceleracion_mcuentas_saldo := (mcuentas_saldo - 2 * mcuentas_saldo_lag1 + mcuentas_saldo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_debito
  if(atributos_presentes(c("ctarjeta_debito", "ctarjeta_debito_lag1", "ctarjeta_debito_lag2"))) {
    dataset[, aceleracion_ctarjeta_debito := (ctarjeta_debito - 2 * ctarjeta_debito_lag1 + ctarjeta_debito_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_debito_transacciones
  if(atributos_presentes(c("ctarjeta_debito_transacciones", "ctarjeta_debito_transacciones_lag1", "ctarjeta_debito_transacciones_lag2"))) {
    dataset[, aceleracion_ctarjeta_debito_transacciones := (ctarjeta_debito_transacciones - 2 * ctarjeta_debito_transacciones_lag1 + ctarjeta_debito_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mautoservicio
  if(atributos_presentes(c("mautoservicio", "mautoservicio_lag1", "mautoservicio_lag2"))) {
    dataset[, aceleracion_mautoservicio := (mautoservicio - 2 * mautoservicio_lag1 + mautoservicio_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_visa
  if(atributos_presentes(c("ctarjeta_visa", "ctarjeta_visa_lag1", "ctarjeta_visa_lag2"))) {
    dataset[, aceleracion_ctarjeta_visa := (ctarjeta_visa - 2 * ctarjeta_visa_lag1 + ctarjeta_visa_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_visa_transacciones
  if(atributos_presentes(c("ctarjeta_visa_transacciones", "ctarjeta_visa_transacciones_lag1", "ctarjeta_visa_transacciones_lag2"))) {
    dataset[, aceleracion_ctarjeta_visa_transacciones := (ctarjeta_visa_transacciones - 2 * ctarjeta_visa_transacciones_lag1 + ctarjeta_visa_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtarjeta_visa_consumo
  if(atributos_presentes(c("mtarjeta_visa_consumo", "mtarjeta_visa_consumo_lag1", "mtarjeta_visa_consumo_lag2"))) {
    dataset[, aceleracion_mtarjeta_visa_consumo := (mtarjeta_visa_consumo - 2 * mtarjeta_visa_consumo_lag1 + mtarjeta_visa_consumo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_master
  if(atributos_presentes(c("ctarjeta_master", "ctarjeta_master_lag1", "ctarjeta_master_lag2"))) {
    dataset[, aceleracion_ctarjeta_master := (ctarjeta_master - 2 * ctarjeta_master_lag1 + ctarjeta_master_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_master_transacciones
  if(atributos_presentes(c("ctarjeta_master_transacciones", "ctarjeta_master_transacciones_lag1", "ctarjeta_master_transacciones_lag2"))) {
    dataset[, aceleracion_ctarjeta_master_transacciones := (ctarjeta_master_transacciones - 2 * ctarjeta_master_transacciones_lag1 + ctarjeta_master_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtarjeta_master_consumo
  if(atributos_presentes(c("mtarjeta_master_consumo", "mtarjeta_master_consumo_lag1", "mtarjeta_master_consumo_lag2"))) {
    dataset[, aceleracion_mtarjeta_master_consumo := (mtarjeta_master_consumo - 2 * mtarjeta_master_consumo_lag1 + mtarjeta_master_consumo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cprestamos_personales
  if(atributos_presentes(c("cprestamos_personales", "cprestamos_personales_lag1", "cprestamos_personales_lag2"))) {
    dataset[, aceleracion_cprestamos_personales := (cprestamos_personales - 2 * cprestamos_personales_lag1 + cprestamos_personales_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mprestamos_personales
  if(atributos_presentes(c("mprestamos_personales", "mprestamos_personales_lag1", "mprestamos_personales_lag2"))) {
    dataset[, aceleracion_mprestamos_personales := (mprestamos_personales - 2 * mprestamos_personales_lag1 + mprestamos_personales_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cprestamos_prendarios
  if(atributos_presentes(c("cprestamos_prendarios", "cprestamos_prendarios_lag1", "cprestamos_prendarios_lag2"))) {
    dataset[, aceleracion_cprestamos_prendarios := (cprestamos_prendarios - 2 * cprestamos_prendarios_lag1 + cprestamos_prendarios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mprestamos_prendarios
  if(atributos_presentes(c("mprestamos_prendarios", "mprestamos_prendarios_lag1", "mprestamos_prendarios_prendarios_lag2"))) {
    dataset[, aceleracion_mprestamos_prendarios := (mprestamos_prendarios - 2 * mprestamos_prendarios_lag1 + mprestamos_prendarios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cprestamos_hipotecarios
  if(atributos_presentes(c("cprestamos_hipotecarios", "cprestamos_hipotecarios_lag1", "cprestamos_hipotecarios_lag2"))) {
    dataset[, aceleracion_cprestamos_hipotecarios := (cprestamos_hipotecarios - 2 * cprestamos_hipotecarios_lag1 + cprestamos_hipotecarios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mprestamos_hipotecarios
  if(atributos_presentes(c("mprestamos_hipotecarios", "mprestamos_hipotecarios_lag1", "mprestamos_hipotecarios_lag2"))) {
    dataset[, aceleracion_mprestamos_hipotecarios := (mprestamos_hipotecarios - 2 * mprestamos_hipotecarios_lag1 + mprestamos_hipotecarios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cplazo_fijo
  if(atributos_presentes(c("cplazo_fijo", "cplazo_fijo_lag1", "cplazo_fijo_lag2"))) {
    dataset[, aceleracion_cplazo_fijo := (cplazo_fijo - 2 * cplazo_fijo_lag1 + cplazo_fijo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mplazo_fijo_dolares
  if(atributos_presentes(c("mplazo_fijo_dolares", "mplazo_fijo_dolares_lag1", "mplazo_fijo_dolares_lag2"))) {
    dataset[, aceleracion_mplazo_fijo_dolares := (mplazo_fijo_dolares - 2 * mplazo_fijo_dolares_lag1 + mplazo_fijo_dolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mplazo_fijo_pesos
  if(atributos_presentes(c("mplazo_fijo_pesos", "mplazo_fijo_pesos_lag1", "mplazo_fijo_pesos_lag2"))) {
    dataset[, aceleracion_mplazo_fijo_pesos := (mplazo_fijo_pesos - 2 * mplazo_fijo_pesos_lag1 + mplazo_fijo_pesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cinversion1
  if(atributos_presentes(c("cinversion1", "cinversion1_lag1", "cinversion1_lag2"))) {
    dataset[, aceleracion_cinversion1 := (cinversion1 - 2 * cinversion1_lag1 + cinversion1_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable minversion1_pesos
  if(atributos_presentes(c("minversion1_pesos", "minversion1_pesos_lag1", "minversion1_pesos_lag2"))) {
    dataset[, aceleracion_minversion1_pesos := (minversion1_pesos - 2 * minversion1_pesos_lag1 + minversion1_pesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable minversion1_dolares
  if(atributos_presentes(c("minversion1_dolares", "minversion1_dolares_lag1", "minversion1_dolares_lag2"))) {
    dataset[, aceleracion_minversion1_dolares := (minversion1_dolares - 2 * minversion1_dolares_lag1 + minversion1_dolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cinversion2
  if(atributos_presentes(c("cinversion2", "cinversion2_lag1", "cinversion2_lag2"))) {
    dataset[, aceleracion_cinversion2 := (cinversion2 - 2 * cinversion2_lag1 + cinversion2_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable minversion2
  if(atributos_presentes(c("minversion2", "minversion2_lag1", "minversion2_lag2"))) {
    dataset[, aceleracion_minversion2 := (minversion2 - 2 * minversion2_lag1 + minversion2_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cseguro_vida
  if(atributos_presentes(c("cseguro_vida", "cseguro_vida_lag1", "cseguro_vida_lag2"))) {
    dataset[, aceleracion_cseguro_vida := (cseguro_vida - 2 * cseguro_vida_lag1 + cseguro_vida_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cseguro_auto
  if(atributos_presentes(c("cseguro_auto", "cseguro_auto_lag1", "cseguro_auto_lag2"))) {
    dataset[, aceleracion_cseguro_auto := (cseguro_auto - 2 * cseguro_auto_lag1 + cseguro_auto_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cseguro_vivienda
  if(atributos_presentes(c("cseguro_vivienda", "cseguro_vivienda_lag1", "cseguro_vivienda_lag2"))) {
    dataset[, aceleracion_cseguro_vivienda := (cseguro_vivienda - 2 * cseguro_vivienda_lag1 + cseguro_vivienda_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cseguro_accidentes_personales
  if(atributos_presentes(c("cseguro_accidentes_personales", "cseguro_accidentes_personales_lag1", "cseguro_accidentes_personales_lag2"))) {
    dataset[, aceleracion_cseguro_accidentes_personales := (cseguro_accidentes_personales - 2 * cseguro_accidentes_personales_lag1 + cseguro_accidentes_personales_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccaja_seguridad
  if(atributos_presentes(c("ccaja_seguridad", "ccaja_seguridad_lag1", "ccaja_seguridad_lag2"))) {
    dataset[, aceleracion_ccaja_seguridad := (ccaja_seguridad - 2 * ccaja_seguridad_lag1 + ccaja_seguridad_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cpayroll_trx
  if(atributos_presentes(c("cpayroll_trx", "cpayroll_trx_lag1", "cpayroll_trx_lag2"))) {
    dataset[, aceleracion_cpayroll_trx := (cpayroll_trx - 2 * cpayroll_trx_lag1 + cpayroll_trx_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mpayroll
  if(atributos_presentes(c("mpayroll", "mpayroll_lag1", "mpayroll_lag2"))) {
    dataset[, aceleracion_mpayroll := (mpayroll - 2 * mpayroll_lag1 + mpayroll_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mpayroll2
  if(atributos_presentes(c("mpayroll2", "mpayroll2_lag1", "mpayroll2_lag2"))) {
    dataset[, aceleracion_mpayroll2 := (mpayroll2 - 2 * mpayroll2_lag1 + mpayroll2_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cpayroll2_trx
  if(atributos_presentes(c("cpayroll2_trx", "cpayroll2_trx_lag1", "cpayroll2_trx_lag2"))) {
    dataset[, aceleracion_cpayroll2_trx := (cpayroll2_trx - 2 * cpayroll2_trx_lag1 + cpayroll2_trx_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccuenta_debitos_automaticos
  if(atributos_presentes(c("ccuenta_debitos_automaticos", "ccuenta_debitos_automaticos_lag1", "ccuenta_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_ccuenta_debitos_automaticos := (ccuenta_debitos_automaticos - 2 * ccuenta_debitos_automaticos_lag1 + ccuenta_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcuenta_debitos_automaticos
  if(atributos_presentes(c("mcuenta_debitos_automaticos", "mcuenta_debitos_automaticos_lag1", "mcuenta_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_mcuenta_debitos_automaticos := (mcuenta_debitos_automaticos - 2 * mcuenta_debitos_automaticos_lag1 + mcuenta_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_visa_debitos_automaticos
  if(atributos_presentes(c("ctarjeta_visa_debitos_automaticos", "ctarjeta_visa_debitos_automaticos_lag1", "ctarjeta_visa_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_ctarjeta_visa_debitos_automaticos := (ctarjeta_visa_debitos_automaticos - 2 * ctarjeta_visa_debitos_automaticos_lag1 + ctarjeta_visa_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mttarjeta_visa_debitos_automaticos
  if(atributos_presentes(c("mttarjeta_visa_debitos_automaticos", "mttarjeta_visa_debitos_automaticos_lag1", "mttarjeta_visa_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_mttarjeta_visa_debitos_automaticos := (mttarjeta_visa_debitos_automaticos - 2 * mttarjeta_visa_debitos_automaticos_lag1 + mttarjeta_visa_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_master_debitos_automaticos
  if(atributos_presentes(c("ctarjeta_master_debitos_automaticos", "ctarjeta_master_debitos_automaticos_lag1", "ctarjeta_master_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_ctarjeta_master_debitos_automaticos := (ctarjeta_master_debitos_automaticos - 2 * ctarjeta_master_debitos_automaticos_lag1 + ctarjeta_master_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mttarjeta_master_debitos_automaticos
  if(atributos_presentes(c("mttarjeta_master_debitos_automaticos", "mttarjeta_master_debitos_automaticos_lag1", "mttarjeta_master_debitos_automaticos_lag2"))) {
    dataset[, aceleracion_mttarjeta_master_debitos_automaticos := (mttarjeta_master_debitos_automaticos - 2 * mttarjeta_master_debitos_automaticos_lag1 + mttarjeta_master_debitos_automaticos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cpagodeservicios
  if(atributos_presentes(c("cpagodeservicios", "cpagodeservicios_lag1", "cpagodeservicios_lag2"))) {
    dataset[, aceleracion_cpagodeservicios := (cpagodeservicios - 2 * cpagodeservicios_lag1 + cpagodeservicios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mpagodeservicios
  if(atributos_presentes(c("mpagodeservicios", "mpagodeservicios_lag1", "mpagodeservicios_lag2"))) {
    dataset[, aceleracion_mpagodeservicios := (mpagodeservicios - 2 * mpagodeservicios_lag1 + mpagodeservicios_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cpagomiscuentas
  if(atributos_presentes(c("cpagomiscuentas", "cpagomiscuentas_lag1", "cpagomiscuentas_lag2"))) {
    dataset[, aceleracion_cpagomiscuentas := (cpagomiscuentas - 2 * cpagomiscuentas_lag1 + cpagomiscuentas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mpagomiscuentas
  if(atributos_presentes(c("mpagomiscuentas", "mpagomiscuentas_lag1", "mpagomiscuentas_lag2"))) {
    dataset[, aceleracion_mpagomiscuentas := (mpagomiscuentas - 2 * mpagomiscuentas_lag1 + mpagomiscuentas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajeros_propios_descuentos
  if(atributos_presentes(c("ccajeros_propios_descuentos", "ccajeros_propios_descuentos_lag1", "ccajeros_propios_descuentos_lag2"))) {
    dataset[, aceleracion_ccajeros_propios_descuentos := (ccajeros_propios_descuentos - 2 * ccajeros_propios_descuentos_lag1 + ccajeros_propios_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcajeros_propios_descuentos
  if(atributos_presentes(c("mcajeros_propios_descuentos", "mcajeros_propios_descuentos_lag1", "mcajeros_propios_descuentos_lag2"))) {
    dataset[, aceleracion_mcajeros_propios_descuentos := (mcajeros_propios_descuentos - 2 * mcajeros_propios_descuentos_lag1 + mcajeros_propios_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_visa_descuentos
  if(atributos_presentes(c("ctarjeta_visa_descuentos", "ctarjeta_visa_descuentos_lag1", "ctarjeta_visa_descuentos_lag2"))) {
    dataset[, aceleracion_ctarjeta_visa_descuentos := (ctarjeta_visa_descuentos - 2 * ctarjeta_visa_descuentos_lag1 + ctarjeta_visa_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtarjeta_visa_descuentos
  if(atributos_presentes(c("mtarjeta_visa_descuentos", "mtarjeta_visa_descuentos_lag1", "mtarjeta_visa_descuentos_lag2"))) {
    dataset[, aceleracion_mtarjeta_visa_descuentos := (mtarjeta_visa_descuentos - 2 * mtarjeta_visa_descuentos_lag1 + mtarjeta_visa_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctarjeta_master_descuentos
  if(atributos_presentes(c("ctarjeta_master_descuentos", "ctarjeta_master_descuentos_lag1", "ctarjeta_master_descuentos_lag2"))) {
    dataset[, aceleracion_ctarjeta_master_descuentos := (ctarjeta_master_descuentos - 2 * ctarjeta_master_descuentos_lag1 + ctarjeta_master_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtarjeta_master_descuentos
  if(atributos_presentes(c("mtarjeta_master_descuentos", "mtarjeta_master_descuentos_lag1", "mtarjeta_master_descuentos_lag2"))) {
    dataset[, aceleracion_mtarjeta_master_descuentos := (mtarjeta_master_descuentos - 2 * mtarjeta_master_descuentos_lag1 + mtarjeta_master_descuentos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccomisiones_mantenimiento
  if(atributos_presentes(c("ccomisiones_mantenimiento", "ccomisiones_mantenimiento_lag1", "ccomisiones_mantenimiento_lag2"))) {
    dataset[, aceleracion_ccomisiones_mantenimiento := (ccomisiones_mantenimiento - 2 * ccomisiones_mantenimiento_lag1 + ccomisiones_mantenimiento_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcomisiones_mantenimiento
  if(atributos_presentes(c("mcomisiones_mantenimiento", "mcomisiones_mantenimiento_lag1", "mcomisiones_mantenimiento_lag2"))) {
    dataset[, aceleracion_mcomisiones_mantenimiento := (mcomisiones_mantenimiento - 2 * mcomisiones_mantenimiento_lag1 + mcomisiones_mantenimiento_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccomisiones_otras
  if(atributos_presentes(c("ccomisiones_otras", "ccomisiones_otras_lag1", "ccomisiones_otras_lag2"))) {
    dataset[, aceleracion_ccomisiones_otras := (ccomisiones_otras - 2 * ccomisiones_otras_lag1 + ccomisiones_otras_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcomisiones_otras
  if(atributos_presentes(c("mcomisiones_otras", "mcomisiones_otras_lag1", "mcomisiones_otras_lag2"))) {
    dataset[, aceleracion_mcomisiones_otras := (mcomisiones_otras - 2 * mcomisiones_otras_lag1 + mcomisiones_otras_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cforex
  if(atributos_presentes(c("cforex", "cforex_lag1", "cforex_lag2"))) {
    dataset[, aceleracion_cforex := (cforex - 2 * cforex_lag1 + cforex_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cforex_buy
  if(atributos_presentes(c("cforex_buy", "cforex_buy_lag1", "cforex_buy_lag2"))) {
    dataset[, aceleracion_cforex_buy := (cforex_buy - 2 * cforex_buy_lag1 + cforex_buy_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mforex_buy
  if(atributos_presentes(c("mforex_buy", "mforex_buy_lag1", "mforex_buy_lag2"))) {
    dataset[, aceleracion_mforex_buy := (mforex_buy - 2 * mforex_buy_lag1 + mforex_buy_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cforex_sell
  if(atributos_presentes(c("cforex_sell", "cforex_sell_lag1", "cforex_sell_lag2"))) {
    dataset[, aceleracion_cforex_sell := (cforex_sell - 2 * cforex_sell_lag1 + cforex_sell_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mforex_sell
  if(atributos_presentes(c("mforex_sell", "mforex_sell_lag1", "mforex_sell_lag2"))) {
    dataset[, aceleracion_mforex_sell := (mforex_sell - 2 * mforex_sell_lag1 + mforex_sell_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctransferencias_recibidas
  if(atributos_presentes(c("ctransferencias_recibidas", "ctransferencias_recibidas_lag1", "ctransferencias_recibidas_lag2"))) {
    dataset[, aceleracion_ctransferencias_recibidas := (ctransferencias_recibidas - 2 * ctransferencias_recibidas_lag1 + ctransferencias_recibidas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtransferencias_recibidas
  if(atributos_presentes(c("mtransferencias_recibidas", "mtransferencias_recibidas_lag1", "mtransferencias_recibidas_lag2"))) {
    dataset[, aceleracion_mtransferencias_recibidas := (mtransferencias_recibidas - 2 * mtransferencias_recibidas_lag1 + mtransferencias_recibidas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctransferencias_emitidas
  if(atributos_presentes(c("ctransferencias_emitidas", "ctransferencias_emitidas_lag1", "ctransferencias_emitidas_lag2"))) {
    dataset[, aceleracion_ctransferencias_emitidas := (ctransferencias_emitidas - 2 * ctransferencias_emitidas_lag1 + ctransferencias_emitidas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mtransferencias_emitidas
  if(atributos_presentes(c("mtransferencias_emitidas", "mtransferencias_emitidas_lag1", "mtransferencias_emitidas_lag2"))) {
    dataset[, aceleracion_mtransferencias_emitidas := (mtransferencias_emitidas - 2 * mtransferencias_emitidas_lag1 + mtransferencias_emitidas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cextraccion_autoservicio
  if(atributos_presentes(c("cextraccion_autoservicio", "cextraccion_autoservicio_lag1", "cextraccion_autoservicio_lag2"))) {
    dataset[, aceleracion_cextraccion_autoservicio := (cextraccion_autoservicio - 2 * cextraccion_autoservicio_lag1 + cextraccion_autoservicio_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mextraccion_autoservicio
  if(atributos_presentes(c("mextraccion_autoservicio", "mextraccion_autoservicio_lag1", "mextraccion_autoservicio_lag2"))) {
    dataset[, aceleracion_mextraccion_autoservicio := (mextraccion_autoservicio - 2 * mextraccion_autoservicio_lag1 + mextraccion_autoservicio_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccheques_depositados
  if(atributos_presentes(c("ccheques_depositados", "ccheques_depositados_lag1", "ccheques_depositados_lag2"))) {
    dataset[, aceleracion_ccheques_depositados := (ccheques_depositados - 2 * ccheques_depositados_lag1 + ccheques_depositados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcheques_depositados
  if(atributos_presentes(c("mcheques_depositados", "mcheques_depositados_lag1", "mcheques_depositados_lag2"))) {
    dataset[, aceleracion_mcheques_depositados := (mcheques_depositados - 2 * mcheques_depositados_lag1 + mcheques_depositados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccheques_emitidos
  if(atributos_presentes(c("ccheques_emitidos", "ccheques_emitidos_lag1", "ccheques_emitidos_lag2"))) {
    dataset[, aceleracion_ccheques_emitidos := (ccheques_emitidos - 2 * ccheques_emitidos_lag1 + ccheques_emitidos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcheques_emitidos
  if(atributos_presentes(c("mcheques_emitidos", "mcheques_emitidos_lag1", "mcheques_emitidos_lag2"))) {
    dataset[, aceleracion_mcheques_emitidos := (mcheques_emitidos - 2 * mcheques_emitidos_lag1 + mcheques_emitidos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccheques_depositados_rechazados
  if(atributos_presentes(c("ccheques_depositados_rechazados", "ccheques_depositados_rechazados_lag1", "ccheques_depositados_rechazados_lag2"))) {
    dataset[, aceleracion_ccheques_depositados_rechazados := (ccheques_depositados_rechazados - 2 * ccheques_depositados_rechazados_lag1 + ccheques_depositados_rechazados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcheques_depositados_rechazados
  if(atributos_presentes(c("mcheques_depositados_rechazados", "mcheques_depositados_rechazados_lag1", "mcheques_depositados_rechazados_lag2"))) {
    dataset[, aceleracion_mcheques_depositados_rechazados := (mcheques_depositados_rechazados - 2 * mcheques_depositados_rechazados_lag1 + mcheques_depositados_rechazados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccheques_emitidos_rechazados
  if(atributos_presentes(c("ccheques_emitidos_rechazados", "ccheques_emitidos_rechazados_lag1", "ccheques_emitidos_rechazados_lag2"))) {
    dataset[, aceleracion_ccheques_emitidos_rechazados := (ccheques_emitidos_rechazados - 2 * ccheques_emitidos_rechazados_lag1 + ccheques_emitidos_rechazados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable mcheques_emitidos_rechazados
  if(atributos_presentes(c("mcheques_emitidos_rechazados", "mcheques_emitidos_rechazados_lag1", "mcheques_emitidos_rechazados_lag2"))) {
    dataset[, aceleracion_mcheques_emitidos_rechazados := (mcheques_emitidos_rechazados - 2 * mcheques_emitidos_rechazados_lag1 + mcheques_emitidos_rechazados_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable tcallcenter
  if(atributos_presentes(c("tcallcenter", "tcallcenter_lag1", "tcallcenter_lag2"))) {
    dataset[, aceleracion_tcallcenter := (tcallcenter - 2 * tcallcenter_lag1 + tcallcenter_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccallcenter_transacciones
  if(atributos_presentes(c("ccallcenter_transacciones", "ccallcenter_transacciones_lag1", "ccallcenter_transacciones_lag2"))) {
    dataset[, aceleracion_ccallcenter_transacciones := (ccallcenter_transacciones - 2 * ccallcenter_transacciones_lag1 + ccallcenter_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable thomebanking
  if(atributos_presentes(c("thomebanking", "thomebanking_lag1", "thomebanking_lag2"))) {
    dataset[, aceleracion_thomebanking := (thomebanking - 2 * thomebanking_lag1 + thomebanking_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable chomebanking_transacciones
  if(atributos_presentes(c("chomebanking_transacciones", "chomebanking_transacciones_lag1", "chomebanking_transacciones_lag2"))) {
    dataset[, aceleracion_chomebanking_transacciones := (chomebanking_transacciones - 2 * chomebanking_transacciones_lag1 + chomebanking_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajas_transacciones
  if(atributos_presentes(c("ccajas_transacciones", "ccajas_transacciones_lag1", "ccajas_transacciones_lag2"))) {
    dataset[, aceleracion_ccajas_transacciones := (ccajas_transacciones - 2 * ccajas_transacciones_lag1 + ccajas_transacciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajas_consultas
  if(atributos_presentes(c("ccajas_consultas", "ccajas_consultas_lag1", "ccajas_consultas_lag2"))) {
    dataset[, aceleracion_ccajas_consultas := (ccajas_consultas - 2 * ccajas_consultas_lag1 + ccajas_consultas_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajas_depositos
  if(atributos_presentes(c("ccajas_depositos", "ccajas_depositos_lag1", "ccajas_depositos_lag2"))) {
    dataset[, aceleracion_ccajas_depositos := (ccajas_depositos - 2 * ccajas_depositos_lag1 + ccajas_depositos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajas_extracciones
  if(atributos_presentes(c("ccajas_extracciones", "ccajas_extracciones_lag1", "ccajas_extracciones_lag2"))) {
    dataset[, aceleracion_ccajas_extracciones := (ccajas_extracciones - 2 * ccajas_extracciones_lag1 + ccajas_extracciones_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ccajas_otras
  if(atributos_presentes(c("ccajas_otras", "ccajas_otras_lag1", "ccajas_otras_lag2"))) {
    dataset[, aceleracion_ccajas_otras := (ccajas_otras - 2 * ccajas_otras_lag1 + ccajas_otras_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable catm_trx
  if(atributos_presentes(c("catm_trx", "catm_trx_lag1", "catm_trx_lag2"))) {
    dataset[, aceleracion_catm_trx := (catm_trx - 2 * catm_trx_lag1 + catm_trx_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable matm
  if(atributos_presentes(c("matm", "matm_lag1", "matm_lag2"))) {
    dataset[, aceleracion_matm := (matm - 2 * matm_lag1 + matm_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable catm_trx_other
  if(atributos_presentes(c("catm_trx_other", "catm_trx_other_lag1", "catm_trx_other_lag2"))) {
    dataset[, aceleracion_catm_trx_other := (catm_trx_other - 2 * catm_trx_other_lag1 + catm_trx_other_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable matm_other
  if(atributos_presentes(c("matm_other", "matm_other_lag1", "matm_other_lag2"))) {
    dataset[, aceleracion_matm_other := (matm_other - 2 * matm_other_lag1 + matm_other_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable ctrx_quarter
  if(atributos_presentes(c("ctrx_quarter", "ctrx_quarter_lag1", "ctrx_quarter_lag2"))) {
    dataset[, aceleracion_ctrx_quarter := (ctrx_quarter - 2 * ctrx_quarter_lag1 + ctrx_quarter_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable tmobile_app
  if(atributos_presentes(c("tmobile_app", "tmobile_app_lag1", "tmobile_app_lag2"))) {
    dataset[, aceleracion_tmobile_app := (tmobile_app - 2 * tmobile_app_lag1 + tmobile_app_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable cmobile_app_trx
  if(atributos_presentes(c("cmobile_app_trx", "cmobile_app_trx_lag1", "cmobile_app_trx_lag2"))) {
    dataset[, aceleracion_cmobile_app_trx := (cmobile_app_trx - 2 * cmobile_app_trx_lag1 + cmobile_app_trx_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_delinquency
  if(atributos_presentes(c("Master_delinquency", "Master_delinquency_lag1", "Master_delinquency_lag2"))) {
    dataset[, aceleracion_Master_delinquency := (Master_delinquency - 2 * Master_delinquency_lag1 + Master_delinquency_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_status
  if(atributos_presentes(c("Master_status", "Master_status_lag1", "Master_status_lag2"))) {
    dataset[, aceleracion_Master_status := (Master_status - 2 * Master_status_lag1 + Master_status_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mfinanciacion_limite
  if(atributos_presentes(c("Master_mfinanciacion_limite", "Master_mfinanciacion_limite_lag1", "Master_mfinanciacion_limite_lag2"))) {
    dataset[, aceleracion_Master_mfinanciacion_limite := (Master_mfinanciacion_limite - 2 * Master_mfinanciacion_limite_lag1 + Master_mfinanciacion_limite_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_Fvencimiento
  if(atributos_presentes(c("Master_Fvencimiento", "Master_Fvencimiento_lag1", "Master_Fvencimiento_lag2"))) {
    dataset[, aceleracion_Master_Fvencimiento := (Master_Fvencimiento - 2 * Master_Fvencimiento_lag1 + Master_Fvencimiento_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_Finiciomora
  if(atributos_presentes(c("Master_Finiciomora", "Master_Finiciomora_lag1", "Master_Finiciomora_lag2"))) {
    dataset[, aceleracion_Master_Finiciomora := (Master_Finiciomora - 2 * Master_Finiciomora_lag1 + Master_Finiciomora_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_msaldototal
  if(atributos_presentes(c("Master_msaldototal", "Master_msaldototal_lag1", "Master_msaldototal_lag2"))) {
    dataset[, aceleracion_Master_msaldototal := (Master_msaldototal - 2 * Master_msaldototal_lag1 + Master_msaldototal_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_msaldopesos
  if(atributos_presentes(c("Master_msaldopesos", "Master_msaldopesos_lag1", "Master_msaldopesos_lag2"))) {
    dataset[, aceleracion_Master_msaldopesos := (Master_msaldopesos - 2 * Master_msaldopesos_lag1 + Master_msaldopesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_msaldodolares
  if(atributos_presentes(c("Master_msaldodolares", "Master_msaldodolares_lag1", "Master_msaldodolares_lag2"))) {
    dataset[, aceleracion_Master_msaldodolares := (Master_msaldodolares - 2 * Master_msaldodolares_lag1 + Master_msaldodolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mconsumospesos
  if(atributos_presentes(c("Master_mconsumospesos", "Master_mconsumospesos_lag1", "Master_mconsumospesos_lag2"))) {
    dataset[, aceleracion_Master_mconsumospesos := (Master_mconsumospesos - 2 * Master_mconsumospesos_lag1 + Master_mconsumospesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mconsumosdolares
  if(atributos_presentes(c("Master_mconsumosdolares", "Master_mconsumosdolares_lag1", "Master_mconsumosdolares_lag2"))) {
    dataset[, aceleracion_Master_mconsumosdolares := (Master_mconsumosdolares - 2 * Master_mconsumosdolares_lag1 + Master_mconsumosdolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mlimitecompra
  if(atributos_presentes(c("Master_mlimitecompra", "Master_mlimitecompra_lag1", "Master_mlimitecompra_lag2"))) {
    dataset[, aceleracion_Master_mlimitecompra := (Master_mlimitecompra - 2 * Master_mlimitecompra_lag1 + Master_mlimitecompra_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_madelantopesos
  if(atributos_presentes(c("Master_madelantopesos", "Master_madelantopesos_lag1", "Master_madelantopesos_lag2"))) {
    dataset[, aceleracion_Master_madelantopesos := (Master_madelantopesos - 2 * Master_madelantopesos_lag1 + Master_madelantopesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_madelantodolares
  if(atributos_presentes(c("Master_madelantodolares", "Master_madelantodolares_lag1", "Master_madelantodolares_lag2"))) {
    dataset[, aceleracion_Master_madelantodolares := (Master_madelantodolares - 2 * Master_madelantodolares_lag1 + Master_madelantodolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_fultimo_cierre
  if(atributos_presentes(c("Master_fultimo_cierre", "Master_fultimo_cierre_lag1", "Master_fultimo_cierre_lag2"))) {
    dataset[, aceleracion_Master_fultimo_cierre := (Master_fultimo_cierre - 2 * Master_fultimo_cierre_lag1 + Master_fultimo_cierre_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mpagado
  if(atributos_presentes(c("Master_mpagado", "Master_mpagado_lag1", "Master_mpagado_lag2"))) {
    dataset[, aceleracion_Master_mpagado := (Master_mpagado - 2 * Master_mpagado_lag1 + Master_mpagado_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mpagospesos
  if(atributos_presentes(c("Master_mpagospesos", "Master_mpagospesos_lag1", "Master_mpagospesos_lag2"))) {
    dataset[, aceleracion_Master_mpagospesos := (Master_mpagospesos - 2 * Master_mpagospesos_lag1 + Master_mpagospesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mpagosdolares
  if(atributos_presentes(c("Master_mpagosdolares", "Master_mpagosdolares_lag1", "Master_mpagosdolares_lag2"))) {
    dataset[, aceleracion_Master_mpagosdolares := (Master_mpagosdolares - 2 * Master_mpagosdolares_lag1 + Master_mpagosdolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_fechaalta
  if(atributos_presentes(c("Master_fechaalta", "Master_fechaalta_lag1", "Master_fechaalta_lag2"))) {
    dataset[, aceleracion_Master_fechaalta := (Master_fechaalta - 2 * Master_fechaalta_lag1 + Master_fechaalta_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mconsumototal
  if(atributos_presentes(c("Master_mconsumototal", "Master_mconsumototal_lag1", "Master_mconsumototal_lag2"))) {
    dataset[, aceleracion_Master_mconsumototal := (Master_mconsumototal - 2 * Master_mconsumototal_lag1 + Master_mconsumototal_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_cconsumos
  if(atributos_presentes(c("Master_cconsumos", "Master_cconsumos_lag1", "Master_cconsumos_lag2"))) {
    dataset[, aceleracion_Master_cconsumos := (Master_cconsumos - 2 * Master_cconsumos_lag1 + Master_cconsumos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_cadelantosefectivo
  if(atributos_presentes(c("Master_cadelantosefectivo", "Master_cadelantosefectivo_lag1", "Master_cadelantosefectivo_lag2"))) {
    dataset[, aceleracion_Master_cadelantosefectivo := (Master_cadelantosefectivo - 2 * Master_cadelantosefectivo_lag1 + Master_cadelantosefectivo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Master_mpagominimo
  if(atributos_presentes(c("Master_mpagominimo", "Master_mpagominimo_lag1", "Master_mpagominimo_lag2"))) {
    dataset[, aceleracion_Master_mpagominimo := (Master_mpagominimo - 2 * Master_mpagominimo_lag1 + Master_mpagominimo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_delinquency
  if(atributos_presentes(c("Visa_delinquency", "Visa_delinquency_lag1", "Visa_delinquency_lag2"))) {
    dataset[, aceleracion_Visa_delinquency := (Visa_delinquency - 2 * Visa_delinquency_lag1 + Visa_delinquency_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_status
  if(atributos_presentes(c("Visa_status", "Visa_status_lag1", "Visa_status_lag2"))) {
    dataset[, aceleracion_Visa_status := (Visa_status - 2 * Visa_status_lag1 + Visa_status_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mfinanciacion_limite
  if(atributos_presentes(c("Visa_mfinanciacion_limite", "Visa_mfinanciacion_limite_lag1", "Visa_mfinanciacion_limite_lag2"))) {
    dataset[, aceleracion_Visa_mfinanciacion_limite := (Visa_mfinanciacion_limite - 2 * Visa_mfinanciacion_limite_lag1 + Visa_mfinanciacion_limite_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_Fvencimiento
  if(atributos_presentes(c("Visa_Fvencimiento", "Visa_Fvencimiento_lag1", "Visa_Fvencimiento_lag2"))) {
    dataset[, aceleracion_Visa_Fvencimiento := (Visa_Fvencimiento - 2 * Visa_Fvencimiento_lag1 + Visa_Fvencimiento_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_Finiciomora
  if(atributos_presentes(c("Visa_Finiciomora", "Visa_Finiciomora_lag1", "Visa_Finiciomora_lag2"))) {
    dataset[, aceleracion_Visa_Finiciomora := (Visa_Finiciomora - 2 * Visa_Finiciomora_lag1 + Visa_Finiciomora_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_msaldototal
  if(atributos_presentes(c("Visa_msaldototal", "Visa_msaldototal_lag1", "Visa_msaldototal_lag2"))) {
    dataset[, aceleracion_Visa_msaldototal := (Visa_msaldototal - 2 * Visa_msaldototal_lag1 + Visa_msaldototal_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_msaldopesos
  if(atributos_presentes(c("Visa_msaldopesos", "Visa_msaldopesos_lag1", "Visa_msaldopesos_lag2"))) {
    dataset[, aceleracion_Visa_msaldopesos := (Visa_msaldopesos - 2 * Visa_msaldopesos_lag1 + Visa_msaldopesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_msaldodolares
  if(atributos_presentes(c("Visa_msaldodolares", "Visa_msaldodolares_lag1", "Visa_msaldodolares_lag2"))) {
    dataset[, aceleracion_Visa_msaldodolares := (Visa_msaldodolares - 2 * Visa_msaldodolares_lag1 + Visa_msaldodolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mconsumospesos
  if(atributos_presentes(c("Visa_mconsumospesos", "Visa_mconsumospesos_lag1", "Visa_mconsumospesos_lag2"))) {
    dataset[, aceleracion_Visa_mconsumospesos := (Visa_mconsumospesos - 2 * Visa_mconsumospesos_lag1 + Visa_mconsumospesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mconsumosdolares
  if(atributos_presentes(c("Visa_mconsumosdolares", "Visa_mconsumosdolares_lag1", "Visa_mconsumosdolares_lag2"))) {
    dataset[, aceleracion_Visa_mconsumosdolares := (Visa_mconsumosdolares - 2 * Visa_mconsumosdolares_lag1 + Visa_mconsumosdolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mlimitecompra
  if(atributos_presentes(c("Visa_mlimitecompra", "Visa_mlimitecompra_lag1", "Visa_mlimitecompra_lag2"))) {
    dataset[, aceleracion_Visa_mlimitecompra := (Visa_mlimitecompra - 2 * Visa_mlimitecompra_lag1 + Visa_mlimitecompra_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_madelantopesos
  if(atributos_presentes(c("Visa_madelantopesos", "Visa_madelantopesos_lag1", "Visa_madelantopesos_lag2"))) {
    dataset[, aceleracion_Visa_madelantopesos := (Visa_madelantopesos - 2 * Visa_madelantopesos_lag1 + Visa_madelantopesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_madelantodolares
  if(atributos_presentes(c("Visa_madelantodolares", "Visa_madelantodolares_lag1", "Visa_madelantodolares_lag2"))) {
    dataset[, aceleracion_Visa_madelantodolares := (Visa_madelantodolares - 2 * Visa_madelantodolares_lag1 + Visa_madelantodolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_fultimo_cierre
  if(atributos_presentes(c("Visa_fultimo_cierre", "Visa_fultimo_cierre_lag1", "Visa_fultimo_cierre_lag2"))) {
    dataset[, aceleracion_Visa_fultimo_cierre := (Visa_fultimo_cierre - 2 * Visa_fultimo_cierre_lag1 + Visa_fultimo_cierre_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mpagado
  if(atributos_presentes(c("Visa_mpagado", "Visa_mpagado_lag1", "Visa_mpagado_lag2"))) {
    dataset[, aceleracion_Visa_mpagado := (Visa_mpagado - 2 * Visa_mpagado_lag1 + Visa_mpagado_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mpagospesos
  if(atributos_presentes(c("Visa_mpagospesos", "Visa_mpagospesos_lag1", "Visa_mpagospesos_lag2"))) {
    dataset[, aceleracion_Visa_mpagospesos := (Visa_mpagospesos - 2 * Visa_mpagospesos_lag1 + Visa_mpagospesos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mpagosdolares
  if(atributos_presentes(c("Visa_mpagosdolares", "Visa_mpagosdolares_lag1", "Visa_mpagosdolares_lag2"))) {
    dataset[, aceleracion_Visa_mpagosdolares := (Visa_mpagosdolares - 2 * Visa_mpagosdolares_lag1 + Visa_mpagosdolares_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_fechaalta
  if(atributos_presentes(c("Visa_fechaalta", "Visa_fechaalta_lag1", "Visa_fechaalta_lag2"))) {
    dataset[, aceleracion_Visa_fechaalta := (Visa_fechaalta - 2 * Visa_fechaalta_lag1 + Visa_fechaalta_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mconsumototal
  if(atributos_presentes(c("Visa_mconsumototal", "Visa_mconsumototal_lag1", "Visa_mconsumototal_lag2"))) {
    dataset[, aceleracion_Visa_mconsumototal := (Visa_mconsumototal - 2 * Visa_mconsumototal_lag1 + Visa_mconsumototal_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_cconsumos
  if(atributos_presentes(c("Visa_cconsumos", "Visa_cconsumos_lag1", "Visa_cconsumos_lag2"))) {
    dataset[, aceleracion_Visa_cconsumos := (Visa_cconsumos - 2 * Visa_cconsumos_lag1 + Visa_cconsumos_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_cadelantosefectivo
  if(atributos_presentes(c("Visa_cadelantosefectivo", "Visa_cadelantosefectivo_lag1", "Visa_cadelantosefectivo_lag2"))) {
    dataset[, aceleracion_Visa_cadelantosefectivo := (Visa_cadelantosefectivo - 2 * Visa_cadelantosefectivo_lag1 + Visa_cadelantosefectivo_lag2) / 1^2]
  }
  
  # Verificación y creación de la variable Visa_mpagominimo
  if(atributos_presentes(c("Visa_mpagominimo", "Visa_mpagominimo_lag1", "Visa_mpagominimo_lag2"))) {
    dataset[, aceleracion_Visa_mpagominimo := (Visa_mpagominimo - 2 * Visa_mpagominimo_lag1 + Visa_mpagominimo_lag2) / 1^2]
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
