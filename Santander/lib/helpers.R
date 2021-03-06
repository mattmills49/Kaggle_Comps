make_calibration <- function(raw_df){
  cal_accounts <- readRDS("Santander/cache/cal_accounts.rds")
  cal_data <- dplyr::inner_join(raw_df, cal_accounts, by = "ncodpers")
  return(cal_data)
}

make_train <- function(raw_df){
  cal_accounts <- readRDS("Santander/cache/cal_accounts.rds")
  train_data <- dplyr::anti_join(raw_df, cal_accounts, by = "ncodpers")
  return(train_data)
}

clean_names <- function(train_df){
  good_names <- c('fecha_dato' = "fetch_date",
                  'ncodpers' = "customer_code",
                  'ind_empleado' = "employee_index",
                  'pais_residencia' = "country_residence",
                  'sexo' = "sex", 
                  'age' = "age", 
                  'fecha_alta' = "signup_date", 
                  'ind_nuevo' = "new_customer", 
                  'antiguedad' = "seniority", 
                  'indrel' = "completed_month",
                  'ult_fec_cli_1t' = "leave_date",
                  'indrel_1mes' = "customer_type",
                  'tiprel_1mes' = "customer_relation", 
                  'indresi' = "residence_same_as_bank", 
                  'indext' = "foreigner", 
                  'conyuemp' = "employee_spouse", 
                  'canal_entrada' = "join_channel", 
                  'indfall' = "dead",
                  'tipodom' = "addres_type",
                  'cod_prov' = "province_code",
                  'nomprov' = "province_name",
                  'ind_actividad_cliente' = "activity_index",
                  'renta' = "gross_income",
                  'segmento' = "segment",
                  'ind_ahor_fin_ult1' = 'savings',
                  'ind_aval_fin_ult1' = 'guarentees',
                  'ind_cco_fin_ult1' = 'current_accounts',
                  'ind_cder_fin_ult1' = 'derivative',
                  'ind_cno_fin_ult1' = 'payroll_account',
                  'ind_ctju_fin_ult1' = 'junior',
                  'ind_ctma_fin_ult1' = 'particular_mas',
                  'ind_ctop_fin_ult1' = 'particular',
                  'ind_ctpp_fin_ult1' = 'particular_plus',
                  'ind_deco_fin_ult1' = 'deposits_short_term',
                  'ind_deme_fin_ult1' = 'deposits_medium_term',
                  'ind_dela_fin_ult1' = 'deposits_long_term',
                  'ind_ecue_fin_ult1' = 'e_account',
                  'ind_fond_fin_ult1' = 'funds',
                  'ind_hip_fin_ult1' = 'mortgage',
                  'ind_plan_fin_ult1' = 'pensions',
                  'ind_pres_fin_ult1' = 'loans',
                  'ind_reca_fin_ult1' = 'taxes',
                  'ind_tjcr_fin_ult1' = 'credit_card',
                  'ind_valo_fin_ult1' = 'securities',
                  'ind_viv_fin_ult1' = 'home_account',
                  'ind_nomina_ult1' = 'payroll',
                  'ind_nom_pens_ult1' = 'pensions_2',
                  'ind_recibo_ult1' = 'direct_debit')
  
  colnames(train_df) <- good_names[colnames(train_df)]
  return(train_df)
}

col_types <- cols(
  .default = col_integer(),
  fecha_dato = col_date(format = ""),
  ind_empleado = col_character(),
  pais_residencia = col_character(),
  sexo = col_character(),
  fecha_alta = col_date(format = ""),
  ult_fec_cli_1t = col_date(format = ""),
  indrel_1mes = col_character(),
  tiprel_1mes = col_character(),
  indresi = col_character(),
  indext = col_character(),
  conyuemp = col_character(),
  canal_entrada = col_character(),
  indfall = col_character(),
  nomprov = col_character(),
  renta = col_double(),
  segmento = col_character(),
  ind_nuevo = col_character(),
  ind_actividad_cliente = col_character(),
  indrel = col_character()
)

theme_mells <- ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 12),
                 plot.title = ggplot2::element_text(face = "bold", size = 14))

