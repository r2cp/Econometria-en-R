get_transformed_df <- function(data, remove.labels, fn, fn.label) {
  # Obtener las columnas de data
  col.names <- names(data)
  # Quitar la lista de columnas especificada por remove.labels
  inputs <- setdiff(col.names, remove.labels)
  # Aplicar la transformación fn a data
  output.df <- fn(data[inputs])
  # Modificar sus nombres de columnas de acuerdo con fn.label
  names(output.df) <- paste(names(output.df), fn.label, sep=".")
  return(output.df)
}
