
# LIBRERIAS ---------------------------------------------------------------


library(xtable)
library(data.table)

#Este código es tomado de: sdaza.com. Permite crear tablas de variables descriptivas con base en una función descriptiva 
#que describe las funciones que se quieren utilizar.


# FUNCION -----------------------------------------------------------------


createDescriptiveTable = function(datasets,
                                  summary_function,
                                  column_names = NULL,
                                  variable_names,
                                  variable_labels = NULL,
                                  group_variable = NULL,
                                  align = NULL,
                                  digits = NULL,
                                  title = "Descriptive statistics",
                                  label = "tab:descriptive",
                                  fontsize = "\\fontsize{9pt}{10pt}\\selectfont",
                                  note = NULL,
                                  notesize = "footnotesize",
                                  arraystretch = 1.5,
                                  tabcolsep = 10,
                                  file = "descriptive_table.tex") {
  
  # initial checks
  if (is.list(datasets)) {
    for (i in seq_along(datasets)) {
      if (!is.data.table(datasets[[i]])) {
        datasets[[i]] = as.data.table(datasets[[i]])
      }
    }
  }
  else  {
    if (!is.data.table(datasets)) {
      datasets = as.data.table(datasets)
    }
  }
  
  if (any(class(datasets) %in% "data.table")) { datasets = list(datasets) }
  if (!is.list(variable_names)) { variable_names = list(variable_names) }
  if (!is.list(variable_labels)) { variable_labels = list(variable_labels) }
  if (is.null(variable_labels)) { variables_labels = variable_names }
  length_lists = unique(length(datasets),
                        length(variable_names),
                        length(variable_labels)
  )
  if (length(length_lists) > 1) {
    stop("Datasets, column names, and variable_labels should be lists of same length")
  }
  
  org_column_names = column_names
  
  # list to save tabs
  tabs = list()
  factor_header = list()
  sumrows = 0
  
  for (i in seq_along(datasets)) {
    
    if (i > 1) { sumrows = sumrows + nrow(tabs[[i-1]]) }
    
    if (is.null(group_variable)) { vars = variable_names[[i]] }
    else { vars = c(group_variable, variable_names[[i]]) }
    tabs[[i]] = data.table::copy(datasets[[i]][, ..vars])
    
    factor_dataset_list = list()
    if (length(variable_names[[i]]) != length(variable_labels[[i]])) {
      stop("Length of variable is different from length of labels!")
    }
    factor_names = names(tabs[[i]])[sapply(tabs[[i]], is.factor)]
    
    # create dummy variables
    for (h in seq_along(factor_names)) {
      categories = as.vector(na.omit(sort(unique(tabs[[i]][[factor_names[h]]]))))
      for (j in seq_along(categories)) {
        tabs[[i]][, (paste0(factor_names[h], "_", j)) :=
                    ifelse(get(factor_names[h]) == categories[j], 1, 0)]
      }
      
      # position and variable name
      position = which(variable_names[[i]] == factor_names[h])
      if (i == 1) {
        adj_position = ifelse(position == 1, 1, position - 1)
      } else {
        adj_position = sumrows - 1 + position
      }
      
      variable_names[[i]] = variable_names[[i]][-position]
      temp_position = ifelse(i == 1, adj_position, position - 1)
      variable_names[[i]] = append(variable_names[[i]], paste0(factor_names[h], "_",
                                                               1:length(categories)), after =  temp_position)
      lname = variable_labels[[i]][position]
      factor_dataset_list[[lname]] = c(adj_position, adj_position + length(categories))
      variable_labels[[i]] = variable_labels[[i]][-position]
      variable_labels[[i]] = append(variable_labels[[i]], paste0("\\quad ", categories),
                                    after = temp_position)
      
    }
    
    factor_header[[i]] = factor_dataset_list
    factor_data_list = list()
    
    # descriptive estimation
    if (is.null(group_variable)) {
      tabs[[i]] = transpose(tabs[[i]][, lapply(.SD, summary_function),
                                      .SDcols = variable_names[[i]]], keep.names = "variables")
      if (is.null(column_names) & i == 1) {
        column_names = paste0("C", 1:(dim(tabs[[i]])[2]-1))
      }
    } else if (length(group_variable) == 1) {
      group_variable_values = sort(na.omit(unique(tabs[[i]][[group_variable]])))
      setorderv(tabs[[i]], group_variable)
      tabs[[i]] = tabs[[i]][, lapply(.SD, summary_function), group_variable,
                            .SDcols = variable_names[[i]]]
      num_cols = tabs[[i]][, .N, group_variable]$N[1]
      
      if (is.null(column_names) & i == 1) {
        if (num_cols != length(column_names)) {
          stop("Number of column names is not the same as outputs of the summary function!")
        }
        column_names = paste0("C", 1:num_cols)
        org_column_names = column_names
      }
      tabs[[i]][, summary := rep(org_column_names, length(group_variable_values))]
      column_names = as.vector(outer(org_column_names, group_variable_values, paste))
      tabs[[i]] = dcast(
        melt(tabs[[i]], id.vars = c(group_variable, "summary"), variable.name = "variables"),
        variables ~ get(group_variable) + summary
      )
    } else {
      stop("Group variable should have length 1!")
    }
    
    if (dim(tabs[[i]])[2] != (length(column_names) + 1)) {
      stop("Descriptive function values are not equal to the number of column names")
    }
    colnames(tabs[[i]]) = c("Variables", column_names)
    tabs[[i]][["Variables"]] = variable_labels[[i]]
  }
  
  if (length(tabs) == 1) { ftabs = tabs[[1]] }
  else { ftabs = rbindlist(tabs) }
  
  total_rows = dim(ftabs)[1]
  total_columns = dim(ftabs)[2]
  
  if (is.null(align)) {
    align = paste0(c("l", "l", rep("r", total_columns - 1)), collapse  = "")
  }
  if (is.null(digits)) {
    digits = rep(2, total_columns + 1)
  }
  if (!is.null(digits) & length(digits) < (total_columns + 1)) {
    digits = c(rep(0, (total_columns + 1) - length(digits)), digits)
  }
  
  # create headers
  addtorow = list()
  positions = -1
  command = list()
  new_columns = c(" ", column_names)
  command[[1]]  = paste0(
    "\\hline\n\\addlinespace\n", paste0(new_columns, collapse = " & "), " \\\\\n\\addlinespace\n")
  
  name_datasets = names(datasets)
  if (is.null(name_datasets)) { name_datasets = rep("", length(datasets)) }
  sumrows = 0
  
  for (i in seq_along(datasets)) {
    
    if (i > 1) { sumrows = sumrows + nrow(tabs[[i-1]]) }
    # conditions for dataset position
    if (i == 1 & name_datasets[i] != "") {
      positions = append(positions, tail(positions, 1) + 1)
    }
    else if (i > 1 & name_datasets[i] != "") {
      positions = append(positions, sumrows)
    }
    if (name_datasets[i] != "") {
      command[[length(command) + 1]] = paste0(
        "\\addlinespace\n\\multicolumn{", total_columns, "}{l}{\\textbf{\\textit{", name_datasets[i], "}}} \\\\\n\\addlinespace\n"
      )
    }
    
    if (length(factor_header[[i]]) > 0) {
      
      for (h in seq_along(factor_header[[i]])) {
        factor_position = factor_header[[i]][[h]][1]
        factor_end_position = factor_header[[i]][[h]][2]
        text = paste0(
          "\\addlinespace\n\\multicolumn{", total_columns, "}{l}{\\textit{", names(factor_header[[i]])[h], "}} \\\\\n")
        text_end = "\\addlinespace\n"
        if (factor_position == 0 & !is.null(name_datasets)) {
          command[[2]] = paste0(command[[2]], text)
        } else {
          positions = append(positions, factor_position)
          command[[length(command) + 1]] = text
          positions = append(positions, factor_end_position)
          command[[length(command) + 1]] = text_end
        }
      }
    }
  }
  
  positions = append(positions, total_rows)
  command[[length(command) + 1]] = "\\addlinespace\n\\hline\n\\addlinespace\n"
  
  addtorow$command = unlist(command)
  addtorow$pos = as.list(positions)
  
  # create latex table
  latex_table = xtable(ftabs,
                       caption = title,
                       label= label,
                       align = align,
                       digits = digits)
  
  output = print(latex_table,
                 hline.after = c(-1),
                 table.placement = 'htp',
                 caption.placement = 'top',
                 add.to.row = addtorow,
                 include.rownames = FALSE,
                 include.colnames = FALSE,
                 type = 'latex',
                 sanitize.text.function = identity,
                 size = fontsize,
  )
  
  # create header
  if (!is.null(group_variable)) {
    
    group_length = rep(length(org_column_names), length(group_variable_values))
    
    # simple header
    table_header = paste0("& ", column_names[1], ".+", column_names[length(column_names)], " \\\\")
    print(table_header)
    # nested header
    table_header_line1 = paste0(paste0("& \\\\multicolumn\\{", group_length, "\\}\\{c\\}\\{",
                                       as.character(group_variable_values), "\\}"), collapse = " ")
    
    # positions
    rule_position = NULL
    for (i in seq(group_variable_values)) {
      stat_number = length(org_column_names)
      if (i == 1) {
        last_value = 2 + stat_number - 1
        rule_position[i] = paste0("2", "-", last_value)
      } else {
        rule_position[i] = paste0(last_value + 1, "-", last_value + stat_number)
        last_value = last_value + stat_number
      }
    }
    
    # rule
    table_header_line2 = paste0(paste0("\\\\cmidrule(lr)\\{", rule_position, "\\}"), collapse = " ")
    # stats
    table_header_line3 = paste0("& ", rep(org_column_names, length(group_variable_values)),
                                collapse = " ")
    
    header_replacement = paste0(table_header_line1, " \\\\\\\\ \\\n", table_header_line2,
                                " \\\n", table_header_line3, " \\\\")
    output = gsub(table_header, header_replacement, output)
    print(output)
  }
  
  # adding comment
  if (!is.null(note)) {
    clean_note = longText(note)
    header_replacement = paste0("begin\\{table\\}\\[htp\\]\\\n",
                                "\\\\setlength\\{\\\\tabcolsep\\}\\{",
                                tabcolsep,
                                "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                                arraystretch,
                                "\\}\\\n\\\\centering\\\n\\\\begin\\{threeparttable\\}\\\n")
    bottom = "end\\{tabular\\}\\\n\\\\endgroup\\\n"
    bottom_replacement = paste0("end\\{tabular\\}\\\n\\\\endgroup\\\n\\\\begin{tablenotes}\\\n\\\\", notesize, "\\\n\\\\item ",
                                note,
                                "\\\n\\\\end{tablenotes}")
    
    add_notes_table(output,
                    comment = clean_note,
                    header_replacement = header_replacement,
                    bottom = bottom,
                    bottom_replacement = bottom_replacement,
                    closing = "end\\{tablenotes\\}",
                    closing_replacement = "end\\{tablenotes\\}\\\n\\\\end{threeparttable}\\\n",
                    filename = file)
  } else {
    header = "begin\\{table\\}\\[htp\\]\\\n"
    header_replacement = paste0("begin\\{table\\}\\[htp\\]\\\n",
                                "\\\\setlength\\{\\\\tabcolsep\\}\\{",
                                tabcolsep,
                                "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                                arraystretch, "}\\\n")
    output = gsub(header, header_replacement, output)
    cat(output, file = file)
  }
}
