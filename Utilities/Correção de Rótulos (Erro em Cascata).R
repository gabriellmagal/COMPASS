library(writexl)

selecionar_pasta <- function(titulo = "Selecione uma pasta") {
  caminho <- NA; tryCatch({ if (Sys.getenv("RSTUDIO") == "1" && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) { caminho <- rstudioapi::selectDirectory(caption = titulo) } else { if (requireNamespace("tcltk", quietly = TRUE)) { caminho <- tcltk::tclvalue(tcltk::tkchooseDirectory(title = titulo)) } else { stop("Nenhum GUI disponível.") } } }, error = function(e) { message("Erro: ", e$message); return(NULL) }); if (is.null(caminho) || is.na(caminho) || nchar(caminho) == 0) return(NULL); return(normalizePath(caminho))
}
selecionar_arquivo <- function(titulo = "Selecione um arquivo") {
  caminho <- NA; tryCatch({ if (Sys.getenv("RSTUDIO") == "1" && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) { caminho <- rstudioapi::selectFile(caption = titulo) } else { if (requireNamespace("tcltk", quietly = TRUE)) { caminho <- tcltk::tclvalue(tcltk::tkgetOpenFile(title = titulo)) } else { stop("Nenhum GUI disponível.") } } }, error = function(e) { message("Erro: ", e$message); return(NULL) }); if (is.null(caminho) || is.na(caminho) || nchar(caminho) == 0) return(NULL); return(normalizePath(caminho))
}
parse_textgrid <- function(filepath) {
  tryCatch({
    lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
    extract_tier <- function(tier_name, text_lines) {
      header_index <- which(grepl(paste0('name = "', tier_name, '"'), tolower(text_lines), fixed = TRUE))
      if (length(header_index) == 0) return(NULL)
      item_headers <- grep("item \\[[0-9]+\\]:", text_lines); tier_block_start <- item_headers[findInterval(header_index, item_headers)]; tier_block_end <- length(text_lines)
      next_item_header_index <- which(item_headers > tier_block_start); if (length(next_item_header_index) > 0) tier_block_end <- item_headers[next_item_header_index[1]] - 1
      tier_lines <- text_lines[tier_block_start:tier_block_end]; interval_indices <- grep("intervals \\[[0-9]+\\]:", tier_lines); if(length(interval_indices) == 0) return(data.frame())
      starts <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 1])); ends <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 2]))
      labels <- gsub('^\\s*text = "(.*)"\\s*$', "\\1", tier_lines[interval_indices + 3])
      return(data.frame(t1 = starts, t2 = ends, label = labels, stringsAsFactors = FALSE))
    }
    words_df <- extract_tier("words", lines); phones_df <- extract_tier("phones", lines)
    if (is.null(words_df) || is.null(phones_df)) { warning("Tiers 'words' e/ou 'phones' não encontrados em: ", basename(filepath)); return(NULL) }
    xmax_line <- grep("xmax =", lines, fixed = TRUE)[1]; tmax <- as.numeric(gsub("[^0-9\\.]", "", lines[xmax_line]))
    return(list(words = words_df, phones = phones_df, tmax = tmax))
  }, error = function(e) { warning("Erro ao parsear ", basename(filepath), ": ", e$message); return(NULL) })
}
escrever_textgrid_manual <- function(caminho_saida, tabela_palavras, tabela_fones, tmax) {
  header <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', '', 'xmin = 0', sprintf('xmax = %s', tmax), 'tiers? <exists>', 'size = 2', 'item []:')
  criar_tier_texto <- function(dados_tier, nome_tier, index_tier) {
    tier_header <- c(sprintf('\titem [%d]:', index_tier), '\t\tclass = "IntervalTier"', sprintf('\t\tname = "%s"', nome_tier), '\t\txmin = 0', sprintf('\t\txmax = %s', tmax), sprintf('\t\tintervals: size = %d', nrow(dados_tier)))
    intervalos_texto <- c(); for (i in 1:nrow(dados_tier)) { intervalo <- c(sprintf('\t\tintervals [%d]:', i), sprintf('\t\t\txmin = %s', dados_tier$t1[i]), sprintf('\t\t\txmax = %s', dados_tier$t2[i]), sprintf('\t\t\ttext = "%s"', dados_tier$label[i])); intervalos_texto <- c(intervalos_texto, intervalo) }
    return(c(tier_header, intervalos_texto))
  }
  texto_tier_palavras <- criar_tier_texto(tabela_palavras, "words", 1); texto_tier_fones <- criar_tier_texto(tabela_fones, "phones", 2)
  texto_final <- c(header, texto_tier_palavras, texto_tier_fones); writeLines(texto_final, con = caminho_saida, useBytes = TRUE)
}
carregar_dicionario_padrao <- function(caminho_dicionario) {
  linhas <- readLines(caminho_dicionario, warn = FALSE, encoding = "UTF-8"); dicionario <- list()
  for (linha in linhas) {
    if (grepl("^#", linha) || nchar(trimws(linha)) == 0) next
    partes <- unlist(strsplit(linha, "\\s+")); palavra <- sanitizar_palavra(partes[1]); fones <- partes[-1]
    if (is.null(dicionario[[palavra]])) dicionario[[palavra]] <- list()
    dicionario[[palavra]][[length(dicionario[[palavra]]) + 1]] <- fones
  }
  return(dicionario)
}
sanitizar_palavra <- function(palavra) {
  return(tolower(gsub("[^[:alnum:]]", "", palavra)))
}
verificar_compatibilidade <- function(palavra, fones, dicionario) {
  palavra_sanitizada <- sanitizar_palavra(palavra)
  pronuncias_candidatas <- dicionario[[palavra_sanitizada]]
  if (is.null(pronuncias_candidatas) || length(fones) == 0) return(FALSE)
  pronuncia_observada_str <- paste(fones, collapse = "")
  for (pron in pronuncias_candidatas) {
    if (pronuncia_observada_str == paste(pron, collapse = "")) return(TRUE)
  }
  return(FALSE)
}

# --- CORRETOR ---

max_lookahead <- 5 

# --- SELEÇÃO INTERATIVA ---
caminho_dicionario <- selecionar_arquivo(titulo = "SELECIONE O SEU ARQUIVO DE DICIONÁRIO")
if (is.null(caminho_dicionario)) stop("Operação cancelada.")
pasta_de_trabalho <- selecionar_pasta(titulo = "SELECIONE A PASTA DE TRABALHO")
if (is.null(pasta_de_trabalho)) stop("Operação cancelada.")
pasta_saida <- selecionar_pasta(titulo = "SELECIONE UMA PASTA DE SAÍDA")
if (is.null(pasta_saida)) stop("Operação cancelada.")
if (pasta_de_trabalho == pasta_saida) stop("ERRO: A pasta de trabalho e a de saída não podem ser a mesma.")

# Carregamento
dicionario <- carregar_dicionario_padrao(caminho_dicionario)
palavras_especiais <- names(dicionario)[sapply(dicionario, function(p) "spn" %in% unlist(p) || "sil" %in% unlist(p))]
cat("Dicionário carregado.\n")
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)
arquivos_tg <- list.files(pasta_de_trabalho, pattern = "\\.TextGrid$", full.names = TRUE)
cat("\nEncontrados", length(arquivos_tg), "TextGrids para reconstruir...\n\n")

relatorio_omissoes <- data.frame(arquivo=character(), palavra_omitida=character(), stringsAsFactors = FALSE)

for (caminho_tg in arquivos_tg) {
  cat("Processando:", basename(caminho_tg), "\n")
  
  nome_base <- tools::file_path_sans_ext(basename(caminho_tg))
  caminho_transcricao <- NULL
  caminho_lab_potencial <- file.path(pasta_de_trabalho, paste0(nome_base, ".lab"))
  caminho_txt_potencial <- file.path(pasta_de_trabalho, paste0(nome_base, ".txt"))
  if (file.exists(caminho_lab_potencial)) { caminho_transcricao <- caminho_lab_potencial } else if (file.exists(caminho_txt_potencial)) { caminho_transcricao <- caminho_txt_potencial }
  if (is.null(caminho_transcricao)) { cat(sprintf("  -> AVISO: Transcrição não encontrada para '%s'.\n", nome_base)); next }
  texto_lab_bruto <- paste(readLines(caminho_transcricao, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  texto_lab_sanitizado <- gsub("[^[:alnum:]' ]", " ", tolower(texto_lab_bruto))
  palavras_lab <- unlist(strsplit(texto_lab_sanitizado, "\\s+")); palavras_lab <- palavras_lab[palavras_lab != ""]
  
  dados_tg <- parse_textgrid(caminho_tg)
  if(is.null(dados_tg)) { cat("  -> ERRO: Pulando arquivo devido a erro de leitura.\n"); next }
  
  tabela_palavras_original <- dados_tg$words
  tabela_fones <- dados_tg$phones
  
  # --- PASSO 1: LIMPEZA AVANÇADA DO TIER 'WORDS' ---
  tabela_palavras_limpa <- tabela_palavras_original
  for(i in 1:nrow(tabela_palavras_limpa)){
    palavra_a_verificar_original <- tabela_palavras_limpa$label[i]
    palavra_a_verificar_sanitizada <- sanitizar_palavra(palavra_a_verificar_original)
    
    if(palavra_a_verificar_sanitizada == "") next
    
    is_special <- palavra_a_verificar_sanitizada %in% palavras_especiais
    
    fones_intervalo <- tabela_fones$label[tabela_fones$t1 >= tabela_palavras_limpa$t1[i] & tabela_fones$t2 <= tabela_palavras_limpa$t2[i]]
    
    if(!is_special) {
      tabela_palavras_limpa$label[i] <- ""
    }
    
    if(is_special && length(fones_intervalo) > 0) {
      tabela_palavras_limpa$label[i] <- ""
    }
  }
  cat("  -> Passo 1: Tier 'words' limpo conforme as regras.\n")
  
  # --- PASSO 2: RECONSTRUÇÃO INTELIGENTE ---
  tabela_palavras_reconstruida <- tabela_palavras_limpa
  idx_lab <- 1
  
  for (i in 1:nrow(tabela_palavras_reconstruida)) {
    if (idx_lab > length(palavras_lab)) break 
    
    intervalo_atual <- tabela_palavras_reconstruida[i, ]
    fones_no_intervalo <- tabela_fones$label[tabela_fones$t1 >= intervalo_atual$t1 & tabela_fones$t2 <= intervalo_atual$t2]
    
    if (intervalo_atual$label != "" || length(fones_no_intervalo) == 0) {
      next
    }
    
    match_encontrado <- FALSE
    for (k in 0:max_lookahead) {
      if ((idx_lab + k) > length(palavras_lab)) break
      
      palavra_a_testar <- palavras_lab[idx_lab + k]
      
      if (verificar_compatibilidade(palavra_a_testar, fones_no_intervalo, dicionario)) {
        if (k > 0) {
          palavras_omitidas <- palavras_lab[idx_lab:(idx_lab + k - 1)]
          for(palavra_omitida in palavras_omitidas){
            relatorio_omissoes <- rbind(relatorio_omissoes, data.frame(arquivo=basename(caminho_tg), palavra_omitida=palavra_omitida))
          }
        }
        
        tabela_palavras_reconstruida$label[i] <- palavra_a_testar
        
        idx_lab <- idx_lab + k + 1
        match_encontrado <- TRUE
        break
      }
    }
    
    if (!match_encontrado) {
      cat(sprintf("  -> AVISO: Impossível preencher o slot de tempo %.2fs-%.2fs.\n", intervalo_atual$t1, intervalo_atual$t2))
    }
  }
  
  cat("  -> Passo 2: Reconstrução finalizada.\n")
  caminho_saida_arquivo <- file.path(pasta_saida, basename(caminho_tg))
  escrever_textgrid_manual(caminho_saida_arquivo, tabela_palavras_reconstruida, tabela_fones, dados_tg$tmax)
}

cat("\n--- Processo Concluído ---\n")
if (nrow(relatorio_omissoes) > 0) {
  # Muda o nome do arquivo para .xlsx
  caminho_relatorio <- file.path(pasta_saida, "relatorio_de_omissoes_MFA.xlsx")
  tryCatch({
    # Usa a nova função para escrever em Excel
    writexl::write_xlsx(relatorio_omissoes, path = caminho_relatorio)
    cat("Total de", nrow(relatorio_omissoes), "omissões foram detectadas. Relatório salvo em:\n", caminho_relatorio, "\n")
  }, error = function(e) {cat("Não foi possível salvar o relatório Excel. Erro:", e$message, "\n")})
} else {
  cat("Nenhuma omissão foi detectada.\n")
}
cat("TextGrids reconstruídos foram salvos em:", pasta_saida, "\n")