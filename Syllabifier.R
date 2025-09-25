# --- SILABIFICAÇÃO AUTOMÁTICA EM TEXTGRIDS ---

# --- CARREGAR BIBLIOTECAS E FUNÇÕES ---
if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

cat("Starting Automatic Syllabifier...\n\n")

# --- DEFINIÇÕES  ---
VOGAIS_IPA <- c("i","e","ɛ","a","u","o","ɔ","ɨ","ɐ","ĩ","ẽ","ã","õ","ũ","ɐ̃","ĩ","ẽ","ã","õ","ũ")
CONSOANTES_IPA <- c("tʃ","dʒ","p","b","t","d","ð","k","g","f","v","s","z","ʃ","ʒ","m","n","ɲ","l","ʎ","ɾ","ʁ","w","j","x","c","ɡ","ɟ","β")
ACENTO_TONICO <- "ˈ"
TODOS_FONEMAS <- c(VOGAIS_IPA, CONSOANTES_IPA, ACENTO_TONICO)
INICIOS_VALIDOS <- c('pl','bl','kl','gl','fl','vl','pɾ','bɾ','tɾ','dɾ','kɾ','gɾ','fɾ','vɾ','ʃɾ','ʒɾ')

tokenizar_ipa <- function(transcricao, fonemas_conhecidos) {
  fonemas_ordenados <- fonemas_conhecidos[order(nchar(fonemas_conhecidos), decreasing = TRUE)]
  tokens <- c()
  i <- 1
  while (i <= nchar(transcricao)) {
    achou_fonema <- FALSE
    for (fonema in fonemas_ordenados) {
      if (startsWith(substring(transcricao, i), fonema)) {
        tokens <- c(tokens, fonema)
        i <- i + nchar(fonema)
        achou_fonema <- TRUE; break
      }
    }
    if (!achou_fonema) {
      warning(paste("Unknown character or sequence ignored:", substring(transcricao, i, i)))
      i <- i + 1
    }
  }
  return(tokens)
}

# ---  SILABIFICAÇÃO ---
silabificar_sem_tonica <- function(transcricao_fonetica) {
  if (is.na(transcricao_fonetica) || nchar(transcricao_fonetica) == 0) return("")
  
  fonemas <- tokenizar_ipa(transcricao_fonetica, c(VOGAIS_IPA, CONSOANTES_IPA))
  if (length(fonemas) == 0) return("")
  
  indices_vogais <- which(fonemas %in% VOGAIS_IPA)
  if (length(indices_vogais) <= 1) return(paste(fonemas, collapse=""))
  
  pontos_de_quebra <- c()
  
  for (i in 1:(length(indices_vogais) - 1)) {
    vogal1_idx <- indices_vogais[i]
    vogal2_idx <- indices_vogais[i+1]
    
    # Garante que o grupo consonantal seja vazio se não houver consoantes
    start_cluster <- vogal1_idx + 1
    end_cluster <- vogal2_idx - 1
    indices_cluster <- if (start_cluster <= end_cluster) start_cluster:end_cluster else c()
    
    num_consoantes <- length(indices_cluster)
    
    if (num_consoantes == 0) { # Hiato V.V
      pontos_de_quebra <- c(pontos_de_quebra, vogal2_idx)
    } else if (num_consoantes == 1) { # VCV -> V.CV
      pontos_de_quebra <- c(pontos_de_quebra, vogal2_idx - 1)
    } else { # VCCV
      cluster_fonemas <- fonemas[indices_cluster]
      duas_ultimas <- paste(cluster_fonemas[(num_consoantes-1):num_consoantes], collapse="")
      if (duas_ultimas %in% INICIOS_VALIDOS) { # V.CCV
        pontos_de_quebra <- c(pontos_de_quebra, vogal2_idx - 2)
      } else { # VC.CV
        pontos_de_quebra <- c(pontos_de_quebra, vogal2_idx - 1)
      }
    }
  }
  
  resultado <- ""
  inicio_silaba <- 1
  for (ponto in sort(unique(pontos_de_quebra))) {
    resultado <- paste0(resultado, paste(fonemas[inicio_silaba:(ponto-1)], collapse=""), ".")
    inicio_silaba <- ponto
  }
  resultado <- paste0(resultado, paste(fonemas[inicio_silaba:length(fonemas)], collapse=""))
  
  return(resultado)
}

adicionar_tonicidade_por_regra <- function(palavra_silabificada, palavra_ortografica) {
  if (nchar(palavra_silabificada) == 0 || nchar(palavra_ortografica) == 0) return(palavra_silabificada)
  partes <- strsplit(palavra_silabificada, "\\.")[[1]]
  num_silabas <- length(partes)
  if (num_silabas <= 1) return(palavra_silabificada)
  
  palavra_norm <- tolower(trimws(palavra_ortografica))
  idx_tonico <- -1
  
  if (grepl("[áéíóúâêô]", palavra_norm)) {
    if (num_silabas >= 3 && grepl("[áéíóúâêô][^áéíóúâêôw̃s]*[aeiou][^áéíóúâêôw̃s]*[aeiouw̃s]$", gsub("qu|gu", " ", palavra_norm))) {
      idx_tonico <- num_silabas - 2
    } else {
      vogais_acentuadas <- str_locate_all(palavra_norm, "[áéíóúâêô]")[[1]]
      if (nrow(vogais_acentuadas) > 0) {
        pos_acento <- tail(vogais_acentuadas, 1)[1, 'start']
        if (!grepl("[aeiou]", substring(palavra_norm, pos_acento + 1))) {
          idx_tonico <- num_silabas
        } else {
          idx_tonico <- num_silabas - 1
        }
      }
    }
  } else {
    if (str_ends(palavra_norm, "r|l|z|x|i|is|u|us|im|um|uns|om|ons|el|eis|ois|ão|ãos|ãe|ães|õe|ões")) {
      idx_tonico <- num_silabas
    } else {
      idx_tonico <- num_silabas - 1
    }
  }
  
  if (idx_tonico > 0 && idx_tonico <= num_silabas) {
    partes[idx_tonico] <- paste0(ACENTO_TONICO, partes[idx_tonico])
    return(paste(partes, collapse="."))
  } else {
    return(palavra_silabificada)
  }
}

parse_textgrid_tier <- function(filepath, tier_name) {
  tryCatch({
    lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
    tier_header_index <- which(grepl(paste0('name = "', tier_name, '"'), tolower(lines), fixed = TRUE))
    if (length(tier_header_index) == 0) return(NULL)
    start_block_indices <- grep("item \\[[0-9]+\\]:", lines)
    current_tier_block_index <- findInterval(tier_header_index, start_block_indices)
    tier_start_line <- start_block_indices[current_tier_block_index]
    tier_end_line <- ifelse(current_tier_block_index < length(start_block_indices), start_block_indices[current_tier_block_index + 1] - 1, length(lines))
    tier_lines <- lines[tier_start_line:tier_end_line]
    interval_starts <- grep("intervals \\[[0-9]+\\]:", tier_lines)
    if(length(interval_starts) == 0) return(data.frame())
    starts <- as.numeric(str_match(tier_lines[interval_starts + 1], "xmin =\\s*([0-9\\.]+)")[,2])
    ends <- as.numeric(str_match(tier_lines[interval_starts + 2], "xmax =\\s*([0-9\\.]+)")[,2])
    labels <- str_match(tier_lines[interval_starts + 3], 'text =\\s*"(.*)"')[,2]
    return(data.frame(start = starts, end = ends, label = labels, stringsAsFactors = FALSE))
  }, error = function(e) {
    warning(paste("Erro ao parsear o TextGrid:", basename(filepath), "-", e$message))
    return(NULL)
  })
}

cat("1. Select the INPUT folder with the original TextGrids.\n")
pasta_entrada <- tk_choose.dir(caption = "Select the Folder with the TextGrid files")
if (is.na(pasta_entrada)) { stop("Selection canceled.") }
cat("2. Select the OUTPUT folder for the new TextGrids.\n")
pasta_saida <- tk_choose.dir(caption = "Select OUTPUT Folder")
if (is.na(pasta_saida)) { stop("Selection canceled.") }
if (pasta_entrada == pasta_saida) { stop("ERROR: Input and output folder cannot be the same.") }

words_tier_name <- "words"; phones_tier_name <- "phones"; new_tier_name <- "syllables"
arquivos_tg_nomes <- list.files(path = pasta_entrada, pattern = "\\.TextGrid$")
if (length(arquivos_tg_nomes) == 0) { stop("ERROR: No .TextGrid file found.") }
cat(paste("\nFound", length(arquivos_tg_nomes), "files to process.\n\n"))

for (nome_arquivo_base in arquivos_tg_nomes) {
  cat(paste("Processing:", nome_arquivo_base, "\n"))
  caminho_tg <- file.path(pasta_entrada, nome_arquivo_base)
  tryCatch({
    df_words <- parse_textgrid_tier(caminho_tg, words_tier_name)
    df_phones <- parse_textgrid_tier(caminho_tg, phones_tier_name)
    if (is.null(df_words) || is.null(df_phones)) {
      cat(paste("  WARNING: Tiers 'words' and/or 'phones' not found in", nome_arquivo_base, ". Skipping.\n")); next
    }
    novos_intervalos <- list()
    for (i in 1:nrow(df_words)) {
      word_label_ortografico <- df_words$label[i]
      if (word_label_ortografico == "" || is.na(word_label_ortografico)) {
        novos_intervalos[[i]] <- data.frame(start = df_words$start[i], end = df_words$end[i], label = ""); next
      }
      fones_da_palavra <- df_phones %>% filter(start >= df_words$start[i] - 0.0001 & end <= df_words$end[i] + 0.0001)
      transcricao_fonetica <- paste(fones_da_palavra$label, collapse = "")
      palavra_silabificada_base <- silabificar_sem_tonica(transcricao_fonetica)
      resultado_final <- adicionar_tonicidade_por_regra(palavra_silabificada_base, word_label_ortografico)
      novos_intervalos[[i]] <- data.frame(start = df_words$start[i], end = df_words$end[i], label = resultado_final)
    }
    df_final_syllables <- do.call(rbind, novos_intervalos)
    linhas_tg_originais <- readLines(caminho_tg, warn = FALSE, encoding="UTF-8")
    size_line_index <- grep("size =", linhas_tg_originais)[1]
    original_size <- as.numeric(gsub("[^0-9]", "", linhas_tg_originais[size_line_index]))
    linhas_tg_originais[size_line_index] <- paste0("    size = ", original_size + 1, " ")
    xmin_geral <- df_words$start[1]; xmax_geral <- tail(df_words$end, 1)
    novo_tier_texto <- c(paste0("item [", original_size + 1, "]:"), '    class = "IntervalTier"', paste0('    name = "', new_tier_name, '"'), paste0("    xmin = ", xmin_geral), paste0("    xmax = ", xmax_geral), paste0("    intervals: size = ", nrow(df_final_syllables)))
    for (j in 1:nrow(df_final_syllables)) {
      intervalo <- df_final_syllables[j, ]; label_escapado <- gsub('"', '""', intervalo$label, fixed = TRUE)
      novo_tier_texto <- c(novo_tier_texto, paste0("        intervals [", j, "]:"), paste0("            xmin = ", intervalo$start), paste0("            xmax = ", intervalo$end), paste0('            text = "', label_escapado, '"'))
    }
    linhas_finais <- c(linhas_tg_originais, novo_tier_texto)
    caminho_saida <- file.path(pasta_saida, nome_arquivo_base)
    writeLines(linhas_finais, con = caminho_saida, sep="\n", useBytes = TRUE)
    cat(paste("  -> Saved in:", basename(caminho_saida), "\n"))
  }, error = function(e) {
    cat(paste("  FATAL ERROR WHILE PROCESSING the file", nome_arquivo_base, ":", e$message, "\n"))
  })
}

cat("\n--- Process completed! ---\n")