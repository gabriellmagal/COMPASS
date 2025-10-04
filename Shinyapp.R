# COMPASS: Corpus Mapper for Phonetical And Syllabic Structures
# Copyright (C) 2025  Gabriel Magalhães da Silveira
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# --- BIBLIOTECAS ---
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
library(DT)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
library(tibble)
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
library(purrr)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)
if (!requireNamespace("av", quietly = TRUE)) install.packages("av")
library(av)
if (!requireNamespace("zip", quietly = TRUE)) install.packages("zip")
library(zip)
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
library(shinyjs)
if (!requireNamespace("shinycssloaders", quietly = TRUE)) install.packages("shinycssloaders")
library(shinycssloaders)

# --- DEFINIÇÕES GLOBAIS ---
options(shiny.maxRequestSize = 100 * 1024^2)

TABELA_DESC <- tribble(
  ~fone, ~tipo,        ~ponto,           ~vozeamento,  ~altura,      ~anterioridade, ~nasalidade,
  "p",   "plosive",    "bilabial",       "voiceless",  NA,           NA,             "oral",
  "b",   "plosive",    "bilabial",       "voiced",     NA,           NA,             "oral",
  "t",   "plosive",    "dental",         "voiceless",  NA,           NA,             "oral",
  "d",   "plosive",    "dental",         "voiced",     NA,           NA,             "oral",
  "k",   "plosive",    "velar",          "voiceless",  NA,           NA,             "oral",
  "g",   "plosive",    "velar",          "voiced",     NA,           NA,             "oral",
  "c",   "plosive",    "palatal",        "voiceless",  NA,           NA,             "oral",
  "ɟ",   "plosive",    "palatal",        "voiced",     NA,           NA,             "oral",
  "f",   "fricative",  "labiodental",    "voiceless",  NA,           NA,             "oral",
  "v",   "fricative",  "labiodental",    "voiced",     NA,           NA,             "oral",
  "s",   "fricative",  "alveolar",       "voiceless",  NA,           NA,             "oral",
  "z",   "fricative",  "alveolar",       "voiced",     NA,           NA,             "oral",
  "ʃ",   "fricative",  "palato-alveolar","voiceless",  NA,           NA,             "oral",
  "ʒ",   "fricative",  "palato-alveolar","voiced",     NA,           NA,             "oral",
  "x",   "fricative",  "velar",          "voiceless",  NA,           NA,             "oral", 
  "ʁ",   "fricative",  "uvular",         "voiced",     NA,           NA,             "oral", 
  "β",   "fricative",  "bilabial",       "voiced",     NA,           NA,             "oral", 
  "ð",   "fricative",  "dental",         "voiced",     NA,           NA,             "oral", 
  "tʃ",  "affricate",  "palato-alveolar","voiceless",  NA,           NA,             "oral", 
  "dʒ",  "affricate",  "palato-alveolar","voiced",     NA,           NA,             "oral", 
  "m",   "nasal",      "bilabial",       "voiced",     NA,           NA,             "nasal",
  "n",   "nasal",      "alveolar",       "voiced",     NA,           NA,             "nasal",
  "ɲ",   "nasal",      "palatal",        "voiced",     NA,           NA,             "nasal",
  "ŋ",   "nasal",      "velar",          "voiced",     NA,           NA,             "nasal",
  "l",   "lateral",    "alveolar",       "voiced",     NA,           NA,             "oral",
  "ʎ",   "lateral",    "palatal",        "voiced",     NA,           NA,             "oral", 
  "ɾ",   "tap",        "alveolar",       "voiced",     NA,           NA,             "oral", 
  "j",   "approximant","palatal",        "voiced",     NA,           NA,             "oral", 
  "w",   "approximant","labiovelar",     "voiced",     NA,           NA,             "oral",
  "j̃",   "approximant","palatal",        "voiced",     NA,           NA,             "nasal", 
  "w̃",   "approximant","labiovelar",     "voiced",     NA,           NA,             "nasal"
  "i",   "vowel",      NA,               "voiced",     "close",       "front",        "oral",
  "e",   "vowel",      NA,               "voiced",     "close-mid",   "front",        "oral",
  "ɛ",   "vowel",      NA,               "voiced",     "open-mid",    "front",        "oral",
  "a",   "vowel",      NA,               "voiced",     "open",        "central",      "oral",
  "u",   "vowel",      NA,               "voiced",     "close",       "back",         "oral",
  "o",   "vowel",      NA,               "voiced",     "close-mid",   "back",         "oral",
  "ɔ",   "vowel",      NA,               "voiced",     "open-mid",    "back",         "oral",
  "ɨ",   "vowel",      NA,               "voiced",     "close",       "central",      "oral", 
  "ɐ",   "vowel",      NA,               "voiced",     "near-open",   "central",      "oral", 
  "ĩ",   "vowel",      NA,               "voiced",     "close",       "front",        "nasal",
  "ẽ",   "vowel",      NA,               "voiced",     "close-mid",   "front",        "nasal",
  "ã",   "vowel",      NA,               "voiced",     "open",        "central",      "nasal",
  "õ",   "vowel",      NA,               "voiced",     "close-mid",   "back",         "nasal",
  "ũ",   "vowel",      NA,               "voiced",     "close",       "back",         "nasal",
  "ɐ̃",   "vowel",      NA,               "voiced",     "near-open",   "central",      "nasal"
)

# --- FUNÇÕES AUXILIARES ---
parse_textgrid_tier <- function(tier_name, text_lines) {
  header_index <- which(grepl(paste0('name = "', tier_name, '"'), text_lines, fixed = TRUE))
  if (length(header_index) == 0) return(NULL)
  item_headers <- grep("item \\[[0-9]+\\]:", text_lines)
  tier_block_start <- item_headers[findInterval(header_index, item_headers)]
  tier_block_end <- length(text_lines)
  next_item_header_index <- which(item_headers > tier_block_start)
  if (length(next_item_header_index) > 0) tier_block_end <- item_headers[next_item_header_index[1]] - 1
  tier_lines <- text_lines[tier_block_start:tier_block_end]
  interval_indices <- grep("intervals \\[[0-9]+\\]:", tier_lines)
  if (length(interval_indices) == 0) return(data.frame(t1 = numeric(0), t2 = numeric(0), label = character(0)))
  starts <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 1]))
  ends <- as.numeric(gsub("[^0-9\\.]", "", tier_lines[interval_indices + 2]))
  labels <- gsub('^\\s*text = "(.*)"\\s*$', "\\1", tier_lines[interval_indices + 3])
  return(data.frame(t1 = starts, t2 = ends, label = labels, stringsAsFactors = FALSE))
}

escrever_textgrid_recortado <- function(caminho_saida, tabela_palavras, tabela_fones, tabela_silabas) {
  tmax <- max(c(tabela_palavras$t2, tabela_fones$t2, tabela_silabas$t2), na.rm = TRUE)
  if (is.infinite(tmax) || is.na(tmax)) tmax <- 0
  
  header <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', '', 'xmin = 0', sprintf('xmax = %.15f', tmax), 'tiers? <exists>', 'size = 3', 'item []:')
  
  criar_tier_texto <- function(dados_tier, nome_tier, index_tier) {
    if (is.null(dados_tier) || nrow(dados_tier) == 0) return(NULL)
    tier_header <- c(sprintf('\titem [%d]:', index_tier), '\t\tclass = "IntervalTier"', sprintf('\t\tname = "%s"', nome_tier), '\t\txmin = 0', sprintf('\t\txmax = %.15f', tmax), sprintf('\t\tintervals: size = %d', nrow(dados_tier)))
    intervalos_texto <- purrr::map(1:nrow(dados_tier), function(i) {
      c(sprintf('\t\tintervals [%d]:', i), 
        sprintf('\t\t\txmin = %.15f', dados_tier$t1[i]), 
        sprintf('\t\t\txmax = %.15f', dados_tier$t2[i]), 
        sprintf('\t\t\ttext = "%s"', dados_tier$label[i]))
    }) %>% unlist()
    return(c(tier_header, intervalos_texto))
  }
  
  texto_tier_palavras <- criar_tier_texto(tabela_palavras, "words", 1)
  texto_tier_fones <- criar_tier_texto(tabela_fones, "phones", 2)
  texto_tier_silabas <- criar_tier_texto(tabela_silabas, "syllables", 3)
  
  texto_final <- c(header, texto_tier_palavras, texto_tier_fones, texto_tier_silabas)
  writeLines(texto_final, con = caminho_saida, useBytes = TRUE)
}

fonetica_ui <- function(prefix) {
  ns <- NS(prefix)
  tagList(
    selectInput(ns("tipo"), "Type:", choices = c("Any", unique(TABELA_DESC$tipo))),
    conditionalPanel(
      condition = paste0("input['", ns("tipo"), "'] != 'vowel' && input['", ns("tipo"), "'] != 'Any'"),
      selectInput(ns("ponto"), "Place:", choices = c("Any", unique(na.omit(TABELA_DESC$ponto)))),
      selectInput(ns("voz"), "Voicing:", choices = c("Any", unique(na.omit(TABELA_DESC$vozeamento))))
    ),
    conditionalPanel(
      condition = paste0("input['", ns("tipo"), "'] == 'vowel'"),
      # As 'choices' aqui foram atualizadas para corresponder à sua tabela
      selectInput(ns("altura"), "Height:", choices = c("Any", "close", "close-mid", "open-mid", "open", "near-open")),
      selectInput(ns("anterioridade"), "Backness:", choices = c("Any", "front", "central", "back")),
      selectInput(ns("nasalidade"), "Nasality:", choices = c("Any", "oral", "nasal"))
    )
  )
}


# --- (ui) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("COMPASS: Corpus Mapper"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("sidebar_content")
    ),
    mainPanel(
      uiOutput("main_content")
    )
  )
)

# --- (server) ---
server <- function(input, output, session) {
  
  corpus_data <- reactiveVal(NULL)
  resultados_busca <- reactiveVal(NULL)
  summary_table <- reactiveVal(NULL)
  
  output$sidebar_content <- renderUI({
    if (is.null(corpus_data())) {
      tagList(
        h3("Welcome!"),
        p("To begin, please select the .rds file of your processed corpus."),
        fileInput("rds_file", "Select .rds file", accept = ".rds", buttonLabel = "Browse..."),
        actionButton("load_button", "Load Corpus")
      )
    } else {
      tagList(
        selectInput("search_mode", "Search Mode:", 
                    choices = c("Word/Lemma" = "palavra_lemma", "Syllabic Structure" = "silabica")),
        
        conditionalPanel(
          condition = "input.search_mode == 'palavra_lemma'",
          textInput("query_palavra", "Search Term:", ""),
          selectInput("search_tier_palavra", "Search in:", choices = c("Word" = "palavra", "Lemma" = "lemma"))
        ),
        
        conditionalPanel(
          condition = "input.search_mode == 'silabica'",
          h4("1. Syllable Filter"),
          selectInput("posicao_tonica", "Stress / Word Position:",
                      choices = c("Any" = "Qualquer", "Word Initial" = "inicio", "Word Final" = "final",
                                  "Tonic Syllable" = "tonica",
                                  "Pre-tonic" = "pretonica",
                                  "Post-tonic" = "postonica",
                                  "Monosyllable" = "monossilabo")),
          hr(),
          h4("2. Phone Filter"),
          selectInput("posicao_anc", "Position in Syllable:",
                      choices = c("Any" = "Qualquer", "Onset" = "A", "Nucleus" = "N", "Coda" = "C")),
          br(),
          p("Define the target phone's features:"),
          fonetica_ui("target_silabico")
        ),
        
        hr(),
        numericInput("n_kwic_contexto", "KWIC Context (words):", value = 7, min = 1, max = 20),
        actionButton("search_button", "Search"),
        hr(),
        h4("Export Options"),
        checkboxInput("incluir_contexto_download", "Include context in clip?", value = FALSE),
        conditionalPanel(condition = "input.incluir_contexto_download == true",
                         numericInput("n_palavras_contexto_download", "Number of words (clip):", value = 3, min = 1, max = 20)),
        hr(),
        actionButton("selecionar_tudo_btn", "Select/Deselect All"),
        downloadButton("download_clips", "Download Selected")
      )
    }
  })
  
  # Conteúdo do Painel Principal
  
  output$main_content <- renderUI({
    
    if (is.null(corpus_data())) {
      
      tagList(
        
        h2("COMPASS"),
        
        h4(em("Corpus Mapper for Phonetical And Syllabic Structures")),
        
        hr(),

        tags$p(
          
          "Developed by: ", br(),
          tags$strong("Gabriel Magalhães da Silveira"), br(),
          tags$em("Universidade Federal de Minas Gerais (UFMG)"), br(),
          "Belo Horizonte, 2025"
          
        ),
        
        hr(),
        h4("About"),
        p("the", strong("COMPASS"), "Shiny application serves as the interactive graphical user interface for the pre-processed phonetical database. It empowers researchers to perform complex, multi-layered queries by combining a variety of structural and segmental filters. Users can specify criteria such as", strong("syllable tonicity"), "(e.g., tonic, pre-tonic)", strong("intra-syllabic position"), "(Onset, Nucleus, Coda), and a wide range of", strong("phonetic descriptions"), "like place and manner of articulation. The interface features a dynamic filtering system where menus intelligently update to show only valid feature combinations based on the current selections. Query results are presented in a clear", em("Key Word in Context"), "(KWIC) format, and the application alopens for the direct extraction of selected occurrences, enabling users to download the corresponding audio segments and", em(".TextGrid"), "files for further analysis."),
        br(),
        h4("Contact"),
        p("For questions, suggestions, or collaborations, please contact:"),
        p(em("gabriellmagallhaes@gmail.com")),
        br(),
        br(),
        p("© 2025, Gabriel Magalhães da Silveira.")
        
      )
      
    } else {
      
      # Após o upload, ele mostra a tabela de resultados
      
      tagList(
        
        h3("Search result"),
        
        DT::DTOutput("kwic_table") %>% withSpinner(color="#0dc5c1", type = 6)
        
      )
      
    }
    
  })
  
  # --- LOAD ---
  
  observeEvent(input$load_button, {
    req(input$rds_file)
    
    progress <- shiny::Progress$new(session, min=0, max=6); on.exit(progress$close())
    
    tryCatch({
      progress$set(message = "Loading base data...", value = 1)
      df <- readRDS(input$rds_file$datapath)
      
      progress$set(message = "Optimizing Corpus", detail = "Creating unique IDs...", value = 2)
      word_ids <- df %>% 
        distinct(id_arquivo, tempo_inicio_palavra) %>% 
        mutate(internal_id_palavra = row_number())
      df_with_ids <- df %>% 
        left_join(word_ids, by = c("id_arquivo", "tempo_inicio_palavra"))
      
      progress$set(detail = "Adding phonetic features...", value = 3)
      df_com_tracos <- df_with_ids %>% 
        left_join(TABELA_DESC, by = "fone")
      
      progress$set(detail = "Calculating neighbors...", value = 4)
      df_otimizado <- df_com_tracos %>%
        group_by(internal_id_palavra) %>%
        mutate(
          across(
            .cols = c(fone, tipo, ponto, vozeamento, altura, anterioridade, nasalidade),
            .fns = list(anterior = ~lag(., default = ""), seguinte = ~lead(., default = "")),
            .names = "{.col}_{.fn}"
          )
        ) %>%
        ungroup()
      
      progress$set(message = "Creating filter index...", value = 5)
      summary_table_data <- df_otimizado %>%
        mutate(
          posicao_geral = case_when(
            n_silabas == 1 ~ "monossilabo",
            silaba_id_na_palavra == 1 ~ "inicio",
            !is.na(n_silabas) & silaba_id_na_palavra == n_silabas ~ "final",
            tonicidade == "tônica" ~ "tonica",
            !is.na(id_tonica_na_palavra) & silaba_id_na_palavra == (id_tonica_na_palavra - 1) ~ "pretonica",
            !is.na(id_tonica_na_palavra) & silaba_id_na_palavra == (id_tonica_na_palavra + 1) ~ "postonica",
            TRUE ~ NA_character_
          )
        ) %>%
        select(posicao_geral, posicao_anc, tipo, ponto, vozeamento, altura, anterioridade, nasalidade) %>%
        filter(!is.na(posicao_geral) & !is.na(posicao_anc) & !is.na(tipo)) %>%
        distinct()
      
      summary_table(summary_table_data)
      
      progress$set(message = "Finalizing...", value = 6)
      corpus_data(df_otimizado)
      
      showNotification("Corpus loaded successfully!", type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error", duration = 15)
    })
  })
  
  observe({
    req(summary_table(), input$posicao_tonica, input$posicao_anc, input[["target_silabico-tipo"]])
    
    tabela_contexto <- summary_table()
    pos_tonica_sel <- input$posicao_tonica
    pos_anc_sel <- input$posicao_anc
    
    if (pos_tonica_sel != "Qualquer") {
      tabela_contexto <- tabela_contexto %>% filter(posicao_geral == pos_tonica_sel)
    }
    if (pos_anc_sel != "Qualquer") {
      tabela_contexto <- tabela_contexto %>% filter(posicao_anc == pos_anc_sel)
    }
    
    opcoes_tipo <- sort(unique(na.omit(tabela_contexto$tipo)))
    sel_tipo_atual <- input[["target_silabico-tipo"]]
    sel_tipo_final <- if (sel_tipo_atual %in% opcoes_tipo) sel_tipo_atual else "Any"
    updateSelectInput(session, "target_silabico-tipo", choices = c("Any" = "Qualquer", opcoes_tipo), selected = sel_tipo_final)
    
    tabela_features <- tabela_contexto
    tipo_sel <- sel_tipo_final
    
    if (tipo_sel != "Qualquer") {
      tabela_features <- tabela_features %>% filter(tipo == tipo_sel)
    }
    
    opcoes_ponto <- sort(unique(na.omit(tabela_features$ponto)))
    opcoes_voz <- sort(unique(na.omit(tabela_features$vozeamento)))
    opcoes_altura <- sort(unique(na.omit(tabela_features$altura)))
    opcoes_anterioridade <- sort(unique(na.omit(tabela_features$anterioridade)))
    opcoes_nasalidade <- sort(unique(na.omit(tabela_features$nasalidade)))
    
    updateSelectInput(session, "target_silabico-ponto", choices = c("Any" = "Qualquer", opcoes_ponto))
    updateSelectInput(session, "target_silabico-voz", choices = c("Any" = "Qualquer", opcoes_voz))
    updateSelectInput(session, "target_silabico-altura", choices = c("Any" = "Qualquer", opcoes_altura))
    updateSelectInput(session, "target_silabico-anterioridade", choices = c("Any" = "Qualquer", opcoes_anterioridade))
    updateSelectInput(session, "target_silabico-nasalidade", choices = c("Any" = "Qualquer", opcoes_nasalidade))
  })
  
  observeEvent(input$search_button, {
    req(corpus_data())
    showNotification("Processing search...", id = "busca_notif", duration = NULL, type = "message")
    on.exit(removeNotification("busca_notif"), add = TRUE)
    
    df_completo <- corpus_data()
    
    if (input$search_mode == 'palavra_lemma') {
      termo_busca <- input$query_palavra
      if (termo_busca == "") {
        resultados_busca(data.frame()); showNotification("Please enter a search term.", type = "warning", id = "busca_notif"); return()
      }
      coluna_busca <- if (input$search_tier_palavra == "palavra") "palavra" else "lemma"
      
      ids_palavras_encontradas <- df_completo %>%
        filter(stringr::str_detect(!!sym(coluna_busca), stringr::fixed(termo_busca, ignore_case = TRUE))) %>%
        pull(internal_id_palavra) %>%
        unique()
      
    } else { 
      
      df_etapa1 <- if (input$posicao_tonica == "Qualquer") {
        df_completo
      } else {
        df_completo %>% filter(
          case_when(
            input$posicao_tonica == "monossilabo" ~ n_silabas == 1,
            input$posicao_tonica == "inicio" ~ silaba_id_na_palavra == 1,
            input$posicao_tonica == "final" ~ !is.na(n_silabas) & silaba_id_na_palavra == n_silabas,
            input$posicao_tonica == "tonica" ~ tonicidade == "tônica",
            input$posicao_tonica == "pretonica" ~ !is.na(id_tonica_na_palavra) & silaba_id_na_palavra == (id_tonica_na_palavra - 1),
            input$posicao_tonica == "postonica" ~ !is.na(id_tonica_na_palavra) & silaba_id_na_palavra == (id_tonica_na_palavra + 1),
            TRUE ~ TRUE
          )
        )
      }
      
      df_etapa2 <- if (input$posicao_anc == "Qualquer") {
        df_etapa1
      } else {
        df_etapa1 %>% filter(!is.na(posicao_anc) & posicao_anc == input$posicao_anc)
      }
      
      df_temp <- df_etapa2 %>% filter(!is.na(tipo))
      
      traco_tipo <- input[["target_silabico-tipo"]]
      if (traco_tipo != "Qualquer") {
        df_temp <- df_temp %>% filter(tipo == traco_tipo)
        
        if (traco_tipo == 'vogal') {
          traco_altura <- input[["target_silabico-altura"]]; if(traco_altura != "Qualquer") df_temp <- df_temp %>% filter(!is.na(altura) & altura == traco_altura)
          traco_anterioridade <- input[["target_silabico-anterioridade"]]; if(traco_anterioridade != "Qualquer") df_temp <- df_temp %>% filter(!is.na(anterioridade) & anterioridade == traco_anterioridade)
          traco_nasalidade <- input[["target_silabico-nasalidade"]]; if(traco_nasalidade != "Qualquer") df_temp <- df_temp %>% filter(!is.na(nasalidade) & nasalidade == traco_nasalidade)
        } else {
          traco_ponto <- input[["target_silabico-ponto"]]; if(traco_ponto != "Qualquer") df_temp <- df_temp %>% filter(!is.na(ponto) & ponto == traco_ponto)
          traco_voz <- input[["target_silabico-voz"]]; if(traco_voz != "Qualquer") df_temp <- df_temp %>% filter(!is.na(vozeamento) & vozeamento == traco_voz)
        }
      }
      df_etapa3 <- df_temp
      
      ids_palavras_encontradas <- unique(df_etapa3$internal_id_palavra)
    }
    
    if (is.null(ids_palavras_encontradas) || length(ids_palavras_encontradas) == 0) {
      resultados_busca(data.frame()); showNotification("No occurrences found.", type = "warning", id = "busca_notif"); return()
    }
    df_palavras_unicas <- df_completo %>% 
      filter(!is.na(internal_id_palavra)) %>% 
      distinct(internal_id_palavra, .keep_all = TRUE)
    resultados_filtrados <- df_palavras_unicas %>% 
      filter(internal_id_palavra %in% ids_palavras_encontradas)
    df_palavras_reais <- df_palavras_unicas %>% 
      filter(palavra != "" & !is.na(palavra))
    indices_reais_por_arquivo <- split(1:nrow(df_palavras_reais), df_palavras_reais$id_arquivo)
    resultados_kwic <- purrr::map_df(1:nrow(resultados_filtrados), function(i){
      linha_atual <- resultados_filtrados[i, ]
      id_arquivo_atual <- linha_atual$id_arquivo
      indices_reais_arquivo <- indices_reais_por_arquivo[[id_arquivo_atual]]
      if (is.null(indices_reais_arquivo)) return(NULL)
      df_reais_do_arquivo <- df_palavras_reais[indices_reais_arquivo, ]
      posicao_na_lista_real <- match(linha_atual$internal_id_palavra, df_reais_do_arquivo$internal_id_palavra)
      if(is.na(posicao_na_lista_real)) return(NULL)
      inicio_esq <- max(1, posicao_na_lista_real - input$n_kwic_contexto)
      fim_esq <- max(0, posicao_na_lista_real - 1)
      contexto_esq <- if(inicio_esq <= fim_esq) paste(df_reais_do_arquivo$palavra[inicio_esq:fim_esq], collapse = " ") else ""
      inicio_dir <- min(nrow(df_reais_do_arquivo) + 1, posicao_na_lista_real + 1)
      fim_dir <- min(nrow(df_reais_do_arquivo), posicao_na_lista_real + input$n_kwic_contexto)
      contexto_dir <- if(inicio_dir <= fim_dir) paste(df_reais_do_arquivo$palavra[inicio_dir:fim_dir], collapse = " ") else ""
      bind_cols(data.frame(Contexto_Esq = contexto_esq, Contexto_Dir = contexto_dir), linha_atual)
    })
    resultados_busca(resultados_kwic)
    showNotification(paste(nrow(resultados_kwic), "words found!"), id = "busca_notif", type = "message")
  })
  
  output$kwic_table <- DT::renderDT({
    req(resultados_busca()); if (nrow(resultados_busca()) == 0) return()
    
    df <- resultados_busca()
    
    df_display <- df %>% 
      mutate(
        `Keyword` = if_else(
          is.na(silabificacao) | silabificacao == "",
          paste0("<b>", palavra, "</b>"),
          paste0("<b>", silabificacao, "</b>")
        )
      ) %>% 
      select(`Left Context` = Contexto_Esq, 
             `Keyword`, 
             `Right Context` = Contexto_Dir, 
             `Part of Speech` = classe_gramatical, 
             `File Name` = id_arquivo)
    
    DT::datatable(df_display, 
                  escape = FALSE,
                  options = list(
                    pageLength = 10, 
                    columnDefs = list(
                      list(className = 'dt-right', targets = 0), 
                      list(className = 'dt-center', targets = 1), 
                      list(className = 'dt-left', targets = 2)
                    )
                  ), 
                  selection = 'multiple', 
                  rownames = FALSE)
  })
  
  observeEvent(input$selecionar_tudo_btn, { 
    proxy <- DT::dataTableProxy('kwic_table')
    if (length(input$kwic_table_rows_selected) > 0) { 
      DT::selectRows(proxy, NULL) 
    } else { 
      DT::selectRows(proxy, 1:nrow(resultados_busca())) 
    } 
  })
  
  output$download_clips <- downloadHandler(
    filename = function() { paste0("extraction_", Sys.Date(), ".zip") },
    content = function(file) {
      req(input$kwic_table_rows_selected)
      linhas_selecionadas <- resultados_busca()[input$kwic_table_rows_selected, ]
      df_completo <- corpus_data()
      df_palavras_unicas <- df_completo %>% 
        filter(!is.na(internal_id_palavra)) %>% 
        distinct(internal_id_palavra, .keep_all = TRUE)
      
      temp_dir <- tempdir(); arquivos_para_zipar <- c()
      showNotification("Preparing .zip file...", duration = NULL, id = "zip_prep")
      on.exit(removeNotification("zip_prep"), add = TRUE)
      
      for (i in 1:nrow(linhas_selecionadas)) { 
        linha <- linhas_selecionadas[i, ]; 
        tryCatch({ 
          # Garante que o caminho do áudio existe no dataframe e no disco
          if (is.na(linha$caminho_audio) || !file.exists(linha$caminho_audio)) {
            cat("AVISO: Arquivo de áudio não encontrado para a linha", i, ":", linha$caminho_audio, "\n")
            next
          }
          
          tempo_recorte_inicio <- linha$tempo_inicio_palavra
          tempo_recorte_fim <- linha$tempo_fim_palavra
          
          if (input$incluir_contexto_download == TRUE) {
            df_palavras_reais_arquivo <- df_palavras_unicas %>% filter(palavra != "" & !is.na(palavra) & id_arquivo == linha$id_arquivo)
            posicao_na_lista_real <- match(linha$internal_id_palavra, df_palavras_reais_arquivo$internal_id_palavra)
            
            if(!is.na(posicao_na_lista_real)){
              idx_inicio <- max(1, posicao_na_lista_real - input$n_palavras_contexto_download)
              idx_fim <- min(nrow(df_palavras_reais_arquivo), posicao_na_lista_real + input$n_palavras_contexto_download)
              
              tempo_recorte_inicio <- df_palavras_reais_arquivo$tempo_inicio_palavra[idx_inicio]
              tempo_recorte_fim <- df_palavras_reais_arquivo$tempo_fim_palavra[idx_fim]
            }
          }
          
          duracao <- tempo_recorte_fim - tempo_recorte_inicio
          
          if (duracao > 0) {
            # --- LÓGICA DE RECORTE ---
            caminho_tg_original <- file.path(dirname(linha$caminho_audio), paste0(linha$id_arquivo, ".TextGrid"))
            if (!file.exists(caminho_tg_original)) { 
              cat("AVISO: Arquivo TextGrid original não encontrado:", caminho_tg_original, "\n")
              next 
            }
            
            # Lê o TextGrid original para recortar os tiers
            linhas_tg_original <- readLines(caminho_tg_original, warn = FALSE, encoding = "UTF-8")
            tier_words_completo <- parse_textgrid_tier("words", linhas_tg_original)
            tier_phones_completo <- parse_textgrid_tier("phones", linhas_tg_original)
            tier_syllables_completo <- parse_textgrid_tier("syllables", linhas_tg_original)
            
            # Filtra os tiers pelo tempo de recorte
            palavras_recortadas <- tier_words_completo %>% filter(t2 > tempo_recorte_inicio & t1 < tempo_recorte_fim)
            fones_recortados <- tier_phones_completo %>% filter(t2 > tempo_recorte_inicio & t1 < tempo_recorte_fim)
            silabas_recortadas <- tier_syllables_completo %>% filter(t2 > tempo_recorte_inicio & t1 < tempo_recorte_fim)
            
            # Normaliza os tempos para o novo clipe
            palavras_normalizadas <- palavras_recortadas %>% mutate(t1 = t1 - tempo_recorte_inicio, t2 = t2 - tempo_recorte_inicio)
            fones_normalizados <- fones_recortados %>% mutate(t1 = t1 - tempo_recorte_inicio, t2 = t2 - tempo_recorte_inicio)
            silabas_normalizadas <- silabas_recortadas %>% mutate(t1 = t1 - tempo_recorte_inicio, t2 = t2 - tempo_recorte_inicio)
            
            # Cria os nomes para os novos arquivos
            nome_arquivo_base <- paste0(linha$id_arquivo, "_", gsub("\\.", "_", format(linha$tempo_inicio_palavra, nsmall = 4)))
            caminho_wav <- file.path(temp_dir, paste0(nome_arquivo_base, ".wav"))
            caminho_tg <- file.path(temp_dir, paste0(nome_arquivo_base, ".TextGrid"))
            
            # Executa o recorte de áudio e a escrita do novo TextGrid
            av::av_audio_convert(audio = linha$caminho_audio, output = caminho_wav, start_time = tempo_recorte_inicio, total_time = duracao)
            escrever_textgrid_recortado(caminho_tg, palavras_normalizadas, fones_normalizados, silabas_normalizadas)
            
            arquivos_para_zipar <- c(arquivos_para_zipar, caminho_wav, caminho_tg)
          }
        }, error = function(e) { cat("ERROR processing row", i, ":", e$message, "\n") }) 
      } 
      
      if (length(arquivos_para_zipar) > 0) { 
        # zip
        old_wd <- getwd(); setwd(temp_dir); zip::zip(zipfile = file, files = basename(arquivos_para_zipar)); setwd(old_wd)
        showNotification("Download complete!", type = "message", id="zip_prep")
      } else { 
        showNotification("No valid clips could be extracted. Please check file paths.", type = "error", duration = 10, id="zip_prep"); 
        file.create(file) # Cria um zip vazio
      } 
    } 
  )
}


# --- APPSHINY ---
shinyApp(ui, server)




