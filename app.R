# --- BIBLIOTECAS ---
#install.packages(c("shiny", "DT", "dplyr", "tibble", "av", "zip", "shinyjs", "shinycssloaders"))
library(shiny)
library(DT)
library(dplyr)
library(tibble)
library(av)
library(zip)
library(shinyjs)
library(shinycssloaders)

# --- AUMENTA O LIMITE DE UPLOAD DE ARQUIVOS DO SHINY PARA 100 MB ---
options(shiny.maxRequestSize = 100 * 1024^2)

# --- DEFINIÇÕES GLOBAIS ---
VOGAIS_IPA <- c("i", "e", "ɛ", "a", "u", "o", "ɔ", "ĩ", "ẽ", "ã", "õ", "ũ")
CONSOANTES_IPA <- c("p", "b", "t", "d", "k", "g", "f", "v", "s", "z", "ʃ", "ʒ", "m", "n", "ɲ", "l", "ʎ", "ɾ", "ʁ")
TABELA_DE_TRACOS <- tribble(
  ~fone, ~tipo,      ~ponto,       ~vozeamento,  ~altura,      ~anterioridade, ~nasalidade,
  "p",   "plosiva",  "bilabial",   "desvozeada", NA,           NA,             NA,
  "b",   "plosiva",  "bilabial",   "vozeada",    NA,           NA,             NA,
  "t",   "plosiva",  "dental",     "desvozeada", NA,           NA,             NA,
  "d",   "plosiva",  "dental",     "vozeada",    NA,           NA,             NA,
  "k",   "plosiva",  "velar",      "desvozeada", NA,           NA,             NA,
  "g",   "plosiva",  "velar",      "vozeada",    NA,           NA,             NA,
  "f",   "fricativa","labiodental","desvozeada", NA,           NA,             NA,
  "v",   "fricativa","labiodental","vozeada",    NA,           NA,             NA,
  "s",   "fricativa","alveolar",   "desvozeada", NA,           NA,             NA,
  "z",   "fricativa","alveolar",   "vozeada",    NA,           NA,             NA,
  "ʃ",   "fricativa","palatoalveolar","desvozeada", NA,       NA,             NA,
  "ʒ",   "fricativa","palatoalveolar","vozeada",    NA,       NA,             NA,
  "m",   "nasal",    "bilabial",   "vozeada",    NA,           NA,             "nasal",
  "n",   "nasal",    "alveolar",   "vozeada",    NA,           NA,             "nasal",
  "ɲ",   "nasal",    "palatal",    "vozeada",    NA,           NA,             "nasal",
  "l",   "lateral",  "alveolar",   "vozeada",    NA,           NA,             NA,
  "ʎ",   "lateral",  "palatal",    "vozeada",    NA,           NA,             NA,
  "ɾ",   "vibrante", "alveolar",   "vozeada",    NA,           NA,             NA,
  "ʁ",   "vibrante", "uvular",     "vozeada",    NA,           NA,             NA,
  "i",   "vogal",    NA,           "vozeada",    "alta",       "anterior",     "oral",
  "e",   "vogal",    NA,           "vozeada",    "média-alta", "anterior",     "oral",
  "ɛ",   "vogal",    NA,           "vozeada",    "média-baixa","anterior",     "oral",
  "a",   "vogal",    NA,           "vozeada",    "baixa",      "central",      "oral",
  "u",   "vogal",    NA,           "vozeada",    "alta",       "posterior",    "oral",
  "o",   "vogal",    NA,           "vozeada",    "média-alta", "posterior",    "oral",
  "ɔ",   "vogal",    NA,           "vozeada",    "média-baixa","posterior",    "oral",
  "ĩ",   "vogal",    NA,           "vozeada",    "alta",       "anterior",     "nasal",
  "ẽ",   "vogal",    NA,           "vozeada",    "média-alta", "anterior",     "nasal",
  "ã",   "vogal",    NA,           "vozeada",    "baixa",      "central",      "nasal",
  "õ",   "vogal",    NA,           "vozeada",    "média-alta", "posterior",    "nasal",
  "ũ",   "vogal",    NA,           "vozeada",    "alta",       "posterior",    "nasal"
)

# --- FUNÇÕES AUXILIARES ---
# (As mesmas funções da versão anterior, sem alterações)
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
escrever_textgrid_recortado <- function(caminho_saida, tabela_palavras, tabela_fones) {
  tmax <- max(tabela_palavras$t2, tabela_fones$t2, na.rm = TRUE)
  if (is.infinite(tmax) || is.na(tmax)) tmax <- 0
  header <- c('File type = "ooTextFile"', 'Object class = "TextGrid"', '', 'xmin = 0', sprintf('xmax = %.15f', tmax), 'tiers? <exists>', 'size = 2', 'item []:')
  criar_tier_texto <- function(dados_tier, nome_tier, index_tier) {
    tier_header <- c(sprintf('\titem [%d]:', index_tier), '\t\tclass = "IntervalTier"', sprintf('\t\tname = "%s"', nome_tier), '\t\txmin = 0', sprintf('\t\txmax = %.15f', tmax), sprintf('\t\tintervals: size = %d', nrow(dados_tier)))
    intervalos_texto <- c(); for (i in 1:nrow(dados_tier)) { intervalo <- c(sprintf('\t\tintervals [%d]:', i), sprintf('\t\t\txmin = %.15f', dados_tier$t1[i]), sprintf('\t\t\txmax = %.15f', dados_tier$t2[i]), sprintf('\t\t\ttext = "%s"', dados_tier$label[i])); intervalos_texto <- c(intervalos_texto, intervalo) }
    return(c(tier_header, intervalos_texto))
  }
  texto_tier_palavras <- criar_tier_texto(tabela_palavras, "words", 1)
  texto_tier_fones <- criar_tier_texto(tabela_fones, "phones", 2)
  texto_final <- c(header, texto_tier_palavras, texto_tier_fones)
  writeLines(texto_final, con = caminho_saida, useBytes = TRUE)
}

# --- FUNÇÃO HELPER PARA GERAR UI FONÉTICA ---
fonetica_ui <- function(prefix, label = "Fone Específico:") {
  ns <- NS(prefix) # Cria um namespace para os IDs
  tagList(
    radioButtons(ns("type"), "Definir por:", choices = c("Fone Específico", "Traços Fonéticos"), inline = TRUE),
    conditionalPanel(
      condition = paste0("input['", ns("type"), "'] == 'Fone Específico'"),
      textInput(ns("phone"), label),
      div(style="margin-bottom: 10px;",
          h6("Vogais", style="margin-top: 5px; margin-bottom: 2px;"),
          lapply(VOGAIS_IPA, function(fone) { actionButton(ns(paste0("btn_", fone)), fone, class = "btn-xs", style = "margin: 2px;") }),
          h6("Consoantes", style="margin-top: 5px; margin-bottom: 2px;"),
          lapply(CONSOANTES_IPA, function(fone) { actionButton(ns(paste0("btn_", fone)), fone, class = "btn-xs", style = "margin: 2px;") })
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("type"), "'] == 'Traços Fonéticos'"),
      selectInput(ns("tipo"), "Tipo:", choices = c("Qualquer", unique(TABELA_DE_TRACOS$tipo))),
      conditionalPanel(
        condition = paste0("input['", ns("tipo"), "'] != 'vogal' && input['", ns("tipo"), "'] != 'Qualquer'"),
        selectInput(ns("ponto"), "Ponto:", choices = c("Qualquer", unique(na.omit(TABELA_DE_TRACOS$ponto)))),
        selectInput(ns("voz"), "Vozeamento:", choices = c("Qualquer", unique(na.omit(TABELA_DE_TRACOS$vozeamento))))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("tipo"), "'] == 'vogal'"),
        selectInput(ns("altura"), "Altura:", choices = c("Qualquer", unique(na.omit(TABELA_DE_TRACOS$altura)))),
        selectInput(ns("anterioridade"), "Anterioridade:", choices = c("Qualquer", unique(na.omit(TABELA_DE_TRACOS$anterioridade)))),
        selectInput(ns("nasalidade"), "Nasalidade:", choices = c("Qualquer", unique(na.omit(TABELA_DE_TRACOS$nasalidade))))
      )
    )
  )
}

# --- INTERFACE DO USUÁRIO (ui) ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Buscador de Corpus Interativo"),
  uiOutput("main_ui")
)

# --- LÓGICA DO SERVIDOR (server) ---
server <- function(input, output, session) {
  
  corpus_data <- reactiveVal(NULL)
  resultados_busca <- reactiveVal(NULL)
  
  output$main_ui <- renderUI({
    if (is.null(corpus_data())) {
      # Tela de Boas-Vindas
      sidebarLayout(
        sidebarPanel(
          h3("Bem-vindo!"),
          p("Selecione o arquivo .rds do seu corpus."),
          fileInput("rds_file", "Selecione o arquivo .rds", accept = ".rds", buttonLabel = "Procurar..."),
          actionButton("load_button", "Carregar Corpus")
        ),
        mainPanel(h4("Aguardando o carregamento do corpus..."))
      )
    } else {
      # Interface de Análise Principal
      sidebarLayout(
        sidebarPanel(
          selectInput("search_mode", "Modo de Busca:", 
                      choices = c("Palavra/Lemma" = "palavra_lemma", "Fone" = "fone")),
          
          conditionalPanel(
            condition = "input.search_mode == 'palavra_lemma'",
            textInput("query_palavra", "Termo de Busca:", ""),
            selectInput("search_tier_palavra", "Buscar em:", choices = c("Palavra" = "palavra", "Lemma" = "lemma"))
          ),
          
          conditionalPanel(
            condition = "input.search_mode == 'fone'",
            h4("1. Definição do Alvo"),
            fonetica_ui("target", label = "Fone-Alvo:"),
            hr(),
            h4("2. Posição do Alvo na Palavra"),
            selectInput("target_position", "Posição:", 
                        choices = c("Qualquer Posição", "Início de Palavra", "Final de Palavra")),
            hr(),
            h4("3. Contexto Fonológico (Opcional)"),
            checkboxInput("use_contexto_anterior", "Definir Contexto Anterior (à Esquerda)"),
            conditionalPanel(condition = "input.use_contexto_anterior == true", fonetica_ui("contexto_anterior", label = "Fone de Contexto Anterior:")),
            checkboxInput("use_contexto_seguinte", "Definir Contexto Posterior (à Direita)"),
            conditionalPanel(condition = "input.use_contexto_seguinte == true", fonetica_ui("contexto_seguinte", label = "Fone de Contexto Posterior:"))
          ),
          
          hr(),
          numericInput("n_kwic_contexto", "Nº de palavras de contexto (KWIC):", value = 7, min = 1, max = 20),
          actionButton("search_button", "Buscar"),
          hr(),
          h4("Opções de Exportação"),
          checkboxInput("incluir_contexto_download", "Incluir contexto no recorte?", value = FALSE),
          conditionalPanel(condition = "input.incluir_contexto_download == true",
                           numericInput("n_palavras_contexto_download", "Nº de palavras (recorte):", value = 3, min = 1, max = 20)),
          hr(),
          actionButton("selecionar_tudo_btn", "Selecionar/Desselecionar Tudo"),
          downloadButton("download_clips", "Baixar Ocorrências Selecionadas")
        ),
        mainPanel(
          h3("Resultados da Busca"),
          DT::DTOutput("kwic_table") %>% withSpinner(color="#0dc5c1", type = 6)
        )
      )
    }
  })
  
  observeEvent(input$load_button, {
    req(input$rds_file); progress <- Progress$new(session, min=0, max=5); on.exit(progress$close())
    progress$set(message = "Carregando e otimizando o corpus...", value = 1)
    tryCatch({
      df <- readRDS(input$rds_file$datapath); progress$set(detail = "Criando IDs únicos...", value = 2)
      word_ids <- df %>% distinct(id_arquivo, tempo_inicio_palavra) %>% mutate(internal_id_palavra = row_number())
      df_with_ids <- df %>% left_join(word_ids, by = c("id_arquivo", "tempo_inicio_palavra"))
      progress$set(detail = "Adicionando traços fonéticos...", value = 3)
      df_com_tracos <- df_with_ids %>% left_join(TABELA_DE_TRACOS, by = "fone")
      progress$set(detail = "Calculando vizinhos...", value = 4)
      df_otimizado <- df_com_tracos %>%
        group_by(internal_id_palavra) %>%
        mutate(
          across(c(fone, tipo, ponto, vozeamento, altura, anterioridade, nasalidade), 
                 list(anterior = ~lag(., default = ""), seguinte = ~lead(., default = "")))
        ) %>%
        ungroup()
      progress$set(detail = "Finalizando...", value = 5)
      corpus_data(df_otimizado)
      showNotification("Corpus carregado com sucesso!", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erro ao carregar:", e$message), type = "error", duration = 15)
    })
  })
  
  # Lógica para os botões do teclado IPA (agora modular)
  observe_ipa_buttons <- function(prefix) {
    lapply(c(VOGAIS_IPA, CONSOANTES_IPA), function(fone) {
      observeEvent(input[[NS(prefix, paste0("btn_", fone))]], {
        updateTextInput(session, NS(prefix, "phone"), value = paste0(input[[NS(prefix, "phone")]], fone))
      })
    })
  }
  observe_ipa_buttons("target")
  observe_ipa_buttons("contexto_anterior")
  observe_ipa_buttons("contexto_seguinte")
  
  observeEvent(input$search_button, {
    req(corpus_data())
    showNotification("Processando busca...", id = "busca_notif", duration = NULL, type = "message")
    on.exit(removeNotification("busca_notif"), add = TRUE)
    
    df_completo <- corpus_data()
    
    if (input$search_mode == 'palavra_lemma') {
      req(input$query_palavra)
      ids_palavras_encontradas <- df_completo %>% 
        filter(!!as.symbol(input$search_tier_palavra) == input$query_palavra) %>%
        pull(internal_id_palavra) %>% unique()
      
    } else {
      resultados <- df_completo
      
      # 1. Filtro pelo ALVO
      if (input[["target-type"]] == 'Fone Específico') {
        req(input[["target-phone"]])
        resultados <- resultados %>% filter(fone == input[["target-phone"]])
      } else {
        if (input[["target-tipo"]] != "Qualquer") {
          resultados <- resultados %>% filter(tipo == input[["target-tipo"]])
          if (input[["target-tipo"]] == 'vogal') {
            if (input[["target-altura"]] != "Qualquer") { resultados <- resultados %>% filter(altura == input[["target-altura"]]) }
            if (input[["target-anterioridade"]] != "Qualquer") { resultados <- resultados %>% filter(anterioridade == input[["target-anterioridade"]]) }
            if (input[["target-nasalidade"]] != "Qualquer") { resultados <- resultados %>% filter(nasalidade == input[["target-nasalidade"]]) }
          } else {
            if (input[["target-ponto"]] != "Qualquer") { resultados <- resultados %>% filter(ponto == input[["target-ponto"]]) }
            if (input[["target-voz"]] != "Qualquer") { resultados <- resultados %>% filter(vozeamento == input[["target-voz"]]) }
          }
        }
      }
      if(nrow(resultados) == 0) { ids_palavras_encontradas <- NULL } else {
        
        # 2. Filtro pela POSIÇÃO
        if (input$target_position == "Início de Palavra") { resultados <- resultados %>% filter(fone_anterior == "") }
        else if (input$target_position == "Final de Palavra") { resultados <- resultados %>% filter(fone_seguinte == "") }
        
        if(nrow(resultados) == 0) { ids_palavras_encontradas <- NULL } else {
          
          # 3. Filtro pelo CONTEXTO ANTERIOR
          if (input$use_contexto_anterior) {
            if (input[["contexto_anterior-type"]] == 'Fone Específico') {
              req(input[["contexto_anterior-phone"]])
              resultados <- resultados %>% filter(fone_anterior == input[["contexto_anterior-phone"]])
            } else { # Traços
              if (input[["contexto_anterior-tipo"]] != "Qualquer") {
                # Lógica completa para traços do contexto anterior
                if(input[["contexto_anterior-tipo"]] == 'vogal'){
                  if (input[["contexto_anterior-altura"]] != "Qualquer") { resultados <- resultados %>% filter(altura_anterior == input[["contexto_anterior-altura"]]) }
                  if (input[["contexto_anterior-anterioridade"]] != "Qualquer") { resultados <- resultados %>% filter(anterioridade_anterior == input[["contexto_anterior-anterioridade"]]) }
                  if (input[["contexto_anterior-nasalidade"]] != "Qualquer") { resultados <- resultados %>% filter(nasalidade_anterior == input[["contexto_anterior-nasalidade"]]) }
                } else {
                  if (input[["contexto_anterior-ponto"]] != "Qualquer") { resultados <- resultados %>% filter(ponto_anterior == input[["contexto_anterior-ponto"]]) }
                  if (input[["contexto_anterior-voz"]] != "Qualquer") { resultados <- resultados %>% filter(vozeamento_anterior == input[["contexto_anterior-voz"]]) }
                }
              }
            }
          }
          
          if(nrow(resultados) == 0) { ids_palavras_encontradas <- NULL } else {
            
            # 4. Filtro pelo CONTEXTO POSTERIOR
            if (input$use_contexto_seguinte) {
              if (input[["contexto_seguinte-type"]] == 'Fone Específico') {
                req(input[["contexto_seguinte-phone"]])
                resultados <- resultados %>% filter(fone_seguinte == input[["contexto_seguinte-phone"]])
              } else { # Traços
                if (input[["contexto_seguinte-tipo"]] != "Qualquer") {
                  if(input[["contexto_seguinte-tipo"]] == 'vogal'){
                    if (input[["contexto_seguinte-altura"]] != "Qualquer") { resultados <- resultados %>% filter(altura_seguinte == input[["contexto_seguinte-altura"]]) }
                    if (input[["contexto_seguinte-anterioridade"]] != "Qualquer") { resultados <- resultados %>% filter(anterioridade_seguinte == input[["contexto_seguinte-anterioridade"]]) }
                    if (input[["contexto_seguinte-nasalidade"]] != "Qualquer") { resultados <- resultados %>% filter(nasalidade_seguinte == input[["contexto_seguinte-nasalidade"]]) }
                  } else {
                    if (input[["contexto_seguinte-ponto"]] != "Qualquer") { resultados <- resultados %>% filter(ponto_seguinte == input[["contexto_seguinte-ponto"]]) }
                    if (input[["contexto_seguinte-voz"]] != "Qualquer") { resultados <- resultados %>% filter(vozeamento_seguinte == input[["contexto_seguinte-voz"]]) }
                  }
                }
              }
            }
            
            ids_palavras_encontradas <- unique(resultados$internal_id_palavra)
          }}}
    }
    
    if (is.null(ids_palavras_encontradas) || length(ids_palavras_encontradas) == 0) {
      resultados_busca(data.frame()); showNotification("Nenhuma ocorrência encontrada.", type = "warning", id = "busca_notif"); return()
    }
    
    # (Lógica de Geração de KWIC sem alterações)
    df_palavras_unicas <- df_completo %>% filter(!is.na(internal_id_palavra)) %>% distinct(internal_id_palavra, .keep_all = TRUE)
    resultados_filtrados <- df_palavras_unicas %>% filter(internal_id_palavra %in% ids_palavras_encontradas)
    df_palavras_reais <- df_palavras_unicas %>% filter(palavra != "")
    indices_reais_por_arquivo <- split(1:nrow(df_palavras_reais), df_palavras_reais$id_arquivo)
    resultados_kwic <- lapply(1:nrow(resultados_filtrados), function(i){
      linha_atual <- resultados_filtrados[i, ]
      id_arquivo_atual <- linha_atual$id_arquivo
      indices_reais_arquivo <- indices_reais_por_arquivo[[id_arquivo_atual]]
      if (is.null(indices_reais_arquivo)) return(NULL)
      df_reais_do_arquivo <- df_palavras_reais[indices_reais_arquivo, ]
      posicao_na_lista_real <- match(linha_atual$internal_id_palavra, df_reais_do_arquivo$internal_id_palavra)
      if(is.na(posicao_na_lista_real)) return(NULL)
      inicio_esq <- max(1, posicao_na_lista_real - input$n_kwic_contexto)
      contexto_esq <- paste(df_reais_do_arquivo$palavra[inicio_esq:(posicao_na_lista_real - 1)], collapse = " ")
      fim_dir <- min(nrow(df_reais_do_arquivo), posicao_na_lista_real + input$n_kwic_contexto)
      contexto_dir <- paste(df_reais_do_arquivo$palavra[(posicao_na_lista_real + 1):fim_dir], collapse = " ")
      return(cbind(data.frame(Contexto_Esq = contexto_esq, Contexto_Dir = contexto_dir), linha_atual))
    })
    resultados_busca(bind_rows(resultados_kwic))
  })
  
  output$kwic_table <- DT::renderDT({
    req(resultados_busca()); if (nrow(resultados_busca()) == 0) return()
    df <- resultados_busca(); df_display <- df %>% mutate(`Palavra-Chave` = paste0("<b>", palavra, "</b>")) %>% select(`Contexto Esquerdo` = Contexto_Esq, `Palavra-Chave`, `Contexto Direito` = Contexto_Dir, `Classe da Palavra` = classe_gramatical, `Nome do Arquivo` = id_arquivo)
    DT::datatable(df_display, escape = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-right', targets = 0), list(className = 'dt-center', targets = 1), list(className = 'dt-left', targets = 2))), selection = 'multiple', rownames = FALSE)
  })
  
  observeEvent(input$selecionar_tudo_btn, {
    proxy <- DT::dataTableProxy('kwic_table'); if (length(input$kwic_table_rows_selected) > 0) { DT::selectRows(proxy, NULL) } else { DT::selectRows(proxy, 1:nrow(resultados_busca())) }
  })
  
  output$download_clips <- downloadHandler(
    filename = function() { paste0("extracao_", Sys.Date(), ".zip") },
    content = function(file) {
      req(input$kwic_table_rows_selected); linhas_selecionadas <- resultados_busca()[input$kwic_table_rows_selected, ]; df_palavras_unicas <- corpus_data() %>% filter(!is.na(internal_id_palavra)) %>% distinct(internal_id_palavra, .keep_all = TRUE)
      temp_dir <- tempdir(); arquivos_para_zipar <- c(); showNotification("Iniciando a preparação do arquivo .zip...", duration = 5, type = "message")
      for (i in 1:nrow(linhas_selecionadas)) {
        linha <- linhas_selecionadas[i, ]; tryCatch({
          tempo_recorte_inicio <- linha$tempo_inicio_palavra; tempo_recorte_fim <- linha$tempo_fim_palavra
          if (input$incluir_contexto_download == TRUE) {
            df_palavras_reais_arquivo <- df_palavras_unicas %>% filter(palavra != "" & id_arquivo == linha$id_arquivo); posicao_na_lista_real <- match(linha$internal_id_palavra, df_palavras_reais_arquivo$internal_id_palavra)
            if(!is.na(posicao_na_lista_real)){
              idx_inicio <- max(1, posicao_na_lista_real - input$n_palavras_contexto_download); idx_fim <- min(nrow(df_palavras_reais_arquivo), posicao_na_lista_real + input$n_palavras_contexto_download)
              tempo_recorte_inicio <- df_palavras_reais_arquivo$tempo_inicio_palavra[idx_inicio]; tempo_recorte_fim <- df_palavras_reais_arquivo$tempo_fim_palavra[idx_fim]
            }
          }
          duracao <- tempo_recorte_fim - tempo_recorte_inicio
          if (duracao > 0) {
            caminho_tg_original <- file.path(dirname(linha$caminho_audio), paste0(linha$id_arquivo, ".TextGrid")); if (!file.exists(caminho_tg_original)) { next }
            linhas_tg_original <- readLines(caminho_tg_original, warn = FALSE, encoding = "UTF-8"); tier_words_completo <- parse_textgrid_tier("words", linhas_tg_original); tier_phones_completo <- parse_textgrid_tier("phones", linhas_tg_original)
            palavras_recortadas <- tier_words_completo %>% filter(t1 >= tempo_recorte_inicio & t2 <= tempo_recorte_fim); fones_recortados <- tier_phones_completo %>% filter(t1 >= tempo_recorte_inicio & t2 <= tempo_recorte_fim)
            palavras_normalizadas <- palavras_recortadas %>% mutate(t1 = t1 - tempo_recorte_inicio, t2 = t2 - tempo_recorte_inicio); fones_normalizados <- fones_recortados %>% mutate(t1 = t1 - tempo_recorte_inicio, t2 = t2 - tempo_recorte_inicio)
            nome_arquivo_base <- paste0(linha$id_arquivo, "_", gsub("\\.", "_", format(linha$tempo_inicio_palavra, nsmall = 4))); caminho_wav <- file.path(temp_dir, paste0(nome_arquivo_base, ".wav")); caminho_tg <- file.path(temp_dir, paste0(nome_arquivo_base, ".TextGrid"))
            av::av_audio_convert(audio = linha$caminho_audio, output = caminho_wav, start_time = tempo_recorte_inicio, total_time = duracao); escrever_textgrid_recortado(caminho_tg, palavras_normalizadas, fones_normalizados)
            arquivos_para_zipar <- c(arquivos_para_zipar, caminho_wav, caminho_tg)
          }
        }, error = function(e) { cat("ERRO ao processar linha", i, ":", e$message, "\n") })
      }
      if (length(arquivos_para_zipar) > 0) {
        old_wd <- getwd(); setwd(temp_dir); zip::zip(zipfile = file, files = basename(arquivos_para_zipar)); setwd(old_wd); showNotification("Download concluído!", type = "message")
      } else {
        showNotification("Nenhum clipe válido pôde ser extraído.", type = "error", duration = 10); file.create(file)
      }
    }
  )
}

# --- INICIAR A APLICAÇÃO ---
shinyApp(ui, server)
