# COMPASS
#### *Corpus Mapper for Phonetical And Syllabic Structures*
---

Desenvolvido por:
**[Gabriel Magalhães da Silveira]**
<br>*[Universidade Federal de Minas Gerais]*
<br>Belo Horizonte, 2025

---

### Apresentação

**COMPASS** é um projeto em andamento de uma suíte de ferramentas computacionais, projetada para analisar alinhamentos fonéticos gerados pelo **Montreal Forced Aligner (MFA)**. O objetivo é preencher a lacuna entre a saída de um alinhador fonético e a necessidade de buscas baseadas em estruturas complexas, como a sílaba.

O fluxo de trabalho é dividido em três etapas principais, cada uma executada por um script dedicado:

1.  **Estruturação Silábica:** O primeiro script processa os arquivos `.TextGrid` gerados pelo MFA, adicionando um tier de sílabas, com informações de silabificação fonética e tonicidade.
2.  **Indexação e Análise Profunda:** O segundo script compila o corpus em IDs e classifica cada fone (Ataque, Núcleo, Coda) e consolida os arquivos em um banco de dados otimizado (`.rds`), integrando informações de lema e classe gramatical.
3.  **Busca e Exploração Interativa:** O terceiro componente é a interface gráfica (aplicativo Shiny), que permite ao pesquisador explorar o banco de dados de forma visual, realizar buscas complexas e extrair segmentos de áudio e anotações.

---

### Como Citar esta Ferramenta

Para creditar o uso desta ferramenta em trabalhos acadêmicos, por favor, utilize a seguinte citação:

```
SILVEIRA, Gabriel M. da. (2025). COMPASS: Corpus Mapper for Phonetical And Syllabic Structures [Software]. Belo Horizonte: Universidade Federal de Minas Gerais.
```

### Contato

Para dúvidas, sugestões ou colaborações, entre em contato através do e-mail: *[gabriellmagallhaes@gmail.com]*

---
© 2025, [Gabriel Magalhães da Silveira]. Todos os direitos reservados.

