library(shiny)
library(questionr)
library(ggplot2)
library(purrr)
library(tibble)
library(stringr)

data(hdv2003)
## Recodage de d$clso
hdv2003$clso <- as.character(hdv2003$clso)
hdv2003$clso[hdv2003$clso == "Ne sait pas"] <- "NSP"
## Réordonnancement de hdv2003$clso
hdv2003$clso <- factor(hdv2003$clso, levels=c("Oui", "Non", "NSP"))
tab <- table(hdv2003$sexe, hdv2003$clso)
p_sexe_clso <- chisq.test(tab)$expected/sum(tab)

function(input, output, session) {
  
  ## Fonction de tirage au sort pour proportions
  prop_tirage <- function(size, prob_value, values = TRUE) {
    probs <- c(1 - (prob_value / 100), prob_value / 100)
    tirage <- sample(c("👨", "👩"), size = size, replace =TRUE, prob = probs)
    tab <- table(tirage)
    if (!("👩" %in% names(tab))) {
      nb <- 0
      pourc <- 0
    }
    else {
      nb <- tab[["👩"]]
      pourc <- round(nb / size * 100, 1)
    }
    if (values) return(list(tirage = tirage, pourc = pourc, nb = nb))
    return(pourc)
  }
  
  ## PROPORTION - BIAIS ---------------------------------------------
  
  ## Simulation d'un tirage
  output$prop_biais_sim <- renderUI({
    if (input$prop_biais_rerun == 0) return(HTML("Cliquez sur <em>Générer</em>"))
    if (input$prop_biais_size < 1) return(HTML("Taille de l'échantillon trop petite."))
    isolate({
      size <- input$prop_biais_size
      value <- input$prop_biais_value
    })
    if (is.na(size)) return(NULL)
    out <- ""
    res <- prop_tirage(size, value, values = TRUE)
    max_display_values <- 2000
    values <- head(res$tirage, max_display_values)
    values <- paste0(values, collapse = " ")
    if (size > max_display_values) values <- paste0(values, "...")
    out <- paste0(out, "<h4>Échantillon obtenu</h4>")
    out <- paste0(out, "<div class='echantillon'>", values, "</div>")
    out <- paste0(out, "<p style='text-align: center; margin-top: 2em; font-size: 130%;'>Pourcentage de femmes : <strong>", res$pourc, "%</strong></p>")
    
    HTML(out)
  })
  
  
  ## PROPORTION - SIMULATION -------------------------------------------------
  
  
  ## Simulation de proportions entre 1 et 3 répétitions
  output$prop_sim3 <- renderUI({
    input$prop_rerun
    if (is.na(input$prop_sim)) return(NULL)
    if (input$prop_sim > 3) return(NULL)
    out <- ""
    for (i in 1:input$prop_sim) {
      res <- prop_tirage(input$prop_size, input$prop_value, values = TRUE)
      max_display_values <- 500
      values <- paste0(head(res$tirage, max_display_values), collapse = " ")
      if (input$prop_size > max_display_values) values <- paste0(values, "...")
      out <- paste0(out, "<h4>Tirage ", i, "</h4>")
      out <- paste0(out, "<div class='echantillon echantillon-small'>", values, "</div>")
      out <- paste0(out, "<p style='margin: 1em 0;'>Nombre de femmes : ", res$nb, ", soit <strong>", res$pourc, "%</strong></p>")
    }
    HTML(out)
  })

  tirages <- reactive({
    input$prop_rerun
    replicate(input$prop_sim, prop_tirage(input$prop_size, input$prop_value, values = FALSE))
  })
  
  ## Simulation de proportions pour plus de 10 répétitions
  output$prop_sim <- renderUI({
    input$prop_rerun
    if (input$prop_sim <= 3) return(NULL)
    out <- ""
    pourcentages <- tirages()
    max_display_values <- 300
    pourcentages <- head(pourcentages, max_display_values)
    pourcentages <- paste0(format(pourcentages, digits=3), collapse = ", ")
    if (input$prop_sim > max_display_values) pourcentages <- paste0(pourcentages, "...")
    out <- paste0(out, "<h4>Tirages</h4>")
    out <- paste0(out, "<p>Pourcentage de femmes obtenus :</p>")
    out <- paste0(out, "<div class='echantillon echantillon-small'>", pourcentages, "</div>")
    HTML(out)
  })
  
  ## Histogramme des proportions obtenues
  output$prop_plot <- renderPlot({
    pourcentages <- tibble(pourc = tirages())
    pourc_range <- range(pourcentages)
    pourc_range[1] <- pourc_range[1] - 1
    pourc_range[2] <- pourc_range[2] + 1
    binwidth <- 1
    breaks <- waiver()
    if (input$prop_size < 100) binwidth <- 5
    if (input$prop_size < 20) binwidth <- 10
    if (input$prop_show_curve) {
        g <- ggplot(pourcentages) +
          geom_histogram(aes(x=pourc, y = ..density..), binwidth = binwidth) +
          scale_y_continuous("Proportion de tirages")
    } else {
        g <- ggplot(pourcentages) +
          geom_histogram(aes(x=pourc), binwidth = binwidth) +
          scale_y_continuous("Nombre de tirages")
    }
    if (!is.na(input$prop_ech) && input$prop_ech >= 0 && input$prop_ech <= 100 && input$prop_show_value) {
      g <- g + 
        geom_vline(xintercept = input$prop_ech, color = "blue", size = 0.5, linetype = 2)
      breaks <- c(scales::cbreaks(pourc_range)$breaks, input$prop_ech)
    }
    if (!is.na(input$prop_ech) && input$prop_ech >= 0 && input$prop_ech <= 100 && input$prop_show_zones) {
      gap <- abs(input$prop_value - input$prop_ech)
      x1 <- input$prop_value - gap
      x2 <- input$prop_value + gap
      g <- g +
        geom_vline(xintercept = c(x1, x2), color = "red", size = 0.5, linetype = 2) +
        annotation_raster(rgb(1,0,0,0.2), xmin = -Inf, xmax = x1, ymin = -Inf, ymax = Inf) +
        annotation_raster(rgb(1,0,0,0.2), xmin = x2, xmax = Inf, ymin = -Inf, ymax = Inf)
      breaks <- c(scales::cbreaks(pourc_range)$breaks, x1, x2)
    }
    if (input$prop_show_curve) {
      range <- pourc_range
      prop <- input$prop_value
      sd <- sqrt((prop/100)*(1-prop/100)/input$prop_size) * 100
      g <- g +
        geom_line(stat = "function", fun = function(x) dnorm(x, mean = prop, sd = sd), color = "blue")
    }
    g <- g + xlab("Pourcentage de femmes")
    limits <- if (input$prop_fix_x) c(0, 100) else c(NA, NA)
    g <- g + scale_x_continuous("Pourcentage de femmes", limits = limits, breaks = breaks)
    g
  })
  
  ## Comparaison tirages et échantillon
  output$prop_extr_values <- renderUI({
    gap <- abs(input$prop_value - input$prop_ech)
    x1 <- input$prop_value - gap
    x2 <- input$prop_value + gap
    nb <- sum(tirages() <= x1 | tirages() >= x2)
    pourc <- round(nb / input$prop_sim * 100, 1)
    out <- paste0("<p>Nombre de tirages pour lesquels le pourcentage de femmes est inférieur à ", x1, " ou supérieur à ", x2, " : <strong>", nb, "</strong><br>")
    out <- paste0(out, "Soit en pourcentage des tirages effectués : <strong>", pourc, " %</strong></p>")
    HTML(out)
  })

  ## Affichage test proportions
  output$prop_p <- renderUI({
    p_test <- binom.test(input$prop_ech / 100 * input$prop_size, input$prop_size, input$prop_value / 100)$p.value
    p_test <- signif(p_test, 3)
    out <- paste0("<p>Résultat d'un test de proportion : <strong>p = ", p_test, " (", p_test * 100," %)</strong></p>")
    HTML(out)
  })
  
  
  ## KHI2 - BIAIS -------------------------------------------------
  
  biais_tab <- reactive({
    if (input$biais_rerun==0) return()
    if (is.na(input$biais_ntot)) return()
    probas <- as.vector(p_sexe_clso)
    values <- sample(1:6, input$biais_ntot, replace = TRUE, prob = probas)
    tab <- table(values)
    for (i in as.character(1:6)) {
      if (!(i %in% names(tab))) {
        tab[[i]] <- 0
      }
    }
    tab <- tab[order(names(tab))]
    mat <- matrix(tab, nrow=2, ncol=3)
    rownames(mat) <- rownames(p_sexe_clso)
    colnames(mat) <- colnames(p_sexe_clso)
    as.table(mat)
  })
  
  output$biais_tabEff <- renderTable({
    if (input$biais_rerun==0) return()
    if (is.na(input$biais_ntot)) return()
    out <- biais_tab()
    as.data.frame.matrix(out)
  }, rownames = TRUE, digits = 0)
  
  output$biais_tablprop <- renderTable({
    if (input$biais_rerun==0) return()
    if (is.na(input$biais_ntot)) return()
    out <- lprop(biais_tab(), drop = FALSE)
    out <- out[-nrow(out),]
    out[] <- sprintf(out, fmt = "%.0f %%")
    as.data.frame.matrix(out)
  }, rownames = TRUE, digits = 1)

  output$biais_tabcprop <- renderTable({
    if (input$biais_rerun==0) return()
    if (is.na(input$biais_ntot)) return()
    out <- cprop(biais_tab(), drop = TRUE)
    out <- out[,-ncol(out)]
    out[] <- sprintf(out, fmt = "%.0f %%")
    as.data.frame.matrix(out)
  }, rownames = TRUE, digits = 1)

  output$biais_tabthq <- renderTable({
    if (is.na(input$biais_ntot)) return(NULL)
    p_sexe_clso * input$biais_ntot
  }, rownames = TRUE, digits = 1)
  
     
## KHI2 - SIMULATIONS DU KHI2 --------------------------------

  sim1_tab <- reactive({
    nothing <- input$sim1_run
    tab <- as.table(t(matrix(c(446, 445,8,490,592,19),3,2)))
    rownames(tab) <- c("Homme","Femme")
    colnames(tab) <- c("Oui","Non","NSP")
    tab
  })

  sim1_tabalea <- reactive({
    nothing <- input$sim1_run
    tab <- sim1_tab()
    tab <- chisq.test(tab)$expected
    nb <- nrow(tab)*ncol(tab)
    ntot <- sum(tab)
    probas <- as.vector(tab)/sum(tab)
    result <- lapply(1:4, function(i) {
        vec <- factor(sample(1:nb, ntot,replace=TRUE, prob=probas),levels=1:nb)
        out <- table(vec)
        mat <- matrix(out, nrow=nrow(tab), ncol=ncol(tab))
        rownames(mat) <- c("Homme","Femme")
        colnames(mat) <- c("Oui","Non","NSP")
        as.table(mat)
    })
    result
  })
  
  output$sim1_thq <- renderTable({
    tab <- sim1_tab()
    tab <- round(chisq.test(tab)$expected,1)
    as.data.frame.matrix(tab)
  }, rownames = TRUE, digits=1)  

  output$sim1_obs1 <- renderTable({as.data.frame.matrix(sim1_tabalea()[[1]])}, rownames = TRUE)
  output$sim1_khid1 <- renderText({paste("χ² =", round(chisq.test(sim1_tabalea()[[1]])$statistic,2))})
  output$sim1_obs2 <- renderTable({as.data.frame.matrix(sim1_tabalea()[[2]])}, rownames = TRUE)
  output$sim1_khid2 <- renderText({paste("χ² =", round(chisq.test(sim1_tabalea()[[2]])$statistic,2))})
  output$sim1_obs3 <- renderTable({as.data.frame.matrix(sim1_tabalea()[[3]])}, rownames = TRUE)
  output$sim1_khid3 <- renderText({paste("χ² =", round(chisq.test(sim1_tabalea()[[3]])$statistic,2))})
  output$sim1_obs4 <- renderTable({as.data.frame.matrix(sim1_tabalea()[[4]])}, rownames = TRUE)
  output$sim1_khid4 <- renderText({paste("χ² =", round(chisq.test(sim1_tabalea()[[4]])$statistic,2))})
  
  sim1_val <- reactive({
    if (input$sim1_nb>100000) return()
    require(stats)
    x <- sim1_tab()
    n <- sum(x)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    dimnames(E) <- dimnames(x)
    tmp <- .Call(stats:::C_chisq_sim, sr, sc, input$sim1_nb, E, PACKAGE="stats")
  })
  
  output$sim1_valeurs <- renderText({
    if (input$sim1_nb<5) return()
    tmp <- sim1_val()
    #STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
    #PARAMETER <- NA
    #PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B + 1)
    paste(c(round(tmp,2)), collapse=", ")
  })
  
  output$sim1_valeurs_head <- renderText({
    if (input$sim1_nb<5) return()
    tmp <- sim1_val()
    paste(c(round(head(tmp,10),2)), collapse=", ")
  })

  output$sim1_hist <- renderPlot({
    if (!("Histogramme" %in% input$sim1_opts)) return(NULL)
    tmp <- data.frame(x=sim1_val())
    if ("Courbe" %in% input$sim1_opts) {
        g <- ggplot(tmp, aes(x=x)) +
            geom_histogram(binwidth=1, aes(y=..density..)) + 
            scale_y_continuous("Proportion de simulations") +
            geom_line(stat="function", fun=function(x) dchisq(x, df=2), col="blue")
    }
    else {
        g <- ggplot(tmp) +
            geom_histogram(aes(x = x), binwidth=1) +
            scale_y_continuous("Nombre de simulations")
    }
    g <- g +
         scale_x_continuous("Valeur du χ²", limits = c(0,20), expand = c(0, 0))  
    if ("Valeur obtenue" %in% input$sim1_opts) {
        g <- g + 
          geom_vline(xintercept = 7.06, color = "blue", linetype = 2) +
          scale_x_continuous("Valeur du χ²", breaks = c(0,5,7.06,10,15,20), limits = c(0,20), expand = c(0, 0))
    }
    if ("Valeurs plus extrêmes" %in% input$sim1_opts) {
        g <- g + 
          geom_vline(xintercept = 7.06, color = "red", linetype = 2) +
          annotation_raster(rgb(1,0,0,0.2), xmin = 7.06, xmax = +Inf, ymin = -Inf, ymax = Inf) +
          scale_x_continuous("Valeur du χ²", breaks = c(0,5,7.06,10,15,20), limits = c(0,20), expand = c(0, 0))
    } 
    g
  })

  output$sim1_comp <- renderText({
    if (!("Valeurs plus extrêmes" %in% input$sim1_opts)) return("")
    refval <- 7.06
    tmp <- sim1_val()
    nbsup <- sum(tmp > refval)
    propsup <- round(nbsup/length(tmp)*100, 2)
    paste0("Le χ² de notre tableau observé vaut ", refval, ".<br />",
           "Nombre de valeurs simulées obtenues sup&eacute;rieures &agrave; ",
           refval, " : <strong>", nbsup, "</strong>",
           "<br />", "Soit <strong>", propsup, " %</strong> des valeurs simul&eacute;es obtenues.")
  })
  
  output$sim1_pval <- renderText({
    if (!("Test" %in% input$sim1_opts)) return("")
    tab <- sim1_tab()
    paste0("Le <i>p</i> du test du χ² sur le tableau observé vaut : <strong>", 
           round(chisq.test(tab)$p.value,5), "</strong>.")
  })

## KHI2 - EXERCICES D'APPLICATION SUR HDV 2003 ------------------------
  
  pq_tab <- reactive({
      if (input$pq_varl=="---" || input$pq_varc=="---") return(NULL)
      tmp <- d
      if (input$pq_subset=="Hommes seulement") tmp <- d[d$sexe=="Homme",]
      if (input$pq_subset=="Femmes seulement") tmp <- d[d$sexe=="Femme",]
      table(tmp[,input$pq_varl], tmp[,input$pq_varc])
  })

  output$pq_eff <- renderTable({
    tab <- pq_tab()
    if (!is.null(tab)) tab <- as.data.frame.matrix(tab)
    tab
  }, rownames = TRUE)

  output$pq_rprop <- renderTable({
      if (input$pq_varl=="---" || input$pq_varc=="---") return()
      tab <- rprop(pq_tab())
      tab[] <- paste(round(tab[],1),"%")
      as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$pq_cprop <- renderTable({
      if (input$pq_varl=="---" || input$pq_varc=="---") return()
      tab <- cprop(pq_tab())
      tab[] <- paste(round(tab[],1),"%")
      as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$pq_resid <- renderTable({
      if (input$pq_varl=="---" || input$pq_varc=="---") return()
      tab <- residus(pq_tab())
      as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$pq_mosaic <- renderPlot({
      if (input$pq_varl=="---" || input$pq_varc=="---") return()
      mosaicplot(pq_tab(), shade=TRUE, main="")
  })

  output$pq_khid <- renderText({
      if (input$pq_varl=="---" || input$pq_varc=="---") return("")
      tab <- pq_tab()
      tmp <- chisq.test(tab)
      paste0("χ² du tableau : <strong>", round(tmp$statistic,2), "</strong><br />",
             "Degrés de libertés : <strong>", prod(dim(tab)-1),"</strong><br />",
             "p = <strong>", round(tmp$p.value,5)) 
  })

  observe({
      nothing <- input$pq_inverse
      isolate({varl <- input$pq_varl
               varc <- input$pq_varc})
      updateSelectInput(session, "pq_varl", "Variable en ligne", choices=c("---", d.vars), selected=varc)
      updateSelectInput(session, "pq_varc", "Variable en colonne", choices=c("---", d.vars), selected=varl)
  })
  
}

