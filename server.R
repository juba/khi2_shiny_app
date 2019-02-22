library("shiny")
library("questionr")
library("ggplot2")
library("purrr")
library("tibble")

function(input, output, session) {
  
  
  
  ## PROPORTION ----------------------------------------------------
  
  ## Fonction de tirage au sort pour proportions
  prop_tirage <- function(size, prob_value, values = TRUE) {
    probs <- c(1 - (prob_value / 100), prob_value / 100)
    tirage <- sample(c("H", "F"), size = input$prop_size, replace =TRUE, prob = probs)
    tab <- table(tirage)
    if (!("F" %in% names(tab))) {
      nb <- 0
      pourc <- 0
    }
    else {
      nb <- tab[["F"]]
      pourc <- round(nb / size * 100, 1)
    }
    if (values) return(list(tirage = tirage, pourc = pourc, nb = nb))
    return(pourc)
  }
  
  ## Simulation de proportions entre 1 et 3 répétitions
  output$prop_sim3 <- renderUI({
    input$prop_rerun
    if (is.na(input$prop_sim)) return(NULL)
    if (input$prop_sim > 3) return(NULL)
    out <- HTML("")
    for (i in 1:input$prop_sim) {
      res <- prop_tirage(input$prop_size, input$prop_value, values = TRUE)
      max_display_values <- 500
      values <- paste0(head(res$tirage, max_display_values), collapse = ", ")
      if (input$prop_size > max_display_values) values <- paste0(values, "...")
      out <- paste0(out, HTML("<h3>Tirage ", i, "</h3>"))
      out <- paste0(out, HTML("<textarea style='width: 100%; height: 7em; color: #888;'>", values, "</textarea><br>"))
      out <- paste0(out, HTML("Nombre de femmes : <strong>", res$nb, "</strong><br>"))
      out <- paste0(out, HTML("Pourcentage de femmes : <strong>", res$pourc, "%</strong>"))
    }
    HTML(out)
  })

  tirages <- reactive({
    input$prop_rerun
    replicate(input$prop_sim, prop_tirage(input$prop_size, input$prop_value, values = FALSE))
  })
  
  ## Simulation de proportions entre 4 et 10 répétitions
  output$prop_sim10 <- renderUI({
    input$prop_rerun
    if (input$prop_sim <= 3 || input$prop_sim > 10) return(NULL)
    out <- HTML("")
    pourcentages <- tirages()
    pourcentages <- paste0(format(pourcentages, digits=3), collapse = "%</li><li>")
    out <- paste0(out, HTML("<h3>Tirages</h3>"))
    out <- paste0(out, HTML("Pourcentage de femmes obtenus : <ul><li>", pourcentages, "%</li></ul>"))
    HTML(out)
  })
  
  ## Simulation de proportions pour plus de 10 répétitions
  output$prop_sim <- renderUI({
    input$prop_rerun
    if (input$prop_sim <= 10) return(NULL)
    out <- HTML("")
    pourcentages <- tirages()
    max_display_values <- 300
    pourcentages <- head(pourcentages, max_display_values)
    pourcentages <- paste0(format(pourcentages, digits=3), collapse = ", ")
    if (input$prop_sim > max_display_values) pourcentages <- paste0(pourcentages, "...")
    out <- paste0(out, HTML("<h3>Tirages</h3>"))
    out <- paste0(out, HTML("Pourcentage de femmes obtenus :<br>"))
    out <- paste0(out, "<textarea style='width: 100%; height: 7em; color: #888;'>", pourcentages, "</textarea><br>")
    HTML(out)
  })
  
  ## Histogramme des proportions obtenues
  output$prop_plot <- renderPlot({
    pourcentages <- tibble(pourc = tirages())
    pourc_range <- range(pourcentages)
    pourc_range[1] <- pourc_range[1] - 1
    pourc_range[2] <- pourc_range[2] + 1
    g <- ggplot(pourcentages) +
          geom_histogram(aes(x=pourc), binwidth = 1) +
          scale_x_continuous("Pourcentage de femmes", limits = c(0,100)) +
          scale_y_continuous("Nombre de tirages")
    if(!is.na(input$prop_ech) && input$prop_ech >= 0 && input$prop_ech <= 100) {
      gap <- abs(input$prop_value - input$prop_ech)
      x1 <- input$prop_value - gap
      x2 <- input$prop_value + gap
      g <- g +
        geom_vline(xintercept = c(x1, x2), color = "red", size = 1, linetype = 2) +
        annotation_raster(rgb(1,0,0,0.2), xmin = -Inf, xmax = x1, ymin = -Inf, ymax = Inf) +
        annotation_raster(rgb(1,0,0,0.2), xmin = x2, xmax = Inf, ymin = -Inf, ymax = Inf)
    }
    g
  })
  
  ## Comparaison tirages et échantillon
  output$prop_p <- renderUI({
    gap <- abs(input$prop_value - input$prop_ech)
    x1 <- input$prop_value - gap
    x2 <- input$prop_value + gap
    nb <- sum(tirages() <= x1 | tirages() >= x2)
    pourc <- round(nb / input$prop_sim * 100, 1)
    p_test <- prop.test(input$prop_ech / 100 * input$prop_size, input$prop_size, input$prop_value / 100)$p.value
    p_test <- signif(p_test, 3)
    out <- paste0("Nombre de tirages pour lesquels le pourcentage de femmes est inférieur à ", x1, " ou supérieur à ", x2, " : <strong>", nb, "</strong><br>")
    out <- paste0(out, "Soit en pourcentage des tirages : <strong>", pourc, "%</strong><br>")
    out <- paste0(out, "Résultat d'un test de proportion : <strong>p = ", p_test, "</strong><br>")
    HTML(out)
  })
  
  ## BIAIS ---------------------------------------------------------
  
  biais_tab <- reactive({
    if (input$biais_rerun==0) return()
    isolate({mr <- input$biais_rowmods
             mc <- input$biais_colmods
             nt <- input$biais_ntot})
    nr <- length(mr)
    nc <- length(mc)
    nb <- nr*nc
    vec <- factor(sample(1:nb, nt, replace=TRUE),levels=1:nb)
    tab <- table(vec)
    mat <- matrix(tab, nrow=nr, ncol=nc)
    rownames(mat) <- mr
    colnames(mat) <- mc
    if (input$biais_rerun>1) Sys.sleep(1)
    as.table(mat)
  })
  
  
  output$biais_tabEff <- renderTable({
    if (input$biais_rerun==0) return()
    out <- biais_tab()
    as.data.frame.matrix(out)
  }, rownames = TRUE)
  
  output$biais_tabPourc <- renderTable({
    if (input$biais_rerun==0) return()
    out <- prop.table(biais_tab())*100
    out[] <- sprintf(out, fmt = "%.1f %%")
    as.data.frame.matrix(out)
  }, rownames = TRUE, digits = 1)


  ## INDÉPEDANCE ---------------------------------------------------------
  
  indep_tab <- reactive({
    row.names <- c("Bruns", "Blonds", "Roux")
    col.names <- c("Marrons", "Bleus", "Verts")
    vals <- c(input$indep_v1,
              input$indep_v2,
              input$indep_v3,
              input$indep_v4,
              input$indep_v5,
              input$indep_v6,
              input$indep_v7,
              input$indep_v8,
              input$indep_v9)
    mat <- matrix(as.integer(vals), nrow=length(row.names), ncol=length(col.names),byrow=TRUE)
    rownames(mat) <- row.names
    colnames(mat) <- col.names    
    as.table(mat)
  })
  
  output$indep_tabobs <- renderTable({
    tab <- indep_tab()
    tab <- cbind(tab, Ensemble=apply(tab,1,sum))
    tab <- rbind(tab, Ensemble=apply(tab,2,sum))   
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  output$indep_tril <- renderTable({
    tab <- indep_tab()
    tmp <- data.frame(n=apply(tab, 1, sum))
    rownames(tmp) <- rownames(tab)
    tmp <- cbind(tmp, `%`=paste(round(tmp$n/sum(tmp$n)*100,1),"%"))
    as.data.frame.matrix(tmp)
  }, rownames = TRUE)
 
  output$indep_tric <- renderTable({
    tab <- indep_tab()
    tmp <- data.frame(n=apply(tab, 2, sum))
    rownames(tmp) <- colnames(tab)
    tmp <- cbind(tmp, `%`=paste(round(tmp$n/sum(tmp$n)*100,1),"%"))
    as.data.frame.matrix(tmp)
  }, rownames = TRUE)

  output$indep_tabopl <- renderTable({
    tab <- indep_tab()
    tab <- lprop(tab)    
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  output$indep_tabopc <- renderTable({
    tab <- indep_tab()
    tab <- cprop(tab)    
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
   output$indep_tabPourc <- renderTable({
    tab <- indep_tab()
    tab <- chisq.test(tab)$expected
    tab <- prop(tab)
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  output$indep_tabEff <- renderTable({
    tab <- indep_tab()
    tab <- chisq.test(tab)$expected
    tab <- cbind(tab, Ensemble=apply(tab,1,sum))
    tab <- rbind(tab, Ensemble=apply(tab,2,sum))
    as.data.frame.matrix(round(tab, 1))
  }, rownames = TRUE, digits=1)
  
  output$indep_tabtpl <- renderTable({
    tab <- indep_tab()
    tab <- chisq.test(tab)$expected
    tab <- lprop(tab)    
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$indep_tabtpc <- renderTable({
    tab <- indep_tab()
    tab <- chisq.test(tab)$expected
    tab <- cprop(tab)    
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  

## KHI2 D'UN TABLEAU ARBITRAIRE --------------------------------------

  khid_tab <- reactive({
    row.names <- c("Homme", "Femme")
    col.names <- c("Oui", "Non", "NSP")
    vals <- c(input$khid_v1,
              input$khid_v2,
              input$khid_v3,
              input$khid_v4,
              input$khid_v5,
              input$khid_v6)
    mat <- matrix(as.integer(vals), nrow=length(row.names), ncol=length(col.names),byrow=TRUE)
    rownames(mat) <- row.names
    colnames(mat) <- col.names    
    as.table(mat)
  })
  
  output$khid_obseff <- renderTable({
    tab <- khid_tab()
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  output$khid_obspourc <- renderTable({
    tab <- khid_tab()
    tab <- prop(tab)
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$khid_obspl <- renderTable({
    tab <- khid_tab()
    tab <- rprop(tab)
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$khid_obspc <- renderTable({
    tab <- khid_tab()
    tab <- cprop(tab)
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$khid_theff <- renderTable({
    tab <- khid_tab()
    tab <- round(chisq.test(tab)$expected,1)
    as.data.frame.matrix(tab)
  }, rownames = TRUE, digits=1)

  output$khid_thpourc <- renderTable({
    tab <- khid_tab()
    tab <- prop(chisq.test(tab)$expected)
    tab[] <- paste(round(tab[],1),"%")
    as.data.frame.matrix(tab)
  }, rownames = TRUE)

  output$khid_ecarts <- renderTable({
    tab <- khid_tab()
    tab <- round(tab - chisq.test(tab)$expected,1)
    as.data.frame.matrix(tab)
  }, rownames = TRUE, digits=1)
  
  output$khid_partiels <- renderTable({
    tab <- khid_tab()
    exp <- chisq.test(tab)$expected
    as.data.frame.matrix((tab-exp)^2 / exp)
  }, rownames = TRUE)
  
  output$khid_val <- renderText({
    tab <- khid_tab()
    tryCatch(paste("χ² =", round(chisq.test(tab)$statistic,2)),
             error = function(e) {return("")})
  }) 
     
## SIMULATIONS DU KHI2 ------------------------------------

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
    if ("Histogramme" %in% input$sim1_opts) {
      tmp <- sim1_val()
      if ("Courbe" %in% input$sim1_opts) {
          g <- ggplot(data=data.frame(tmp=tmp), aes(x=tmp)) +
              geom_histogram(binwidth=1, aes(y=..density..)) + 
              scale_y_continuous("Proportion de simulations") +
              geom_line(stat="function", fun=function(x) dchisq(x, df=2), col="red")
      }
      else {
          g <- qplot(tmp, geom="histogram", binwidth=1) + 
                  scale_y_continuous("Nombre de simulations")
      }
      if ("Comparaison" %in% input$sim1_opts) {
          g <- g + 
            geom_vline(xintercept = 7.06, color = "blue", linetype = 2) +
            scale_x_continuous("Valeur du χ²", breaks = c(0,5,7.06,10,15,20), limits = c(0,20))
      } 
      else {
         g <- g +
           scale_x_continuous("Valeur du χ²", limits = c(0,20))           
      }
      g
    }
    else {
      plot.new()
    }
  })

  output$sim1_comp <- renderText({
    if (!("Comparaison" %in% input$sim1_opts)) return("")
    refval <- 7.06
    tmp <- sim1_val()
    nbsup <- sum(tmp > refval)
    propsup <- round(nbsup/length(tmp)*100, 2)
    paste0("Le χ² de notre tableau d'origine valait ", refval, ".<br />",
           "Nombre de valeurs simulées obtenues sup&eacute;rieures &agrave; ",
           refval, " : <strong>", nbsup, "</strong>",
           "<br />", "Soit <strong>", propsup, " %</strong> des valeurs simul&eacute;es obtenues.")
  })
  
  output$sim1_pval <- renderText({
    if (!("p-value" %in% input$sim1_opts)) return("")
    tab <- sim1_tab()
    paste0("Le <i>p</i> du test du χ² sur le tableau d'origine vaut : <strong>", 
           round(chisq.test(tab)$p.value,5), "</strong>.")
  })

## EXERCICES D'APPLICATION SUR HDV 2003 ------------------------------------
  
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

