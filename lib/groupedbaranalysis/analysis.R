getAnalysisGroupedBarPlot <- function(dataset, refArea, refPeriod, data) { # refPeriod & datasetY hinzugefügt & datasetX zu dataset
    id <- digest(paste0(dataset, refArea), algo="sha1", serialize=FALSE) # datasetX zu dataset

    write.csv(data, file=paste0("www/csv/", id, ".csv"))

    minValues = ''
    q1Values = ''
    meanValues = ''
    q3Values = ''
    maxValues = ''
    medianValues = ''
    # Generates variables for summary for each Dataset
    for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = Amount of measureVariables
        q <- quantile(data[ ,i])
        min <- q[1]
        q1 <- q[2]     
        mean <- q[3]
        q3 <- q[4]
        max <- q[5]
        median <- median(data[ ,i])

        minValues <- append(minValues, min)
        q1Values <- append(q1Values, q1)
        meanValues <- append(meanValues, mean)
        q3Values <- append(q3Values, q3)
        maxValues <- append(maxValues, max)
        medianValues <- append(medianValues, median)
    } 

    meta <- data.frame("n"= nrow(data), "minValues"=minValues, "q1Values"=q1Values, "meanValues"=meanValues, "q3Values"=q3Values, "maxValues"=maxValues, "medianValues"=medianValues) 


    return(list("dataset"=dataset, "refArea"=refArea, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id)) # refPeriod & datasetY hinzugefügt damit Ausgabe möglich & datasetX zu dataset
}


outputPlotGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        csvPath <- paste0("/csv/", analysis$id, ".csv")

        # Genereates Vector of Datasets -> [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
        d <- strsplit(c(d = analysis$dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
        ds = ''
        datasetLabel = ''    
        # Loop over datasets
        for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
             t <- strsplit(c(t=d$d[i]), ":") # teilt Dataset bei : um Prefix und Dataset zu erhalten
             dataset <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
             datasetLabel <- append(datasetLabel, resourceLabels[dataset]) # fügt alle datasetLabel in einem Vector zusammen -> datasetLabel[2] = "Expenditure per student, primary (% of GDP per capita)"
             ds <- append(ds, dataset) # fügt alle Datasetnamen in einem Vector zusammen  
        }
        dataset <- ds # dataset ist Vector bestehend aus allen Datasets [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 

        # Generates refArea Labels which will be used to label plot
        r <- strsplit(c(r = analysis$refArea), ",")
        refAreaLabel = ''
        for (i in 1:length(r$r)) { # 1:length(r$r) = Amount of refAreas
            rAL <- paste0(r$r[i])
            refAreaLabel <- append(refAreaLabel, resourceLabels[rAL])
        }
        refAreaLabels <- paste0(refAreaLabel[2:length(refAreaLabel)])


        if (is.null(analysis$meta$graph)) {
            plotPath <- paste0("plots/", analysis$id, ".svg") # erstellt Pfadname des Plots

            # Plot doesn't exists in directory
            if (!file.exists(paste0("www/", plotPath))) { # wenn nicht auskommentiert, springt gar nicht in File, da Plot schon existiert, aber leer
        
                data <- analysis$data
                x <- data$x
                refArea <- resourceLabels[analysis$refArea] # resourceLabels in /var/shiny-server/www/lsd-analysis/lib/resourceLabels.R -> dort Länder ergänzen

                # Creates Grouped Bar Plot

                # Loop through measureVariables to retrieve observation Values and the measureVariables names
                obsValues = ''
                for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = Amount of measureVariables
                    oV <- data[i]
                    obsValues <- append(obsValues, oV)
                }
                
                # Puts together the Plot Title that will be shown above the plot
                plotTitle = ''
                for (i in 2:length(datasetLabel)) {
                    pT <- datasetLabel[i]
                    if (i == 2) { 
                        plotTitle <- paste(plotTitle, pT, sep="") # if just one Dataset was selected, the Labels won't be separated
                    }
                    else {
                        plotTitle <- paste(plotTitle, pT, sep=" and \n") # multiple Datasets will be separated with an "and"
                    }
                }
               

                # Melts obsValues from CSV file into one column in a Data Frame, which is necessary to output a Grouped Bar Plot
                df = data.frame(refArea=refAreaLabels, obsValues[2:length(obsValues)])
                colnames(df) <- c("refArea", datasetLabel[2:length(datasetLabel)]) # benennt Spaltenname, welche für Plot benötigt werden
                plotData <- melt(df, id=c(df$refArea), id.vars=1)

                # Creates Grouped Bar Plot
                g <- ggplot(plotData, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=plotTitle)) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)

                # TODO: Stacked Plot
                #g <- ggplot(plotData, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="stack") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=plotTitle)) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)

                g <- g + annotate("text", x=Inf, y=Inf, label="270a.info", hjust=1.3, vjust=2, color="#0000E4", size=4)

                ggsave(plot=g, file=paste0("www/", plotPath), width=7, height=7)
            }


            o <- HTML(paste0("
                <img src=\"", urlProtocol(), "//", urlHostname(), "/", plotPath, "\" width=\"100%\"/>
            "))
        }
        else {
            o <- HTML(paste0("
                <img src=\"", gsub("<|>", '', as.character(analysis$meta$graph)), "\" width=\"100%\"/>
            "))
        }

        o <- HTML(paste0(o, "<p id=\"download-csv\"><a href=\"", csvPath , "\">CSV</a></p>"))

        cat(format(o))
    }
}


outputAnalysisSummaryGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
       
        data <- analysis$data 

        # Genereates Vector of Datasets -> [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
        d <- strsplit(c(d = analysis$dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
        ds = ''
        datasetLabel = ''    
        # Loop over datasets
        for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
             t <- strsplit(c(t=d$d[i]), ":") # teilt Dataset bei : um Prefix und Dataset zu erhalten
             dataset <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
             datasetLabel <- append(datasetLabel, resourceLabels[dataset]) # fügt alle datasetLabel in einem Vector zusammen -> datasetLabel[2] = "Expenditure per student, primary (% of GDP per capita)"
             ds <- append(ds, dataset) # fügt alle Datasetnamen in einem Vector zusammen  
        }
        dataset <- ds # dataset ist Vector bestehend aus allen Datasets [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 


        # Creates Summary


        o <- HTML(paste0("

            <table id=\"lsd-analysis-results-quantile\">
                <caption>Analysis results</caption>
                <tbody>
                    
        "))
        # Generates a row for each Dataset and outputs variables for summary
            # analysis$meta$min[i] defined in getAnalysisGroupedBarPlot(...)
        for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = Amount of measureVariables        
            o <- HTML(paste0(o, "
                <tr><td rowspan=4><a href=\"", dataset[i], "\">", datasetLabel[i] ,"</a></td>
                    <th>Min</th><th>Q1</th><th>Mean</th>
                </tr>
                <tr>
                    <td>", analysis$meta$min[i], "</td><td>", analysis$meta$q1[i], "</td><td>", analysis$meta$mean[i], "</td>
                </tr>
                <tr>
                    <th>Q3</th><th>Max</th><th>Median</th>
                </tr>
                <tr>
                    <td>", analysis$meta$q3[i], "</td><td>", analysis$meta$max[i], "</td><td>", analysis$meta$median[i], "</td>
                </tr>
            "))
        }


        o <- HTML(paste0(o, "
                </tbody>
            </table>
            <table id=\"lsd-analysis-results\">
                <tbody>
        "))
        # Generates refArea URIs that will be used in summary as a link -> http://worldbank.270a.info/classification/country/CH
        refArea = ''
        for (i in 1:length(data$refArea)) { # 1:length(data$refArea) = Amount of refAreas
            rA <- paste0(namespaces$wbcountry, data$refArea[i]) # merges namespace from Worldbank and skos:notation refArea
            refArea <- append(refArea, rA)

            if (i == 1) { # adds text "Reference Area" in the row of the first refArea and displays URI and name of all the other refAreas
                o <- HTML(paste0(o, "
                    <tr><th>Reference Areas</th><td><a href=\"", refArea[i+1], "\">", resourceLabels[data$refArea[i]] ,"</a></td></tr>
                "))
            }
            else { # displays URI and name of all the other refAreas
                if (data$refArea[i-1] != data$refArea[i]) { # only displays the same refArea once
                    o <- HTML(paste0(o, "
                            <tr><th></th><td><a href=\"", refArea[i+1], "\">", resourceLabels[data$refArea[i]] ,"</a></td></tr>
                    "))
                }
            }

        }
        o <- HTML(paste0(o, "
                    <tr><th>Reference Period </th><td>", analysis$refPeriod, "</td></tr>
                    <tr><th>N (sample size)</th><td>", nrow(analysis$data), "</td></tr>
                </tbody>
            </table>

            <p id=\"oh-yeah\"><a href=\"", siteURI, "provenance/", analysis$id, "\">Oh yeah?</a></p>
        "))


        cat(format(o))
    }
    else {
        warning <- analysis[["warning"]]
        o <- HTML(warning)

        cat(format(o))
    }
}
