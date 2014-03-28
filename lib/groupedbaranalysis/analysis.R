 # @author         Anna-Barbara Krebs
 # @name           analysis.R  
 # @date           xx.xx.xxxx     
 # @fileOverview   Generates a Grouped Bar Plot and a summary of the results of    
 #                 the Grouped Bar Analysis. 
 #                 
 #          Note:  Some of the code has been adopted from Sarven Capadisli, from
 #                 the files "analysis.R" which are used for the Regression and  
 #                 Time Series Analysis. 

getAnalysisGroupedBarPlot <- function(dataset, refArea, refPeriod, data) { 
    id <- digest(paste0(dataset, refArea), algo="sha1", serialize=FALSE) 

    write.csv(data, file=paste0("www/csv/", id, ".csv"))

    minValues = ''
    q1Values = ''
    meanValues = ''
    q3Values = ''
    maxValues = ''
    medianValues = ''
    # Generates variables for the summary for each dataset
    for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = amount of measureVariables
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

    meta <- data.frame("n"= nrow(data), "minValues"=minValues, "q1Values"=q1Values, "meanValues"=meanValues, "q3Values"=q3Values, "maxValues"=maxValues, "medianValues"=medianValues) # creates data frame of values -> used for SPQRQL Update to store the results of the analysis in the Graph Store


    return(list("dataset"=dataset, "refArea"=refArea, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id))
}


# Generates Grouped Bar Plot
outputPlotGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        csvPath <- paste0("/csv/", analysis$id, ".csv") # creates path name of the csv file wicht contains the analysis results

        # Genereates vector of datasets ->  [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
        d <- strsplit(c(d = analysis$dataset), ",") # splits different datasets in URI at ","
        ds = ''
        datasetLabel = ''    
        # Loop over datasets
        for (i in 1:length(d$d)) { # length(d$d) = amount of Datasets
             t <- strsplit(c(t=d$d[i]), ":") # splits dataset at ":" to  retrieve prefix and dataset
             dataset <- paste0(namespaces[t$t[1]], t$t[2]) # puts namespace in front of dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
             datasetLabel <- append(datasetLabel, resourceLabels[dataset]) # appends all datasetLabels in vector -> datasetLabel[2] = "Expenditure per student, primary (% of GDP per capita)"
             ds <- append(ds, dataset) # appends all dataset names in one vector  
        }
        dataset <- ds # "dataset" -> vector which consists of all datasets in URI [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 

      
        if (is.null(analysis$meta$graph)) {
            plotPath <- paste0("plots/", analysis$id, ".svg") # creates path name of the plot

            # Plot doesn't exists in directory
            if (!file.exists(paste0("www/", plotPath))) { 
        
                data <- analysis$data

                # Generates refArea labels which will be used to label plot -> 'Switzerland'
                rAL = ''
                for (i in 1:length(data$refArea)) { # data$refArea = amount of refAreas
                    rA <- paste0(data$refArea[i])
                    rAL <- append(rAL, resourceLabels[rA])
                }
                refAreaLabel <- paste0(rAL[2:length(rAL)]) #refAreaLabel ->  "" "France" "Switzerland"

                # Loop through measureVariables to retrieve observation values and the measureVariables names
                obsValues = ''
                for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = amount of measureVariables
                    oV <- data[i]
                    obsValues <- append(obsValues, oV)
                }
                
                # Creates the plot title that will be shown above the plot
                plotTitle = ''
                for (i in 2:length(datasetLabel)) {
                    pT <- datasetLabel[i]
                    if (i == 2) { 
                        plotTitle <- paste(plotTitle, pT, sep="") # if just one dataset was selected, the label won't be separated
                    }
                    else {
                        plotTitle <- paste(plotTitle, pT, sep=" and \n") # multiple datasets will be separated with an "and"
                    }
                }

                #TODO: if only one refArea exists in URI, for which no data exists, error message "object 'analysis' not found" appears. But if two refAreas exist is URI, for one of which data exists and for one of which no data exists, it shows a plot of BOTH refArea that both show the values of the refArea for which data exists
                #TODO: e.g. worldbank:SE.XPD.PRIM.PC.ZS/JP/2009 and worldbank:SE.XPD.PRIM.PC.ZS/JP,FR/2009
               

                # Melts obsValues from CSV file into one column in a data frame, which is necessary to output a Grouped Bar Plot
                df = data.frame(refArea=refAreaLabel, obsValues[2:length(obsValues)])
                colnames(df) <- c("refArea", datasetLabel[2:length(datasetLabel)]) # names columns, which will be used for the plot
                plotData <- melt(df, id=c(df$refArea), id.vars=1) # melts data frame in appropriate form, so it can be used for Grouped Bar Plot

                # Creates Grouped Bar Plot
                g <- ggplot(plotData, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=plotTitle)) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)

                # TODO: Creates Stacked Plot
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

# Generates analysis summary
outputAnalysisSummaryGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
       
        data <- analysis$data 

        # Genereates vector of datasets -> [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
        d <- strsplit(c(d = analysis$dataset), ",") # splits different datasets in URI at ",""
        ds = ''
        datasetLabel = ''    
        # Loop over datasets
        for (i in 1:length(d$d)) { # length(d$d) = amount of Datasets
             t <- strsplit(c(t=d$d[i]), ":") # splits dataset at ":" to  retrieve prefix and dataset
             dataset <- paste0(namespaces[t$t[1]], t$t[2]) # puts namespace in front of dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
             datasetLabel <- append(datasetLabel, resourceLabels[dataset]) # appends all datasetLabels in vector -> datasetLabel[2] = "Expenditure per student, primary (% of GDP per capita)"
             ds <- append(ds, dataset) # appends all dataset names in one vector  
        }
        dataset <- ds # "dataset" -> vector which consists of all datasets in URI [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS


        # Creates summary
        o <- HTML(paste0("

            <table id=\"lsd-analysis-results-quantile\">
                <caption>Analysis results</caption>
                <tbody>
                    
        "))
        # Generates a row for each dataset and outputs the variables
            # analysis$meta$min[i] defined in getAnalysisGroupedBarPlot(...)
        for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = amount of measureVariables        
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
        for (i in 1:length(data$refArea)) { # 1:length(data$refArea) = amount of refAreas
            rA <- paste0(namespaces$wbcountry, data$refArea[i]) # merges namespace from Worldbank and skos:notation refArea
            refArea <- append(refArea, rA)

            if (i == 1) { # URI and name of first refArea
                o <- HTML(paste0(o, "
                    <tr><th>Reference Areas</th><td><a href=\"", refArea[i+1], "\">", resourceLabels[data$refArea[i]] ,"</a></td></tr>
                "))
            }
            else { # URI and name of all the other refAreas
                if (data$refArea[i-1] != data$refArea[i]) { # checks whether same refArea has already been outputed -> only displays the same refArea once
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
