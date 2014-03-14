getAnalysisGroupedBarPlot <- function(dataset, refArea, refPeriod, data) { # refPeriod & datasetY hinzugefügt & datasetX zu dataset
    id <- digest(paste0(dataset, refArea), algo="sha1", serialize=FALSE) # datasetX zu dataset

    write.csv(data, file=paste0("www/csv/", id, ".csv"))

    meta <- data.frame("n"= nrow(data$x))

    return(list("dataset"=dataset, "refArea"=refArea, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id)) # refPeriod & datasetY hinzugefügt damit Ausgabe möglich & datasetX zu dataset
}


outputPlotGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        csvPath <- paste0("/csv/", analysis$id, ".csv")

    # Genereates Vector of Datasets -> [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
    d <- strsplit(c(d = analysis$dataset), ",") # teilt unterschiedliche Datasets in URL bei ","

    ds = ''
    datasetLabel = ''    
    # Looping over datasets
    for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
         t <- strsplit(c(t=d$d[i]), ":") # teilt Dataset bei : um Prefix und Dataset zu erhalten
         dataset <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
         datasetLabel <- append(datasetLabel, resourceLabels[dataset]) # fügt alle datasetLabel in einem Vector zusammen -> datasetLabel[2] = "Expenditure per student, primary (% of GDP per capita)"
         ds <- append(ds, dataset) # fügt alle Datasetnamen in einem Vector zusammen  
    }
    dataset <- ds # dataset ist Vector bestehend aus allen Datasets [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 


        
# TODO: GANZER BLOCK LÖSCHEN
    d <- strsplit(c(d = analysis$dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
    # print(d$d[1]) # worldbank:SE.XPD.PRIM.PC.ZS
    # print(d$d[2]) # worldbank:SE.XPD.SECO.PC.ZS

    if (length(d$d) == 1) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        print(paste0("datasetX = ", datasetX))
        datasetXLabel <- resourceLabels[datasetX]
    }
    else if (length(d$d) == 2) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        print(paste0("datasetX = ", datasetX))
        t <- strsplit(c(t = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
        datasetY <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
        print(paste0("datasetY = ", datasetY))
        datasetXLabel <- resourceLabels[datasetX] # Zeile hinzugefügt
        datasetYLabel <- resourceLabels[datasetY] # Zeile hinzugefügt
    }
    else if (length(d$d) == 3) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        print(paste0("datasetX = ", datasetX))
        t <- strsplit(c(t = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
        datasetY <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
        print(paste0("datasetY = ", datasetY))
        t <- strsplit(c(t = d$d[3]), ":") # teilt 3. Dataset bei : um Prefix und Dataset zu erhalten
        datasetZ <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.TERT.PC.ZS
        print(paste0("datasetZ = ", datasetZ))
        datasetXLabel <- resourceLabels[datasetX] # Zeile hinzugefügt
        datasetYLabel <- resourceLabels[datasetY] # Zeile hinzugefügt
        datasetZLabel <- resourceLabels[datasetZ] # Zeile hinzugefügt
    }
# TODO: BLOCK LÖSCHENT ENDE



        #print(analysis$datasetX)
        print(analysis$dataset)
        print(paste0("DATASETX analysis: ", analysis$dataset[[1]][1]))   
        print(paste0("DATASETY analysis: ", analysis$dataset[[1]][2])) 
        print(analysis$refArea)
        print(analysis$refPeriod)
        print(analysis$data)
        #print(analysis$data$x)

        if (is.null(analysis$meta$graph)) {
            plotPath <- paste0("plots/", analysis$id, ".svg") # erstellt Pfadname des Plots

            print("test1")

            # Plot doesn't exists in directory
            if (!file.exists(paste0("www/", plotPath))) { # wenn nicht auskommentiert, springt gar nicht in File, da Plot schon existiert, aber leer

                print("test2")                

                data <- analysis$data
                x <- data$x
                #datasetXLabel <- resourceLabels[analysis$datasetX]
                #datasetYLabel <- resourceLabels[analysis$datasetY] # Zeile hinzugefügt
                #datasetXLabel <- resourceLabels[datasetX] # Zeile hinzugefügt
                #datasetYLabel <- resourceLabels[datasetY] # Zeile hinzugefügt
                refArea <- resourceLabels[analysis$refArea] # resourceLabels in /var/shiny-server/www/lsd-analysis/lib/resourceLabels.R -> dort Länder ergänzen

                print(paste0("DATASET X Label: ", datasetLabel[2]))
                #print(paste0("ANALYSIS DATASET X: ", analysis$datasetX))
                print("test3")
                print(data)
                print(data[1,2])
                print(paste0("analysis$refArea: ", analysis$refArea))
                print(data$refArea)
                print(paste0("REFAREA1: ", data$refArea[1]))
                print(paste0("DATASETX data: ", data$dataset))
                print(data$x)

# TODO: ABSCHNITT LÖSCHEN
                #dfX = data.frame(refArea=data$refArea, obsValue=data$x, dataset=analysis$datasetX)
                #dfX = data.frame(refArea=data$refArea, obsValue=data$x, dataset=analysis$datasetX, stringsAsFactors=FALSE)
                #dfX = data.table("refArea"=data$refArea, obsValue=data$x, dataset=analysis$datasetX)
                #print(dfX)

                #dfY = data.frame(refArea=data$refArea, obsValue=data$y, dataset=analysis$datasetY)
                #dfY = data.frame(refArea=data$refArea, obsValue=data$y, dataset=analysis$datasetY, stringsAsFactors=FALSE)
                #dfX = data.table("refArea"=data$refArea, obsValue=data$x, dataset=analysis$datasetX)
                #print(dfY)
                #plotdata <- rbind(dfX, dfY) # verbindet Data Frames damit Spalten "refArea", "obsValue", "datasets"
                #print(paste0("Plotdata: ", plotdata))
                #print(paste0("PlotdataREFArea: ", plotdata$refArea))
                #print(paste0("PlotdataOBSVALUE: ", plotdata$obsValue))
                #print(paste0("PlotdataDATASET: ", plotdata$dataset))
                #write.table(plotdata)
# TODO: ABSCHNITT LÖSCHEN ENDE

# TODO: wird ev. nicht benötigt
                # Generates obsValue[2] = obsValueABCDEF & measureVariables[2] = abcdef 
                obsValue = ''
                measureVariables = ''
                for (i in 2:length(data[1, ])) { 
                    oV <- paste0("obsValue", toupper(colnames(data)[i]))
                    obsValue <- append(obsValue, oV)
                    mV <- colnames(data)[i]
                    measureVariables <- append(measureVariables, mV)

                } 
# TODO: ENDE ev. nicht benötigt

# TODO: ist im Moment noch hartcodiert -> data[, 2], data[, 3] muss heraufgezählt werden, wie an anderen Orten
# datasetLabel[2], datasetLabel[3] & datasetLabel[2], " and ", datasetLabel[3] MUSS ALLES Variabel sein
                if (length(d$d) == 2) { # TODO: Länge nicht hartcodieren
                    # Melts obsValues from CSV file into one column in a DataFrame, which is necessary to output a grouped bar plot
                    df = data.frame(refArea=data$refArea, data[, 2], data[, 3])
                    colnames(df) <- c("refArea", datasetLabel[2], datasetLabel[3]) # benennt Spaltenname, welche für Plot benötigt werden
                    plotdata <- melt(df, id=c(df$refArea), id.vars=1)


                    g <- ggplot(plotdata, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=paste0(datasetLabel[2], " and ", datasetLabel[3]))) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
                }




# TODO: Abschnitt löschen
                # Melts obsValues from CSV file into one column in a DataFrame, which is necessary to output a grouped bar plot
                else if (length(d$d) == 1) {
                    df = data.frame(refArea=data$refArea, obsValueX=data$x) # schreibt Daten aus CSV-File in data.frame, damit obsValue Werte in eine Spalte zusammengefügt werden können, anstelle von zwei Spalten
                    colnames(df) <- c("refArea", datasetXLabel) # benennt Spaltenname, welche für Plot benötigt werden
                    plotdata <- melt(df, id=c(df$refArea), id.vars=1) # fügt X und Y Werte aus zwei Spalten in eine Spalte zusammen
                    g <- ggplot(plotdata, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=paste0(datasetXLabel))) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
 # TODO: Abschnitt löschen ENDE               
                }

# TODO: Abschnitt löschen

 #               else if (length(d$d) == 2) {
 #                   df = data.frame(refArea=data$refArea, obsValueX=data$x, obsValueY=data$y) # schreibt Daten aus CSV-File in data.frame, damit #obsValue Werte in eine Spalte zusammengefügt werden können, anstelle von zwei Spalten
 #                   colnames(df) <- c("refArea", datasetXLabel, datasetYLabel) # benennt Spaltenname, welche für Plot benötigt werden
 #                   plotdata <- melt(df, id=c(df$refArea), id.vars=1) # fügt X und Y Werte aus zwei Spalten in eine Spalte zusammen
 #                   print(paste0("DF: ", df))
 #                   print(plotdata)

  #                  g <- ggplot(plotdata, environment = environment(), aes(x=refArea, y=value, fill=variable)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title=paste0(datasetXLabel, " and ", datasetYLabel))) + theme(legend.position="bottom") + geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)
                
#                }

                #g <- ggplot(plotdata, aes(x=plotdata$refArea, y=plotdata$obsValue, fill=plotdata$dataset)) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", fill="Datasets", title="Test"))

                #g <- ggplot(data, environment = environment(), aes(x=data$refArea, y=data$x)) + geom_bar(stat="identity", width=.5, position="dodge") + labs(list(x="Reference Area", y="Value", title=paste0(datasetXLabel, " and ", datasetYLabel, "for ", data$refArea)))

               # g <- ggplot(data, environment = environment(), aes(x=data$refPeriod, y=data$x)) + geom_line(aes=(size=2)) + labs(list(x="Reference Period", y="Value", title=paste0(datasetXLabel, " for ", refArea)))

# TODO: Abschnitt löschen ENDE
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


# TODO: Summary erstellen 
       
        data <- analysis$data 

# GANZER BLOCK HINZUGEFÜGT FÜR VARIABLE ANZAHL DATASETS
    d <- strsplit(c(d = analysis$dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
    if (length(d$d) == 1) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
    }
    else if (length(d$d) == 2) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        t <- strsplit(c(t = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
        datasetY <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
    }
    else if (length(d$d) == 3) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        t <- strsplit(c(t = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
        datasetY <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
        t <- strsplit(c(t = d$d[3]), ":") # teilt 3. Dataset bei : um Prefix und Dataset zu erhalten
        datasetZ <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.TERT.PC.ZS
    }
# BLOCK ENDE

       

        # refArea URIs that will be used in Summary 
        # fügt Namespace von Worldbank mit skos:notation refArea zusammen, damit als Link verwendet werden kann -> http://worldbank.270a.info/classification/country/CH        
        refAreaURI1 <- paste0(namespaces$wbcountry, data$refArea[1])
        refAreaURI2 <- paste0(namespaces$wbcountry, data$refArea[2])
        refAreaURI3 <- paste0(namespaces$wbcountry, data$refArea[3])

#round(x, digits = 0)
        # Variables used for Summary
        qX <- quantile(data$x)
        minX <- round(qX[1], digits=2)
        q1X <- round(qX[2], digits=2)      
        meanX <- round(qX[3], digits=2)
        q3X <- round(qX[4], digits=2) 
        maxX <- round(qX[5], digits=2)
        medianX <- round(median(data$x), digits = 2) # speichert Median für datasetX

        if (length(d$d) == 2) {
            qY <- quantile(data$y)
            minY <- round(qY[1], digits=2)
            q1Y <- round(qY[2], digits=2)    
            meanY <- round(qY[3], digits=2)
            q3Y <- round(qY[4], digits=2)
            maxY <- round(qY[5], digits=2)
            medianY <- round(median(data$y), digits = 2) # speichert Median für datasetX
        }



        # TODO: Summary erstellen
        # TODO: refAreas NICHT in Summary darstellen -> unnötige Information, da schon in Plot vorhanden
        # refAreas will probably not be outputed, as information is available in Plot
        o <- HTML(paste0("

            <table id=\"lsd-analysis-results-quantile\">
                <caption>Analysis results</caption>
                <tbody>
                    <tr><th></th><td colspan='6'>* Values are rounded to two decimal places</caption></td></tr>
                    <tr><th></th><th>Min</th><th>Q1</th><th>Mean</th><th>Q3</th><th>Max</th><th>Median</th></tr>
                    <tr><td><a href=\"", datasetX, "\">", resourceLabels[datasetX] ,"</a></td>
                        <td>", minX, "</td><td>", q1X, "</td><td>", meanX, "</td><td>", q3X, "</td><td>", maxX, "</td><td>", medianX, "</td></tr>
        "))
        if (length(d$d) == 2) {
            o <- HTML(paste0(o, " 
                    <tr><th></th><th>Min</th><th>Q1</th><th>Mean</th><th>Q3</th><th>Max</th><th>Median</th></tr>
                    <tr><td><a href=\"", datasetY, "\">", resourceLabels[datasetY] ,"</a></td>
                        <td>", minY, "</td><td>", q1Y, "</td><td>", meanY, "</td><td>", q3Y, "</td><td>", maxY, "</td><td>", medianY, "</td></tr>
            "))
        }
        o <- HTML(paste0(o, "
                </tbody>
            <table>
            <table id=\"lsd-analysis-results\">
                <tbody>
                    <tr><th>Reference Areas</th><td><a href=\"", refAreaURI1, "\">", resourceLabels[data$refArea[1]] ,"</a></td></tr>
                    <tr><th></th><td><a href=\"", refAreaURI2, "\">", resourceLabels[data$refArea[2]] ,"</a></td></tr>
                    <tr><th></th><td><a href=\"", refAreaURI3, "\">", resourceLabels[data$refArea[3]] ,"</a></td></tr>
                    <tr><th>Reference Period </th><td>", analysis$refPeriod, "</td></tr>
                    <tr><th>N (sample size)</th><td>", nrow(analysis$data), "</td></tr>
                </tbody>
            <table>

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
