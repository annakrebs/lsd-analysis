prefixes <- paste0("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx#>
PREFIX sdmx-dimension: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX sdmx-measure: <http://purl.org/linked-data/sdmx/2009/measure#>
PREFIX year: <http://reference.data.gov.uk/id/year/>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX stats: <http://stats.270a.info/vocab#>
PREFIX provenance: <", siteURI, "provenance/>
")

sparqlUpdateGroupedBarPlot <- function(analysisURI, dataset, refArea, refPeriod, data, analysis) { # refPeriod & datasetY hinzugefügt & datasetX zu dataset
    sparqlQueryStringEncoded <- URLencode(sparqlQueryStringGroupedBarPlot(dataset, refArea, refPeriod), reserved=TRUE) # refPeriod & datasetY hinzugefügt & datasetX zu dataset

#cat(paste0(data), file=stderr())
#cat(data[,1], file=stderr())


    # Genereates Vector of Datasets -> [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 
    d <- strsplit(c(d = dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
    ds = ''    
    # Looping over datasets
    for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
         t <- strsplit(c(t=d$d[i]), ":") # teilt Dataset bei : um Prefix und Dataset zu erhalten
         dataset <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
         ds <- append(ds, dataset) # fügt alle Datasetnamen in einem Vector zusammen 
    }
    dataset <- ds # dataset ist Vector bestehend aus allen Datasets [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 

    
    # Splits refAreas at "," & writes them in Vector
    s <- strsplit(c(s = refArea), ",") # trennt refArea, wo "," sind & schreibt in Vector -> refArea besteht aus allen refAreas in URL


#FIXME: xsd:decimal assignment is problematic because not all values are xsd:decimal!

    # data[i, 2] -> ist obsValue Wert (z.B. "18") in 1. Spalte der measureVariable "abcdef"
    statsData <- paste0("<", analysisURI, ">")
    for (i in 1:length(data[, 1])) {
        statsData <- paste0(statsData, "
            stats:data [
                a stats:DataRow ;
                stats:refArea <", paste0(namespaces$wbcountry, data[i, 'refArea']), "> ;
 
        ")
        # Loop through measureVariables to get measure Values -> stats:measureABCDEF "23.20144"^^xsd:decimal ;
        for (j in 2:length(data[1, ])) { # gibt measureVariable in Grossbuchstaben aus & hängt passender obsValue an
            statsData <- paste0(statsData, "
                stats:measure \"", data[i, j], "\"^^xsd:decimal ;
            ")
        }
        statsData <- paste0(statsData, "
            ] ;"
        )
    }

    statsSummary <- paste0("<", analysisURI, ">") # Abschnitt hinzugefügt
    for (i in 2:length(analysis$meta[, 1])) {
        statsSummary <- paste0(statsSummary, "
            stats:summary [
                a stats:Summary ;
                stats:dataset <", dataset[i], "> ;
                stats:min \"", analysis$meta$minValues[i], "\"^^xsd:decimal ; 
                stats:q1 \"", analysis$meta$q1Values[i], "\"^^xsd:decimal ;
                stats:mean \"", analysis$meta$meanValues[i], "\"^^xsd:decimal ;
                stats:q3 \"", analysis$meta$q3Values[i], "\"^^xsd:decimal ;
                stats:max \"", analysis$meta$maxValues[i], "\"^^xsd:decimal ;
                stats:median \"", analysis$meta$medianValues[i], "\"^^xsd:decimal ;
            ")       

        statsSummary <- paste0(statsSummary, "            
                stats:n \"", analysis$meta[i, 'n'], "\"^^xsd:double
            ] ;"
        )
    }


    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

    plotURI <- paste0(siteURI, "plots/", digest(paste0(dataset, refArea, refPeriod), algo="sha1", serialize=FALSE), ".svg") # refPeriod  & datasetY hinzugefügt & datasetX zu dataset

    sparqlQueryURI <- paste0("<", sparqlEndpoints$stats, "?query=", sparqlQueryStringEncoded, ">")
 

    # Generates resourceLabels of the Datasets
    # Loop through measureVariables to get resourceLabels of the Datasets -> Expenditure per student, primary (% of GDP per capita)
    for (i in 2:length(data[1, ])) { # gibt measureVariable in Grossbuchstaben aus & hängt passender obsValue an
        if (i == 2) { # is first real dataset [1] -> ""
            rL <- paste(resourceLabels[dataset[i]], sep="") # beim 1. relevanten Dataset wird kein "and" vorangestellt
        }
        else {
            rL <- paste(rL, resourceLabels[dataset[i]], sep=" and ") # trennt mehrere Datasets mit "and"
        }
    } 

    query <- paste0("
INSERT DATA {
    GRAPH <http://stats.270a.info/graph/analysis> {
        ", sparqlQueryURI, "
            rdfs:label \"SPARQL Query URI to retrieve the data for '", rL, "'\"@en .

        provenance:", analysis$id, "
            a prov:Activity ;
            rdfs:label \"Generated Analysis '", rL, "'\"@en ;

            prov:startedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasAssociatedWith <http://csarven.ca/#i> ;
            prov:used ", sparqlQueryURI, " ;
            prov:used <https://github.com/csarven/lsd-analysis> ;
        ")

        # Loop through measureVariables to retrieve all Dataset names -> prov:used <http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS> ;
        for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = Amount of measureVariables
            query <- paste0(query, "
                prov:used <", dataset[i], "> ;
            ")
        }
        # Loop through obsValues to retrieve all refAreas names and adds namespace from Worldbank in order to get URI
        for(i in 1:length(data[, 1])) { # 1:length(data[, 1]) = Amount of obsValues
            if (i == 1) { # adds text "Reference Area" in the row of the first refArea and displays URI and name of all the other refAreas
                query <- paste0(query, "
                    prov:used <", paste0(namespaces$wbcountry, data[i, 'refArea']), "> ;
                ") 
            }
            else { # displays URI and name of all the other refAreas
                if (data[i-1, 'refArea'] != data[i, 'refArea']) { # only displays the same refArea once
                    query <- paste0(query, "
                        prov:used <", paste0(namespaces$wbcountry, data[i, 'refArea']), "> ;
                    ") 
                }
            } 
        }

        query <- paste0(query, "  
            prov:generated <", analysisURI, "> ;
            dcterms:license <", licenseURI, ">
        .

        <", analysisURI, ">
            a stats:Analysis ;
            a prov:Entity ;        
            rdfs:label \"Analysis of '", rL, "'\"@en ;

            prov:wasGeneratedBy provenance:", analysis$id, " ;
            prov:generatedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasDerivedFrom ", sparqlQueryURI, " ;
            prov:wasAttributedTo <", creatorURI, "> ;
            dcterms:creator <", creatorURI, "> ;
            dcterms:license <", licenseURI, "> ;

            stats:graph <", plotURI ,"> ;
        ")

        # Loop through measureVariables to retrieve Dataset names -> stats:datasetABCDEF <http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS> ;
        for (i in 2:length(data[1, ])) { # 2:length(data[1, ]) = Amount of measureVariables
            query <- paste0(query, "
                stats:dataset <", dataset[i], "> ;
            ")
        }
        # Loop through obsValues to retrieve all refAreas names and adds namespace from Worldbank in order to get URI
        for(i in 1:length(data[, 1])) { # 1:length(data[, 1]) = Amount of obsValues
            if (i == 1) { # adds text "Reference Area" in the row of the first refArea and displays URI and name of all the other refAreas
                query <- paste0(query, "
                    stats:refArea <", paste0(namespaces$wbcountry, data[i, 'refArea']), "> ;
                ") 
            }
            else { # displays URI and name of all the other refAreas
                if (data[i-1, 'refArea'] != data[i, 'refArea']) { # only displays the same refArea once
                    query <- paste0(query, "
                        stats:refArea <", paste0(namespaces$wbcountry, data[i, 'refArea']), "> ;
                    ") 
                }
            } 
        }

        query <- paste0(query, "  

            stats:n \"", nrow(data), "\"^^xsd:integer
        .

        ", statsData, "
        .

        ", statsSummary, "
        .
    }
}
")
    q <- paste0(prefixes, query)
    r <- SPARQL(sparqlServiceUpdateURI, update=q, curl_args=list(style="post"))

    return(r)
}



sparqlQueryGetAnalysisSummaryGroupedBarPlot <- function(analysisURI) {


    q <- paste0("
PREFIX stats: <http://stats.270a.info/vocab#>

SELECT *
WHERE {
    GRAPH <http://stats.270a.info/graph/analysis> {
        <", analysisURI, ">
            stats:dataset ?dataset ; 
            stats:refArea ?refArea ; 
            stats:refPeriod ?refPeriod ;
            stats:graph ?graph ;
            stats:n ?n ;
            stats:minValues ?minValues ;
            stats:q1Values ?q1Values ;
            stats:meanValues ?meanValues ;
            stats:q3Values ?q3Values ;
            stats:maxValues ?maxValues ;
            stats:medianValues ?medianValues 
    }
}
");

    r <- SPARQL(sparqlServiceQueryURI, q)
    return(r$results)
}





sparqlQueryGroupedBarPlot <- function(dataset, refArea, refPeriod) { # refPeriod & datasetY hinzugefügt & datasetX zu dataset
    q <- sparqlQueryStringGroupedBarPlot(dataset, refArea, refPeriod) # refPeriod & datasetY hinzugefügt & datasetX zu dataset
    r <- SPARQL(sparqlEndpoints$stats, q)
    return(r$results)
}

# erhält Daten aus sQGroupedBarPlot(....) aus server.R
sparqlQueryStringGroupedBarPlot <- function(dataset, refArea, refPeriod) { # refPeriod & datasetY hinzugefügt & datasetX zu dataset

    d <- strsplit(c(d = dataset), ",") # teilt unterschiedliche Datasets in URL bei ","

    endpoints = ''
    datasetNames = ''
    ds = ''
    
    # Looping over datasets
    for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
         t <- strsplit(c(t=d$d[i]), ":") # teilt Dataset bei : um Prefix und Dataset zu erhalten
         dataset <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
         datasetName <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", dataset, perl=TRUE)
         datasetNames <- append(datasetNames, datasetName) # fügt alle Prefixes in einem Vector zusammen -> "http://worldbank.270a.info/sparql"
         endpoints <- append(endpoints, sparqlEndpoints[datasetName]) # fügt alle Endpointnamen in einem Vector zusammen
         ds <- append(ds, dataset) # fügt alle Datasetnamen in einem Vector zusammen 
    }
    dataset <- ds # dataset ist Vector bestehend aus allen Datasets [2]http://worldbank.../../SE.XPD.PRIM.PC.ZS [3]http://worldbank.../../SE.XPD.SECO.PC.ZS 




    # TODO: ev. noch ergänzen, da auch Möglichkeit, dass nur ein oder zwei Datasets exitieren 
    #if (datasetNameX != datasetX && datasetNameY != datasetY && datasetNameZ != datasetZ) { # ganzer Block hinzugefügt (3 Zeilen & obere 2 Zeilen auskommentiert)
        
    # Splits refAreas at "," & writes them in Vector
    s <- strsplit(c(s = refArea), ",") # trennt refArea, wo "," sind & schreibt in Vector -> refArea besteht aus allen refAreas in URL
    # lengt(s$s) = Amount of refAreas


    # Generates measureVariables according to the amount of Datasets that will be used in SELECT part of query
    measureVariables = ''
    # Loop endpoints
    for (i in 1:length(d$d)) { # length(d$d) = Amount of Datasets
        getRandString <- function(len=10) return(paste(sample(c(letters),len,replace=TRUE),collapse='')) # erstellt zufälligen String der Länge 10
        c = getRandString()

        # "?abcdef ?uvwxyz"
        measureVariables = paste(measureVariables, c, sep=" ?") # Strings werden aneinandergehängt und "?" vorangesetzt        
    } 
    mV <- strsplit(measureVariables, " ") # trennt measureVariables bei " ", damit sie einzeln aufgerufen werden können
                                          # mV[[1]][2] = "?abcdef" # ruft 1. relevanten measureVariable auf 
                                          # mV[[1]][3] = "?uvwxyz" # ruft 2. relevanten measureVariable an 


    # Generates FILTER according to the amount of refAreas that will be used in the query
    refAreaFILTER = "FILTER ("
    # Loop refAreas
    for (i in 1:length(s$s)) { # length(s$s) = Amount of refAreas 
        rA = paste0("?refArea = '", s$s[i], "'")

        if (i == 1) { # "FILTER (?refArea  = 'FR')" # Schleife wird beim 1. Durchlauf betreten, damit keine "||" vorangesetzt wird
            refAreaFILTER = paste(refAreaFILTER, rA, sep="") 
        }
        else {  # "FILTER (?refArea  = 'FR' || ?refArea  = 'US')"
            refAreaFILTER = paste(refAreaFILTER, rA, sep=" || ") # hängt "refArea = 'XX'" FILTER-Teile aneinander und trennt sie mit "||" 
        }                    
    }
    refAreaFILTER = paste(refAreaFILTER, ")", sep="") # closes FILTER bracket 
    # refAreaFILTER = FILTER (?refArea = 'FR' || ?refArea = 'US')    



    # lists all the measureVariables -> "SELECT DISTINCT ?refArea ?abcdef ?uvwxyz"
    query <- paste0("
        SELECT DISTINCT ?refArea ", measureVariables, " 
        WHERE {
    ")
    # Loop endpoints
    # SERVICE Call wird für jeden Dataset durchlaufen
    for(i in 2:length(endpoints)) {  # begins at [2] because [1] is ""
     
        # gibt eine measureVariable aus -> mV[[1]][2]: "?abcdef"
        query <- paste0(query, "
            SERVICE <",endpoints[i],"> {                
                SELECT DISTINCT ?refArea ", mV[[1]][i], "  
                WHERE {
                    ?observation qb:dataSet <", dataset[i], "> .

                    ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .
                    ?observation ?propertyRefArea ?refAreaEndpoint . # Zeile hinzugefügt

                    ?observation ?propertyRefPeriod year:", refPeriod, " . # Zeile hinzugefügt

                    ?propertyMeasure rdfs:subPropertyOf* sdmx-measure:obsValue .
                    ?observation ?propertyMeasure ", mV[[1]][i], " .

                    ?refAreaEndpoint skos:notation* ?refArea . # Zeile hinzugefügt 

                    # outputs FILTER -> FILTER (?refArea = 'FR' || ?refArea = 'US')  
                    ", refAreaFILTER, " 
                }
            }
        ")

    } # closes for-loop of "Loop endpoints"

    query <- paste0(query, "
        } # closes WHERE of first SELECT
    ")
#ORDER BY ?refPeriodX ?x


        q <- paste(prefixes, query)
#        print(q)

        return(q)
 #   } # TODO: Klammer von if (datasetNameX != datasetX && datasetNameY != datasetY) {


#    }
#    else {
#        #TODO: Error: Unrecognized dataset
#    }
}
