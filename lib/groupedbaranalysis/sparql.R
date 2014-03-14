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

# GANZER BLOCK HINZUGEFÜGT FÜR VARIABLE ANZAHL DATASETS
    d <- strsplit(c(d = dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
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

#FIXME: xsd:decimal assignment is problematic because not all values are xsd:decimal!
    statsData <- paste0("<", analysisURI, ">")
    for (i in 1:length(data[, 1])) {
        statsData <- paste0(statsData, "
            stats:data [
                a stats:DataRow ;
                stats:refArea \"", data[i, 'refArea'], "\" ;  
                stats:measureX \"", data[i, 'x'], "\"^^xsd:decimal ;
                #measureY hinzugefügt
                stats:measureY \"", data[i, 'y'], "\"^^xsd:decimal 
            ] ;"
        )
    }

    statsSummary <- paste0("<", analysisURI, ">")
    for (i in 1:length(analysis$modelsData[, 1])) {
        statsSummary <- paste0(statsSummary, "
            stats:summary [
                a stats:Summary ;
                stats:n \"", analysis$modelsData[i, 'n'], "\"^^xsd:double
            ] ;"
        )
    }


    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

    plotURI <- paste0(siteURI, "plots/", digest(paste0(dataset, refArea, refPeriod), algo="sha1", serialize=FALSE), ".svg") # refPeriod  & datasetY hinzugefügt & datasetX zu dataset

    sparqlQueryURI <- paste0("<", sparqlEndpoints$stats, "?query=", sparqlQueryStringEncoded, ">")
  

    query <- paste0("
INSERT DATA {
    GRAPH <http://stats.270a.info/graph/analysis> {
        ", sparqlQueryURI, "
            # ZEILE rdfs:label GELÖSCHT UND ERSETZT -> datasetX usw. hinzufügen -> siehe früheren Code
            # ersetzt
            rdfs:label \"SPARQL Query URI to retrieve the data for \"@en . 

        provenance:", analysis$id, "
            a prov:Activity ;
            # ZEILE rdfs:label GELÖSCHT UND ERSETZT -> datasetX usw. hinzufügen -> siehe früheren Code
            # ersetzt
            rdfs:label \"Generated Analysis \"@en ;

            prov:startedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasAssociatedWith <http://csarven.ca/#i> ;
            prov:used ", sparqlQueryURI, " ;
            prov:used <https://github.com/csarven/lsd-analysis> ;
        ")
        if (length(d$d) == 1)
        {
            query <- paste0(query, "
                prov:used <", datasetX, "> ;
            ")
        }
        else if (length(d$d) == 2)
        {
            query <- paste0(query, "
                prov:used <", datasetX, "> ;
                prov:used <", datasetY, "> ;
            ")
        }
        else if (length(d$d) == 3)
        {
            query <- paste0(query, "
                prov:used <", datasetX, "> ;
                prov:used <", datasetY, "> ;
                prov:used <", datasetZ, "> ;
            ")
        }
        query <- paste0(query, "  
            # datasetY GELÖSCHT -> gleich wie Zeile mit datasetX
            #prov:used <", refArea, "> ; # TODO: refArea entfernen

            prov:generated <", analysisURI, "> ;
            dcterms:license <", licenseURI, ">
        .

        <", analysisURI, ">
            a stats:Analysis ;
            a prov:Entity ;
            # ZEILE rdfs:label GELÖSCHT UND ERSETZT -> datasetX usw. hinzufügen -> siehe früheren Code
            # ersetzt            
            rdfs:label \"Analysis of \"@en ;

            prov:wasGeneratedBy provenance:", analysis$id, " ;
            prov:generatedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasDerivedFrom ", sparqlQueryURI, " ;
            prov:wasAttributedTo <", creatorURI, "> ;
            dcterms:creator <", creatorURI, "> ;
            dcterms:license <", licenseURI, "> ;

            stats:graph <", plotURI ,"> ;
        ")
        if (length(d$d) == 1)
        {
            query <- paste0(query, "
                stats:datasetX <", datasetX, "> ;
            ")
        }
        else if (length(d$d) == 2)
        {
            query <- paste0(query, "
                stats:datasetX <", datasetX, "> ;
                stats:datasetY <", datasetY, "> ;
            ")
        }
        else if (length(d$d) == 3)
        {
            query <- paste0(query, "
                stats:datasetX <", datasetX, "> ;
                stats:datasetY <", datasetY, "> ;
                stats:datasetZ <", datasetZ, "> ;
            ")
        }
        query <- paste0(query, "  
            # datasetY GELÖSCHT -> gleich wie Zeile mit datasetX
            #stats:refArea <", refArea, "> ; # TODO: refArea entfernen

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
            stats:dataset ?dataset ; # datasetX zu dataset
            # datasetY GELÖSCHT -> gleich wie Zeile mit datasetX
            #stats:refArea ?refArea ; # TODO: refArea entfernen
            stats:graph ?graph ;
            stats:n ?n ;
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

# GANZER BLOCK HINZUGEFÜGT FÜR VARIABLE ANZAHL DATASETS
    d <- strsplit(c(d = dataset), ",") # teilt unterschiedliche Datasets in URL bei ","
    # print(d$d[1]) # worldbank:SE.XPD.PRIM.PC.ZS
    # print(d$d[2]) # worldbank:SE.XPD.SECO.PC.ZS

    if (length(d$d) == 1) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        print(paste0("datasetX = ", datasetX))

        datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
        domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
        # TODO: Teil später löschen, da Abfrage benötigt -> weiter unten
        # if (datasetNameX != datasetX) {
        endpointX <- sparqlEndpoints[datasetNameX]
    }
    else if (length(d$d) == 2) {
        t <- strsplit(c(t = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
        datasetX <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
        print(paste0("datasetX = ", datasetX))
        t <- strsplit(c(t = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
        datasetY <- paste0(namespaces[t$t[1]], t$t[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
        print(paste0("datasetY = ", datasetY))

        datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
        datasetNameY <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetY, perl=TRUE)
        domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
        domainY <- gsub("http://([^/]*).*", "\\1", datasetY, perl=TRUE)

        # TODO: Teil später löschen, da Abfrage benötigt -> weiter unten
        # if (datasetNameX != datasetX && datasetNameY != datasetY) {
        endpointX <- sparqlEndpoints[datasetNameX]
        endpointY <- sparqlEndpoints[datasetNameX]
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

        datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
        datasetNameY <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetY, perl=TRUE)
        datasetNameZ <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetZ, perl=TRUE)
        domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
        domainY <- gsub("http://([^/]*).*", "\\1", datasetY, perl=TRUE)
        domainZ <- gsub("http://([^/]*).*", "\\1", datasetZ, perl=TRUE)

        # TODO: Teil später löschen, da Abfrage benötigt -> weiter unten
        # if (datasetNameX != datasetX && datasetNameY != datasetY ...) {
        endpointX <- sparqlEndpoints[datasetNameX]
        endpointY <- sparqlEndpoints[datasetNameY]
        endpointZ <- sparqlEndpoints[datasetNameZ]
    }
# ABSCHNITT ENDE


#XXX: Move this to config
    #datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)    # AUSKOMMENTIERT
    #datasetNameY <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetY, perl=TRUE) # datasetY hinzugefügt # AUSKOMMENTIERT
#print(datasetNameX)

    #domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE) # AUSKOMMENTIERT
    #domainY <- gsub("http://([^/]*).*", "\\1", datasetY, perl=TRUE) # datasetY hinzugefügt # AUSKOMMENTIERT
#print(domainX)


    # TODO: ev. noch ergänzen, da auch Möglichkeit, dass nur ein oder zwei Datasets exitieren 
    #if (datasetNameX != datasetX && datasetNameY != datasetY && datasetNameZ != datasetZ) { # ganzer Block hinzugefügt (3 Zeilen & obere 2 Zeilen auskommentiert)
    #TODO: if gestalten, dass für alle Datasets Kombinationen funktioniert
   # if (datasetNameX != datasetX && datasetNameY != datasetY) {
   #     endpointX <- sparqlEndpoints[datasetNameX]
   #     endpointY <- sparqlEndpoints[datasetNameY]
   #     #endpointZ <- sparqlEndpoints[datasetNameZ]

        print(paste0("RefAreaListe: ", refArea))
        
        # Splits refAreas & writes them in Vector
        s <- strsplit(c(s = refArea), ",") # trennt refArea, wo "," sind & schreibt in Vector -> refArea besteht aus allen refAreas in URL

        # TODO: Schleife, die durch refArea geht, je nach dem wie viele refAreas in URL vorhanden sind
        for(i in 1:length(s$s)) {
            print(paste0("Reference Area: ", s$s[i]))
        }  
        
        # TODO: Teil wird nicht benötigt -> Ausgabe in SPARQL Query mit s$s[1] usw.
        refArea1 <- s$s[1] # teilt 1. refArea der refArea1 zu
        refArea2 <- s$s[2] # teilt 2. refArea der refArea2 zu
        print(paste0("REFAREA1: ", refArea1))
        print(paste0("REFAREA2: ", refArea2))


#print(endpointX)

# Query je nach Anzahl der im URL vorhandenen Datasets gestalten
# TODO: SERVICE für datasetZ erstellen -> & alle nötigen Anpassungen zur Darstellung in analysis.R vornehmen
if (length(d$d) == 1) {
    query <- paste0("
        SELECT DISTINCT ?refArea ?x 
    ")
}
else if (length(d$d) == 2) {
    query <- paste0("
        SELECT DISTINCT ?refArea ?x ?y # ?refPeriodX in ?refArea geändert, ?y hinzugefügt
    ")
}
else if (length(d$d) == 3) {
    query <- paste0("
        SELECT DISTINCT ?refArea ?x ?y ?z # ?refPeriodX in ?refArea geändert, ?y hinzugefügt
    ")
}
    query <- paste0(query, "
        WHERE {
            SERVICE <",endpointX,"> {
                SELECT DISTINCT ?refArea ?x # ?refPeriodX in ?refArea geändert
                WHERE {
                    ?observationX qb:dataSet <", datasetX, "> .

                    ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .
                    ?observationX ?propertyRefArea ?refAreaEndpoint . # Zeile hinzugefügt

                    ?observationX ?propertyRefPeriod year:", refPeriod, " . # Zeile hinzugefügt

                    ?propertyMeasureX rdfs:subPropertyOf* sdmx-measure:obsValue .
                    ?observationX ?propertyMeasureX ?x .

                    ?refAreaEndpoint skos:notation* ?refArea . # Zeile hinzugefügt 

                    # wird 1. refArea in URL gefiltert
                    FILTER (?refArea  = '", s$s[1], "' 
    ")
    if (length(s$s) > 1) # existiert mehr als eine refArea in URL -> FILTER wird erweitert
    {
        for(i in 2:length(s$s)) { # liest jede refArea in URL aus & fügt zu FILTER hinzu (query wird ergänzt)
            query <- paste0(query, "
                || ?refArea  = '", s$s[i], "'
            ")
        }
    }
    # schliesst Klammer von FILTER
    query <- paste0(query, ") 

                }
            }
    ")
    if (length(d$d) == 2) {
    query <- paste0(query, "
            SERVICE <",endpointY,"> {
                SELECT DISTINCT ?refArea ?y # ?refPeriodX in ?refArea geändert
                WHERE {
                    ?observationY qb:dataSet <", datasetY, "> .

                    ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .

                    ?observationY ?propertyRefArea ?refAreaEndpoint . # Zeile hinzugefügt

                    ?observationY ?propertyRefPeriod year:", refPeriod, " . # Zeile hinzugefügt

                    ?propertyMeasureY rdfs:subPropertyOf* sdmx-measure:obsValue .
                    ?observationY ?propertyMeasureY ?y .

                    ?refAreaEndpoint skos:notation* ?refArea. # Zeile hinzugefügt 

                    # wird 1. refArea in URL gefiltert
                    FILTER (?refArea  = '", s$s[1], "' 
    ")
    if (length(s$s) > 1) # existiert mehr als eine refArea in URL -> FILTER wird erweitert
    {
        for(i in 2:length(s$s)) { # liest jede refArea in URL aus & fügt zu FILTER hinzu (query wird ergänzt)
            query <- paste0(query, "
                || ?refArea  = '", s$s[i], "'
            ")
        }
    }
    # schliesst Klammer von FILTER
    query <- paste0(query, ") 

                }
            }
    ")
    }
    query <- paste0(query, "
        }
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
