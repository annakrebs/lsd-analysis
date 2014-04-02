/**
 * @author         Sarven Capadisli, Anna-Barbara Krebs
 * @name           index.js   
 * @date           xx.xx.xxxx     
 * @fileOverview   Contains the JavaScript functions that are called in the index.html
 */

// sets values in input fields when page is loaded 
$(document).ready(function(){
     var href = window.location.href.match(new RegExp(window.location.protocol + "\/\/" + window.location.hostname + "\/analysis/dev\/([^\/]*)\/([^\/]*)\/([^\.]*).html")); // gets URI

    if(href != null) { // URI contains analysis elements
        switch(href.length) {

            case 3:
                var domainX = href[1].split(":")[0];
                var domainY = href[2].split(":")[0];
                var dX = href[1].split(":")[1];
                var rA = href[2].split(":")[1];

                var datasetX = "http://" + domainX + ".270a.info/dataset/" + dX
                var refArea = "http://" + domainY + ".270a.info/classification/country/" + rA

                $("#datasetX").val(datasetX);
                $("#refArea").val(refArea);

                $(".entry-content").css("background", "url(/theme/default/images/icons/icon_loading.gif) no-repeat 65% 100px");
                break;

            // TODO: Regression Analysis & Grouped Bar Analysis are both case 4
/*            case 4:
                var domainX = href[1].split(":")[0];
                var domainY = href[2].split(":")[0];
                var domainZ = href[3].split(":")[0];
                var dX = href[1].split(":")[1];
                var dY = href[2].split(":")[1];
                var rP = href[3].split(":")[1];

                var datasetX = "http://" + domainX + ".270a.info/dataset/" + dX
                var datasetY = "http://" + domainY + ".270a.info/dataset/" + dY
                var refPeriod = "http://reference.data.gov.uk/id/" + domainZ + "/" + rP

                $("#datasetX").val(datasetX);
                $("#datasetY").val(datasetY);
                $("#refPeriod").val(refPeriod);

                $(".entry-content").css("background", "url(/theme/default/images/icons/icon_loading.gif) no-repeat 65% 100px");
                break;
*/
            // Grouped Bar Analysis
            case 4:
                // sets values of "Datasets" & "Selected Datasets" selection lists according to values in URI
                var datasets = href[1].split(","); // splits datasets at ","
    
                for (var i = 0; i < datasets.length; i++) {
                    var domain = datasets[i].split(":")[0]; // "worldbank"
                    var d = datasets[i].split(":")[1]; // "SE.XPD.PRIM.PC.ZS"

                    var dataset = "http://" + domain + ".270a.info/dataset/" + d
                    $("#datasets").val(dataset); // selects values of URI in "Datasets" selection list -> $("#datasets") = id of select in html
                    var datsetText = $("#datasets option:selected").html(); // gets text of selected dataset -> "Expenditure per student, primary (% of GDP per capita)"

                    var option = document.createElement("option");
                    option.text = datsetText; // -> "Expenditure per student, primary (% of GDP per capita)"
                    option.value = dataset; // -> "http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS"
                    selectedDatasets.add(option); // adds the text of the URI datasets to selection list "Selected Datasets"

                    $("#datasets option:selected").remove(); // removes the text of the URI datasets from selection list "Datasets"
                }

                // sets values of "Reference Areas" & "Selected Reference Areas" selection lists according to values in URI
                var refAreas = href[2].split(","); // splits refAreas at ","

                for (var i = 0; i < refAreas.length; i++) {
                    var rA = refAreas[i]; // "CH"
                    var refArea = "http://worldbank.270a.info/classification/country/" + rA

                    $("#refAreas").val(refArea); // selects values of URI in "Reference Area" selection list 
                    var refAreaText = $("#refAreas option:selected").html(); // gets text of selected refAreas -> "Switzerland"

                    var option = document.createElement("option");
                    option.text = refAreaText; // -> "Switzerland"
                    option.value = refArea; // -> "http://worldbank.270a.info/classification/country/CH"
                    selectedRefAreas.add(option); // adds the text of the URI refAreas to selection list "Selected Reference Areas"

                    $("#refAreas option:selected").remove(); // removes the text of the URI refAreas from selection list "Reference Areas"
                }
                
                // sets "Reference Period" slider & text according to value in URI
                var refPeriod = href[3];
                $("#sliderRefPeriod").val(refPeriod);
                document.getElementById("rP").innerHTML = refPeriod;

                break;


            default:
                break;
        }
    }

    $("#submit").click(function() {
        var datasetX = $('#datasetX').val().match(/http:\/\/([^.]*).270a.info\/dataset\/(.*)/);
        var datasetY = $('#datasetY').val().match(/http:\/\/([^.]*).270a.info\/dataset\/(.*)/);
        var refPeriod = $('#refPeriod').val().match(/http:\/\/reference.data.gov.uk\/id\/([^\/]*)\/(.*)/);

        port = '';
        if (window.location.port.length > 0) { port = ":" + window.location.port }

        window.location.href = window.location.protocol + "//" + window.location.hostname + port + "/analysis/" +
            datasetX[1] + ":" + datasetX[2] + "/" +
            datasetY[1] + ":" + datasetY[2] + "/" +
            refPeriod[1] + ":" + refPeriod[2];
    });

    $('#download-csv a').on('click', function() {
        $("body").removeClass();
    });

});



// Grouped Bar Analysis functions





// displays selected Reference Period



/*$(function() {
    $("#sliderRefPeriod").click( function() { // #select-dataset = id of button in index.html

        alert("test");
        var val = $('#sliderRefPeriod').slider("option", "value");
        //var val = $("#sliderRefPeriod").slider("value");


        //$("#sliderRefPeriod").val('value');
        alert(val);
        //document.getElementById("rP").innerHTML = refPeriod;

        //var val = $("#sliderRefPeriod").slider('value'); // Get value
        //alert(val);
        //alert(document.getElementById("rP").innerHTML);
        //document.getElementById("rP").innerHTML = refPeriod;
    } );
});*/


function showRefPeriod(refPeriod) {
    document.getElementById("rP").innerHTML = refPeriod;
}


// adds datasets to "Selected Datasets" selection list
$(function() {
    $("#select-dataset").click( function() { // #select-dataset = id of button in index.html

        // TODO: adds selected dataset at end of selection list and not at appropriate position

        var selectedDatasets = document.getElementById("selectedDatasets");
        var datasets = document.getElementById("datasets"); // gets the selected dataset of the "Datasets" selection list

        // adds selected datasets from "Dataset" selection list to "Selected Datasets" selection list
        for (var i = 0; i < datasets.options.length; i++) {
            if(datasets.options[i].selected == true) {
                var option = document.createElement("option");
                option.text = datasets.options[i].text; // text of selected dataset -> "Expenditure per student, primary (% of GDP per capita)"
                option.value = datasets.options[i].value; // value of selected dataset -> "http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS"
                selectedDatasets.add(option); // adds the text of the selected dataset to selection list "Selected Datasets"
            }
        }

        // removes added datasets from "Dataset" selection list
        for (var i = datasets.options.length-1; i>=0; i--) {
            if (datasets.options[i].selected) {
                datasets.remove(i);
            }
        }

    } );
});


// removes datasets from "Selected Datasets" selection list
$(function() {
    $("#deselect-dataset").click( function() { // #deselect-dataset = id of button in index.html

        // TODO: adds deselected dataset at end of selection list and not at the original position

        var datasets = document.getElementById("datasets");
        var selectedDatasets = document.getElementById("selectedDatasets"); // gets the selected dataset of the "Selected Datasets" selection list

        // adds selected datasets from "Selected Dataset" selection list to "Datasets" selection list
        for (var i = 0; i < selectedDatasets.options.length; i++) {
            if(selectedDatasets.options[i].selected == true) {
                var option = document.createElement("option");
                option.text = selectedDatasets.options[i].text; // text of selected dataset -> "Expenditure per student, primary (% of GDP per capita)"
                option.value = selectedDatasets.options[i].value; // value of selected dataset -> "http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS"
                datasets.add(option); // adds the text of the selected dataset to selection list "Datasets"
            }
        }

        // removes removed datasets from "Selected Dataset" selection list
        for (var i = selectedDatasets.options.length-1; i>=0; i--) {
            if (selectedDatasets.options[i].selected) {
                selectedDatasets.remove(i);
            }
        }

    } );
});


// adds refAreas to "Selected Reference Areas" selection list
$(function() {
    $("#select-refArea").click( function() { // #select-refArea = id of button in index.html

        // TODO: adds deselected refAreas at end of selection list and not at the original position

        var selectedRefAreas = document.getElementById("selectedRefAreas");
        var refAreas = document.getElementById("refAreas"); // gets the selected refArea of the "Reference Area" selection list

        // adds selected refAreas from "Reference Areas" selection list to "Selected Reference Areas" selection list
        for (var i = 0; i < refAreas.options.length; i++) {
            if(refAreas.options[i].selected == true) {
                var option = document.createElement("option");
                option.text = refAreas.options[i].text; // text of selected refArea -> "Switzerland"
                option.value = refAreas.options[i].value; // value of selected refArea -> "http://worldbank.270a.info/classification/country/CH"
                selectedRefAreas.add(option); // adds the text and the value of the selected dataset to selection list "Selected Datasets"
            }
        }

        // removes added refAreas from "Reference Areas" selction list
        for (var i = refAreas.options.length-1; i>=0; i--) {
            if (refAreas.options[i].selected) {
                refAreas.remove(i);
            }
        }

    } );
});


// removes refAreas from "Selected Reference Areas" selection list
$(function() {
    $("#deselect-refArea").click( function() { // #deselect-refArea = id of button in index.html

        // TODO: adds selected dataset at end of selection list and not at appropriate position

        var refAreas = document.getElementById("refAreas");
        var selectedRefAreas = document.getElementById("selectedRefAreas"); // gets the selected refArea of the "Reference Area" selection list

        // adds selected refAreas from "Selected Reference Areas" selection list to "Reference Areas" selection list
        for (var i = 0; i < selectedRefAreas.options.length; i++) {
            if(selectedRefAreas.options[i].selected == true) {
                var option = document.createElement("option");
                option.text = selectedRefAreas.options[i].text; // text of selected refArea -> "Switzerland"
                option.value = selectedRefAreas.options[i].value; // value of selected refArea -> "http://worldbank.270a.info/classification/country/CH"
                refAreas.add(option); // adds the text of the selected dataset to selection list "Selected Datasets"
            }
        }

        // removes added refArea from "Selected Reference Area" selction list
        for (var i = selectedRefAreas.options.length-1; i>=0; i--) {
            if (selectedRefAreas.options[i].selected) {
                selectedRefAreas.remove(i);
            }
        }

    } );
});


// concatenates all selected values (datasets, refAreas & refPeriod) for URI 
$(function() {
    $("#analyze").click( function() { // #analyze = id of button in index.html

        // concatenates datasets for URI -> 'worldbank:SE.XPD.PRIM.PC.ZS,worldbank:SE.XPD.SECO.PC.ZS'
        var selectedDatasets = document.getElementById("selectedDatasets");
        var datasetsString = "";
        var i;
        for (i = 0; i < selectedDatasets.length; i++) { // concatenates all selected datasets to use in URI
            var dataset = selectedDatasets[i].value;
            var ds = dataset.split("270a.info/dataset/").pop(); // splits string after dataset -> only gets 'SE.XPD.PRIM.PC.ZS' 

            var p = dataset.split("http://").pop(); // cuts off 'http://' at the beginning of the string
            var prefix = p.split(".270a.info/dataset/").shift(); // splits string before prefix -> only gets 'worldbank'

            if(i == selectedDatasets.length - 1) { // concatenates prefix & dataset -> worldbank:SE.XPD.PRIM.PC.ZS
                datasetsString = datasetsString + prefix + ":" + ds // doesn't set "," after last dataset value
            }
            else
                datasetsString = datasetsString + prefix + ":" + ds + ","; // separates refAreas with ","
        }

        // concatenates refArea for URI -> 'CH,US'
        var selectedRefAreas = document.getElementById("selectedRefAreas");
        var refAreasString = "";
        var i;
        for (i = 0; i < selectedRefAreas.length; i++) { // concatenates all selected refAreas to use in URI
            var refArea = selectedRefAreas[i].value;
            var rA = refArea.split("270a.info/classification/country/").pop(); // splits string after country -> only gets 'CH' 

            if(i == selectedRefAreas.length - 1) {
                refAreasString = refAreasString + rA // doesn't set "," after last refArea value
            }
            else
                refAreasString = refAreasString + rA + ","; // separates refAreas with ","
        }

        // refPeriod for URI
        var refPeriodString = document.getElementById("rP").innerHTML;

        // concatenates URI
        window.location.href = window.location.protocol + "//" + window.location.hostname + "/analysis/" + "dev/" + datasetsString + "/" + refAreasString + "/" + refPeriodString;

    } );
});


$(function() {
    $("#clear").click( function() { // #clear = id of button in index.html

        // TODO: just loads initial page, instead of just clearing the values
        window.location.href = window.location.protocol + "//" + window.location.hostname + "/analysis/" + "dev/"

    } );
});

