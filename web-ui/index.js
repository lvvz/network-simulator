function drawNetwork (nodesJSON, edgesJSON) {
    var nodes = new vis.DataSet(nodesJSON);
    let edges = new vis.DataSet(edgesJSON);
    
    // create a network
    let container = document.getElementById('mynetwork');
    let data = {
	nodes: nodes,
	edges: edges
    };
    let options = {
	autoResize: true,
	height: '100%',
	width: '100%'
    };
    let network = new vis.Network(container, data, options);
}

function drawNetworkFromJSON (result) {
    drawNetwork (result.nodes, result.edges);
}

// create an array with nodes
function getNetwork () {
    $.getJSON ("/nodes", function (nodesJSON) {
	// create an array with edges
	$.getJSON ("/edges", function (edgesJSON) {
	    drawNetwork(nodesJSON, edgesJSON);
	});
    });
}

function customSubmit (id_expr, jsonHandler) {
    $("document").ready(() => {
	$(id_expr).submit(function(event){
	    event.preventDefault();
	    var post_url = $(this).attr("action");
	    var form_data = $(this).serialize();
	    console.log(form_data);
	    
	    $.post( post_url, form_data, function( response ) {
		let result = $.parseJSON(response);
		console.log (result);
		jsonHandler (result);
	    });
	});
    });
}
/*
$( "form" ).on( "submit", function( event ) {
  event.preventDefault();
  console.log( $( this ).serialize() );
});*/
