function putTable (id, tabledata, columns) {
    var table = new Tabulator("#"+id, {
	layout:"fitColumns",      //fit columns to width of table	
	paginationSize:17,         //allow 7 rows per page of data
	pagination:"local",       //paginate the data
	movableColumns:true,      //allow column order to be changed
	/*
	responsiveLayout:"hide",  //hide columns that dont fit on the table
	tooltips:true,            //show tool tips on cells
	addRowPos:"top",          //when adding a new row, add it to the top of the table
	history:true,             //allow undo and redo actions on the table
	resizableRows:true,       //allow row order to be changed
	initialSort:[             //set the initial sort order of the data
		{column:"name", dir:"asc"},
	],*/
	columns:columns,
    });
    table.setData (tabledata);
}

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
	width: '100%',
	interaction: {selectConnectedEdges: false},
	groups: {
	    defaultNode:{
		color:{
		    highlight:{
			background:'blue',
			borderWidth:3
		    }
		}
	    },
	    shortestPath:{
		color:{
		    highlight:{
			background:'red',
			borderWidth:3
		    }
		}
	    }
	}
    };
    let network = new vis.Network(container, data, options);
    let shortestPaths = undefined;
    function drawShortestPaths (shortest_paths, from, to) {
	shortestPaths = shortest_paths.nodes.map (x=>x);
	shortest_paths.nodes.forEach ((id) => {
	    nodes.update ({id:id, group: "shortestPath"});
	});
	network.setSelection (shortest_paths);
	//nodes.update ({id:from, group: "shortestPath"});
	//nodes.update ({id:to, group: "shortestPath"});
    }
    function networkClick (params) {
	if (shortestPaths != undefined) {
	    shortestPaths.forEach ((id) => {
		console.log ("im doing"+id);
		nodes.update ({id:id, group: "defaultNode"});
	    });
	}
	console.log (params);
	let from = params.nodes[0];
	if (from != undefined) {
	    console.log('click event on '+from);
	    $.getJSON ("rt-cols", function (rt_cols) {
		$.post ("rt-data", {
		    "node-id": from
		}, function (response) {
		    //console.log (response);
		    let rt_table = $.parseJSON(response);
		    putTable("route-table", rt_table, rt_cols);
		});
	    });
	}
    }
    $("#next-node").on ("click", function () {
	console.log (network);
	let from = network.getSelection ().nodes [0];
	//console.log (network.getSelection ());
	network.once ("click", function (params) {
	    console.log ("klik");
	    if (from != undefined) {
		let to = params.nodes[0];
		$.post ("rt-shortest-paths", {
		    "from": from,
		    "to": to
		}, function (response) {
		    let shortest_paths = $.parseJSON(response);
		    console.log (shortest_paths);
		    drawShortestPaths (shortest_paths, from, to);
		});
	    }
	    // network.on ("click", networkClick);
	});
    });
    network.on ("click", networkClick);
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
